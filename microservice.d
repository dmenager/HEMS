import std.socket, std.json, std.datetime, std.parallelism, std.stdio, std.uni, std.typecons, std.functional;
import std.conv, std.algorithm, std.format, std.string, std.exception, std.uri, std.array, std.regex;
import core.stdc.signal, core.stdc.stdlib, core.thread.osthread, core.stdc.errno, core.stdc.string;

static ushort DEFAULT_PORT = 26583;
static uint DEFAULT_BACKLOG = 50;
static auto DEFAULT_TIMEOUT = dur!"seconds"(10);//TODO: If it's actively sending packets for more than this, does it time out? If so, increase timeout.
static auto BUFSIZ = 1024;

alias Packet = Tuple!(string, "startline", string[string], "headers", string, "content");
alias PacketCallback = void function(Socket conn, Packet packet);
alias EndpointDelegate = void delegate(Socket conn, string startline, string[string] headers, string content);
alias EndpointFunction = void function(Socket conn, string startline, string[string] headers, string content);
alias CEndpointFunction = void function(void *conn, const char *startline, void *headers, const char *content);

static shared EndpointDelegate[string] endpoints;

// Once set, will stop accepting new connections, but will still finish
// handling any active connections.
static shared bool terminate = 0;
//TODO: Should interrupt the select call, and also unconditionally kill everything after some delay

// Method is assumed to be POST. Protocol is HTTP/1.1
// So if the start line is 'POST uri HTTP/1.1', callback will be called
// The function should either call sendPacket *once*, or throw an HttpException (or any other exception, in which case it'll send a 500 packet)
void register_endpoint(string uri, EndpointDelegate callback) {
	//TODO: should I do any sort of uri encoding?
	uri = uri.strip();
	if (uri in endpoints) {
		throw new Exception(format("Endpoint already exists at %s", uri));
	}
	endpoints[uri] = callback;
}
void register_endpoint(string uri, EndpointFunction callback) {
	register_endpoint(uri, callback.toDelegate);
}
extern(C) void register_endpoint(string uri, CEndpointFunction callback) {
	//TODO: need to indicate which type of callback each endpoint is.
	//For the C ones, run_microservice will need to either convert each argument to the appopriate type, or pass its address as a void*
	assert(0, "Not yet implemented");
}


extern(C) void sendPacketC(void *conn, uint status, const char *content, const char *content_type) {
	string body = fromStringz(content).to!string;
	string type = fromStringz(content_type).to!string;
	sendPacket(*cast(Socket*)conn, status, body, type);
}

// Wrapper function that lets C code read from an associative array
extern(C) const(char)* get_header(const void *headers, const char *name) {
	auto h = *cast(string[string]*)headers;
	auto key = fromStringz(name);
	if (key !in h) return null;//TODO: is this the same as NULL?
	return h[key].toStringz;
}

bool remove_endpoint(string uri) {
	return endpoints.remove(uri);
}

static void error_log(string msg) {
	//TODO: Write to actual log file
	stderr.writef("%s\n", msg);
}

extern (C) void handle_kill(int sig) nothrow @nogc {	
	static count = 0;
	terminate = 1;
	if (++count > 2) { // ^C means terminate gracefully. ^C^C^C means DIE ALREADY
		exit(1);
	}
}

class HttpException : Exception {//Throw this if it's their fault. Throw Exception if it's our fault.
	uint status;
	string[] extra_headers;
	this(uint status, string msg, string[] extra_headers = null, string file = __FILE__, size_t line = __LINE__) @nogc @safe pure nothrow {
		this.status = status;
		this.extra_headers = extra_headers;
		super(msg, file, line, null);
	}
}

void sendPacket(Socket conn, uint status, string content, string content_type = "text/plain", string[] extra_headers = null) {
	//writef("sendPacket(%s, [%s])\n", status, content);
	immutable string[uint] status_msg = [ 100: "Continue", 200: "OK", 400 : "Bad Request", 404: "Not Found", 
		405: "Method Not Allowed", 408: "Request Timeout", 411: "Length Required", 418: "I'm a Teapot", 500 : "Internal Server Error" ];
	if (status !in status_msg) {
		error_log(format("Tried to send unknown status code: %s", status));
		status = 500;
	}

	string[] headers = [
		format("Content-type: %s", content_type),
		format("Content-length: %s", content.length),
	];
	if (extra_headers) headers ~= extra_headers;
	if (100 != status) headers ~= "Connection: close";
	auto response = format("HTTP/1.1 %s %s\r\n%s\r\n\r\n%s",
		status, status_msg[status], headers.join("\r\n"), content);
	//writef("Response = [%s]\n", response);
	if (Socket.ERROR == conn.send(response)) {
		error_log(format("Failed to send %s %s packet.\n", status, status_msg[status]));//TODO: more detailed error message
	}
}

// Decide which microservice the request should be forwarded to
static void run_microservice(Socket conn, Packet packet) {
	auto re = ctRegex!(`^([A-Z]+) (.*) (HTTP\/[0-9]+(?:\.[0-9]+)?)$`,"i");
	auto captures = packet.startline.strip.matchFirst(re);
	enforce(4 == captures.length, new HttpException(400, format("Invalid start line:\n%s\n", packet.startline)));
	if ("POST" != captures[1].toUpper) {
		throw new HttpException("BREW" == captures[1].toUpper ? 418 : 405, "", ["Allow: POST"]);//RFC 2324 compatibility
	}

	//TODO: It's post, so we should be seeing any ?foo=bar stuff. But check for it anyway
	auto uri = captures[2].strip;
	enforce(uri in endpoints, new HttpException(404, ""));
	endpoints[uri](conn, packet.startline, packet.headers, packet.content);
}

// Function should indicate errors by throwing HttpException (or any other Exception, to throw a 500 Internal Server Error)
// TODO: content should probably be a byte[] instead of a string. No requirement that content be valid UTF
// Which in turn means that readChunk needs to read byte[], and only convert to string once the header/content boundary is identified
static void handleConnection(Socket conn, PacketCallback callback, Duration timeout) {
	scope(exit) {
		// Wait for the client to send a close message
		char[64] buf;
		while (0 != conn.receive(buf)) {}
		conn.close();
	}

	// Only sets .startline and .headers in return value
	Packet parseHeaders(string str) {
		auto lines = str.split("\r\n");
		string[string] headers;
		string startline = lines[0];
		if (lines.length > 1) {
			foreach (idx, line; lines[1..$].map!`a.toLower`.array) {
				auto jdx = line.indexOf(":");
				enforce(-1 != jdx, new HttpException(400, format("Invalid header:\n%s", line)));
				headers[ line[0..jdx].strip ] = line[jdx+1..$].strip;
			}
		}
		return Packet(startline, headers, null);
	}

	Packet readPacket() {
		string readChunk(ulong bufsize) {
			//writef("readChunk(%s)\n", bufsize);
			auto buf = new char[bufsize];
			auto n = conn.receive(buf);
			enforce(Socket.ERROR != n, new Exception("Receive failed"));
			return buf[0..n].to!string;
		}

		string buf = readChunk(BUFSIZ);
		if (!buf.length) return Packet(null, null, null);//Empty packet to get accept to stop blocking

		// Double newline indicates that the headers are done. Receive until we reach them.
		// HTTP specifies CRLF in headers
		ptrdiff_t contentIdx;
		while (-1 == (contentIdx = buf.indexOf("\r\n\r\n"))) {
			auto chunk = readChunk(BUFSIZ);
			if (0 == chunk.length) break;
			buf ~= chunk;
		}
		enforce(-1 != contentIdx, new HttpException(400, `HTTP headers never terminated (missing \r\n\r\n)`));
		//writef("Packet: [%s]\n", buf);
		auto packet = parseHeaders(buf[0..contentIdx]);
		packet.content = buf[contentIdx+4..$];
	
		// Client may send one of these if it's about to send a large packet. As an aside, curl thinks that 4038 is large.
		// If we send it a 100 Continue, it'll give us the rest of the packet.
		if ("100-continue" == packet.headers.get("expect", null)) {
			sendPacket(conn, 100, "");
		}
		
		// Fetch remaining content. Prepend any we got in the last chunk.
		enforce("content-length" in packet.headers, new HttpException(411,""));
		auto content_length = packet.headers["content-length"].to!ulong;
	
		if (packet.content.length < content_length) {
			packet.content ~= readChunk(content_length - packet.content.length);
		}
		enforce(packet.content.length == content_length, 
			new HttpException(400, format("Content-length: %s but received %s bytes", content_length, packet.content.length)));
		return packet;
	}

	try {
		conn.setOption(SocketOptionLevel.SOCKET, SocketOption.SNDTIMEO, timeout);
		conn.setOption(SocketOptionLevel.SOCKET, SocketOption.RCVTIMEO, timeout);
		auto packet = readPacket();
		if (null != packet.content) {
			callback(conn, packet);
		}
	}
	catch (HttpException e) { // These may happen if the user screws up
		if (errno) {
			e.msg = format("%s\n%s", strerror(errno).fromStringz.to!string, e.msg);
		}
		//error_log(e.msg);
		sendPacket(conn, e.status, e.msg ~ "\n", "text/plain", e.extra_headers);
	}
	catch (Exception e) { // These should never happen if the network stack is working
		if (errno) {
			if (EAGAIN == errno) errno = ETIMEDOUT;// For a nonblocking recv, EAGAIN means timeout. For some reason.
			e.msg = format("%s\n%s", strerror(errno).fromStringz.to!string, e.msg);
		}
		if (ETIMEDOUT != errno) {
			error_log(e.msg);
			//TODO: Also dump the stack trace
		}
		sendPacket(conn, (errno == ETIMEDOUT) ? 408 : 500, e.msg ~ "\n");
	}
}

// Not reentrant
void runServer(ushort port = DEFAULT_PORT, uint backlog = DEFAULT_BACKLOG, Duration timeout = DEFAULT_TIMEOUT) {// can throw exceptions
	//TODO: if signal handlers already exist, call *those* after mine.
	signal(SIGINT, &handle_kill);
	signal(SIGTERM, &handle_kill);

	Socket server;
	try {
		server = new Socket(AddressFamily.INET, SocketType.STREAM, ProtocolType.TCP);
		scope(exit) {
			server.shutdown(SocketShutdown.BOTH);
			server.close();
		}
		auto addr = new InternetAddress("localhost", port);
		server.bind(addr);
		server.listen(backlog);

		writef("Listening at %s\n", addr);
		auto sockets = new SocketSet();
		while (!terminate) {
			sockets.reset();
			sockets.add(server);
			if (server.select(sockets, sockets, sockets, dur!"msecs"(100)) > 0) {
				auto conn = server.accept();
				auto t = task!handleConnection(conn, &run_microservice, timeout);
				taskPool.put(t);
			}
		}
	}
	catch (Exception e) {
		if (errno) {
			e.msg = format("%s\n%s", strerror(errno).fromStringz.to!string, e.msg);
		}
		error_log(e.msg);
		//TODO: Also dump the stack trace
		throw e;
	}
}

unittest {
	import std.process, std.range;

	auto foo = function void(Socket conn, string startline, string[string] headers, string content) {
		sendPacket(conn, 200, format("Hit /foo endpoint\n%s", content), "text/plain");
	};
	auto bar = function void(Socket conn, string startline, string[string] headers, string content) {
		sendPacket(conn, 200, `{"msg": "Hit /foo/bar endpoint"}`~"\n", "application/json");
	};
	register_endpoint("/foo", foo);
	register_endpoint("/foo/bar", bar);
	//runServer(); // Spawning a thread for the tests, but for real code, just call runServer directly like this
	task!runServer(DEFAULT_PORT, DEFAULT_BACKLOG, dur!"seconds"(1)).executeInNewThread();
	Thread.sleep(dur!"seconds"(1));//A bit of a race condition, but can just restart the test if the server takes forever to start

	const auto commands = [
		[ `curl -i -s -d 42 http://localhost:PORT/foo`, // 200
			"HTTP/1.1 200 OK\r\nContent-type: text/plain\r\nContent-length: 20\r\nConnection: close\r\n\r\nHit /foo endpoint\n42" ],
		[ `curl -i -s -d 42 http://localhost:PORT/foo/bar`, // 200
			"HTTP/1.1 200 OK\r\nContent-type: application/json\r\nContent-length: 33\r\nConnection: close\r\n\r\n{\"msg\": \"Hit /foo/bar endpoint\"}" ],
		[ `curl -i -s -d 42 http://localhost:PORT/baz`, // 404
			"HTTP/1.1 404 Not Found\r\nContent-type: text/plain\r\nContent-length: 1\r\nConnection: close\r\n\r\n" ],
		[ `curl -X GET -i -s -d 42 http://localhost:PORT/foo`, // 405
			"HTTP/1.1 405 Method Not Allowed\r\nContent-type: text/plain\r\nContent-length: 1\r\nAllow: POST\r\nConnection: close\r\n\r\n" ],
		[ `curl -X BREW -i -s -d 42 http://localhost:PORT/foo`, //418
			"HTTP/1.1 418 I'm a Teapot\r\nContent-type: text/plain\r\nContent-length: 1\r\nAllow: POST\r\nConnection: close\r\n\r\n" ],
		[ `echo -e 'POST /foo HTTP/1.1\r\n\r\n' | nc -q0 localhost PORT`, // 411
			"HTTP/1.1 411 Length Required\r\nContent-type: text/plain\r\nContent-length: 1\r\nConnection: close\r\n\r\n" ],
		[ `echo -e '' | nc -q0 localhost PORT`, //400
			"HTTP/1.1 400 Bad Request\r\nContent-type: text/plain\r\nContent-length: 49\r\nConnection: close\r\n\r\nHTTP headers never terminated (missing \\r\\n\\r\\n)" ],
		[ `sleep 2 | nc -q4 localhost PORT`, // 408. Will delay a bit. Testing timeout.
			"HTTP/1.1 408 Request Timeout\r\nContent-type: text/plain\r\nContent-length: 36\r\nConnection: close\r\n\r\nConnection timed out\nReceive failed" ],
		[ format(`seq 1 %s | awk '{print "hello"}' | curl -i -s -d @- http://localhost:PORT/foo`, BUFSIZ), // 200. Big enough packet that readChunk has to be called multiple times.
			"HTTP/1.1 100 Continue\r\nContent-type: text/plain\r\nContent-length: 0\r\n\r\n" ~ // TODO: I'm not happy with this part. It only gets sent if curl sends an Expect header, which it doesn't need to.
			format("HTTP/1.1 200 OK\r\nContent-type: text/plain\r\nContent-length: %s\r\nConnection: close\r\n\r\nHit /foo endpoint\n%s", 
				BUFSIZ * 5 + 18, iota(0,BUFSIZ).map!(a => "hello").join("")) ],
	];

	foreach (cmd, expect; commands.map!`tuple(a[0],a[1])`) {
		cmd = cmd.replaceAll(ctRegex!`PORT`, DEFAULT_PORT.to!string);
		writef("Testing [%s]\n", cmd);
		auto pipes = pipeProcess([ "/bin/bash", "-c", cmd], Redirect.stdin | Redirect.stdout | Redirect.stderr);
		pipes.stdin.close();
		auto status = wait(pipes.pid);
		auto output = pipes.stdout.byLineCopy.join("\n");
		auto error = pipes.stderr.byLineCopy.join("\n");
		assert(0 == status && expect == output && 0 == error.length,
			format("[%s] failed.\nStatus: %s\nExpected [%s]\nGot [%s]\nstderr: [%s]\n",
				cmd, status, expect, output, error));
	}

	// Note: Can't test the 500s. Since those are always indicators of bugs (or a general failure of the network stack),
	// every time I see one, I change the code to make it impossible for it to happen again. But they *do* get sent
	// correctly before the bugs are fixed.
	// I guess I could include instructions for the tester: Pull the ethernet cord in 5..4..3..2..1..

	static if (0) {
		writef("\nHit ^C to terminate, or send further tests via curl and netcat\n");
	}
	else {
		terminate = 1;
	}

	//TODO: Be sure that there's no way a client can keep a connection open indefinitely.
}
version (unittest) {
	void main() {}
}

