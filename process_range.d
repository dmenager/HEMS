module process_range;

import std.stdio, std.format, std.range;
import std.process, std.parallelism;
import core.sys.posix.signal: SIGTERM;

/* Convenient interface for piping input through a series of processes.  Each
 * ProcessRange spawns a thread to consume lines from the input range as they
 * arrive, and is an InputRange itself, so the output can be consumed as such.
 *
 * The command is processed through /bin/bash
 * 
 * Only stdout is processed; stderr from the forked processes just go straight
 * through as normal.  
 * 
 * If the process terminates abnormally, an exception will be thrown.
 */
class ProcessRange(InputRange) if (isInputRange!InputRange) {
	private:
	typeof(pipes.stdout.byLineCopy()) outRange;
	
	public:
	ProcessPipes pipes; // This will generally not be used directly, but they're there if you need it.

	static void input_thread(T1, T2)(T1 input, T2 output) {
		foreach (line; input) {
			output.writeln(line);
			output.flush();
		}
		output.flush();
		output.close();
	}

	this(string command) {
		//pipeShell is basically this, but with "/bin/sh".
		//writef("ProcessRange: '%s'\n", command);
		stdout.flush();
		pipes = pipeProcess([ "/bin/bash", "-c", command ], Redirect.stdin | Redirect.stdout);
		outRange = pipes.stdout.byLineCopy();
	}

	this(InputRange input, string command) {
		//pipeShell is basically this, but with "/bin/sh".
		//writef("ProcessRange: '%s'\n", command);
		stdout.flush();
		pipes = pipeProcess([ "/bin/bash", "-c", command ], Redirect.stdin | Redirect.stdout);
		task!input_thread(input, pipes.stdin).executeInNewThread();
		outRange = pipes.stdout.byLineCopy();
	}
	
	this(string[] argv) {
		stdout.flush();
		pipes = pipeProcess(argv, Redirect.stdin | Redirect.stdout);
		outRange = pipes.stdout.byLineCopy();
	}

	this(InputRange input, string[] argv) {
		stdout.flush();
		pipes = pipeProcess(argv, Redirect.stdin | Redirect.stdout);
		task!input_thread(input, pipes.stdin).executeInNewThread();
		outRange = pipes.stdout.byLineCopy();
	}

	void kill(int signal = SIGTERM) {
		std.process.kill(pipes.pid, signal);
	}
	
	/* There aren't any signals going backwards here, so e.g.
	   the equivalent of "cat hugefile | head -20" won't finish until the cat has.
	   But *literally* calling process("cat hugefile | head -20") will finish
	   early, since the shell still works normally.
	   But I can still test isRunning() before writing to it and manually kill the
	   other process. */
	bool isRunning() {
		//I want an abnormal termination to *stop* propagating through the pipe.
		auto r = tryWait(pipes.pid);
		if (r.terminated && 0 != r.status) {
			throw new Exception((r.status < 0) 
				? format("Process terminated by signal %d", -1 * r.status)
				: format("Process failed with error code %d", r.status));
		}
		return !r.terminated;	
	}

	string front() { return outRange.front(); }
	void popFront() { outRange.popFront(); }
	bool empty() {
		isRunning();//just calling this so we get an exception if the proc failed
		return outRange.empty();
	}

	auto pid() {
		return pipes.pid;
	}

	~this() {
		wait(pipes.pid);
	}
}

auto processRange(InputRange)(InputRange input, string command) {
	return new ProcessRange!InputRange(input, command);
}

auto processRange(InputRange)(InputRange input, string[] argv) {
	return new ProcessRange!InputRange(input, argv);
}

//No input stream, which means no separate thread. Basically just a wrapper around pipeProcess at this point.
auto processRange(string command) {
	return new ProcessRange!(string[])(command);
}

auto processRange(string[] argv) {
	return new ProcessRange!(string[])(argv);
}


unittest {
	import std.array;
	auto bar = ["a", "b", "c"]
		.processRange("echo foo bar baz; cat && echo 1 2 3; echo bogus > /dev/null")
		.processRange("tr [a-z] [A-Z]")
		.join(" ");
	assert("FOO BAR BAZ A B C 1 2 3" == bar);

	//TODO: need tests for the argv versions
}
version(unittest) {
	void main() {}
}

