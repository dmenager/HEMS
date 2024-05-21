import std.stdio, std.random, std.json, std.typecons, std.format, std.conv, std.algorithm, std.regex,
	std.array, std.range, std.socket, std.parallelism, std.datetime, std.file, std.process, std.zip,
	std.conv, std.path;
import microservice, process_range;

immutable int
	PADDING_BIGBOX = 8,
	PADDING_SMALLBOX = 2,
	PADDING_TEXT = 3,
	PADDING_TREE = 4,
	PADDING_TRIANGLE = 4,
	TRIANGLE_SIZE = 80,
	FONTSIZE = 14,
	VLINE_SHORT = 25,
	VLINE_LONG = 70,
	TEXTALIGN_HACK = 4,
	
	// graph will be scaled down s.t. the largest episode box fits in (MAX_WIDTH, MAX_HEIGHT)
	MAX_WIDTH = 1000,
	MAX_HEIGHT = 600
;
immutable double TEXTSCALE_HACK = 2;

shared double scale_factor = 1.0;
		
auto python_filter = q"EOS
import sys
from bs4 import BeautifulSoup
svg = BeautifulSoup(sys.stdin.read(), 'xml')
print(svg.svg['width'])
print(svg.svg['height'])
print(svg.g)
EOS";

enum Type { SELF, ANCESTOR, PARENT, SIBLING, CHILD, DESCENDENT, NIECE, COUSIN, UNKNOWN };
struct Node {
	string name;
	uint width, height;
	Node*[] children;
	string svg;
	Type type;
}

Node *copy_node(const Node *from) {
	return new Node(from.name, from.width, from.height,
		from.children.map!(a => a.copy_node).array,
		from.svg, from.type);
}

void dump(const Node *node, uint indent = 0) {
	foreach (idx; 0..indent) writef("  ");
	writef("%s\n", node.name);
	foreach (child; node.children) dump(child, indent + 1);
}

//contains everything needed to draw the contents, given the x,y of its top-left.
class Box {
	Node *node;
	uint width, height;
	string name;
	abstract void draw(ref string[] svg_text, int x, int y);
	//TODO: need to add html area tags
}

// Represents a full subtree that isn't worth drawing out even in miniature
class TriangleNode : Box {
	alias pad = PADDING_TRIANGLE;
	uint textwidth, sz;
	this(Node *root) {
		node = root;
		name = node.name;
		textwidth = cast(uint)(FONTSIZE * node.name.length / TEXTSCALE_HACK);
		sz = max(TRIANGLE_SIZE, textwidth);
		width = sz + pad * 2;
		height = sz;
	}
	override void draw(ref string[] svg_text, int x, int y) {
		auto center_x = x + pad + sz/2;
		svg_text ~= `<g><a href="%s"><polygon points="%s,%s %s,%s %s,%s" style="fill:white;stroke:blue;stroke-width:2" />`.format(
			node.name.name2link,
			center_x,     y,
			x + pad,      y + sz, 
			x + pad + sz, y + sz);
		svg_text ~= `	<text x="%s" y="%s" class="triangle" textLength="%spx">%s</text></a></g>`
			.format(center_x, y + sz - FONTSIZE, textwidth, node.name);
	}

}

//subgraphs that we have a particular interest in
class BigNode : Box {
	this(Node *root) {
		node = root;
		name = node.name;
		width = node.width + PADDING_BIGBOX * 2;
		height = node.height + PADDING_BIGBOX * 2;
	}
	override void draw(ref string[] svg_text, int x, int y) {
		//just a simple box around the name
		auto r = 10;//curve of the corners
		auto box_w = width - 2 * PADDING_BIGBOX;
		auto box_h = height - 2 * PADDING_BIGBOX;
		auto color = (Type.SELF == node.type) ? "rgb(128,0,255)" : "rgb(0,0,255)";
		auto weight = (Type.SELF == node.type) ? 6 : 2;
		svg_text ~= `<g><a href="%s"><rect width="%s" height="%s" x="%s" y="%s" rx="%s" ry="%s" style="fill:rgb(255,255,255);stroke-width:%s;stroke:%s" />`
			.format(node.name.name2link, box_w, box_h, x + PADDING_BIGBOX, y + PADDING_BIGBOX, r, r, weight, color);
		svg_text ~= `	<text x="%s" y="%s" class="big">%s</text>`
			.format(x + PADDING_BIGBOX + FONTSIZE, y, node.name);
		svg_text ~= `	<g transform="translate(%s %s)">%s</g>`
			.format(x + PADDING_BIGBOX, y + PADDING_BIGBOX, node.svg);
		svg_text ~= "</a></g>";
	}
}

//relatively boring subgraphs
class SmallNode : Box {
	uint textwidth;
	this(Node *root) {
		node = root;
		textwidth = cast(uint)(FONTSIZE * node.name.length / TEXTSCALE_HACK);

		width = 2 * PADDING_SMALLBOX + 2 * PADDING_TEXT + textwidth;//TODO: this is an estimate of the text width,height.
		height = PADDING_SMALLBOX * 2 + FONTSIZE;
	}

	override void draw(ref string[] svg_text, int x, int y) {
		//just a simple box around the name
		auto r = 4;//curve of the corners
		//TODO: need to define .small in svg's style tag
		auto box_w = width - 2 * PADDING_SMALLBOX;
		auto box_h = height - 2 * PADDING_SMALLBOX;
		auto center_x = x + width / 2;
		auto center_y = y + height / 2;
		auto style="fill:rgb(255,255,255);stroke-width:1;stroke:blue";
		svg_text ~= `<g><a href="%s"><rect width="%s" height="%s" x="%s" y="%s" rx="%s" ry="%s" style="%s" />`
			.format(node.name.name2link, box_w, box_h, x + PADDING_SMALLBOX, y + PADDING_SMALLBOX, r, r, style);
		//svg_text ~= `	<text font-size="%spx" x="%s" y="%s" class="smallname" text-anchor="middle" textLength="%s">%s</text></a></g>`
		svg_text ~= `	<text x="%s" y="%s" class="small" textLength="%spx">%s</text></a></g>`
			.format(center_x, center_y + TEXTALIGN_HACK, textwidth, node.name);
	}
}

alias Subgraph = Tuple!(string, "graph", uint, "width", uint, "height");
Subgraph readGraph(string name) {
	//auto r = [ "dot", "-Tsvg", "episodes/%s.dot" ].execute;
	//if (0 != r) return "ERROR";
	string graph = "TEMP";
	uint width = 0, height = 0;
	//TODO: real values
	return Subgraph(graph, width, height);
}

string name2link(string name) {
	return "visualization?node=%s".format(name);
}

void drawline(ref string[] svg_text, int x0, int y0, int x1, int y1, string color="black") {
	auto linestyle = "stroke:%s;stroke-width:1".format(color);
	svg_text ~= `<line x1="%s" y1="%s" x2="%s" y2="%s" style="%s" />`.format(x0, y0, x1, y1, linestyle);
}

bool is_big(Node *node) {
	return [ Type.SELF, Type.ANCESTOR, Type.PARENT, Type.SIBLING, Type.CHILD ].canFind(node.type);
}

bool is_triangle(Node *node) {
	return [ Type.COUSIN ].canFind(node.type);
}

class TreeBox {
	Node *root;
	Box nodebox;
	TreeBox[] children;
	uint width, height;
	uint vline_length;

	this(Node *root) {
		this.root = root;
		if (is_triangle(root)) {
			nodebox = new TriangleNode(root);
			children = null;
		}
		else {
			nodebox = is_big(root) ? new BigNode(root) : new SmallNode(root);
			children = root.children.map!(a => new TreeBox(a)).array;
		}
		vline_length = is_big(root) ? VLINE_LONG : VLINE_SHORT;

		width = max(nodebox.width, children.map!(a => a.width).sum) + PADDING_TREE * 2;
		height = nodebox.height + 2 * vline_length;
		if (children.length) height += children.map!(a => a.height).maxElement;
		//TODO: maybe use longer vertical lines if bigboxes are involved
	}

	void draw(ref string[] svg_text, int x, int y) {
		auto centerx = x + width / 2;
		int nodex = centerx - nodebox.width / 2;
		auto childx = centerx - children.map!(a => a.width).sum / 2;
		assert(childx >= 0 && nodex >= 0);

		nodebox.draw(svg_text, nodex, y);
		if (!children) return;

		y += nodebox.height + vline_length;
		int minx = childx + children[0].width / 2;
		foreach (child; children) {
			if (children.length > 1) {
				//the lines below the horizontal line
				auto center = childx + child.width / 2;
				auto y1 = y + vline_length + (is_big(child.root) ? PADDING_BIGBOX : PADDING_SMALLBOX);
				drawline(svg_text, center, y, center, y1);
			}
			child.draw(svg_text, childx, y + vline_length);
			childx += child.width;
		}
		int maxx = childx - children[$-1].width / 2;

		auto y0 = y - vline_length - (is_big(root) ? PADDING_BIGBOX : PADDING_SMALLBOX);
		if (1 == children.length) {//single vertical edge
			auto y1 = y + vline_length;
			drawline(svg_text, minx, y0, minx, y1);
		}
		else {//upper vertical line (lower vertical lines already drawn), horizontal line
			drawline(svg_text, centerx, y0, centerx, y);
			drawline(svg_text, minx, y, maxx, y);
		}
	}
}


void focus(Node *root, Node *target) {
	//return true if `name` is this node or an ancestor
	//descendent is 0 for non-descendents and dist(node, target) for descendents
	bool aux(Node *node, uint descendent) {
		if (node == target) {
			node.type = Type.SELF;
			foreach (child; node.children) {
				aux(child, 1);
			}
			return 1;
		}

		bool ancestor = 0;
		foreach (child; node.children) {
			ancestor |= aux(child, descendent ? descendent + 1 : 0);
		}

		void mark_nieces(Node *node) {
			node.type = Type.NIECE;
			if (!node.children) return;
			foreach (child; node.children) mark_nieces(child);
		}

		if (ancestor) {
			node.type = Type.ANCESTOR;
			if (node.children.map!(a => a == target).sum) { //parent
				node.type = Type.PARENT;
				foreach (child; node.children) {
					if (Type.SELF != child.type) {
						mark_nieces(child);
						child.type = Type.SIBLING;
					}
				}
			}
			return 1;
		}
		else {
			node.type = (0 == descendent) ? Type.COUSIN
				: (1 == descendent) ? Type.CHILD
				: Type.DESCENDENT;
			return 0;
		}
	}
	aux(root, 0);
}

auto create_graph(uint nlayers) {
	uint node_idx = 0;
	Node* create_node(uint layer) {
		auto nchildren = uniform!"[]"(1,7);
		auto name = "%s".format(node_idx++);
		auto children = (nlayers == layer + 1) ? null : iota(0, nchildren).map!(a => create_node(layer + 1)).array;
		auto svg = null;//TODO: something else
		//TODO: get width, height from svg
		//return new Node(name, uniform(600,800), uniform(400,800), children, svg);
		return new Node(name, uniform(300,400), uniform(200,400), children, svg);
	}
	return create_node(0);
}
	
Node *find_target(Node *node, string target_name) {
	if (node.name == target_name) return node;
	foreach (child; node.children) {
		auto r = find_target(child, target_name);
		if (r !is null) return r;
	}
	return null;
}

string display_graph(const Node *tree, string focus_node) {
	auto root = tree.copy_node;
			
	auto target = find_target(root, focus_node);
	if (target is null) {
		return "<!DOCTYPE html>\n<html><body>Unrecognized node label</body></html>\n";
	}
	focus(root, target);//NOTE: Could crash if those don't exist. But they usually will, and this ijust a quick test

	auto box = new TreeBox(root);
	string[] svg_text;
	box.draw(svg_text, 0, PADDING_BIGBOX + FONTSIZE);

	uint width = 2000;
	uint height = cast(uint)(box.height / cast(double)box.width * width);
	
	auto svg_header = [
	//	`<?xml version="1.0" encoding="UTF-8" standalone="no"?>`,
	//	`<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">`,
		`<!DOCTYPE html>`,
		`<html>`,
		`<body>`,
		`<svg x="0" y="0" width="%spx" height="%spx" viewBox="0.00 0.00 %.2f %.2f" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">`.format(width, height, box.width, box.height),
		`<style>`,
		`	text.small {`,
		`		text-anchor: middle;`,
		`		font-family: Courier, monospace;`,
		`		font-size: %spx;`.format(FONTSIZE),
		`	}`,
		`	text.big {`,
		`		text-anchor: left;`,
		`		font-family: Courier, monospace;`,
		`		font-size: %spx;`.format(FONTSIZE),
		`	}`,
		`	text.triangle {`,
		`		text-anchor: middle;`,
		`		font-family: Courier, monospace;`,
		`		font-size: %spx;`.format(FONTSIZE),
		`	}`,
		`</style>`,
		`<rect width="100%" height="100%" x="0" y="0" fill="white" />`,
		`<g transform="scale(%.6f %.6f)">`.format(scale_factor, scale_factor)
	];

	string output = chain(svg_header, svg_text).join("\n");
	output ~= "</g>\n</svg>\n</body>\n</html>\n";

	//stderr.writef("Box: %s, %s\n", box.width, box.height);
	//stderr.writef(" Px: %s, %s\n", width, height);
	//TODO: Still a bit wide. Maybe display ancestors and children as big boxes, siblings and nieces as small boxes, and cousins as either dots or just replace the entire trees with ellipses
	//dots will probably work if we have a PADDING_DOTBOX and PADDING_DOTTREE to keep it *super*-compressed
	//Do that after getting bigbox working, though.
	//Instead of ellipses, a triangle with a node count and/or observation count in it. Clicking the triangle gets the root node

	return output;
}

Node* read_graph(string zipfile) {
	auto zip = new ZipArchive(zipfile.read);
	string[int] dot;
	int[][int] edges;//directed

	foreach (name, am; zip.directory) {
		writef("%s\n", name);
		if ("eltm-struct.dot" == name.baseName) {
			zip.expand(am);
			string s = cast(string)am.expandedData.idup;
			foreach (line; s.split('\n')) {
				auto m = line.matchFirst(`^(\d+) -> (\d+);$`);
				if (!m) continue;
				auto src = m[1].to!int;
				auto dst = m[2].to!int;
				if (src !in edges) edges[src] = null;
				edges[src] ~= dst;
			}
		}
		else {
			auto m = name.baseName.matchFirst(`^EPISODE-(\d+).dot$`);
			if (!m) continue;
			auto n = m[1].to!uint;
			zip.expand(am);
			dot[n] = cast(string)am.expandedData.idup;
		}
	}

	// find root
	int root_idx = -1;
	foreach (node; dot.keys) {
		bool is_child = 0;
		foreach (src, lst; edges) {
			foreach (dst; lst) {
				if (dst == node) is_child = 1;
			}
		}
		if (!is_child) {
			assert(-1 == root_idx, "Multiple roots: %s and %s\n".format(root_idx, node));
			root_idx = node;
		}
	}
	assert(-1 != root_idx, "No root node");
	
	uint max_width = 0, max_height = 0;
	Node *construct(int idx) {
		auto pipeline = dot[idx].split('\n')
			.processRange(["dot", "-Tsvg"])
			.processRange(["python3", "-c", python_filter])
		;

		auto pop_pt() {
			auto s = pipeline.front;
			pipeline.popFront;
			assert("pt" == s[$-2..$], "Expected number in form of 1234pt. Received: `%s`".format(s));
			return s[0..$-2].to!uint;
		}

		auto width = pop_pt;
		auto height = pop_pt;
		max_width = max(width, max_width);
		max_height = max(height, max_height);
		auto svg = pipeline.join("\n\t\t");//TODO: add leading tabs as well.

		auto children = (idx in edges) ? edges[idx].map!(a => construct(a)).array : null;

		return new Node(idx.to!string, width, height, children, svg, Type.UNKNOWN);
	}

	auto root = construct(root_idx);
	scale_factor = min(cast(double)MAX_WIDTH / max_width, cast(double)MAX_HEIGHT / max_height);
	return root;
}

void main(string[] argv) {
	//TODO: parse argv properly. permit --port
	auto 

	auto root = read_graph(argv[1]);

	void process_request(Socket conn, string startline, string[string] headers, string[string] args, string content) {
		string target = ("node" !in args) ? root.name : args["node"];
		auto html = display_graph(root, target);
		sendPacket(conn, 200, html, "text/html");
	}

	register_endpoint("/visualization", &process_request);
	task!runServer(DEFAULT_PORT, DEFAULT_BACKLOG, dur!"seconds"(4)).executeInNewThread();
}
