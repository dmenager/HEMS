import std.stdio, std.random, std.json, std.typecons, std.format, std.conv, std.algorithm, std.array, std.range;

immutable uint
	PADDING_BIGBOX = 14,
	PADDING_SMALLBOX = 10,
	PADDING_TEXT = 4,
	PADDING_TREE = 20,
	FONTSIZE = 14,
	VLINE_LENGTH = 25,
	TEXTALIGN_HACK = 3
;

struct Node {
	string name;
	uint width, height;
	Node*[] children;
	string svg;
}

string[] svg_text;

//contains everything needed to draw the contents, given the x,y of its top-left.
class Box {
	uint width, height;
	string name;
	abstract void draw(int x, int y);
}

class BigNode : Box {
	Node *node;
	this(Node *root) {
		node = root;
		name = node.name;
		width = node.width + PADDING_BIGBOX * 2;
		height = node.height + PADDING_BIGBOX * 2;
	}
	override void draw(int x, int y) {
		//just a simple box around the name
		auto r = 10;//curve of the corners
		//TODO: need to define .small in svg's style tag
		svg_text ~= `<rect width="%s" height="%s" x="%s" y="%s" rx="%s" ry="%s" style="fill:rgb(255,255,255);stroke-width:2;stroke:black" />`.format(width, height, x + PADDING_BIGBOX, y + PADDING_BIGBOX, r, r);
		//TODO: embed the rest of the original svg
	}
}

class SmallNode : Box {
	this(Node *root) {
		name = root.name;
		width = 2 * PADDING_SMALLBOX + 2 * PADDING_TEXT + FONTSIZE * name.length.to!uint;//TODO: this is an estimate of the text width,height.
		height = PADDING_SMALLBOX * 2 + FONTSIZE;
	}

	override void draw(int x, int y) {
		//just a simple box around the name
		auto r = 4;//curve of the corners
		//TODO: need to define .small in svg's style tag
		auto box_w = width - 2 * PADDING_SMALLBOX;
		auto box_h = height - 2 * PADDING_SMALLBOX;
		auto center_x = x + width / 2;
		auto center_y = y + height / 2;
		auto style="fill:rgb(255,255,255);stroke-width:1;stroke:blue";
		svg_text ~= `<rect width="%s" height="%s" x="%s" y="%s" rx="%s" ry="%s" style="%s" />`
			.format(box_w, box_h, x + PADDING_SMALLBOX, y + PADDING_SMALLBOX, r, r, style);
		svg_text ~= `<text x="%s" y="%s" class="smallname" text-anchor="middle" textlength="%s">%s</text>"`
			.format(center_x, center_y + TEXTALIGN_HACK, name.length.to!uint, name);
		//TODO: textlength is being ignored
	}
}

void drawline(int x0, int y0, int x1, int y1, string color="black") {
	auto linestyle = "stroke:%s;stroke-width:1".format(color);
	svg_text ~= `<line x1="%s" y1="%s" x2="%s" y2="%s" style="%s" />`.format(x0, y0, x1, y1, linestyle);
}

bool is_big(string name) {
	return 0;//TODO:
}

class TreeBox : Box {
	Box nodebox;
	TreeBox[] children;
	this(Node *root) {
		name = "(tree)";
		nodebox = is_big(root.name) ? new BigNode(root) : new SmallNode(root);
		children = root.children.map!(a => new TreeBox(a)).array;
		width = max(nodebox.width, children.map!(a => a.width).sum) + PADDING_TREE * 2;
		height = max(nodebox.height, children.map!(a => a.height).sum);
		//TODO: maybe use longer vertical lines if bigboxes are involved
	}

	override void draw(int x, int y) {
		auto centerx = x + width / 2;
		int nodex = centerx - nodebox.width / 2;
		auto childx = centerx - children.map!(a => a.width).sum / 2;
		nodebox.draw(nodex, y);
		if (!children) return;

		y += nodebox.height + VLINE_LENGTH;
		int minx = childx + children[0].width / 2;
		foreach (child; children) {
			if (children.length > 1) {
				//the lines below the horizontal line
				auto center = childx + child.width / 2;
				auto y1 = y + VLINE_LENGTH + (is_big(child.name) ? PADDING_BIGBOX : PADDING_SMALLBOX);
				drawline(center, y, center, y1);
			}
			child.draw(childx, y + VLINE_LENGTH);
			childx += child.width;
		}
		int maxx = childx - children[$-1].width / 2;

		auto y0 = y - VLINE_LENGTH - (is_big(root.name) ? PADDING_BIGBOX : PADDING_SMALLBOX);
		if (1 == children.length) {//single vertical edge
			auto y1 = y + VLINE_LENGTH;
			drawline(minx, y0, minx, y1);
		}
		else {//upper vertical line (lower vertical lines already drawn), horizontal line
			drawline(centerx, y0, centerx, y);
			drawline(minx, y, maxx, y);
		}
	}
}

Node *root;

auto create_graph(uint nlayers) {
	uint node_idx = 0;
	Node* create_node(uint layer) {
		auto nchildren = uniform!"[]"(1,10);
		nchildren = 4;//TODO: remove
		auto name = "subgraph%s".format(node_idx++);
		auto children = (nlayers == layer + 1) ? null : iota(0, nchildren).map!(a => create_node(layer + 1)).array;
		auto svg = null;//TODO: something else
		//TODO: get width, height from svg
		return new Node(name, uniform(600,800), uniform(400,800), children, svg);
	}
	return create_node(0);
}

void main() {
	root = create_graph(4);

	auto box = new TreeBox(root);
	box.draw(0, 0);

	uint width = 2000;
	uint height = cast(uint)(box.height / cast(double)box.width * width);
	
	auto svg_header = [
		`<?xml version="1.0" encoding="UTF-8" standalone="no"?>`,
		`<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">`,
		`<svg width="%spt" height="%spt" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">`.format(width, height),
		`<rect width="100%" height="100%" x="0" y="0" fill="white" />`,
		`<g id="graph0" class="graph" transform="scale(1 1) rotate(0)">`,
	];//TODO: how to make that scale not necessary?

	svg_header.each!(a => writef("%s\n", a));
	foreach (line; svg_text) {
		writef("%s\n", line);
	}
	writef("</g>\n</svg>\n");
}
