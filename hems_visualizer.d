import std.stdio, std.random, std.json, std.typecons, std.format, std.conv, std.algorithm, std.array, std.range;

immutable int
	PADDING_BIGBOX = 8,
	PADDING_SMALLBOX = 2,
	PADDING_TEXT = 2,
	PADDING_TREE = 4,
	PADDING_TRIANGLE = 4,
	TRIANGLE_SIZE = 80,
	FONTSIZE = 14,//TODO: This isn't really used fully
	VLINE_LENGTH = 25,
	TEXTALIGN_HACK = 3
;
immutable double TEXTSCALE_HACK = 2;


enum Type { SELF, ANCESTOR, PARENT, SIBLING, CHILD, DESCENDENT, NIECE, COUSIN };
struct Node {
	string name;
	uint width, height;
	Node*[] children;
	string svg;
	Type type;
}

string[] svg_text;
Node *root;

//contains everything needed to draw the contents, given the x,y of its top-left.
class Box {
	uint width, height;
	string name;
	abstract void draw(int x, int y);
	//TODO: need to add html area tags
}

// Represents a full subtree that isn't worth drawing out even in miniature
class TriangleNode : Box {
	Node *node;
	alias pad = PADDING_TRIANGLE;
	alias sz = TRIANGLE_SIZE;
	this(Node *root) {
		node = root;
		name = node.name;
		width = sz + pad * 2;
		height = sz;
		stderr.writef("Triangle %s, %s\n", width, height);
	}
	override void draw(int x, int y) {
		auto s = `<polygon points="%s,%s %s,%s %s,%s" style="fill:white;stroke:blue;stroke-width:2" />`.format(
			x + pad + sz/2,  y,
			x + pad,         y + sz, 
			x + pad + sz,    y + sz);
		svg_text ~= s;
		stderr.writef("x=%s, y=%s, w=%s, h=%s -> %s\n", x, y, width, height, s);
	}

}

//subgraphs that we have a particular interest in
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
		auto box_w = width - 2 * PADDING_BIGBOX;
		auto box_h = height - 2 * PADDING_BIGBOX;
		auto color = (Type.SELF == node.type) ? "rgb(128,0,255)" : "rgb(0,0,255)";
		svg_text ~= `<rect width="%s" height="%s" x="%s" y="%s" rx="%s" ry="%s" style="fill:rgb(255,255,255);stroke-width:2;stroke:%s" />`.format(box_w, box_h, x + PADDING_BIGBOX, y + PADDING_BIGBOX, r, r, color);
		//TODO: embed the rest of the original svg
	}
}

//relatively boring subgraphs
class SmallNode : Box {
	uint textwidth;
	this(Node *root) {
		name = root.name;
		textwidth = cast(uint)(FONTSIZE * name.length / TEXTSCALE_HACK);

		width = 2 * PADDING_SMALLBOX + 2 * PADDING_TEXT + textwidth;//TODO: this is an estimate of the text width,height.
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
			.format(center_x, center_y + TEXTALIGN_HACK, textwidth, name);
		//TODO: textlength is being ignored
	}
}

void drawline(int x0, int y0, int x1, int y1, string color="black") {
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

		width = max(nodebox.width, children.map!(a => a.width).sum) + PADDING_TREE * 2;
		height = nodebox.height + 2 * VLINE_LENGTH;
		if (children.length) height += children.map!(a => a.height).maxElement;
		//TODO: maybe use longer vertical lines if bigboxes are involved
	}

	void draw(int x, int y) {
		auto centerx = x + width / 2;
		int nodex = centerx - nodebox.width / 2;
		auto childx = centerx - children.map!(a => a.width).sum / 2;
		assert(childx >= 0 && nodex >= 0);

		nodebox.draw(nodex, y);
		if (!children) return;

		y += nodebox.height + VLINE_LENGTH;
		int minx = childx + children[0].width / 2;
		foreach (child; children) {
			if (children.length > 1) {
				//the lines below the horizontal line
				auto center = childx + child.width / 2;
				auto y1 = y + VLINE_LENGTH + (is_big(child.root) ? PADDING_BIGBOX : PADDING_SMALLBOX);
				drawline(center, y, center, y1);
			}
			child.draw(childx, y + VLINE_LENGTH);
			childx += child.width;
		}
		int maxx = childx - children[$-1].width / 2;

		auto y0 = y - VLINE_LENGTH - (is_big(root) ? PADDING_BIGBOX : PADDING_SMALLBOX);
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


void focus(Node *target) {
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
			if (node == root) {
				stderr.writef("ancestor = %s\n", ancestor);
			}
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
		auto nchildren = uniform!"[]"(1,4);
		nchildren = 4;//TODO: remove
		auto name = "%s".format(node_idx++);
		auto children = (nlayers == layer + 1) ? null : iota(0, nchildren).map!(a => create_node(layer + 1)).array;
		auto svg = null;//TODO: something else
		//TODO: get width, height from svg
		return new Node(name, uniform(600,800), uniform(400,800), children, svg);
	}
	return create_node(0);
}

void main() {
	root = create_graph(5);

	focus(root.children[1].children[2]);//NOTE: Could crash if those don't exist. But they usually will, and this ijust a quick test
//	assert(root.type == Type.ANCESTOR, format("%s", root.type));

	auto box = new TreeBox(root);
	box.draw(0, 0);

	uint width = 2000;
	uint height = cast(uint)(box.height / cast(double)box.width * width);
	
	auto svg_header = [
		`<?xml version="1.0" encoding="UTF-8" standalone="no"?>`,
		`<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">`,
		`<svg x="0" y="0" width="%spt" height="%spt" viewBox="0.00 0.00 %.2f %.2f" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">`.format(width, height, box.width, box.height),
		`<rect width="100%" height="100%" x="0" y="0" fill="white" />`,
	];
	drawline(0, box.height, 2000, box.height, "red");

	svg_header.each!(a => writef("%s\n", a));
	foreach (line; svg_text) {
		writef("%s\n", line);
	}
	writef("</svg>\n");
	stderr.writef("Box: %s, %s\n", box.width, box.height);
	stderr.writef(" Px: %s, %s\n", width, height);
	//TODO: Still a bit wide. Maybe display ancestors and children as big boxes, siblings and nieces as small boxes, and cousins as either dots or just replace the entire trees with ellipses
	//dots will probably work if we have a PADDING_DOTBOX and PADDING_DOTTREE to keep it *super*-compressed
	//Do that after getting bigbox working, though.
	//Instead of ellipses, a triangle with a node count and/or observation count in it. Clicking the triangle gets the root node
}
