import std.stdio, std.random, std.json, std.typecons, std.format, std.conv;

struct Node {
	string name;
	uint width, height;
	Node[] children;
}

//contains everything needed to draw the contents, given the x,y of its top-left.
class Box {
	uint width, height;
	abstract void draw(int x, int y);
}

class BigNode : Box {
	Node node;
	this(Node *root) {
		node = root;
		width = node.width + PADDING_BIGBOX * 2;
		height = node.height + PADDING_BIGBOX * 2;
	}
	void draw(int x, int y) {
		//just a simple box around the name
		auto r = 10;//curve of the corners
		//TODO: need to define .small in svg's style tag
		svg ~= `<rect width="%s" height="%s" x="%s" y="%s" rx="%s" ry="%s" />\n`.format(width, height, x + PADDING_BIGBOX, y + PADDING_BIGBOX, r, r);
		//TODO: embed the rest of the original svg
	}
}

class SmallNode : Box {
	string name;
	this(Node *root) {
		name = root.name;
		width = PADDING_SMALLBOX * 2 + FONTSIZE * name.length;//TODO: this is an estimate of the text width,height.
		height = PADDING_SMALLBOX * 2 + FONTSIZE;
	}

	void draw(int x, int y) {
		//just a simple box around the name
		auto r = 10;//curve of the corners
		//TODO: need to define .small in svg's style tag
		svg ~= `<rect width="%s" height="%s" x="%s" y="%s" rx="%s" ry="%s" />\n`.format(width, height, x + PADDING_SMALLBOX, y + PADDING_SMALLBOX, r, r);
		svg ~= `<text x="%s" y="%s" class="smallname" text-anchor="middle">%s</text>\n"`.format(x + width / 2, y + height / 2,name);
	}
}

void drawline(int x0, int y0, int x1, int y1) {
	auto linestyle = "stroke:black;stroke-width:2";
	svg ~= `<line x1="%s" y1="%s" x2="%s" y2="%s" style="%s" />\n`.format(x0, y0, x1, y1, linestyle);
}

class TreeBox : Box {
	Box nodebox;
	TreeBox[] children;
	this(Node *root) {
		nodebox = is_big(root.name) ? new BigNode(root) : new SmallNode(root);
		children = children.map!(a => new TreeBox(child)).array;
		width = max(nodebox.width, children.map!(a => a.width).sum) + TREE_PADDING * 2;
		height = max(nodebox.height, children.map!(a => a.height).sum);
		//TODO: maybe use longer vertical lines if bigboxes are involved
	}

	void draw(int x, int y) {
		auto centerx = width / 2;
		auto nodex = centerx - nodebox.width / 2 + x;
		auto childx = centerx - children.map!(a => a.width).sum / 2 + x;
		nodebox.draw(nodex, y);
		if (!children) return;

		y += nodebox.height + VLINE_LENGTH;
		int minx = childx + child[0].width / 2;
		foreach (child; children) {
			if (children.length > 1) drawline(childx, y, childx, y + VLINE_LENGTH);
			child.draw(childx, y + VLINE_LENGTH);
			childx += child.width;
		}
		int maxx = childx - child[$-1].width / 2;

		if (1 == children.length) {//single vertical edge
			drawline(centerx, y - VLINE_LENGTH, y + VLINE_LENGTH);
		}
		else {//edges drawn in tree structure (lower vertical lines already drawn)
			drawline(centerx, y - VLINE_LENGTH, centerx, y);
			drawline(minx, y, maxx, y);
		}
	}
}

Node *root;

auto create_graph(nlayers) {
	uint node_idx = 0;
	auto create_node(uint layer) {
		auto nchildren = uniform!"[]"(1,10);
		auto name = "subgraph%s".format(node_idx++);
		auto children = (nlayers == layer) ? null : iota(0, nchildren).map!(a => create_node(layer + 1)).array;
		return new Node(name, uniform(600,800), uniform(400,800), children);
	}
	return create_node(0);
}

void main() {
	root = create_graph(4);
}
