import std.stdio, std.math, std.random, std.algorithm;

bool zeus_angry() {
	return uniform01 < 0.2;
}

bool rain(bool zeus_angry) {
	return uniform01 < (zeus_angry ? 0.9 : 0.3);
}

bool sprinkler(bool rain) {
	return uniform01 < (rain ? 0.2 : 0.7);
}

bool wet(bool rain, bool sprinkler) {
	if (!rain && !sprinkler) return uniform01 < 0.05;
	if (!rain && sprinkler) return uniform01 < 0.7;
	if (rain && !sprinkler) return uniform01 < 0.8;
	if (rain && sprinkler) return uniform01 < 0.94;
	assert(0);
}
	
double prob(bool[] obs) {
	return obs.sum / cast(double)obs.length;
}

string tnil(bool a) {
	return a ? "T" : "NIL";
}

void main() {
	uint N = 300;

	bool[] zc = null;
	bool[][] rc = [ null, null ];
	bool[][] sc = [ null, null ];
	bool[][][] wc = [ [null,null], [null,null]];
	writef( "(ql:quickload :hems)\n" ~
		"(in-package :hems)\n\n" ~
		"(defun example ()\n" ~
		"(let (observations)\n" ~
		"    (setq observations\n(list "
	);
	foreach (obs_idx; 0..N) {
		bool z = zeus_angry;
		bool r = rain(z);
		bool s = sprinkler(r);
		bool w = wet(r, s);

		writef(
			"(compile-program\n" ~
			"       nil\n" ~
			"	c1 = (percept-node zeus_angry :value \"%s\")\n" ~
			"	c2 = (percept-node rain :value \"%s\")\n" ~
			"	c3 = (percept-node sprinkler :value \"%s\")\n" ~
			"	c4 = (percept-node wet :value \"%s\")\n" ~
			"	c1 -> c2\n" ~
			"	c2 -> c3\n" ~
			"	c2 -> c4\n" ~
			"	c3 -> c4)\n",
			z.tnil, r.tnil, s.tnil, w.tnil);

		zc ~= z;
		rc[z] ~= r;
		sc[r] ~= s;
		wc[r][s] ~= w;
	}
	writef("))\n");
	
	writef(
		";; insert into event memory\n" ~
		"(map nil #'(lambda (bn)\n" ~
		"	 (new-push-to-ep-buffer :observation bn :insertp t :temporal-p nil))\n" ~
		" observations)))\n");

	writef("\n\n;; ----------------------------\n\n;; CPDs\n");
	writef(";; P(zeus_angry=1) = %.4f\n", zc.prob);
	writef(";; P(rain=1 | zeus_angry=0) = %.4f\n", rc[0].prob);
	writef(";; P(rain=1 | zeus_angry=1) = %.4f\n", rc[1].prob);
	writef(";; P(sprinkler=1 | rain=0) = %.4f\n", sc[0].prob);
	writef(";; P(sprinkler=1 | rain=1) = %.4f\n", sc[1].prob);
	foreach (r; [0,1]) {
		foreach (s; [0,1]) {
			writef(";; P(wet=1 | rain=%s, sprinkler=%s) = %.4f\n", r, s, wc[r][s].prob);
		}
	}
	writef("\n")
	writef( "#| TESTS\n" ~
		"(load \"sprinkler-example.lisp\")\n" ~
	           	"(hems::example)\n" ~
	           	"(hems::H[bn] (car (hems::get-eltm)) (make-hash-table))\n" ~
	           	"|#")
}
