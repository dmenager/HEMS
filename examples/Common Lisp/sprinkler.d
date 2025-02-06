#!/usr/bin/dmd -run
import std.stdio, std.math, std.random, std.algorithm, std.uni, std.array, std.format;

double[string][char] P;

immutable VARIATION = true;

void initialize() {
	// capital letter means true; lowercase means false. Empty if no parents.
	// e.g. P['W']["rS"] means P(wet=1 | rain=0, sprinkler=1)
	if (!VARIATION) {
		P['Z'][""] = 0.2;
		P['R']["z"] = 0.3;
		P['R']["Z"] = 0.9;
		P['S']["r"] = 0.7;
		P['S']["R"] = 0.2;
		P['W'] = [
			"rs" : 0.05,
			"rS" : 0.7,
			"Rs" : 0.8,
			"RS" : 0.943,
		];
	}
	else {
		P['Z'][""] = uniform(0.1, 0.8);
		P['R']["z"] = uniform(0.1, 0.7);
		P['R']["Z"] = uniform(0.7, 0.9);
		P['S']["r"] = uniform(0.1, 0.9);
		P['S']["R"] = P['S']["r"] * uniform(0.1, 0.9);//second factor is prob that we turn off sprinkler if it rains
		P['W'] = [
			"rs" : uniform(0.0, 0.1),
			"rS" : uniform(0.5, 0.8),
			"Rs" : uniform(0.6, 0.9),
		];

		// We assume that all the things that cause wet (including the prior) act independently
		P['W']["RS"] = 1.0 - (1.0 - P['W']["rS"]) * (1.0 - P['W']["Rs"]) * (1.0 - P['W']["rs"]);
	}
}

static this() {
  initialize();
}

bool zeus_angry() {
	return uniform01 < P['Z'][""];
}

bool rain(bool zeus_angry) {
	return uniform01 < (zeus_angry ? P['R']["Z"] : P['R']["z"]);
}

bool sprinkler(bool rain) {
	return uniform01 < (rain ? P['S']["R"] : P['S']["r"]);
}

bool wet(bool rain, bool sprinkler) {
	if (!rain && !sprinkler) return uniform01 < P['W']["rs"];
	if (!rain && sprinkler) return uniform01  < P['W']["rS"];
	if (rain && !sprinkler) return uniform01  < P['W']["Rs"];
	if (rain && sprinkler) return uniform01   < P['W']["RS"];
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
	uint num_pop = 5;
	writef( "(ql:quickload :hems)\n" ~
		"(in-package :hems)\n\n");
	for (int i = 1; i <= num_pop; i++) {
	  initialize();
	  bool[] zc = null;
	  bool[][] rc = [ null, null ];
	  bool[][] sc = [ null, null ];
	  bool[][][] wc = [ [null,null], [null,null]];
	  writef(
		 "(defun build-pop-%s ()\n" ~
		 "(let (observations)\n" ~
		 "    (setq observations\n(list ",
		 i);
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
		   "	c1 --> c2\n" ~
		   "	c2 --> c3\n" ~
		   "	c2 --> c4\n" ~
		   "	c3 --> c4)\n",
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
		 " observations)\n");
	  
	  writef(" (save-eltm-to-file eltm* :filename \"eltm-pop-%s.txt\")\n (setq eltm* nil)\n", i);
	  writef(" observations))\n");
	  
	  writef("\n\n;; ----------------------------\n\n");
	  
	  string[char] var_name = [
				   'Z' : "zeus_angry",
				   'R' : "rain",
				   'S' : "sprinkler",
				   'W' : "wet"
				   ];
	  
	  writef(";; Population %s distribution:\n", i);
	  foreach (var, table; P) {
	    auto keys = table.keys.sort!((a,b) => a > b);
	    foreach (cond; keys) {
	      auto prob = table[cond];
	      if (!cond.length) {
		writef(";; P(%s=1) = %s\n", var_name[var], prob);
	      }
	      else {
		string f(dchar a) {
		  auto r = format("%s=%s", var_name[cast(char)toUpper(a)], (isUpper(a) ? 1 : 0));
		  return r;
		}
		
		auto cond_str = cond
		  .map!f
		  .join(", ");
		writef(";; P(%s=1 | %s) = %s\n", var_name[var], cond_str, prob);
	      }
	    }
	  }
	  writef("\n");
	  
	  writef(";; CPDs (sample distribution)\n");
	  writef(";; P(zeus_angry=1) = %.4f\n", zc.prob);
	  writef(";; P(rain=1 | zeus_angry=0) = %.4f\n", rc[0].prob);
	  writef(";; P(rain=1 | zeus_angry=1) = %.4f\n", rc[1].prob);
	  writef(";; P(sprinkler=1 | rain=0) = %.4f\n", sc[0].prob);
	  writef(";; P(sprinkler=1 | rain=1) = %.4f\n", sc[1].prob);
	  foreach (r; [0,1]) {
	    foreach (s; [0,1]) {
	      writef(";; P(wet=1 | rain=%s, sprinkler=%s) = %.4f\n\n", r, s, wc[r][s].prob);
	    }
	  }
	}
	writef("\n");
	writef( "#| TESTS\n" ~
		"(let ("
		);
	for (int i = 1; i <= num_pop; i++)
	  {
	    writef( "(obs%s (hems::build-pop-%s))\n", i, i);
	  }
	writef(")\n");
	writef("(hems:load-eltm-from-file \"eltm-pop-1.txt\")\n" ~
	       "(hems:log-message (list \"MODEL_ID,POPULATION_ID,OBSERVATION_ID,SCORE~%%\") \"model-scores.csv\")\n");
	for (int i = 1; i <= num_pop; i++)
	  {
	    writef("(loop\n" ~
		   "with score\n");
	    writef("for obs in obs%s\n", i);
	    writef("for i from 0\n" ~
		   "do\n" ~
		   "(setq score (float (hems:bn-score obs (hems:episode-observation (car hems:eltm*)) (make-hash-table) (make-hash-table) :bic-p nil)))\n");
	    writef("(hems:log-message (list \"~A,~d,~d,~d~%%\" \"eltm-pop-1\" %s i score) \"model-scores.csv\"))\n\n", i);
	  }
	writef(")\n");
	writef( "(load \"sprinkler-example.lisp\")\n" ~
		"(hems::example)\n" ~
		"(hems::H[bn] (car (hems::get-eltm)) (make-hash-table))\n"
		);
	writef("\n");
	writef( ";; certain observations\n" ~
		"(let ((observations (make-hash-table :test #'equal)))\n" ~
		"(setf (gethash \"ZEUS_ANGRY_243\" observations) (list (cons \"T\" 1)))\n" ~
		"(hems::H[bn] (car (hems::get-eltm)) observations))\n" ~
		"(let (evidence-bn)\n" ~
		"(setq evidence-bn (compile-program nil c = (percept-node zeus_angry :value \"T\")))\n" ~
		"(hems::H[bn] (car (hems::get-eltm)) evidence-bn))\n\n" ~
		
		";; uncertain observations\n" ~
		"(let ((observations (make-hash-table :test #'equal)))\n" ~
		"(setf (gethash \"SPRINKLER_245\" observations) (list (cons \"T\" .3) (cons \"NIL\" .7)))\n" ~
		"(hems::H[bn] (car (hems::get-eltm)) observations))\n" ~
		"|#");
}
