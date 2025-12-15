# HEMS
An implementation of the Hybrid Event Memory System (HEMS) by Menager et al.. HEMS performs clustering and classification of structured representations, specifically Bayesian networks, to enable event cognition for intelligent agents. It stores observations, represented as DAGs, at the lowest level of an event hierarchy, and learns generalizations on top of them to form a probabilistic taxonomy of events. Learning occurs in an online, and incremental fashion:the system continually updates its event hierarchy, composing new generalizations out of pre-existing ones. Learning and performance are interleaved, so predictive power improves as examples are encountered.

Contributions of the work are as follows:
* Novel hybrid theory of event memory combines exemplar-based and schema-based perspectives on event memory to explain the full range of human event memory usage;
* Computational implementation of this theory that enables constructing event memory-enabled intelligent agents;
* Modeling and demonstrating the full range of event memory phenomena inlcuding: sucessful remembering, misremembering, and confabulation;
* Novel rule-based representation for compactly storing probability distributions to enable efficient state estimation under various partial observability conditions; and
* Declarative DSL for specifying Bayesian networks using an easy-to-read notation based on node definitions and edge statements.

Further details on the system's theoretical claims and technical details may be found in the /papers directory.

Note: This repository is still under development; APIs may evolve, but the examples below reflect the current codebase.

## Repository layout
Core pieces of the system include:
* `package.lisp` – Package definition and public API exports.

* `graph-representation.lisp` – Internal data structures for Bayesian networks, rule-based CPDs, and related utilities.

* `hems-program-compiler.lisp` – Compiler for the HEMS probabilistic program DSL (percept-node, relation-node, edge syntax, etc.).

* `episodic.lisp` – Episodic buffer, event hierarchy (ELTM), retrieval (remember, remember-temporal), and ELTM visualization (eltm-to-pdf).

* `sampler.lisp` – Sampling utilities.

* `metrics.lisp`, performance-stats.lisp – Scoring and evaluation utilities (e.g., bn-score, G² tests).

* `serializer.lisp` – Serialization utilities for saving and loading learned structures.

* `segmentation.lisp` – Event segmentation over experience.

* `ep-log.lisp` – Logging utilities for episodic memory.

* `papers/` – Theoretical and empirical writeups.

## Requirements
### SBCL
SBCL is the primary supported implementation (the system is developed and tested with SBCL).
* On Unix-like systems, install via your package manager _or_ from the [SBCL website
](https://www.sbcl.org/platform-table.html) for the latest version.
* On Windows, installers are available via the same link.

Other Common Lisp implementations _may_ work but are not actively supported.
### Quicklisp
Quicklisp is a library manager for Common Lisp. It downloads and installs libraries and manages dependencies.

### Lisp libraries
* `alexandria`
* `split-sequence`
* `uiop` (comes with ASDF)
* `cl-ppcre`

If you use Quicklisp, these will be pulled automatically when you load `:hems`.
### External Tools
For visualization, HEMS uses Graphviz’s `dot` program via `uiop:run-program` (see `(eltm-to-pdf)` in `episodic.lisp`).

To install Graphviz:
* Ubuntu/Debian: `sudo apt-get install graphviz`
* macOS (Homebrew): `brew install graphviz`
* Windows: Install Graphviz from the official website and ensure `dot.exe` is in your `PATH`.

Without `dot` installed, `(eltm-to-pdf)` and related ELTM visualization utilities will fail.

### Python (Optional)
Those wishing to utilize HEMS in Python programs will need:
* Python 3.x
* [cl4py](https://github.com/marcoheisig/cl4py) for calling HEMS functions from Python
* An accessible SBCL binary for cl4py to launch.

See the *Python Interoperability* section below for more details.

## Installation
1. Install SBCL

   Follow the instructions at: https://www.sbcl.org/platform-table.html

3. Install Quicklisp

   Instructions at: https://www.quicklisp.org/beta/
4. Clone HEMS into Quicklisp local projects
```
cd ~/quicklisp/local-projects
git clone https://github.com/dmenager/HEMS.git
```
5. Load the HEMS system

   In a Lisp REPL run:
```
(ql:quickload :hems)
```
6. Install Graphviz (Optional)

   If you plan to use `(eltm-to-pdf)` or other visualization utilities, install Graphviz as described above so that the `dot` binary is available.

## Core Concepts
Observations are encoded as Bayesian networks using a lightweight DSL:
* Node constructors (used inside `(compile-program)`):
	* `(percept-node)` represents observable variables (e.g., sensor readings)
 	* `(relation-node)` represents inferred or latent relations (beliefs) that exert causal influence.
  * Additional nodes for temporal reasoning (`observation-node`,`action-node`,and `state-node`) and functional constraints (`(functional-node)` exist. More details are forthcoming.
* Edges:
	* `c1 --> c2` defines a directed edge from `c1` to `c2`.
* Variable prior definitions
* 	In more advanced programs, priror distributions (e.g., `(discrete-uniform)`) can be attached to instantiated variables using `~`:
```
c2 = (percept-node b :value "10")
c2 ~ (discrete-uniform :values ("10" "20" "30" "40"))
```

## Usage

### Specifying observations as DAGs for HEMS.
Observations are encoded as DAGs in the following way:
```
c1 = (percept-node casualty :value "CASUALTY-A" :kb-concept-id "CNPT-1")
c2 = (percept-node age :value "22" :kb-concept-id "CNPT-2")
c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
c4 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
c5 = (percept-node hrpmin :value "145" :kb-concept-id "CNPT-5")
c6 = (percept-node mmHG :value "60" :kb-concept-id "CNPT-6")
c7 = (percept-node Spo2 :value "85" :kb-concept-id "CNPT-7")
c8 = (percept-node RR :value "40" :kb-concept-id "CNPT-8")
c9 = (percept-node pain :value "0" :kb-concept-id "CNPT-9")
c10 = (relation-node IED_injury :value "T" :kb-concept-id "CNPT-10")
c11 = (relation-node 2nd_degree_burn :value "T" :kb-concept-id "CNPT-11")
c12 = (relation-node 3rd_degree_burn :value "T" :kb-concept-id "CNPT-12")
c13 = (relation-node unconscious :value "T" :kb-concept-id "CNPT-13")
```
Observations from sensors are encoded using the `percept-node` type. The first argument is the name of the node in the DAG. Next, the `:value` field stores the observed sensor reading. Currently `percept-node` accepts string inputs. Numeric values are not yet supported, but is in progress. Finally, `:kb-concept-id` is an optional parameter that holds reference to a defined concept in a knowledge base for the given node so its semantics are well defined.

`relation-node`s encode relations for exerting causal influence among the observed perceptions. These represent unobserved, but inferred, beliefs that are true in the environment so the `:value` field must be set to "T" if true. False inferences/relations should not be described in the observed state.

```
c1 --> c2
c1 --> c3
c1 --> c4
c1 --> c5
c1 --> c6
c1 --> c7
c1 --> c8
c1 --> c9
c10 --> c1
c11 --> c1
c12 --> c1
c13 --> c1
```
Arrows define connections between nodes. The only supported connection type is a directed edge. For example, `c1 --> c2` denotes an edge eminating from `c1` flowing to `c2`.

Lastly, passing this program to the HEMS compiler returns a Bayesian network. In Common Lisp, the program statements can be directly supplied to the compiler in the following way:
```
(setq bn (compile-program
		c1 = (percept-node casualty :value "CASUALTY-A" :kb-concept-id "CNPT-1")
		c2 = (percept-node age :value "22" :kb-concept-id "CNPT-2")
		c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")
		c4 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
		c5 = (percept-node hrpmin :value "145" :kb-concept-id "CNPT-5")
		c6 = (percept-node mmHG :value "60" :kb-concept-id "CNPT-6")
		c7 = (percept-node Spo2 :value "85" :kb-concept-id "CNPT-7")
		c8 = (percept-node RR :value "40" :kb-concept-id "CNPT-8")
		c9 = (percept-node pain :value "0" :kb-concept-id "CNPT-9")
		c10 = (relation-node IED_injury :value "T" :kb-concept-id "CNPT-10")
		c11 = (relation-node 2nd_degree_burn :value "T" :kb-concept-id "CNPT-11")
		c12 = (relation-node 3rd_degree_burn :value "T" :kb-concept-id "CNPT-12")
		c13 = (relation-node unconscious :value "T" :kb-concept-id "CNPT-13")
		c1 --> c2
		c1 --> c3
		c1 --> c4
		c1 --> c5
		c1 --> c6
		c1 --> c7
		c1 --> c8
		c1 --> c9
		c10 --> c1
		c11 --> c1
		c12 --> c1
		c13 --> c1))
```
In Python, the program should be output to a file, then compiled from the file in the following manner:
```
bn = hems.compile_program_from_file("prog1.hems")
```
such that "prog1.hems" contains the probabilistic program.

Observations encoded in this way are degenerate Bayesian networks since they are probabilistic models that can only make predictions about the observed state.

### Inserting Into, and Making Inferences from Event  Memory
Arbitrarily many Bayesian networks may be compiled by following the above procedure. Inserting them into memory involves making a call to `push-to-ep-buffer` for each network. In Common Lisp, we can write:
```
(map nil #'(lambda (bn)
	(new-push-to-ep-buffer :state bn :insert-episode-p t))
     (list bn1 bn2 bn3 bn4 bn5))
```
to sequentially insert five Bayesian networks. 

For Python programs, this is:
```
for bn in [bn1, bn2, bn3, bn4, bn5]:
    hems.py_push_to_ep_buffer(state=bn, insertp=True)
```
Given a retrieval cue, HEMS retrieves the most similar event memory element from memory and performs probabilistic inference conditioning the inference process on the elements from the cue. The retrieval cue is specified in the same manner as the observations. For example, one possible retrieval cue might be some partial observation of the state:
```
(setq q1 (compile-program
	       c1 = (percept-node rank :value "MILITARY" :kb-concept-id "CNPT-4")
	       c2 = (percept-node hrpmin :value "120" :kb-concept-id "CNPT-5")
	       c3 = (percept-node sex :value "M" :kb-concept-id "CNPT-3")))
```
Then, the sytem can call `(remember)` to perform state estimation and recover the missing information:
```
(multiple-value-bind (recollection eme)
	(remember eltm* (list q1) '+  1 t))
```

where eltm* is the event memory hierarchy. `'+` denotes sum-product message passing with a learning rate of 1.

In Python, this is written as:

```
(recollection, eme) = hems.remember(hems.get_eltm(), cl4py.List(cue), cl4py.Symbol('+', 'HEMS'), 1, True)
```

