# HEMS
An implementation of the Hybrid Event Memory System (HEMS) by Menager et al.. HEMS performs clustering and classification of structured representations, namely Bayesian networks, to enable event cognition for intelligent agents. It stores observations, represented as DAGs, at the lowest level of an event hierarchy, and learns generalizations on top of them to form a probabilistic taxonomy of events. Learning occurs in an online, and incremental fashion, so the system learns continually, forming new generalizations by composing pre-existing ones together. Learning and performance are interleaved, so predictive power improves as examples are encountered.

Contributions of the work are as follows:
* Novel hybrid theory of event memory combines exemplar-based and schema-based perspectives on event memory to explain the full range of human event memory usage;
* Computational implementation of this theory that enables constructing event memory-enabled intelligent agents;
* Modeling and demonstrating the full range of event memory phenomena inlcuding: sucessful remembering, misremembering, and confabulation;
* Novel rule-based representation for compactly storing probability distributions to enable efficient state estimation under various partial observability conditions; and
* Novel easy-to-use programming language for specifying probabilistic programs.

Further details on the system's theoretical claims and technical details may be found in the /papers directory.

This page is currently under development.

## Requirements
This codebase is written and testing using SBCL. Other versions of Common Lisp may work, but we provide no guarantees. This software also depends on quicklisp for installing other Common Lisp packages. A full list of external dependencies may be found in hems.asd.

Python programs can use HEMS by importing cl4py into their projects. See the /examples folder for details.

## Install
```
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/dmenager/HEMS.git
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
c1 -> c2
c1 -> c3
c1 -> c4
c1 -> c5
c1 -> c6
c1 -> c7
c1 -> c8
c1 -> c9
c10 -> c1
c11 -> c1
c12 -> c1
c13 -> c1
```
Arrows define connections between nodes. The only supported connection type is a directed edge. For example, `c1 -> c2` denotes an edge eminating from `c1` flowing to `c2'.

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
		c1 -> c2
		c1 -> c3
		c1 -> c4
		c1 -> c5
		c1 -> c6
		c1 -> c7
		c1 -> c8
		c1 -> c9
		c10 -> c1
		c11 -> c1
		c12 -> c1
		c13 -> c1))
```
In Python, the program should be output to a file, then compiled from the file in the following manner:
```
bn = hems.compile_program_from_file("prog1.hems")
```
such that "prog1.hems" contains the probabilistic program.

Observations encoded in this way are degenerate Bayesian networks since they are probabilistic models that can only make predictions about the observed state.

### Remembering
### Inference

## Tests

