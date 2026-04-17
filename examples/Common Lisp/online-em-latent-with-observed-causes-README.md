# Online EM Latent Variable With Observed Causes

This example measures online EM convergence when the hidden variable is not a
root prior. The hidden variable has observed causes and is itself the cause of
another observed variable. The experiment is implemented in
`online-em-latent-with-observed-causes.lisp`, and the convergence trace can be
plotted with `plot_online_em_convergence.py`.

## Experimental Design

The ground truth distribution is a Bayesian network with two observed causes,
one hidden middle variable, and one observed effect:

```text
A -> H -> E
D -> H
```

All variables are represented as percept nodes. `A`, `D`, and `E` are observed
in each training instance. `H` is marked latent, so it is never supplied as
observed evidence during training.

The ground truth parameters are:

| Parameter | Probability |
| --- | ---: |
| `P(A = HIGH)` | `0.45` |
| `P(D = ON)` | `0.55` |
| `P(H = HOT | A = LOW, D = OFF)` | `0.10` |
| `P(H = HOT | A = LOW, D = ON)` | `0.30` |
| `P(H = HOT | A = HIGH, D = OFF)` | `0.60` |
| `P(H = HOT | A = HIGH, D = ON)` | `0.80` |
| `P(E = YES | H = COLD)` | `0.15` |
| `P(E = YES | H = HOT)` | `0.90` |

Training samples are drawn from this distribution with deterministic seed
`424242`. The default invocation writes 2000 examples.

## Data

The experiment writes two CSV files in the repository root:

| File | Purpose |
| --- | --- |
| `online-em-middle-latent-training-set.csv` | The generated training set. It includes the hidden `H` value for evaluation only. |
| `online-em-middle-latent-convergence.csv` | The online EM trace after each seen training instance. |

The training set has these columns:

| Column | Meaning |
| --- | --- |
| `instance` | One-based training instance number. |
| `a` | Observed value of cause `A`. |
| `d` | Observed value of cause `D`. |
| `h_hidden` | The sampled latent value. This is not passed to online EM. |
| `e` | Observed value of effect `E`. |

The convergence CSV has these columns:

| Column | Meaning |
| --- | --- |
| `seen_instances` | Number of training instances processed so far. |
| `target_a` | Parent value for the tracked hidden CPD row. |
| `target_d` | Parent value for the tracked hidden CPD row. |
| `truth_p_h_hot` | Ground truth value for the tracked row. |
| `raw_p_h_hot` | The model's direct current estimate for the tracked row. |
| `aligned_p_h_hot` | The estimate after correcting for possible latent-label swapping. |
| `latent_label_swapped` | Whether the raw latent labels were swapped for evaluation. |
| `abs_error_p_h_hot` | Absolute error between the aligned estimate and ground truth. |

The hidden `h_hidden` column should only be used to inspect the generated data
and evaluate convergence. The learner receives evidence for `A`, `D`, and `E`
only.

## Training Procedure

The model is initialized with the correct structure and known value domains:

```text
A in {HIGH, LOW}
D in {ON, OFF}
H in {HOT, COLD}
E in {YES, NO}
```

The root distributions for observed causes `A` and `D` are initialized to the
ground truth and are not updated by this experiment. The observed effect CPD
`P(E | H)` is also initialized to the ground truth and kept fixed. The hidden
CPD rows for `P(H | A, D)` are initialized uniformly:

```text
P(H = HOT | A, D) = 0.5
P(H = COLD | A, D) = 0.5
```

Online EM is run once per sample. Each update infers a posterior over the
hidden `H` value from the observed `A`, `D`, and `E` values, then updates the
hidden CPD. The convergence CSV records the tracked row after each update.

By default, the tracked row is:

```text
P(H = HOT | A = HIGH, D = ON)
```

The ground truth for that row is `0.80`.

## Results

In the latest 2000-example run, the tracked parent context appeared 473 times:

```text
A = HIGH, D = ON: 473 / 2000
```

Within that context, the sampled hidden value was `HOT` with empirical
frequency:

```text
386 / 473 = 0.816068
```

That empirical rate is close to the ground truth row value `0.80`, but it is
not exactly equal because the training set is finite.

The final online EM estimate for the tracked row was:

```text
aligned P(H = HOT | A = HIGH, D = ON) = 0.89116557
absolute error                         = 0.09116557
```

The late-stage per-instance changes became small:

| Window | Mean absolute error delta | Max absolute error delta |
| --- | ---: | ---: |
| Last 100 examples | `0.00269195` | `0.03136500` |

These deltas show that the estimate is still moving, but late updates are much
smaller on average than early updates.

## Plot Interpretation

Run the shared plotting script to generate
`online-em-middle-latent-convergence.png`:

```sh
python3 "examples/Common Lisp/plot_online_em_convergence.py" \
  online-em-middle-latent-convergence.csv \
  -o online-em-middle-latent-convergence.png
```

The plot has three panels:

| Panel | How to read it |
| --- | --- |
| Estimate | Compares the raw estimate, label-aligned estimate, and tracked-row ground truth `0.80`. |
| Absolute error | Shows the distance between the aligned estimate and ground truth. Lower is better. |
| Error delta | Shows `error[t] - error[t-1]` after each instance. Values near zero mean the estimate is changing slowly. Negative values mean the latest update reduced error; positive values mean it increased error. |

The raw and label-aligned estimates can differ because a hidden variable's
labels are not intrinsically identifiable from observations alone. If the model
learns the opposite internal assignment for `HOT` and `COLD`, the aligned
estimate flips `p` to `1 - p` for evaluation.

The error curve is not expected to be monotonic. Online EM updates the model
after each individual sample, and the plot tracks parameter error rather than
the batch observed-data likelihood. Individual samples can move the estimate
toward or away from the ground truth even when the update rule is behaving as
expected.

## Reproducing

From the repository root, generate the default 2000-example convergence data:

```sh
sbcl --disable-debugger \
  --load "examples/Common Lisp/online-em-latent-with-observed-causes.lisp" \
  --quit
```

To use a different number of examples or track a different parent context:

```sh
sbcl --disable-debugger \
  --load "examples/Common Lisp/online-em-latent-with-observed-causes.lisp" \
  --eval '(hems::write-middle-demo-convergence :n 4000 :target-a "LOW" :target-d "ON")' \
  --quit
```

Then regenerate the plot with the Python command shown above.

The CSV and PNG outputs are generated artifacts. They are useful for inspection
and plotting, but the committed source of the experiment is the Lisp convergence
test, the shared Python plotting script, and this README.
