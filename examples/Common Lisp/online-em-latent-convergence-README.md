# Online EM Latent Convergence Example

This example measures whether online EM can recover the prior probability of a
hidden variable in a small Bayesian network as it sees more training instances.
The experiment is implemented in `online-em-latent-convergence.lisp`, and the
plotting script is `plot_online_em_convergence.py`.

## Experimental Design

The ground truth distribution is a Bayesian network with one hidden cause and
two observed effects:

```text
H -> B
H -> C
```

All three variables are represented as percept nodes in the model. `H` is marked
latent, so it is never supplied as observed evidence during training. `B` and
`C` are observed in every training instance.

The ground truth parameters are:

| Parameter | Probability |
| --- | ---: |
| `P(H = HOT)` | `0.35` |
| `P(B = YES | H = COLD)` | `0.20` |
| `P(B = YES | H = HOT)` | `0.85` |
| `P(C = YES | H = COLD)` | `0.70` |
| `P(C = YES | H = HOT)` | `0.25` |

Training samples are drawn from this distribution with deterministic seed
`8675309`. The current default script invocation writes 1000 examples. The most
recent convergence run used 2000 examples.

## Data

The experiment writes two CSV files in the repository root:

| File | Purpose |
| --- | --- |
| `online-em-latent-training-set.csv` | The generated training set. It includes the hidden `H` value for evaluation only. |
| `online-em-latent-convergence.csv` | The online EM trace after each seen training instance. |

The training set has these columns:

| Column | Meaning |
| --- | --- |
| `instance` | One-based training instance number. |
| `h_hidden` | The sampled latent value. This is not passed to online EM. |
| `b` | Observed value of `B`. |
| `c` | Observed value of `C`. |

The convergence CSV has these columns:

| Column | Meaning |
| --- | --- |
| `seen_instances` | Number of training instances processed so far. |
| `truth_p_h_hot` | Ground truth value, `P(H = HOT) = 0.35`. |
| `raw_p_h_hot` | The model's direct current estimate for the first latent label. |
| `aligned_p_h_hot` | The estimate after correcting for possible latent-label swapping. |
| `latent_label_swapped` | Whether the raw latent labels were swapped for evaluation. |
| `abs_error_p_h_hot` | Absolute error between the aligned estimate and ground truth. |

`H` is hidden during training, so the `h_hidden` column should only be used for
checking the generated data and interpreting results. The learner receives only
the observed values of `B` and `C`.

## Training Procedure

The model is initialized with the correct structure and known value domains:

```text
H in {HOT, COLD}
B in {YES, NO}
C in {YES, NO}
```

The initial prior over `H` is uniform:

```text
P(H = HOT) = 0.5
P(H = COLD) = 0.5
```

For this convergence test, the conditional probability rows for `B` and `C` are
initialized to the ground truth and kept fixed. That isolates the behavior of
the latent prior estimate, so the plotted convergence is specifically about
recovering `P(H = HOT)` rather than simultaneously learning every CPD in the
network.

Online EM is run once per sample. The convergence CSV records the latent prior
estimate after each update.

## Results

In the latest 2000-example run, the generated sample contained 677 hidden `HOT`
assignments:

```text
677 / 2000 = 0.3385
```

That empirical hidden rate is close to the ground truth value `0.35`, but it is
not exactly equal because the training set is finite.

The final online EM estimate was:

```text
aligned P(H = HOT) = 0.32351973
absolute error     = 0.02648027
```

The late-stage per-instance changes became small:

| Window | Mean absolute error delta | Max absolute error delta |
| --- | ---: | ---: |
| Last 100 examples | `0.00643099` | `0.01172230` |
| Last 50 examples | `0.00593971` | `0.01148960` |

These deltas show that the online estimate is still moving, but by much smaller
amounts than during the early updates.

## Plot Interpretation

Run the plotting script to generate `online-em-latent-convergence.png`:

```sh
python3 "examples/Common Lisp/plot_online_em_convergence.py" \
  online-em-latent-convergence.csv \
  -o online-em-latent-convergence.png
```

The plot has three panels:

| Panel | How to read it |
| --- | --- |
| Estimate | Compares the raw estimate, label-aligned estimate, and ground truth `0.35`. |
| Absolute error | Shows the distance between the aligned estimate and ground truth. Lower is better. |
| Error delta | Shows `error[t] - error[t-1]` after each instance. Values near zero mean the estimate is changing slowly. Negative values mean the latest update reduced error; positive values mean it increased error. |

The raw and label-aligned estimates can differ because a latent variable's label
names are not intrinsically identifiable from observed data. If the model's
learned latent state corresponding to `HOT` is assigned the opposite internal
label, the aligned estimate flips `p` to `1 - p` for evaluation.

Online EM is stochastic and incremental, so the absolute error is not expected
to decrease monotonically after every sample. The important behavior is the
overall trend of the estimate toward the ground truth and the shrinking scale of
the error deltas as more data is processed.

## Reproducing

From the repository root, generate the default convergence data:

```sh
sbcl --disable-debugger --load "examples/Common Lisp/online-em-latent-convergence.lisp" --quit
```

To generate a 2000-example run:

```sh
sbcl --disable-debugger \
  --load "examples/Common Lisp/online-em-latent-convergence.lisp" \
  --eval '(hems::write-online-em-demo-convergence :n 2000)' \
  --quit
```

Then regenerate the plot with the Python command shown above.

The CSV and PNG outputs are generated artifacts. They are useful for inspection
and plotting, but the committed source of the experiment is the Lisp convergence
test, the Python plotting script, and this README.
