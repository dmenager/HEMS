#!/usr/bin/env python3
"""Plot online EM latent-parameter convergence from CSV output."""

import argparse
import csv
from pathlib import Path

import matplotlib.pyplot as plt


def read_convergence_csv(path):
    rows = []
    with path.open(newline="") as stream:
        reader = csv.DictReader(stream)
        for row in reader:
            rows.append(
                {
                    "seen_instances": int(row["seen_instances"]),
                    "truth_p_h_hot": float(row["truth_p_h_hot"]),
                    "raw_p_h_hot": float(row["raw_p_h_hot"]),
                    "aligned_p_h_hot": float(row["aligned_p_h_hot"]),
                    "abs_error_p_h_hot": float(row["abs_error_p_h_hot"]),
                }
            )
    if not rows:
        raise ValueError(f"No convergence rows found in {path}")
    return rows


def plot_convergence(rows, output_path):
    x = [row["seen_instances"] for row in rows]
    truth = [row["truth_p_h_hot"] for row in rows]
    raw = [row["raw_p_h_hot"] for row in rows]
    aligned = [row["aligned_p_h_hot"] for row in rows]
    error = [row["abs_error_p_h_hot"] for row in rows]
    error_delta = [0.0]
    error_delta.extend(
        error[i] - error[i - 1]
        for i in range(1, len(error))
    )

    fig, (ax_estimate, ax_error, ax_delta) = plt.subplots(
        3,
        1,
        figsize=(10, 9),
        sharex=True,
        constrained_layout=True,
    )

    ax_estimate.plot(x, raw, label="Raw estimate", color="#1f77b4", linewidth=1.8)
    ax_estimate.plot(
        x,
        aligned,
        label="Label-aligned estimate",
        color="#2ca02c",
        linewidth=1.5,
        linestyle="--",
    )
    ax_estimate.plot(
        x,
        truth,
        label="Ground truth",
        color="#d62728",
        linewidth=1.4,
        linestyle=":",
    )
    ax_estimate.set_ylabel("P(H = HOT)")
    ax_estimate.set_title("Online EM Latent Parameter Convergence")
    ax_estimate.grid(True, alpha=0.25)
    ax_estimate.legend()

    ax_error.plot(x, error, label="Absolute error", color="#9467bd", linewidth=1.8)
    ax_error.set_ylabel("Absolute error")
    ax_error.grid(True, alpha=0.25)
    ax_error.legend()

    ax_delta.axhline(0.0, color="#555555", linewidth=1.0, alpha=0.8)
    ax_delta.plot(
        x,
        error_delta,
        label="Error delta",
        color="#ff7f0e",
        linewidth=1.2,
    )
    ax_delta.set_xlabel("Seen training instances")
    ax_delta.set_ylabel("Delta error")
    ax_delta.grid(True, alpha=0.25)
    ax_delta.legend()

    output_path.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output_path, dpi=160)
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser(
        description="Plot online EM convergence as a function of training instances."
    )
    parser.add_argument(
        "csv",
        nargs="?",
        default="online-em-latent-convergence.csv",
        type=Path,
        help="Path to the convergence CSV.",
    )
    parser.add_argument(
        "-o",
        "--output",
        default="online-em-latent-convergence.png",
        type=Path,
        help="Path for the output PNG.",
    )
    args = parser.parse_args()

    rows = read_convergence_csv(args.csv)
    plot_convergence(rows, args.output)
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()
