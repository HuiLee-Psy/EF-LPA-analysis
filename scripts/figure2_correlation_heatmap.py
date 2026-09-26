#!/usr/bin/env python3
"""Create the manuscript's lower-triangle Pearson-correlation heatmap."""

import os
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import LinearSegmentedColormap
import numpy as np
import pandas as pd


ROOT = Path(os.environ.get("EF_LPA_PROJECT_ROOT", ".")).resolve()
RESULTS = Path(os.environ.get("EF_LPA_OUTPUT_DIR", ROOT / "results")).resolve()


def read_matrix(filename: str) -> pd.DataFrame:
    frame = pd.read_csv(RESULTS / filename, index_col=0, encoding="utf-8-sig")
    return frame.apply(pd.to_numeric, errors="coerce")


def stars(p_value: float) -> str:
    if pd.isna(p_value):
        return ""
    if p_value < 0.001:
        return "***"
    if p_value < 0.01:
        return "**"
    if p_value < 0.05:
        return "*"
    return ""


label_mapping = {
    "Sustained attention": "Sustained\nattention",
    "Working memory": "Working\nmemory",
    "Cognitive flexibility": "Cognitive\nflexibility",
    "Interference inhibition": "Interference\ninhibition",
    "Response inhibition": "Response\ninhibition",
    "Chinese score": "Chinese\nscore",
    "English score": "English\nscore",
    "Math score": "Math\nscore",
    "Hyperactivity/inattention": "Hyperactivity/\ninattention",
    "Emotional symptoms": "Emotional\nsymptoms",
    "Conduct problems": "Conduct\nproblems",
    "Peer problems": "Peer\nproblems",
    "Prosocial behavior": "Prosocial\nbehavior",
    "Total difficulties": "Total\ndifficulties",
}

r_values = read_matrix("correlation_r_matrix.csv")
p_values = read_matrix("correlation_p_values.csv")
variables = [v for v in r_values.index if v in r_values.columns and v in p_values.index]
r_values = r_values.loc[variables, variables]
p_values = p_values.loc[variables, variables]
if not np.allclose(r_values.to_numpy(), r_values.to_numpy().T, equal_nan=True):
    raise ValueError("The correlation matrix is not symmetric.")

mask = np.triu(np.ones(r_values.shape, dtype=bool), k=0)
masked = np.ma.array(r_values.to_numpy(), mask=mask)
labels = [label_mapping.get(v, v) for v in variables]

colors = list(reversed([
    "#f33e4c", "#f34855", "#f2535f", "#f25e6a", "#f26974", "#f2757e",
    "#f28089", "#f28b93", "#f2959c", "#f2a1a7", "#f2acb1", "#f2b7bc",
    "#f2c2c6", "#f2ced0", "#f2d9db", "#f2e3e4", "#f2eeee", "#e9edf1",
    "#dde7ed", "#d2e0ea", "#c6dae7", "#bbd3e4", "#afcde1", "#a5c7de",
    "#9ac1db", "#8ebad7", "#82b3d4", "#77add1", "#6ba7ce", "#60a0ca",
    "#549ac7", "#4a94c4", "#3f8ec1",
]))
cmap = LinearSegmentedColormap.from_list("original_diverging", colors, N=256)
cmap.set_bad("white")

plt.rcParams.update({
    "font.family": "sans-serif",
    "font.sans-serif": ["Arial", "Helvetica", "DejaVu Sans"],
    "pdf.fonttype": 42,
    "ps.fonttype": 42,
})
figure, axis = plt.subplots(figsize=(11, 11))
image = axis.imshow(masked, cmap=cmap, vmin=-0.8, vmax=0.8, aspect="equal")
axis.set_xticks(np.arange(len(labels)), labels=labels, rotation=30, ha="center", fontsize=7.5)
axis.set_yticks(np.arange(len(labels)), labels=labels, fontsize=7.5)
axis.tick_params(length=0)
axis.set_xticks(np.arange(-0.5, len(labels), 1), minor=True)
axis.set_yticks(np.arange(-0.5, len(labels), 1), minor=True)
axis.grid(which="minor", color="white", linewidth=0.7)
axis.tick_params(which="minor", bottom=False, left=False)
for spine in axis.spines.values():
    spine.set_visible(False)

for row in range(len(variables)):
    for column in range(row):
        value = r_values.iloc[row, column]
        if pd.notna(value):
            axis.text(
                column, row, f"{value:.2f}{stars(p_values.iloc[row, column])}",
                ha="center", va="center", fontsize=7,
            )

colorbar = figure.colorbar(
    image, ax=axis, shrink=0.62, aspect=40, pad=0.025,
    ticks=[-0.8, -0.4, 0, 0.4, 0.8],
)
colorbar.set_label("Pearson correlation coefficient (r)", fontsize=9)
colorbar.ax.tick_params(labelsize=8)
figure.tight_layout()
figure.savefig(RESULTS / "Figure2_Correlation_Heatmap.png", dpi=600, bbox_inches="tight", facecolor="white")
figure.savefig(RESULTS / "Figure2_Correlation_Heatmap.pdf", bbox_inches="tight", facecolor="white")
plt.close(figure)
