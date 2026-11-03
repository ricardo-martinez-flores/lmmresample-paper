import numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from pathlib import Path
OUT = Path(__file__).parent / "img"
plt.rcParams.update({"font.family": "Liberation Serif", "font.size": 8,
                     "font.weight": "bold", "axes.linewidth": 0.8,
                     "axes.spines.top": False, "axes.spines.right": False})
WALD, GREY = "#D55E00", "#8C8C8C"
rng = np.random.default_rng(3)

def save(fig, name):
    fig.savefig(OUT / f"{name}.png", dpi=400, transparent=True,
                bbox_inches="tight", pad_inches=0.03)
    plt.close(fig)

# Box 2: assumed vs actual dependence of samples within one trial
fig, axs = plt.subplots(1, 2, figsize=(1.85, 0.95), sharey=True)
n = 24; t = np.arange(n)
ind = rng.normal(0, 1, n)
x = np.zeros(n); e = rng.normal(0, 0.35, n)
for i in range(1, n): x[i] = 0.93 * x[i-1] + e[i]
x = (x - x.mean()) / x.std()
for ax, y, ttl, col in [(axs[0], ind, "Assumed", GREY), (axs[1], x, "Actual", WALD)]:
    ax.plot(t, y, color=col, lw=0.6, alpha=0.6)
    ax.scatter(t, y, s=4, color=col, zorder=3)
    ax.set_title(ttl, fontsize=8, fontweight="bold", pad=2)
    ax.set_xticks([]); ax.set_yticks([])
    ax.set_xlabel("Samples", fontsize=7)
save(fig, "problem_dependence")

# Box 3: nominal vs observed
fig, axs = plt.subplots(2, 1, figsize=(1.75, 1.05))
rows = [("False positives", 5, 50, "≈50%"), ("95% CI coverage", 95, 61, "61%")]
for ax, (lab, nom, obs, txt) in zip(axs, rows):
    ax.barh([1], [nom], color=GREY, height=0.7)
    ax.barh([0], [obs], color=WALD, height=0.7)
    ax.text(nom + 2, 1, f"{nom}%", va="center", fontsize=7, color="#555555")
    ax.text(obs + 2, 0, txt, va="center", fontsize=7.5, color=WALD)
    ax.set_xlim(0, 118); ax.set_ylim(-0.6, 1.6)
    ax.set_yticks([1, 0]); ax.set_yticklabels(["Nominal", "Observed"], fontsize=6.5)
    ax.tick_params(axis="y", length=0, pad=1)
    ax.set_xticks([]); ax.spines["bottom"].set_visible(False)
    ax.set_title(lab, fontsize=7.5, fontweight="bold", pad=1, loc="left")
fig.subplots_adjust(hspace=0.75)
save(fig, "problem_result")
