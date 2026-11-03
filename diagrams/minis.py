import numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from pathlib import Path

OUT = Path(__file__).parent / "img"
plt.rcParams.update({"font.family": "Liberation Serif", "font.size": 9,
                     "font.weight": "bold", "axes.labelweight": "bold",
                     "axes.linewidth": 1.0, "axes.spines.top": False,
                     "axes.spines.right": False, "mathtext.fontset": "stix"})
WALD, PERM, BOOT, GREY = "#D55E00", "#0072B2", "#08306B", "#8C8C8C"
rng = np.random.default_rng(7)
W, H = 2.0, 1.45


def new(xlabel=None, ylabel=None):
    fig, ax = plt.subplots(figsize=(W, H))
    ax.set_xticks([]); ax.set_yticks([])
    if xlabel: ax.set_xlabel(xlabel)
    if ylabel: ax.set_ylabel(ylabel)
    return fig, ax


def save(fig, name):
    fig.savefig(OUT / f"{name}.png", dpi=400, transparent=True,
                bbox_inches="tight", pad_inches=0.03)
    plt.close(fig)


def ar1(n, phi, sd):
    x = np.zeros(n); e = rng.normal(0, sd, n)
    for i in range(1, n): x[i] = phi * x[i - 1] + e[i]
    return x


# Problem: correlated samples within trials
fig, ax = new("Time in trial", "Trials")
t = np.linspace(0, 1, 60)
for k, c in enumerate(["#4D4D4D", "#7F7F7F", "#A6A6A6"]):
    y = ar1(60, 0.95, 0.22) + k * 1.5
    ax.plot(t, y, color=c, lw=1.1)
    ax.plot(t[::3], y[::3], "o", ms=2, color=c)
save(fig, "series")

# 1 Model fit: residuals of a misspecified vs adequate model
fig, ax = new("Time", "Mean residual")
t = np.linspace(0, 1, 100)
bad = 0.9 * (1 / (1 + np.exp(-(t - 0.45) * 14)) - 0.55)
good = 0.06 * np.sin(t * 9)
for y, col in [(bad, WALD), (good, PERM)]:
    ax.fill_between(t, y - 0.12, y + 0.12, color=col, alpha=0.18, lw=0)
    ax.plot(t, y, color=col, lw=1.8)
ax.axhline(0, color="black", lw=0.8, ls="--")
ax.text(0.98, 0.42, "misfit", color=WALD, ha="right", fontsize=9)
ax.text(0.98, -0.25, "good fit", color=PERM, ha="right", fontsize=9)
save(fig, "timecourse")

# 2 Dependence: decaying autocorrelation
fig, ax = new("Lag", "Autocorrelation")
lag = np.arange(0, 21)
ax.plot(lag, 0.96 ** (lag * 1.6), color="black", lw=1.8, marker="o", ms=2.5)
ax.axhline(0, color="black", lw=0.8, ls="--")
ax.set_ylim(-0.1, 1.05)
save(fig, "acf")

# 3 Validity: false-positive rates under the null
fig, ax = new(None, "False positives")
ax.bar([0, 1], [0.8, 0.05], color=[WALD, PERM], width=0.6)
ax.axhline(0.05, color="black", lw=0.8, ls="--")
ax.set_xticks([0, 1]); ax.set_xticklabels(["Wald", "Permutation"])
ax.text(1.42, 0.09, "5%", va="bottom", fontsize=9)
ax.set_xlim(-0.5, 1.7); ax.set_ylim(0, 0.9)
save(fig, "calibrate")

# 4 Test: permutation null and observed statistic
fig, ax = new("Statistic", "Resamples")
ax.hist(rng.chisquare(4, 4999), bins=40, color=PERM, alpha=0.8)
ax.axvline(17, color=WALD, lw=1.8)
ax.text(16.5, ax.get_ylim()[1] * 0.9, "observed ", color=WALD, ha="right",
        fontsize=9)
save(fig, "null")

# 5 Size: narrow Wald vs bootstrap interval
fig, ax = new("Estimate", None)
ax.spines["left"].set_visible(False)
for y, half, col, lab in [(1, 0.05, WALD, "Wald"), (0, 0.33, BOOT, "Bootstrap")]:
    ax.plot([1 - half, 1 + half], [y, y], color=col, lw=2.6,
            solid_capstyle="butt")
    ax.plot(1, y, "o", color=col, ms=5)
    ax.text(1 + half + 0.04, y, lab, va="center", color=col, fontsize=9)
ax.set_ylim(-0.7, 1.7); ax.set_xlim(0.5, 1.75)
save(fig, "intervals")

# 6 Several tests: distribution of the largest statistic
fig, ax = new("Largest statistic", "Resamples")
mx = np.max(np.abs(rng.normal(size=(4999, 3)) @ np.linalg.cholesky(
    np.array([[1, .8, .8], [.8, 1, .8], [.8, .8, 1]])).T), axis=1)
ax.hist(mx, bins=40, color=GREY, alpha=0.75)
thr = np.quantile(mx, 0.95)
ax.axvline(thr, color="black", lw=0.9, ls="--")
for v, c in [(1.3, PERM), (3.1, WALD), (3.7, WALD)]:
    ax.axvline(v, color=c, lw=1.6)
save(fig, "maxt")

# Concept diagram: null and bootstrap distributions
def concept_hist(values, color, name, lines, labels):
    fig, ax = plt.subplots(figsize=(1.75, 1.2))
    ax.hist(values, bins=32, color=color, alpha=0.85)
    ymax = ax.get_ylim()[1]
    for v, st, c in lines:
        ax.axvline(v, color=c, lw=1.6 if st == "-" else 1.1, ls=st)
    for x, txt_, ha, c in labels:
        ax.text(x, ymax * 0.97, txt_, ha=ha, va="top", fontsize=8, color=c)
    ax.set_xticks([]); ax.set_yticks([]); ax.spines["left"].set_visible(False)
    ax.set_ylim(0, ymax * 1.05)
    save(fig, name)

concept_hist(rng.normal(size=4999), PERM, "concept_null",
             [(2.9, "-", WALD)], [(2.75, "$T_{obs}$", "right", WALD)])
b = rng.normal(0, 1, 4999)
lo, hi = np.quantile(b, [0.025, 0.975])
concept_hist(b, BOOT, "concept_boot",
             [(lo, "--", "black"), (hi, "--", "black")],
             [(lo - 0.12, "2.5%", "right", "black"), (hi + 0.12, "97.5%", "left", "black")])
print("ok")
