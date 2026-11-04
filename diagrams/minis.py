"""Small panels for Figures 1 and 8, drawn at the exact size they occupy on
the slide, so that nothing is stretched and text keeps its point size."""
import numpy as np, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from pathlib import Path

OUT = Path(__file__).parent / "img"
OUT.mkdir(exist_ok=True)
plt.rcParams.update({"font.family": "Liberation Serif", "font.size": 8,
                     "font.weight": "bold", "axes.labelweight": "bold",
                     "axes.labelsize": 8, "axes.linewidth": 0.9,
                     "axes.spines.top": False, "axes.spines.right": False,
                     "mathtext.fontset": "stix"})
WALD, PERM, BOOT, GREY = "#D55E00", "#0072B2", "#08306B", "#8C8C8C"

# Sizes (inches) of the picture frames on the slides
SIZE = {"series_concept": (1.44, 1.03), "series_workflow": (2.08, 1.24),
        "problem_dependence": (1.70, 0.95), "problem_result": (1.79, 1.00),
        "panel": (1.91, 1.60), "concept_hist": (1.24, 0.88)}


def fig_of(size, nrows=1, ncols=1, **kw):
    fig, ax = plt.subplots(nrows, ncols, figsize=SIZE[size], layout="constrained", **kw)
    fig.get_layout_engine().set(w_pad=0.015, h_pad=0.015, wspace=0.04, hspace=0.04)
    return fig, ax


def save(fig, name):
    fig.savefig(OUT / f"{name}.png", dpi=400, transparent=True)
    plt.close(fig)


def bare(ax, xlabel=None, ylabel=None):
    ax.set_xticks([]); ax.set_yticks([])
    if xlabel: ax.set_xlabel(xlabel, labelpad=2)
    if ylabel: ax.set_ylabel(ylabel, labelpad=2)


def ar1(rng, n, phi, sd):
    x = np.zeros(n); e = rng.normal(0, sd, n)
    for i in range(1, n): x[i] = phi * x[i - 1] + e[i]
    return x


# Correlated samples within trials (Figure 1A and Figure 8A, box 1)
def series(name):
    rng = np.random.default_rng(7)
    fig, ax = fig_of(name)
    t = np.linspace(0, 1, 60)
    for k, c in enumerate(["#4D4D4D", "#7F7F7F", "#A6A6A6"]):
        y = ar1(rng, 60, 0.95, 0.22) + k * 1.5
        ax.plot(t, y, color=c, lw=1.0)
        ax.plot(t[::3], y[::3], "o", ms=1.8, color=c)
    bare(ax, "Time in trial", "Trials")
    save(fig, name)


series("series_concept")
series("series_workflow")

# Figure 8A, box 2: assumed vs actual dependence within one trial
rng = np.random.default_rng(3)
fig, axs = fig_of("problem_dependence", 1, 2, sharey=True)
n = 24; t = np.arange(n)
ind = rng.normal(0, 1, n)
x = np.zeros(n); e = rng.normal(0, 0.35, n)
for i in range(1, n): x[i] = 0.93 * x[i - 1] + e[i]
x = (x - x.mean()) / x.std()
for ax, y, ttl, col in [(axs[0], ind, "Assumed", GREY), (axs[1], x, "Actual", WALD)]:
    ax.plot(t, y, color=col, lw=0.6, alpha=0.6)
    ax.scatter(t, y, s=3.5, color=col, zorder=3, linewidths=0)
    ax.set_title(ttl, fontsize=8, fontweight="bold", pad=2)
    bare(ax, "Samples")
save(fig, "problem_dependence")

# Figure 8A, box 3: nominal vs observed
fig, axs = fig_of("problem_result", 2, 1)
rows = [("False positives", 5, 50, "≈50%"), ("95% CI coverage", 95, 61, "61%")]
for ax, (lab, nom, obs, txt) in zip(axs, rows):
    ax.barh([1], [nom], color=GREY, height=0.68)
    ax.barh([0], [obs], color=WALD, height=0.68)
    ax.text(nom + 2, 1, f"{nom}%", va="center", fontsize=7, color="#555555")
    ax.text(obs + 2, 0, txt, va="center", fontsize=7, color=WALD)
    ax.set_xlim(0, 122); ax.set_ylim(-0.55, 1.55)
    ax.set_yticks([1, 0]); ax.set_yticklabels(["Nominal", "Observed"], fontsize=7)
    ax.tick_params(axis="y", length=0, pad=1)
    ax.set_xticks([]); ax.spines["bottom"].set_visible(False)
    ax.set_title(lab, fontsize=7.5, fontweight="bold", pad=1, loc="left")
save(fig, "problem_result")

# Figure 8B panels
rng = np.random.default_rng(7)

# 1 Model fit: residuals of a misspecified vs adequate model
fig, ax = fig_of("panel")
t = np.linspace(0, 1, 100)
bad = 0.9 * (1 / (1 + np.exp(-(t - 0.45) * 14)) - 0.55)
good = 0.06 * np.sin(t * 9)
for y, col in [(bad, WALD), (good, PERM)]:
    ax.fill_between(t, y - 0.12, y + 0.12, color=col, alpha=0.18, lw=0)
    ax.plot(t, y, color=col, lw=1.6)
ax.axhline(0, color="black", lw=0.7, ls="--")
gap = 0.16  # same distance from each curve
ax.text(0.98, bad[-1] + gap, "misfit", color=WALD, ha="right", va="bottom", fontsize=8)
ax.text(0.98, good[-1] - gap, "good fit", color=PERM, ha="right", va="top", fontsize=8)
ax.set_ylim(-0.75, 0.82)
bare(ax, "Time", "Mean residual")
save(fig, "timecourse")

# 2 Dependence: decaying autocorrelation
fig, ax = fig_of("panel")
lag = np.arange(0, 21)
ax.plot(lag, 0.96 ** (lag * 1.6), color="black", lw=1.6, marker="o", ms=2.2)
ax.axhline(0, color="black", lw=0.7, ls="--")
ax.set_ylim(-0.1, 1.05)
bare(ax, "Lag", "Autocorrelation")
save(fig, "acf")

# 3 Validity: false-positive rates under the null
fig, ax = fig_of("panel")
ax.bar([0, 1], [0.8, 0.05], color=[WALD, PERM], width=0.6)
ax.axhline(0.05, color="black", lw=0.7, ls="--")
ax.set_xticks([0, 1]); ax.set_xticklabels(["Wald", "Permutation"], fontsize=8)
ax.tick_params(axis="x", length=0)
ax.set_yticks([])
ax.text(1.42, 0.09, "5%", va="bottom", fontsize=8)
ax.set_xlim(-0.5, 1.7); ax.set_ylim(0, 0.9)
ax.set_ylabel("False positives", labelpad=2)
save(fig, "calibrate")

# 4 Test: permutation null and observed statistic
fig, ax = fig_of("panel")
ax.hist(rng.chisquare(4, 4999), bins=40, color=PERM, alpha=0.8)
ax.axvline(17, color=WALD, lw=1.6)
ax.text(16.3, ax.get_ylim()[1] * 0.9, "observed", color=WALD, ha="right", fontsize=8)
bare(ax, "Statistic", "Resamples")
save(fig, "null")

# 5 Size: narrow Wald vs bootstrap interval
fig, ax = fig_of("panel")
ax.spines["left"].set_visible(False)
for y, half, col, lab in [(1, 0.05, WALD, "Wald"), (0, 0.33, BOOT, "Bootstrap")]:
    ax.plot([1 - half, 1 + half], [y, y], color=col, lw=2.4, solid_capstyle="butt")
    ax.plot(1, y, "o", color=col, ms=4.5)
    ax.text(1 + half + 0.04, y, lab, va="center", color=col, fontsize=8)
ax.set_ylim(-0.7, 1.7); ax.set_xlim(0.5, 1.8)
bare(ax, "Estimate")
save(fig, "intervals")

# 6 Several tests: distribution of the largest statistic
fig, ax = fig_of("panel")
mx = np.max(np.abs(rng.normal(size=(4999, 3)) @ np.linalg.cholesky(
    np.array([[1, .8, .8], [.8, 1, .8], [.8, .8, 1]])).T), axis=1)
ax.hist(mx, bins=40, color=GREY, alpha=0.75)
ax.axvline(np.quantile(mx, 0.95), color="black", lw=0.8, ls="--")
for v, c in [(1.3, PERM), (3.1, WALD), (3.7, WALD)]:
    ax.axvline(v, color=c, lw=1.4)
bare(ax, "Largest statistic", "Resamples")
save(fig, "maxt")


# Figure 1D: null and bootstrap distributions
def concept_hist(values, color, name, lines, labels):
    fig, ax = fig_of("concept_hist")
    ax.hist(values, bins=32, color=color, alpha=0.85)
    ymax = ax.get_ylim()[1]
    for v, st, c in lines:
        ax.axvline(v, color=c, lw=1.4 if st == "-" else 1.0, ls=st)
    for x, txt_, ha, c in labels:
        ax.text(x, ymax * 0.97, txt_, ha=ha, va="top", fontsize=7, color=c)
    ax.set_xticks([]); ax.set_yticks([]); ax.spines["left"].set_visible(False)
    ax.set_ylim(0, ymax * 1.05); ax.set_xlim(-4.2, 4.2)
    save(fig, name)


concept_hist(rng.normal(size=4999), PERM, "concept_null",
             [(2.9, "-", WALD)], [(2.75, "$T_{obs}$", "right", WALD)])
b = rng.normal(0, 1, 4999)
lo, hi = np.quantile(b, [0.025, 0.975])
concept_hist(b, BOOT, "concept_boot",
             [(lo, "--", "black"), (hi, "--", "black")],
             [(lo - 0.12, "2.5%", "right", "black"), (hi + 0.12, "97.5%", "left", "black")])
print("ok")
