const pptxgen = require("pptxgenjs");
const path = require("path");
const IMG = path.join(__dirname, "img");
const F = "Times New Roman", MONO = "Courier New";
const INK = "222222", MUTED = "555555", LINE = "9E9E9E", BOXF = "F7F7F7";
const WALD = "D55E00", PERM = "0072B2", BOOT = "08306B";
const CA = "D9D9D9", CB = "FDAE6B";

function box(s, x, y, w, h, name, fill = BOXF) {
  s.addShape("roundRect", { x, y, w, h, rectRadius: 0.08, objectName: name,
    fill: { color: fill }, line: { color: LINE, width: 0.75 } });
}
function txt(s, text, o) {
  s.addText(text, Object.assign({ isTextBox: true, fontFace: F, color: INK,
    margin: 0, valign: "top" }, o));
}
function arrow(s, x1, y1, x2, y2, name) {
  s.addShape("line", { x: Math.min(x1, x2), y: Math.min(y1, y2),
    w: Math.abs(x2 - x1) || 0.001, h: Math.abs(y2 - y1) || 0.001,
    flipV: y2 < y1, objectName: name,
    line: { color: INK, width: 1.25, endArrowType: "triangle" } });
}
function grid(s, x, y, rows, name, sq = 0.15, gap = 0.04) {
  rows.forEach((r, i) => {
    txt(s, r.label, { x: x - 0.3, y: y + i * (sq + gap) - 0.01, w: 0.27,
      h: sq + 0.02, fontSize: 8, bold: true, align: "right", valign: "middle" });
    r.cells.split("").forEach((c, j) => {
      s.addShape("rect", { x: x + j * (sq + gap), y: y + i * (sq + gap),
        w: sq, h: sq, objectName: `${name}-${i}-${j}`,
        fill: { color: c === "A" ? CA : CB },
        line: { color: r.highlight || "FFFFFF", width: r.highlight ? 1 : 0.5 } });
    });
    if (r.note) txt(s, r.note, { x: x + r.cells.length * (sq + gap) + 0.03,
      y: y + i * (sq + gap) - 0.01, w: 0.3, h: sq + 0.02, fontSize: 7, bold: true,
      color: r.highlight || INK, valign: "middle" });
  });
}
const base = [
  { label: "P1", cells: "ABABBA" }, { label: "P2", cells: "BAABAB" },
  { label: "P3", cells: "ABBAAB" }, { label: "P4", cells: "BABAAB" }];
const flip = (c) => c.split("").map((z) => (z === "A" ? "B" : "A")).join("");

// ---------------------------------------------------------------------------
// Figure 1a: concept diagram
// ---------------------------------------------------------------------------
async function concept() {
  const pres = new pptxgen();
  pres.defineLayout({ name: "FIG", width: 7.25, height: 4.3 });
  pres.layout = "FIG";
  pres.title = "lmmresample: how resampling works";
  const s = pres.addSlide();
  s.background = { color: "FFFFFF" };
  const top = 0.55, H = 3.1;
  const heads = [["A", "Data", 0.2], ["B", "Fit the model once", 1.95],
                 ["C", "Resample whole blocks", 3.55], ["D", "Refit and compare", 5.7]];
  heads.forEach(([l, t, x]) => txt(s, [{ text: l + "  ", options: { bold: true, fontSize: 11 } },
    { text: t, options: { bold: true, fontSize: 10 } }], { x, y: 0.2, w: 2, h: 0.28 }));

  box(s, 0.2, top, 1.5, H, "box-data");
  grid(s, 0.58, top + 0.28, base, "data", 0.14, 0.035);
  txt(s, [{ text: "Condition ", options: { bold: true } },
          { text: "A", options: { bold: true, color: "8C8C8C" } },
          { text: " / ", options: {} },
          { text: "B", options: { bold: true, color: "E6550D" } }],
      { x: 0.3, y: top + 1.05, w: 1.3, h: 0.2, fontSize: 8, align: "center" });
  s.addImage({ path: path.join(IMG, "series_concept.png"), x: 0.23, y: 1.863,
    w: 1.44, h: 1.03, objectName: "img-series" });
  txt(s, "Participants (rows) complete trials (squares); each trial is a series of correlated samples",
      { x: 0.3, y: 2.928, w: 1.3, h: 0.7, fontSize: 8, italic: true, color: MUTED });

  arrow(s, 1.72, top + H / 2, 1.93, top + H / 2, "arr-1");

  const bTop = top + 0.6, bH = 1.9;
  box(s, 1.95, bTop, 1.35, bH, "box-fit");
  txt(s, "lmer(y ~\n  condition *\n  ns(time, 4) +\n  (1 | participant))",
      { x: 2.03, y: bTop + 0.15, w: 1.25, h: 0.6, fontSize: 7, fontFace: MONO, bold: true });
  txt(s, [{ text: "Observed statistic ", options: { bold: true } },
          { text: "T", options: { bold: true, italic: true } },
          { text: "obs", options: { bold: true, italic: true, subscript: true } },
          { text: " (e.g. joint Wald χ² of condition × time)", options: {} }],
      { x: 2.05, y: bTop + 0.82, w: 1.17, h: 0.45, fontSize: 8 });
  txt(s, "The same model is refitted to every resample",
      { x: 2.05, y: bTop + 1.4, w: 1.17, h: 0.4, fontSize: 8, italic: true, color: MUTED });

  arrow(s, 3.32, top + H / 2, 3.53, top + H / 2, "arr-2");

  box(s, 3.55, top, 1.9, 1.5, "box-perm");
  txt(s, "Permutation (tests)", { x: 3.65, y: top + 0.07, w: 1.7, h: 0.2, fontSize: 9, bold: true, color: PERM });
  grid(s, 3.98, top + 0.32, base.map((r, i) => i === 1 || i === 3 ?
    { label: r.label, cells: flip(r.cells), highlight: WALD } : r), "perm");
  txt(s, "Outlined participants: labels of all their trials flipped",
      { x: 3.65, y: top + 1.1, w: 1.75, h: 0.38, fontSize: 7, italic: true, color: MUTED });

  box(s, 3.55, top + 1.6, 1.9, 1.5, "box-boot");
  txt(s, "Bootstrap (intervals)", { x: 3.65, y: top + 1.67, w: 1.7, h: 0.2, fontSize: 9, bold: true, color: BOOT });
  grid(s, 3.98, top + 1.92, [
    { label: "P3", cells: base[2].cells, highlight: BOOT, note: "×2" }, { label: "P1", cells: base[0].cells },
    { label: "P3", cells: base[2].cells, highlight: BOOT, note: "×2" }, { label: "P4", cells: base[3].cells }], "boot");
  txt(s, "Participants drawn with replacement with all their trials (P3 twice, P2 not drawn)",
      { x: 3.65, y: top + 2.72, w: 1.75, h: 0.36, fontSize: 7, italic: true, color: MUTED });

  arrow(s, 5.47, top + 0.75, 5.68, top + 0.75, "arr-3");
  arrow(s, 5.47, top + 2.35, 5.68, top + 2.35, "arr-4");

  box(s, 5.7, top, 1.35, 1.5, "box-p");
  s.addImage({ path: path.join(IMG, "concept_null.png"), x: 5.76, y: top + 0.1, w: 1.24, h: 0.88, objectName: "img-null" });
  txt(s, [{ text: "p-value: ", options: { bold: true } },
          { text: "share of resamples with T ≥ T", options: {} },
          { text: "obs", options: { subscript: true } }],
      { x: 5.78, y: top + 1.04, w: 1.22, h: 0.42, fontSize: 7.5 });

  box(s, 5.7, top + 1.6, 1.35, 1.5, "box-ci");
  s.addImage({ path: path.join(IMG, "concept_boot.png"), x: 5.76, y: top + 1.7, w: 1.24, h: 0.88, objectName: "img-boot" });
  txt(s, [{ text: "95% CI: ", options: { bold: true } },
          { text: "percentiles of the refitted estimates", options: {} }],
      { x: 5.78, y: top + 2.64, w: 1.22, h: 0.42, fontSize: 7.5 });

  txt(s, "Trials are never split, so the dependence between samples within a trial is carried into every resample without being modelled.",
      { x: 0.2, y: 3.8, w: 6.85, h: 0.35, fontSize: 9, italic: true, align: "center" });
  await pres.writeFile({ fileName: path.join(__dirname, "figure1_concept.pptx") });
}

// ---------------------------------------------------------------------------
// Figure 1b: workflow, journal style
// ---------------------------------------------------------------------------
async function workflow() {
  const pres = new pptxgen();
  pres.defineLayout({ name: "FIG", width: 7.25, height: 8.4 });
  pres.layout = "FIG";
  pres.title = "lmmresample workflow";
  const s = pres.addSlide();
  s.background = { color: "FFFFFF" };

  txt(s, [{ text: "A  ", options: { bold: true, fontSize: 12 } },
          { text: "The problem", options: { bold: true, fontSize: 11 } }],
      { x: 0.2, y: 0.15, w: 4, h: 0.3 });
  box(s, 0.25, 0.55, 2.5, 1.65, "a-data");
  s.addImage({ path: path.join(IMG, "series_workflow.png"), x: 0.46, y: 0.6, w: 2.08, h: 1.24,
    objectName: "a-img" });
  txt(s, [{ text: "Densely sampled trials", options: { bold: true, breakLine: true } },
          { text: "e.g. 67 samples per trial at 33 Hz", options: { italic: true } }],
      { x: 0.3, y: 1.85, w: 2.4, h: 0.34, fontSize: 9, align: "center" });
  arrow(s, 2.79, 1.37, 3.01, 1.37, "a-arr1");
  box(s, 3.05, 0.55, 1.85, 1.65, "a-model");
  txt(s, "Usual mixed-model tests", { x: 3.1, y: 0.62, w: 1.75, h: 0.2, fontSize: 9, bold: true, align: "center" });
  s.addImage({ path: path.join(IMG, "problem_dependence.png"), x: 3.125, y: 0.84, w: 1.70, h: 0.95,
    objectName: "a-dep" });
  txt(s, "Wald and Satterthwaite tests treat correlated samples as nearly independent",
      { x: 3.1, y: 1.84, w: 1.75, h: 0.34, fontSize: 7.5, italic: true, align: "center" });
  arrow(s, 4.94, 1.37, 5.11, 1.37, "a-arr2");
  box(s, 5.15, 0.55, 1.85, 1.65, "a-result", "FDF0E6");
  txt(s, "Condition × time effect", { x: 5.2, y: 0.62, w: 1.75, h: 0.2, fontSize: 9, bold: true, align: "center" });
  s.addImage({ path: path.join(IMG, "problem_result.png"), x: 5.18, y: 0.84, w: 1.79, h: 1.0,
    objectName: "a-res" });
  txt(s, "Simulated data with no true effect (false positives) and with a known effect (coverage)",
      { x: 5.2, y: 1.86, w: 1.75, h: 0.32, fontSize: 6.5, italic: true, color: MUTED, align: "center" });

  s.addShape("line", { x: 0.2, y: 2.4, w: 6.85, h: 0, objectName: "rule",
    line: { color: "BDBDBD", width: 0.75 } });

  txt(s, [{ text: "B  ", options: { bold: true, fontSize: 12 } },
          { text: "The lmmresample workflow: keep the model, replace the inference", options: { bold: true, fontSize: 11 } }],
      { x: 0.2, y: 2.55, w: 6.8, h: 0.3 });

  const panels = [
    ["1", "Model fit", "Is the time course captured?", "diag_timecourse()", "timecourse.png"],
    ["2", "Dependence", "How correlated are the samples?", "diag_acf()", "acf.png"],
    ["3", "Validity", "Is the test valid in my data?", "perm_calibrate()", "calibrate.png"],
    ["4", "Test", "Is there an effect?", "perm_test()", "null.png"],
    ["5", "Size", "How large is it?", "boot_ci()", "intervals.png"],
    ["6", "Several tests", "Which features or pairs differ?", "perm_maxt(), perm_pairwise()", "maxt.png"]];
  const xs = [0.25, 2.65, 5.05], ys = [3.0, 5.65], W = 1.95;
  panels.forEach((p, k) => {
    const x = xs[k % 3], y = ys[Math.floor(k / 3)];
    txt(s, [{ text: p[0] + "  ", options: { bold: true, fontSize: 12 } },
            { text: p[1], options: { bold: true, fontSize: 10 } }],
        { x, y, w: W, h: 0.27, objectName: `p${p[0]}-title` });
    txt(s, p[2], { x, y: y + 0.28, w: W, h: 0.22, fontSize: 8.5, italic: true, bold: true });
    txt(s, p[3], { x, y: y + 0.5, w: W, h: 0.2, fontSize: 8, fontFace: MONO, color: MUTED });
    s.addImage({ path: path.join(IMG, p[4]), x: x + 0.02, y: y + 0.78, w: 1.91,
      h: 1.6, objectName: `p${p[0]}-img` });
    if (k % 3 < 2) arrow(s, x + W + 0.05, y + 1.5, x + W + 0.38, y + 1.5, `p${p[0]}-arr`);
  });
  txt(s, "Schematic illustrations; results for the real data are shown in Figures 9–11.",
      { x: 0.25, y: 8.1, w: 6.75, h: 0.22, fontSize: 7.5, italic: true, color: MUTED });
  await pres.writeFile({ fileName: path.join(__dirname, "figure1_workflow.pptx") });
}

(async () => { await concept(); await workflow(); console.log("ok"); })();
