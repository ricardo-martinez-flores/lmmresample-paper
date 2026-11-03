# =============================================================================
# lmmresample: figures
#
# Input:    lmmresample_simulations/ and lmmresample_realdata/ (results of
#           simulation_lmmresample.R and analysis_real_data.R)
# Usage:    Rscript figures_lmmresample.R
# Settings: LMM_BASE (folder containing both result folders),
#           LMM_DATA (real data file, for the pupil curves)
# =============================================================================

# ---- Settings
BASE_DIR <- Sys.getenv("LMM_BASE", ".")
SIM_DIR  <- file.path(BASE_DIR, "lmmresample_simulations")
REAL_DIR <- file.path(BASE_DIR, "lmmresample_realdata")
FIG_DIR  <- file.path(BASE_DIR, "figures")
DATA_FILE <- Sys.getenv("LMM_DATA", "")
if (!nzchar(DATA_FILE)) {
  candidates <- file.path(BASE_DIR, c("data", "."),
                          rep(c("preprocessed_accepted_trials.csv.gz",
                                "preprocessed_accepted_trials.csv"), each = 2))
  DATA_FILE <- c(candidates[file.exists(candidates)], "")[1]
}
FONT     <- "Times New Roman"
FORMATS  <- c("pdf", "png", "tiff")
DPI      <- 600
DRAW     <- "all"

DEFAULT <- list(
  width        = 174,
  height       = 120,
  axis_title   = 11,
  axis_text    = 9,
  legend_title = 10,
  legend_text  = 9,
  strip_text   = 10,
  annotation   = 8,
  point_size   = 1.8,
  line_width   = 0.6,
  legend_position = "bottom",
  legend_columns  = 3
)

FIG <- list(
  # ---- simulation
  fig01 = list(height = 150),
  fig02 = list(height = 150, legend_inside = c(0.995, 0.985), legend_just = c(1, 1), legend_columns = 2),
  fig03 = list(height = 120, legend_inside = c(0.375, 0.43), legend_just = c(0, 1), legend_columns = 1, legend_text = 8),
  fig04 = list(height = 170),
  fig05 = list(height = 85, legend_inside = c(0.49, 0.03), legend_just = c(1, 0), legend_columns = 1),
  fig06 = list(height = 100, legend_inside = c(0.511, 0.985), legend_just = c(0, 1), legend_columns = 1),
  fig07 = list(height = 100, legend_columns = 5, legend_title_show = FALSE),
  fig08 = list(height = 190),
  fig09 = list(height = 105, legend_inside = c(0.995, 0.555), legend_just = c(1, 0), legend_columns = 2),
  fig10 = list(height = 110, legend_inside = c(0.005, 0.995), legend_just = c(0, 1), legend_columns = 2),
  fig11 = list(height = 110),
  fig12 = list(height = 95, legend_columns = 4, legend_title_show = FALSE),
  fig13 = list(height = 80, legend_inside = c(0.995, 0.03), legend_just = c(1, 0), legend_columns = 1),
  fig14 = list(height = 80, legend_inside = c(0.99, 0.99)),
  fig15 = list(height = 90),
  fig16 = list(width = 140, height = 90),
  fig17 = list(height = 90),
  fig18 = list(width = 100, height = 80),
  fig19 = list(width = 100, height = 80),
  fig20 = list(height = 100, legend_columns = 6, legend_title_show = FALSE),
  # ---- real data
  real0 = list(height = 70, legend_inside = c(0.01, 0.97), legend_just = c(0, 1)),
  real_acf = list(width = 100, height = 75),
  real1 = list(width = 140, height = 70, legend_inside = c(0.55, 0.5), legend_just = c(0.5, 0.5)),
  real2 = list(height = 150),
  real3 = list(height = 110, legend_inside = c(0.98, 0.03), legend_just = c(1, 0), legend_columns = 1),
  # ---- worked example (package plots)
  ex_acf              = list(width = 120, height = 80),
  ex_timecourse       = list(width = 120, height = 80),
  ex_timecourse_spline = list(width = 120, height = 80),
  ex_agreement        = list(width = 120, height = 90),
  ex_perm             = list(width = 120, height = 80),
  ex_perm_timecourse  = list(width = 120, height = 80),
  ex_maxt             = list(width = 120, height = 80),
  ex_minp             = list(width = 120, height = 80),
  ex_boot_case        = list(width = 174, height = 80),
  ex_boot_wild        = list(width = 174, height = 80),
  ex_loso             = list(width = 174, height = 90),
  ex_calibration_pp   = list(width = 100, height = 90),
  ex_calibration_rej  = list(width = 100, height = 90)
)

# Method labels: short codes, explained in the figure captions.
# RI = random intercept only; RS = random condition slope; MAX = maximal model;
# T = random intercept for trials; Satt = Satterthwaite; Perm = permutation
# (lmmresample); CBP = cluster-based permutation; Boot = case bootstrap;
# EXP/PCT/BCa/BAS = expanded, percentile, BCa and basic intervals;
# WY = Westfall-Young; ss/sd = single-step/step-down.
METHODS <- data.frame(rbind(
  c("wald_simple",   "Wald\u00b7RI",      "Wald, intercept-only",           "#D55E00"),
  c("wald_full",     "Wald\u00b7RS",      "Wald, random slope",             "#E69F00"),
  c("wald_trial",    "Wald\u00b7RS+T",    "Wald, random slope + trial",     "#F0C35A"),
  c("wald_complete", "Wald\u00b7MAX",     "Wald, maximal",                  "#8C510A"),
  c("satt_simple",   "Satt\u00b7RI",      "Satterthwaite, intercept-only",  "#AA77CC"),
  c("satt_full",     "Satt\u00b7RS",      "Satterthwaite, random slope",    "#CC79A7"),
  c("perm_simple",   "Perm\u00b7RI",      "Permutation, intercept-only",    "#0072B2"),
  c("perm_full",     "Perm\u00b7RS",      "Permutation, random slope",      "#56B4E9"),
  c("two_stage",     "2-stage",            "Two-stage",                      "#009E73"),
  c("two_stage_unadjusted", "2-stage unadj.", "Two-stage, unadjusted",      "#7F7F7F"),
  c("cluster",       "CBP",                "Cluster-based permutation",      "#66A61E"),
  c("gamm_noar",     "GAMM",               "GAMM, no AR(1)",                 "#999999"),
  c("gamm_ar1",      "GAMM\u00b7AR1",     "GAMM, AR(1)",                    "#4D4D4D"),
  c("wald_gca",      "Wald\u00b7GCA",     "Wald, GCA model",                "#C9A100"),
  c("wald_mirman",   "Wald\u00b7Mirman",  "Wald, Mirman model",             "#4D2C0A")
), stringsAsFactors = FALSE)
names(METHODS) <- c("method", "label", "full", "colour")
METHODS_FULL <- transform(METHODS, label = full)

INTERVALS <- data.frame(rbind(
  c("wald",            "Wald",              "#D55E00"),
  c("wald_simple",     "Wald\u00b7RI",     "#D55E00"),
  c("wald_full",       "Wald\u00b7RS",     "#E69F00"),
  c("wald_complete",   "Wald\u00b7MAX",    "#8C510A"),
  c("wald_gca",        "Wald\u00b7GCA",    "#C9A100"),
  c("wald_mirman",     "Wald\u00b7Mirman", "#4D2C0A"),
  c("satterthwaite",   "Satt",              "#CC79A7"),
  c("satt_simple",     "Satt\u00b7RI",     "#AA77CC"),
  c("satt_full",       "Satt\u00b7RS",     "#CC79A7"),
  c("case_expanded",   "Boot\u00b7EXP",    "#08306B"),
  c("case_percentile", "Boot\u00b7PCT",    "#2171B5"),
  c("case_bca",        "Boot\u00b7BCa",    "#6BAED6"),
  c("case_basic",      "Boot\u00b7BAS",    "#9ECAE1"),
  c("wild_expanded",   "Wild\u00b7EXP",    "#00441B"),
  c("wild_percentile", "Wild\u00b7PCT",    "#238B45"),
  c("wild_bca",        "Wild\u00b7BCa",    "#74C476"),
  c("wild_basic",      "Wild\u00b7BAS",    "#A1D99B")
), stringsAsFactors = FALSE)
names(INTERVALS) <- c("method", "label", "colour")

FAMILY_METHODS <- data.frame(rbind(
  c("wald_raw",        "Wald\u00b7RS unadj.",  "#D55E00"),
  c("wald_holm",       "Wald\u00b7RS +Holm",   "#E69F00"),
  c("perm_raw",        "Perm unadj.",           "#7F7F7F"),
  c("perm_bonferroni", "Perm +Bonf",            "#AA77CC"),
  c("perm_holm",       "Perm +Holm",            "#CC79A7"),
  c("maxt_single",     "Perm +maxT (ss)",       "#56B4E9"),
  c("maxt_stepdown",   "Perm +maxT (sd)",       "#0072B2"),
  c("minp_single",     "Perm +minP (ss)",       "#7FD3A8"),
  c("minp_stepdown",   "Perm +minP (sd)",       "#009E73")
), stringsAsFactors = FALSE)
names(FAMILY_METHODS) <- c("method", "label", "colour")

POSTHOC_METHODS <- data.frame(rbind(
  c("wald_simple_holm", "Wald\u00b7RI +Holm",   "#D55E00"),
  c("wald_full_raw",    "Wald\u00b7RS unadj.",  "#F0C35A"),
  c("wald_full_holm",   "Wald\u00b7RS +Holm",   "#E69F00"),
  c("satt_full_holm",   "Satt\u00b7RS +Holm",   "#CC79A7"),
  c("perm_simple_raw",  "Perm\u00b7RI unadj.",  "#7F7F7F"),
  c("perm_full_raw",    "Perm\u00b7RS unadj.",  "#BDBDBD"),
  c("perm_simple_holm", "Perm\u00b7RI +Holm",   "#9ECAE1"),
  c("perm_simple_wy",   "Perm\u00b7RI +WY",     "#0072B2"),
  c("perm_full_holm",   "Perm\u00b7RS +Holm",   "#C6DBEF"),
  c("perm_full_wy",     "Perm\u00b7RS +WY",     "#56B4E9")
), stringsAsFactors = FALSE)
names(POSTHOC_METHODS) <- c("method", "label", "colour")

DESIGN_LABELS <- c(
  within      = "Within-participant",
  between     = "Between-group",
  mixed       = "Group × condition",
  covariate   = "Adjusted for a covariate",
  biomarker   = "Continuous predictor",
  three_level = "Three-level factor",
  timecourse  = "Condition × time",
  triple      = "Group × condition × time",
  level       = "Trial means",
  timecourse_ph = "Time course"
)
VARIANT_LABELS <- c(
  base         = "Base",
  n12          = "N = 12",
  n60          = "N = 60",
  ar05         = "AR(1) = 0.5",
  ar_hetero    = "Heterogeneous AR(1)",
  normal       = "Normal residuals",
  skewed       = "Skewed residuals",
  balanced     = "Balanced trials",
  unbalanced   = "Unbalanced",
  sparse       = "Sparse trials",
  trial_ar     = "Trial-level AR(1)",
  gaps_interp  = "Gaps, interpolated",
  gaps_removed = "Gaps, removed",
  real_res     = "Residuals of the data"
)
GCA_TERM_LABELS <- c(level = "Condition (level)", linear = "× Linear",
                     quadratic = "× Quadratic", cubic = "× Cubic")
GCA_SCENARIO_LABELS <- c(null = "No effect", level = "Level effect",
                         shape = "Shape effect", onset = "Delayed onset")
RE_LABELS <- c(normal = "Normal random effects",
               skewed = "Skewed random effects")
CALIB_LABELS <- c(perm_trials  = "Permutation of trials (valid)",
                  perm_samples = "Permutation of samples (invalid)",
                  wald         = "Wald")
TC_LABELS <- c(correct = "Time-varying effect (correct)",
               misspecified = "Constant effect (misspecified)")
AGREE_LABELS <- c(no_confound = "No confound", confound = "Confound")
REAL_ANALYSIS_LABELS <- c(
  stimulus                 = "Stimulus (trial means)",
  cohort_x_stimulus        = "Cohort × stimulus (trial means)",
  stimulus_adjusted_order  = "Stimulus, adjusted for trial order",
  stimulus_x_time          = "Stimulus × time",
  cohort_x_stimulus_x_time = "Cohort × stimulus × time",
  gca_level = "GCA: level", gca_linear = "GCA: linear",
  gca_quadratic = "GCA: quadratic", gca_cubic = "GCA: cubic"
)
BRADLEY <- c(2.5, 7.5)

SINGULAR_EXTRA <- data.frame(
  design = c("timecourse", "timecourse", "triple", "triple", "gca", "gca"),
  model  = c("RI", "RS", "RI", "RS", "RI", "GCA"),
  singular_pct = c(0, 0, 0, 0, 0, 10),
  stringsAsFactors = FALSE)

# ---- Code
for (p in c("ggplot2", "ragg", "systemfonts")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
suppressPackageStartupMessages({ library(ggplot2) })
dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

font_path <- tryCatch(systemfonts::match_font(FONT)$path,
                      error = function(e) "")
if (!grepl("times", tolower(font_path))) {
  warning("Font '", FONT, "' was not found; another font will be used.")
}
`%||%` <- function(a, b) if (is.null(a)) b else a
cfg <- function(name) utils::modifyList(DEFAULT, FIG[[name]] %||% list())
want <- function(name) identical(DRAW, "all") || name %in% DRAW

paper_elements <- function(s) {
  bold <- function(size) element_text(family = FONT, face = "bold",
                                      size = size, colour = "black")
  theme(text = element_text(family = FONT, face = "bold"),
        axis.title = bold(s$axis_title),
        axis.text = bold(s$axis_text),
        legend.title = if (isFALSE(s$legend_title_show)) element_blank() else
          bold(s$legend_title),
        legend.text = bold(s$legend_text),
        strip.text = bold(s$strip_text),
        plot.title = element_blank(), plot.subtitle = element_blank(),
        plot.caption = element_blank(),
        legend.position = s$legend_position) +
    if (utils::packageVersion("ggplot2") >= "3.5.0") {
      theme(legend.title.position = "top")
    }
}
legend_inside <- function(s) {
  if (is.null(s$legend_inside)) return(theme())
  pos <- s$legend_inside
  th <- theme(legend.justification = s$legend_just %||% pos,
              legend.title = element_blank(),
              legend.text = element_text(family = FONT, face = "bold",
                                         size = s$legend_text - 1),
              legend.background = element_rect(
                fill = grDevices::adjustcolor("white", 0.85), colour = NA),
              legend.key = element_blank(),
              legend.key.height = grid::unit(3.5, "mm"),
              legend.key.width = grid::unit(5, "mm"),
              legend.margin = margin(1, 3, 1, 3),
              legend.spacing.y = grid::unit(0, "mm"))
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    th + theme(legend.position = "inside", legend.position.inside = pos)
  } else {
    th + theme(legend.position = pos)
  }
}
theme_paper <- function(s) {
  theme_bw(base_family = FONT) +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "grey92", colour = NA),
          legend.key.width = grid::unit(8, "mm")) +
    paper_elements(s) + legend_inside(s)
}
restyle <- function(p, s) {
  for (i in seq_along(p$layers)) {
    g <- p$layers[[i]]$geom
    if (inherits(g, "GeomText") || inherits(g, "GeomLabel")) {
      p$layers[[i]]$aes_params$family <- FONT
      p$layers[[i]]$aes_params$fontface <- "bold"
      p$layers[[i]]$aes_params$size <- s$annotation / .pt
    }
  }
  p + labs(title = NULL, subtitle = NULL, caption = NULL) + paper_elements(s)
}
CAIRO_OK <- local({
  before <- grDevices::dev.cur()
  suppressWarnings(try(grDevices::cairo_pdf(tempfile(fileext = ".pdf")),
                       silent = TRUE))
  opened <- grDevices::dev.cur() != before
  if (opened) grDevices::dev.off()
  opened
})
if (!CAIRO_OK && "pdf" %in% FORMATS) {
  if (!FONT %in% names(grDevices::pdfFonts())) {
    do.call(grDevices::pdfFonts,
            stats::setNames(list(grDevices::pdfFonts()$Times), FONT))
  }
  message("Cairo is not available (on a Mac, install XQuartz to use it); ",
          "PDF figures use the standard PDF device with Times.")
}
PLOTS <- list()
save_fig <- function(p, name, s) {
  PLOTS[[name]] <<- list(p = p, s = s)
  for (fmt in FORMATS) {
    f <- file.path(FIG_DIR, paste0(name, ".", fmt))
    if (fmt == "pdf") {
      ggsave(f, p, device = if (CAIRO_OK) grDevices::cairo_pdf else grDevices::pdf,
             width = s$width, height = s$height, units = "mm")
    } else if (fmt == "png") {
      ggsave(f, p, device = ragg::agg_png, width = s$width,
             height = s$height, units = "mm", dpi = DPI)
    } else if (fmt == "tiff") {
      ggsave(f, p, device = ragg::agg_tiff, width = s$width,
             height = s$height, units = "mm", dpi = DPI, compression = "lzw")
    }
  }
  message("saved ", name)
}
draw <- function(name, expr) {
  if (!want(name)) return(invisible())
  tryCatch(expr, error = function(e) {
    message("could not draw ", name, ": ", conditionMessage(e))
  })
}
read_sim <- function(file) {
  f <- file.path(SIM_DIR, file)
  if (!file.exists(f)) f <- paste0(f, ".gz")
  if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else NULL
}
read_real <- function(file) {
  f <- file.path(REAL_DIR, file)
  if (file.exists(f)) utils::read.csv(f, stringsAsFactors = FALSE) else NULL
}
lab <- function(x, table) {
  if (is.data.frame(table)) {
    labels <- stats::setNames(table$label, table$method)
  } else {
    labels <- table
  }
  present <- names(labels)[names(labels) %in% unique(x)]
  out <- labels[x]
  out[is.na(out)] <- x[is.na(out)]
  factor(out, levels = unique(c(labels[present], setdiff(unique(out),
                                                         labels[present]))))
}
colours_of <- function(table) stats::setNames(table$colour, table$label)
num <- function(x) suppressWarnings(as.numeric(x))
PCT_BREAKS <- c(0, 2.5, 5, 10, 20, 40, 60, 80, 100)
pct_labels <- function(x) format(x, drop0trailing = TRUE, trim = TRUE)
sqrt_y <- function(...) {
  scale_y_continuous(trans = "sqrt", breaks = c(0, 5, 10, 20, 40, 60, 80, 100),
                     labels = pct_labels, ...)
}
sqrt_x <- function(...) {
  scale_x_continuous(trans = "sqrt", breaks = c(0, 5, 20, 50, 100),
                     labels = pct_labels, ...)
}
mc_band <- function(reps, nominal = 95) {
  nominal + c(-1, 1) * 1.96 * sqrt(nominal * (100 - nominal) / reps)
}
rate_dots <- function(d, s, facet, table, xlab = "False-positive rate (%)",
                      ncol = 2, band = TRUE) {
  d$method_l <- lab(d$method, table)
  d$method_l <- factor(d$method_l, levels = rev(levels(d$method_l)))
  p <- ggplot(d, aes(x = reject_pct, y = method_l, colour = method_l))
  if (band) {
    p <- p + annotate("rect", xmin = BRADLEY[1], xmax = BRADLEY[2],
                      ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.6) +
      geom_vline(xintercept = 5, linetype = "dashed", linewidth = s$line_width)
  }
  p + geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.3,
                    linewidth = s$line_width) +
    geom_point(size = s$point_size) +
    facet_wrap(facet, ncol = ncol, scales = "free_y") +
    sqrt_x() +
    scale_colour_manual(values = colours_of(table), guide = "none") +
    labs(x = xlab, y = NULL) +
    theme_paper(s)
}

# ---- Simulation

# ---- Trial-level designs
ss <- read_sim("single_summary.csv")
sr <- read_sim("single_raw.csv")
if (!is.null(ss)) {
  ss$design_l <- lab(ss$design, DESIGN_LABELS)
  ss$variant_l <- lab(ss$variant, VARIANT_LABELS)

  draw("fig01", {
    s <- cfg("fig01")
    d <- ss[ss$variant == "base" & ss$effect == 0, ]
    p <- rate_dots(d, s, ~ design_l, METHODS, ncol = 2)
    save_fig(p, "fig01_trial_false_positives", s)
  })

  draw("fig02", {
    s <- cfg("fig02")
    d <- ss[ss$effect == 0 & ss$design %in% c("within", "between", "mixed",
                                              "covariate"), ]
    d$method_l <- lab(d$method, METHODS)
    p <- ggplot(d, aes(x = variant_l, y = reject_pct, colour = method_l)) +
      annotate("rect", ymin = BRADLEY[1], ymax = BRADLEY[2], xmin = -Inf,
               xmax = Inf, fill = "grey85", alpha = 0.6) +
      geom_hline(yintercept = 5, linetype = "dashed", linewidth = s$line_width) +
      geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0,
                    linewidth = s$line_width,
                    position = position_dodge(width = 0.7)) +
      geom_point(size = s$point_size, position = position_dodge(width = 0.7)) +
      facet_wrap(~ design_l, ncol = 2, scales = "free_x") +
      sqrt_y() +
      scale_colour_manual(values = colours_of(METHODS), name = "Method") +
      labs(x = "Scenario", y = "False-positive rate (%)") +
      theme_paper(s) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      guides(colour = guide_legend(ncol = s$legend_columns))
    save_fig(p, "fig02_trial_robustness", s)
  })

  draw("fig03", {
    s <- cfg("fig03")
    d <- ss[ss$variant == "base" & ss$method != "wald_simple" &
              ss$method != "two_stage_unadjusted", ]
    d$method_l <- lab(d$method, METHODS)
    p <- ggplot(d, aes(x = effect, y = reject_pct, colour = method_l)) +
      geom_hline(yintercept = 5, linetype = "dashed", linewidth = s$line_width) +
      geom_line(linewidth = s$line_width) +
      geom_point(size = s$point_size) +
      facet_wrap(~ design_l, scales = "free_x", ncol = 3) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_colour_manual(values = colours_of(METHODS), name = "Method") +
      labs(x = "Effect size (outcome units)", y = "Rejection rate (%)") +
      theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
    save_fig(p, "fig03_trial_power", s)
  })
}

# ---- Time course and triple interaction
ts <- read_sim("timecourse_summary.csv")
if (!is.null(ts)) {
  ts$variant_l <- lab(ts$variant, VARIANT_LABELS)

  draw("fig04", {
    s <- cfg("fig04")
    d <- ts[ts$effect == 0, ]
    d$method_l <- lab(d$method, METHODS)
    d$method_l <- factor(d$method_l, levels = rev(levels(d$method_l)))
    p <- ggplot(d, aes(x = reject_pct, y = method_l, colour = method_l)) +
      annotate("rect", xmin = BRADLEY[1], xmax = BRADLEY[2], ymin = -Inf,
               ymax = Inf, fill = "grey85", alpha = 0.6) +
      geom_vline(xintercept = 5, linetype = "dashed", linewidth = s$line_width) +
      geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.3,
                    linewidth = s$line_width * 0.8) +
      geom_point(size = s$point_size * 0.9) +
      facet_wrap(~ variant_l, ncol = 5, labeller = label_wrap_gen(16)) +
      scale_x_continuous(trans = "sqrt", breaks = c(0, 5, 20, 50, 100),
                         labels = pct_labels) +
      scale_colour_manual(values = colours_of(METHODS), guide = "none") +
      labs(x = "False-positive rate (%)", y = NULL) +
      theme_paper(s) +
      theme(axis.text.x = element_text(size = s$axis_text * 0.8))
    save_fig(p, "fig04_timecourse_false_positives", s)
  })

  draw("fig05", {
    s <- cfg("fig05")
    d <- ts[ts$variant == "base" & ts$effect > 0, ]
    d$size_adj <- num(d$power_size_adjusted_pct)
    long <- rbind(
      data.frame(d[c("method", "effect")], value = d$reject_pct,
                 type = "Raw rejection rate"),
      data.frame(d[c("method", "effect")], value = d$size_adj,
                 type = "Size-adjusted power"))
    long <- long[!is.na(long$value), ]
    long$method_l <- lab(long$method, METHODS)
    p <- ggplot(long, aes(x = effect, y = value, colour = method_l)) +
      geom_line(linewidth = s$line_width) +
      geom_point(size = s$point_size) +
      facet_wrap(~ type) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      scale_x_continuous(breaks = sort(unique(long$effect))) +
      scale_colour_manual(values = colours_of(METHODS), name = "Method") +
      labs(x = "Effect size (outcome units)", y = "Rejection rate (%)") +
      theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
    save_fig(p, "fig05_timecourse_power", s)
  })
}

tr <- read_sim("triple_summary.csv")
if (!is.null(tr)) draw("fig06", {
  s <- cfg("fig06")
  tr$variant_l <- lab(tr$variant, VARIANT_LABELS)
  tr$size_adj <- num(tr$power_size_adjusted_pct)
  long <- rbind(
    data.frame(tr[c("method", "effect", "variant_l")], value = tr$reject_pct,
               type = "Raw rejection rate"),
    data.frame(tr[c("method", "effect", "variant_l")], value = tr$size_adj,
               type = "Size-adjusted power"))
  long <- long[!is.na(long$value) & !(long$type == "Size-adjusted power" &
                                        long$effect == 0), ]
  long$method_l <- lab(long$method, METHODS)
  p <- ggplot(long, aes(x = effect, y = value, colour = method_l)) +
    geom_hline(yintercept = 5, linetype = "dashed", linewidth = s$line_width) +
    geom_line(linewidth = s$line_width) +
    geom_point(size = s$point_size) +
    facet_grid(variant_l ~ type) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    scale_x_continuous(breaks = sort(unique(long$effect))) +
    scale_colour_manual(values = colours_of(METHODS), name = "Method") +
    labs(x = "Effect size (outcome units)", y = "Rejection rate (%)") +
    theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
  save_fig(p, "fig06_triple_interaction", s)
})

ti <- read_sim("timecourse_intervals_summary.csv")
tri <- read_sim("triple_intervals_summary.csv")
if (!is.null(ti) || !is.null(tri)) draw("fig07", {
  s <- cfg("fig07")
  parts <- list()
  if (!is.null(ti)) parts$tc <- data.frame(ti, panel = "Condition × time")
  if (!is.null(tri)) {
    tri$method <- sub("^case_strata_", "case_", tri$method)
    parts$tr <- data.frame(tri, panel = paste("Group × condition × time,",
                                              tolower(VARIANT_LABELS[tri$variant])))
  }
  d <- do.call(rbind, lapply(parts, function(x) x[c("panel", "coef", "method",
    "coverage_pct", "ci_low", "ci_high", "reps")]))
  d$coef_l <- paste("Spline", sub(".*\\)", "", d$coef))
  d$method_l <- lab(d$method, INTERVALS)
  mc <- mc_band(stats::median(d$reps))
  p <- ggplot(d, aes(x = coef_l, y = coverage_pct, colour = method_l)) +
    annotate("rect", ymin = mc[1], ymax = mc[2], xmin = -Inf, xmax = Inf,
             fill = "grey85", alpha = 0.6) +
    geom_hline(yintercept = 95, linetype = "dashed", linewidth = s$line_width) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0,
                  linewidth = s$line_width,
                  position = position_dodge(width = 0.7)) +
    geom_point(size = s$point_size, position = position_dodge(width = 0.7)) +
    facet_wrap(~ panel, labeller = label_wrap_gen(30)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    scale_colour_manual(values = colours_of(INTERVALS), name = "Interval") +
    labs(x = NULL, y = "Coverage of 95% intervals (%)") +
    theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
  save_fig(p, "fig07_timecourse_interval_coverage", s)
})

# ---- Growth curve analysis
gt <- read_sim("gca_tests_summary.csv")
if (!is.null(gt)) draw("fig08", {
  s <- cfg("fig08")
  gt$scenario[is.na(gt$scenario)] <- "null"
  gt <- gt[!is.na(gt$reject_pct), ]
  gt$method_l <- lab(gt$method, METHODS)
  gt$method_l <- factor(gt$method_l, levels = rev(levels(gt$method_l)))
  gt$term_l <- lab(gt$term, GCA_TERM_LABELS)
  gt$scenario_l <- lab(gt$scenario, GCA_SCENARIO_LABELS)
  gt$null_term <- abs(gt$truth) < 1e-8
  band <- unique(gt[gt$null_term, c("scenario_l", "term_l")])
  p <- ggplot(gt, aes(x = reject_pct, y = method_l, colour = method_l)) +
    geom_rect(data = transform(band, xmin = BRADLEY[1], xmax = BRADLEY[2]),
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = "grey85", alpha = 0.6) +
    geom_vline(xintercept = 5, linetype = "dashed", linewidth = s$line_width) +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.3,
                  linewidth = s$line_width) +
    geom_point(aes(shape = null_term), size = s$point_size * 1.2) +
    facet_grid(scenario_l ~ term_l) +
    scale_x_continuous(trans = "sqrt", breaks = c(0, 5, 20, 50, 100),
                       labels = pct_labels) +
    scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 17),
                       labels = c(`TRUE` = "No true effect (false positives)",
                                  `FALSE` = "True effect (power)"),
                       name = NULL) +
    scale_colour_manual(values = colours_of(METHODS), guide = "none") +
    labs(x = "Rejection rate (%)", y = NULL) +
    theme_paper(s)
  save_fig(p, "fig08_gca_tests", s)
})

gi <- read_sim("gca_intervals_summary.csv")
if (!is.null(gi)) draw("fig09", {
  s <- cfg("fig09")
  gi$scenario[is.na(gi$scenario)] <- "null"
  gi <- gi[gi$method %in% c("wald_gca", "wald_mirman", "wald_simple",
                            "case_expanded", "case_percentile", "case_bca"), ]
  gi$method_l <- lab(gi$method, INTERVALS)
  gi$term_l <- lab(gi$term, GCA_TERM_LABELS)
  gi$scenario_l <- lab(gi$scenario, GCA_SCENARIO_LABELS)
  mc <- mc_band(stats::median(gi$reps))
  p <- ggplot(gi, aes(x = term_l, y = coverage_pct, colour = method_l)) +
    annotate("rect", ymin = mc[1], ymax = mc[2], xmin = -Inf, xmax = Inf,
             fill = "grey85", alpha = 0.6) +
    geom_hline(yintercept = 95, linetype = "dashed", linewidth = s$line_width) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0,
                  linewidth = s$line_width,
                  position = position_dodge(width = 0.7)) +
    geom_point(size = s$point_size, position = position_dodge(width = 0.7)) +
    facet_wrap(~ scenario_l) +
    scale_colour_manual(values = colours_of(INTERVALS), name = "Interval") +
    labs(x = NULL, y = "Coverage of 95% intervals (%)") +
    theme_paper(s) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
    guides(colour = guide_legend(ncol = s$legend_columns))
  save_fig(p, "fig09_gca_interval_coverage", s)
})

# ---- Families of tests
fs <- read_sim("families_summary.csv")
if (!is.null(fs)) draw("fig10", {
  s <- cfg("fig10")
  fs$k_l <- factor(paste(fs$k, "measures"),
                   levels = paste(sort(unique(fs$k)), "measures"))
  err <- fs[fs$effect == 0, ]
  pow <- fs[fs$effect > 0 & !fs$method %in% c("wald_raw", "perm_raw"), ]
  long <- rbind(
    data.frame(err[c("method", "rho", "k_l")], value = err$fwer_pct,
               lo = err$fwer_ci_low, hi = err$fwer_ci_high,
               type = "Family-wise error, no effect"),
    data.frame(pow[c("method", "rho", "k_l")], value = num(pow$power_f1_pct),
               lo = NA_real_, hi = NA_real_,
               type = "Power for the true effect"))
  long$method_l <- lab(long$method, FAMILY_METHODS)
  dodge <- position_dodge(width = 0.12)
  p <- ggplot(long, aes(x = rho, y = value, colour = method_l)) +
    geom_hline(data = data.frame(type = "Family-wise error, no effect",
                                 y = 5),
               aes(yintercept = y), linetype = "dashed",
               linewidth = s$line_width) +
    geom_line(linewidth = s$line_width, position = dodge) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0,
                  linewidth = s$line_width, position = dodge, na.rm = TRUE) +
    geom_point(size = s$point_size, position = dodge) +
    facet_grid(type ~ k_l, scales = "free_y") +
    scale_x_continuous(breaks = sort(unique(fs$rho))) +
    scale_colour_manual(values = colours_of(FAMILY_METHODS), name = "Method") +
    labs(x = "Correlation between measures", y = "Rate (%)") +
    theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
  save_fig(p, "fig10_families", s)
})

# ---- Post hoc comparisons
ph <- read_sim("posthoc_summary.csv")
if (!is.null(ph)) draw("fig11", {
  s <- cfg("fig11")
  ph$design_l <- ifelse(ph$design == "level", DESIGN_LABELS[["level"]],
                        DESIGN_LABELS[["timecourse_ph"]])
  ph$design_l <- factor(ph$design_l, levels = unique(ph$design_l))
  err <- ph[ph$effect == 0, ]
  pow <- ph[ph$effect > 0, ]
  long <- rbind(
    data.frame(err[c("method", "design_l")], value = err$fwer_pct,
               lo = err$fwer_ci_low, hi = err$fwer_ci_high,
               type = "FWER: no effect"),
    data.frame(pow[c("method", "design_l")], value = pow$fwer_pct,
               lo = pow$fwer_ci_low, hi = pow$fwer_ci_high,
               type = "FWER: D only"),
    data.frame(pow[c("method", "design_l")], value = num(pow$power_any_pct),
               lo = NA_real_, hi = NA_real_,
               type = "Power: pairs with D"))
  long <- long[!is.na(long$value), ]
  long$type <- factor(long$type, levels = unique(long$type))
  long$method_l <- lab(long$method, POSTHOC_METHODS)
  long$method_l <- factor(long$method_l, levels = rev(levels(long$method_l)))
  p <- ggplot(long, aes(x = value, y = method_l, colour = method_l)) +
    geom_vline(data = data.frame(type = factor(levels(long$type)[1:2],
                                               levels = levels(long$type)),
                                 x = 5),
               aes(xintercept = x), linetype = "dashed",
               linewidth = s$line_width) +
    geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.3,
                  linewidth = s$line_width, na.rm = TRUE) +
    geom_point(size = s$point_size) +
    facet_grid(design_l ~ type, scales = "free_y") +
    sqrt_x() +
    scale_colour_manual(values = colours_of(POSTHOC_METHODS), guide = "none") +
    labs(x = "Rate (%)", y = NULL) +
    theme_paper(s)
  save_fig(p, "fig11_posthoc", s)
})

# ---- Bootstrap with few participants
bw <- read_sim("boot_within_summary.csv")
if (!is.null(bw)) draw("fig12", {
  s <- cfg("fig12")
  bw$method_l <- lab(bw$method, INTERVALS)
  bw$re_l <- lab(bw$re, RE_LABELS)
  mc <- mc_band(stats::median(bw$reps))
  dodge <- position_dodge(width = 2)
  p <- ggplot(bw, aes(x = n, y = coverage_pct, colour = method_l)) +
    annotate("rect", ymin = mc[1], ymax = mc[2], xmin = -Inf, xmax = Inf,
             fill = "grey85", alpha = 0.6) +
    geom_hline(yintercept = 95, linetype = "dashed", linewidth = s$line_width) +
    geom_line(linewidth = s$line_width, position = dodge) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0,
                  linewidth = s$line_width, position = dodge) +
    geom_point(size = s$point_size, position = dodge) +
    facet_wrap(~ re_l) +
    scale_x_continuous(breaks = sort(unique(bw$n))) +
    expand_limits(y = 80) +
    scale_colour_manual(values = colours_of(INTERVALS), name = "Interval") +
    labs(x = "Number of participants", y = "Coverage of 95% intervals (%)") +
    theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
  save_fig(p, "fig12_bootstrap_few_participants", s)
})

# ---- Cost of the alternatives: singular fits
tm <- read_sim("timecourse_model_details.csv")
trm <- read_sim("triple_model_details.csv")
gm <- read_sim("gca_mirman_singular.csv")
if (!is.null(tm)) draw("fig13", {
  s <- cfg("fig13")
  rows <- list(SINGULAR_EXTRA)
  base_tm <- tm[tm$variant == "base" & tm$effect == 0, ]
  rows$tc <- data.frame(design = "timecourse", model = "MAX",
                        singular_pct = base_tm$complete_singular_pct)
  if (!is.null(trm)) {
    b <- trm[trm$variant == "base" & trm$effect == 0, ]
    rows$tr <- data.frame(design = "triple", model = "MAX",
                          singular_pct = b$complete_singular_pct)
  }
  if (!is.null(gm)) {
    gm$scenario[is.na(gm$scenario)] <- "null"
    rows$gm <- data.frame(design = "gca", model = "Mirman",
                          singular_pct = gm$singular_pct[gm$scenario == "null"])
  }
  d <- do.call(rbind, rows)
  d$design_l <- factor(c(timecourse = "Condition × time",
                         triple = "Group × condition × time",
                         gca = "Growth curve analysis")[d$design],
                       levels = c("Condition × time", "Group × condition × time",
                                  "Growth curve analysis"))
  d$model <- factor(d$model, levels = c("RI", "RS", "GCA", "Mirman", "MAX"))
  d$valid <- ifelse(d$model %in% c("MAX", "Mirman"),
                    "Wald valid", "Wald invalid")
  p <- ggplot(d, aes(x = singular_pct, y = model, fill = valid)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = paste0(round(singular_pct), "%")),
              hjust = -0.15, size = s$annotation / .pt, family = FONT,
              fontface = "bold") +
    facet_wrap(~ design_l, scales = "free_y", labeller = label_wrap_gen(18)) +
    scale_x_continuous(limits = c(0, 115), breaks = seq(0, 100, 25)) +
    scale_fill_manual(values = c(`Wald valid` = "#8C510A",
                                 `Wald invalid` = "#F0C35A"), name = NULL) +
    labs(x = "Singular fits (%)", y = NULL) +
    theme_paper(s)
  save_fig(p, "fig13_singular_fits", s)
})

# ---- Computing time at the size of the real data
tg <- read_sim("timing.csv")
if (!is.null(tg)) draw("fig14", {
  s <- cfg("fig14")
  tg$minutes <- ifelse(is.na(tg$minutes_B4999), tg$seconds / 60,
                       tg$minutes_B4999)
  tg$what <- ifelse(is.na(tg$minutes_B4999), "One model fit",
                    "Whole analysis, B = 4999")
  tg$analysis <- sub("complete time-course model", "maximal model",
                     sub("maximal model", "random-slope model", tg$analysis))
  tg$analysis <- factor(tg$analysis, levels = rev(tg$analysis))
  workers <- unique(tg$workers)[1]
  p <- ggplot(tg, aes(x = minutes, y = analysis, colour = what)) +
    geom_point(size = s$point_size * 1.4) +
    scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100),
                  labels = c("0.01", "0.1", "1", "10", "100")) +
    scale_colour_manual(values = c(`One model fit` = "#7F7F7F",
                                   `Whole analysis, B = 4999` = "#0072B2"),
                        name = NULL) +
    labs(x = paste0("Minutes (log scale; resampling on ", workers, " cores)"),
         y = NULL) +
    theme_paper(s)
  save_fig(p, "fig14_computing_time", s)
})

# ---- Diagnostics
a4 <- read_sim("diag_acf_raw.csv")
if (!is.null(a4)) draw("fig15", {
  s <- cfg("fig15")
  a4$true_l <- factor(a4$ar1)
  a4$len_l <- factor(paste(a4$n_time, "samples per trial"),
                     levels = paste(sort(unique(a4$n_time)),
                                    "samples per trial"))
  truth <- unique(a4[c("ar1", "true_l", "len_l")])
  long <- rbind(
    data.frame(a4[c("true_l", "len_l")], value = a4$estimate,
               type = "Bias-corrected"),
    data.frame(a4[c("true_l", "len_l")], value = a4$estimate_raw,
               type = "Uncorrected"))
  long$type <- factor(long$type, levels = c("Uncorrected", "Bias-corrected"))
  p <- ggplot(long, aes(x = true_l, y = value, fill = type)) +
    geom_boxplot(outlier.size = 0.6, linewidth = s$line_width * 0.8,
                 position = position_dodge(width = 0.75), width = 0.6) +
    geom_point(data = truth, aes(x = true_l, y = ar1), inherit.aes = FALSE,
               shape = 4, size = s$point_size * 2, stroke = 1.2,
               colour = "#C0392B") +
    facet_wrap(~ len_l) +
    scale_fill_manual(values = c(Uncorrected = "white",
                                 `Bias-corrected` = "grey70"),
                      name = "Estimate") +
    labs(x = "True autocorrelation", y = "Estimated autocorrelation") +
    theme_paper(s)
  save_fig(p, "fig15_diag_acf", s)
})
c4 <- read_sim("diag_calibrate_raw.csv")
c4s <- read_sim("diag_calibrate_summary.csv")
if (!is.null(c4) && !is.null(c4s)) draw("fig16", {
  s <- cfg("fig16")
  c4$test_l <- lab(c4$test, CALIB_LABELS)
  c4s$test_l <- lab(c4s$test, CALIB_LABELS)
  p <- ggplot(c4, aes(x = test_l, y = 100 * predicted_rate)) +
    geom_hline(yintercept = 5, linetype = "dashed", linewidth = s$line_width) +
    geom_boxplot(width = 0.5, fill = "grey90", outlier.size = 0.6,
                 linewidth = s$line_width * 0.8) +
    geom_errorbar(data = c4s, aes(y = NULL, ymin = true_ci_low,
                                  ymax = true_ci_high),
                  width = 0, colour = "#C0392B", linewidth = s$line_width,
                  position = position_nudge(x = 0.38)) +
    geom_point(data = c4s, aes(y = true_rate_pct), colour = "#C0392B",
               shape = 18, size = s$point_size * 2,
               position = position_nudge(x = 0.38)) +
    scale_x_discrete(labels = function(x) sub(" (", "\n(", x, fixed = TRUE)) +
    labs(x = NULL, y = "False-positive rate (%)") +
    theme_paper(s)
  save_fig(p, "fig16_diag_perm_calibrate", s)
})
t4 <- read_sim("diag_timecourse_raw.csv")
if (!is.null(t4)) draw("fig17", {
  s <- cfg("fig17")
  t4$scenario_l <- lab(t4$scenario, TC_LABELS)
  t4$effect_l <- factor(paste("Effect", t4$effect),
                        levels = paste("Effect", sort(unique(t4$effect))))
  thr <- stats::qnorm(1 - 0.025 / 40)
  p <- ggplot(t4, aes(x = scenario_l, y = max_z)) +
    geom_hline(yintercept = thr, linetype = "dashed",
               linewidth = s$line_width) +
    geom_boxplot(width = 0.5, fill = "grey90", outlier.size = 0.6,
                 linewidth = s$line_width * 0.8) +
    facet_wrap(~ effect_l) +
    scale_x_discrete(labels = function(x) sub(" (", "\n(", x, fixed = TRUE)) +
    labs(x = "Fitted model", y = "Largest standardised mean residual") +
    theme_paper(s)
  save_fig(p, "fig17_diag_timecourse", s)
})
l4 <- read_sim("diag_loso_summary.csv")
if (!is.null(l4)) draw("fig18", {
  s <- cfg("fig18")
  n_part <- 20
  p <- ggplot(l4, aes(x = factor(size), y = ranked_first_pct)) +
    geom_hline(yintercept = 100 / n_part, linetype = "dashed",
               linewidth = s$line_width) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0,
                  linewidth = s$line_width) +
    geom_point(size = s$point_size * 1.5) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(x = "Extra effect of the aberrant participant (slope SDs)",
         y = "Aberrant participant ranked first (%)") +
    theme_paper(s)
  save_fig(p, "fig18_diag_loso", s)
})
g4 <- read_sim("diag_agreement_raw.csv")
if (!is.null(g4)) draw("fig19", {
  s <- cfg("fig19")
  g4$scenario_l <- lab(g4$scenario, AGREE_LABELS)
  p <- ggplot(g4, aes(x = scenario_l, y = 100 * relative)) +
    geom_hline(yintercept = c(-25, 25), linetype = "dashed",
               linewidth = s$line_width) +
    geom_hline(yintercept = 0, linewidth = s$line_width * 0.5,
               colour = "grey50") +
    geom_boxplot(width = 0.5, fill = "grey90", outlier.size = 0.6,
                 linewidth = s$line_width * 0.8) +
    labs(x = NULL, y = "Model minus participant differences (%)") +
    theme_paper(s)
  save_fig(p, "fig19_diag_agreement", s)
})

# ---- Calibration of p-values under the null (QQ)
tcr <- read_sim("timecourse_raw.csv")
if (!is.null(sr) || !is.null(tcr)) draw("fig20", {
  s <- cfg("fig20")
  parts <- list()
  if (!is.null(sr)) {
    parts$single <- sr[sr$variant == "base" & sr$effect == 0 &
                         sr$design %in% c("within", "mixed"),
                       c("design", "method", "p")]
  }
  if (!is.null(tcr)) {
    parts$tc <- tcr[tcr$variant == "base" & tcr$effect == 0,
                    c("design", "method", "p")]
  }
  d <- do.call(rbind, parts)
  d <- d[!is.na(d$p), ]
  d <- do.call(rbind, lapply(split(d, list(d$design, d$method), drop = TRUE),
                             function(z) {
    z <- z[order(z$p), ]
    z$expected <- seq_len(nrow(z)) / (nrow(z) + 1)
    z
  }))
  d$design_l <- lab(d$design, DESIGN_LABELS)
  d$method_l <- lab(d$method, METHODS)
  p <- ggplot(d, aes(x = expected, y = p, colour = method_l)) +
    geom_abline(linetype = "dashed", linewidth = s$line_width) +
    geom_step(linewidth = s$line_width) +
    facet_wrap(~ design_l, ncol = 3) +
    coord_equal() +
    scale_colour_manual(values = colours_of(METHODS), name = "Method") +
    labs(x = "Expected p-value (uniform)", y = "Observed p-value") +
    theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
  save_fig(p, "fig20_pvalue_calibration", s)
})

# ---- Real data
read_obj <- function(name) {
  f <- file.path(REAL_DIR, "objects", paste0(name, ".rds"))
  if (file.exists(f)) readRDS(f) else NULL
}
STIM_COLOURS <- c(Distractor = "#7F7F7F", Target = "#D55E00")
SAMPLING_HZ <- 33

draw("real0", {
  s <- cfg("real0")
  panels <- c(data = "Pupil response (mm)",
              constant = "Residuals: constant effect",
              spline = "Residuals: condition \u00d7 spline")
  parts <- list()
  if (file.exists(DATA_FILE)) {
    raw <- utils::read.csv(DATA_FILE, stringsAsFactors = FALSE)
    pm <- stats::aggregate(pupil_bc ~ participant + stimulus + timestamp_ms,
                           raw, mean)
    cm <- stats::aggregate(pupil_bc ~ stimulus + timestamp_ms, pm,
                           function(v) c(m = mean(v),
                                         se = stats::sd(v) / sqrt(length(v))))
    parts$data <- data.frame(group = cm$stimulus,
                             time = cm$timestamp_ms / 1000,
                             mean = cm$pupil_bc[, "m"],
                             se = cm$pupil_bc[, "se"],
                             panel = panels[["data"]])
    rm(raw, pm, cm)
  }
  tc_csv <- read_real("real_diag_timecourse.csv")
  for (k in c("constant", "spline")) {
    tc <- if (!is.null(tc_csv)) {
      tc_csv[tc_csv$model == k, ]
    } else {
      read_obj(paste0("diag_timecourse_", k))$timecourse
    }
    if (!is.null(tc) && nrow(tc) > 0) {
      parts[[k]] <- data.frame(tc[c("group", "time", "mean", "se")],
                               panel = panels[[k]])
    }
  }
  d <- do.call(rbind, parts)
  d$panel <- factor(d$panel, levels = unname(panels[names(parts)]))
  zero <- data.frame(panel = factor(unname(panels[intersect(c("constant",
    "spline"), names(parts))]), levels = levels(d$panel)), y = 0)
  p <- ggplot(d, aes(x = time, y = mean, colour = group, fill = group)) +
    geom_hline(data = zero, aes(yintercept = y), linetype = "dashed",
               linewidth = s$line_width * 0.8) +
    geom_ribbon(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                alpha = 0.2, colour = NA) +
    geom_line(linewidth = s$line_width * 1.3) +
    facet_wrap(~ panel, nrow = 1, scales = "free_y") +
    scale_colour_manual(values = STIM_COLOURS, name = "Stimulus") +
    scale_fill_manual(values = STIM_COLOURS, name = "Stimulus") +
    labs(x = "Time from stimulus onset (s)", y = NULL) +
    theme_paper(s)
  save_fig(p, "real0_data_and_model_fit", s)
})

draw("real_acf", {
  s <- cfg("real_acf")
  ac_csv <- read_real("real_diag_acf.csv")
  ac <- if (!is.null(ac_csv)) {
    list(acf = ac_csv, phi = ac_csv$phi[1])
  } else {
    read_obj("diag_acf")
  }
  if (!is.null(ac)) {
    a <- ac$acf
    a$lag_ms <- a$lag * 1000 / SAMPLING_HZ
    p <- ggplot(a, aes(x = lag_ms, y = acf)) +
      geom_hline(yintercept = 0, linetype = "dashed",
                 linewidth = s$line_width * 0.8) +
      geom_ribbon(aes(ymin = lower, ymax = upper), fill = "grey80") +
      geom_line(linewidth = s$line_width) +
      geom_point(size = s$point_size * 0.7) +
      annotate("text", x = max(a$lag_ms), y = 0.95, hjust = 1, family = FONT,
               fontface = "bold", size = s$annotation / .pt,
               label = sprintf("AR(1) = %.2f", ac$phi)) +
      labs(x = "Lag (ms)", y = "Residual autocorrelation") +
      theme_paper(s)
    save_fig(p, "real_acf_autocorrelation", s)
  }
})

rc <- read_real("real_perm_calibrate.csv")
if (!is.null(rc)) draw("real1", {
  s <- cfg("real1")
  rc$analysis_l <- lab(rc$analysis, REAL_ANALYSIS_LABELS)
  rc$test_l <- ifelse(grepl("^permutation", rc$test), "Permutation", "Wald")
  p <- ggplot(rc, aes(x = rate, y = analysis_l, colour = test_l)) +
    annotate("rect", xmin = BRADLEY[1], xmax = BRADLEY[2], ymin = -Inf,
             ymax = Inf, fill = "grey85", alpha = 0.6) +
    geom_vline(xintercept = 5, linetype = "dashed", linewidth = s$line_width) +
    geom_errorbar(aes(xmin = low, xmax = high), width = 0.25,
                  linewidth = s$line_width,
                  position = position_dodge(width = 0.5)) +
    geom_point(size = s$point_size * 1.3,
               position = position_dodge(width = 0.5)) +
    sqrt_x() +
    scale_colour_manual(values = c(Permutation = "#0072B2", Wald = "#D55E00"),
                        name = NULL) +
    labs(x = "False-positive rate in simulated null data (%)", y = NULL) +
    theme_paper(s)
  save_fig(p, "real1_perm_calibrate", s)
})

rt <- read_real("real_tests_trial_level.csv")
rs <- read_real("real_tests_sample_level.csv")
if (!is.null(rt) || !is.null(rs)) draw("real2", {
  s <- cfg("real2")
  parts <- list()
  if (!is.null(rt)) {
    rt <- rt[is.na(rt$note) | rt$note == "", ]
    parts$trial <- rt[c("analysis", "method", "p")]
  }
  if (!is.null(rs)) {
    rs <- rs[rs$analysis %in% c("stimulus_x_time", "cohort_x_stimulus_x_time",
                                "gca"), ]
    rs$analysis <- ifelse(rs$analysis == "gca", paste0("gca_", rs$term),
                          rs$analysis)
    parts$sample <- rs[c("analysis", "method", "p")]
  }
  d <- do.call(rbind, parts)
  d <- d[!is.na(d$p), ]
  d$label <- ifelse(d$p < 0.001, "<0.001", formatC(d$p, format = "f",
                                                   digits = 3))
  not_sig <- "p \u2265 0.05"
  d$significant <- ifelse(d$p < 0.05, "p < 0.05", not_sig)
  d$analysis_l <- lab(d$analysis, REAL_ANALYSIS_LABELS)
  d$method_l <- lab(d$method, METHODS_FULL)
  d$method_l <- factor(d$method_l, levels = rev(levels(d$method_l)))
  p <- ggplot(d, aes(x = analysis_l, y = method_l, fill = significant)) +
    geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = label), size = s$annotation / .pt, family = FONT,
              fontface = "bold") +
    scale_fill_manual(values = stats::setNames(c("#9ECAE1", "grey92"),
                                               c("p < 0.05", not_sig)),
                      name = NULL) +
    scale_x_discrete(labels = function(x) gsub(" \\(", "\n(", x)) +
    labs(x = NULL, y = NULL) +
    theme_paper(s) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 35, hjust = 1))
  save_fig(p, "real2_tests_by_method", s)
})

ri <- read_real("real_intervals_timecourse.csv")
rg <- read_real("real_intervals_gca.csv")
if (!is.null(ri) || !is.null(rg)) draw("real3", {
  s <- cfg("real3")
  parts <- list()
  if (!is.null(ri)) {
    ri$method <- sub("^case_strata_", "case_", ri$method)
    ri$coef_l <- paste("Spline", sub(".*\\)", "", ri$coef))
    parts$tc <- ri[c("analysis", "coef_l", "method", "estimate", "lower",
                     "upper")]
  }
  if (!is.null(rg)) {
    rg$method <- sub("^case_strata_", "case_", rg$method)
    rg$analysis <- "gca"
    rg$coef_l <- GCA_TERM_LABELS[rg$term]
    parts$gca <- rg[c("analysis", "coef_l", "method", "estimate", "lower",
                      "upper")]
  }
  d <- do.call(rbind, parts)
  d$width <- d$upper - d$lower
  key <- paste(d$analysis, d$coef_l)
  ref <- d$width[d$method == "case_expanded"]
  names(ref) <- key[d$method == "case_expanded"]
  d$relative <- 100 * d$width / ref[key]
  d <- d[d$method %in% c("wald_simple", "wald_full", "wald_gca", "wald_mirman",
                         "satt_full", "wald_complete", "case_expanded") &
           !is.na(d$relative), ]
  d$analysis_l <- factor(c(stimulus_x_time = "Stimulus × time",
                           cohort_x_stimulus_x_time = "Cohort × stimulus × time",
                           gca = "Growth curve analysis")[d$analysis],
                         levels = c("Stimulus × time", "Cohort × stimulus × time",
                                    "Growth curve analysis"))
  d$coef_l <- factor(d$coef_l, levels = rev(c(paste("Spline", 1:3),
                                              GCA_TERM_LABELS)))
  d$method_l <- lab(d$method, INTERVALS)
  p <- ggplot(d, aes(x = relative, y = coef_l, colour = method_l)) +
    geom_vline(xintercept = 100, linetype = "dashed",
               linewidth = s$line_width) +
    geom_point(size = s$point_size * 1.3,
               position = position_dodge(width = 0.6)) +
    facet_wrap(~ analysis_l, ncol = 2, scales = "free_y",
               labeller = label_wrap_gen(24)) +
    scale_x_continuous(limits = c(0, NA), breaks = seq(0, 150, 25)) +
    scale_colour_manual(values = colours_of(INTERVALS), name = "Interval") +
    labs(x = "Width of the 95% interval (% of the bootstrap interval)",
         y = NULL) +
    theme_paper(s) + guides(colour = guide_legend(ncol = s$legend_columns))
  save_fig(p, "real3_intervals", s)
})

# ---- Worked example
ex_dir <- file.path(SIM_DIR, "example")
if (dir.exists(ex_dir) && requireNamespace("lmmresample", quietly = TRUE)) {
  suppressPackageStartupMessages({ library(lmmresample); library(lme4) })
  draw_ex <- function(name, file, out, ...) {
    draw(name, {
      f <- file.path(ex_dir, paste0(file, ".rds"))
      if (file.exists(f)) {
        s <- cfg(name)
        save_fig(restyle(plot(readRDS(f), ...), s), out, s)
      }
    })
  }
  draw_ex("ex_acf", "diag_acf", "ex_residual_autocorrelation")
  draw_ex("ex_timecourse", "diag_timecourse", "ex_residual_timecourse")
  draw_ex("ex_timecourse_spline", "diag_timecourse_spline",
          "ex_residual_timecourse_spline")
  draw_ex("ex_agreement", "diag_agreement", "ex_agreement")
  draw_ex("ex_perm", "perm_test", "ex_permutation_null")
  draw_ex("ex_perm_timecourse", "perm_test_timecourse",
          "ex_permutation_null_timecourse")
  draw_ex("ex_maxt", "perm_maxt", "ex_maxt")
  draw_ex("ex_minp", "perm_minp", "ex_minp")
  draw_ex("ex_boot_case", "boot_case", "ex_bootstrap_case")
  draw_ex("ex_boot_wild", "boot_wild", "ex_bootstrap_wild")
  draw_ex("ex_loso", "diag_loso", "ex_leave_one_out")
  draw_ex("ex_calibration_pp", "perm_calibrate", "ex_calibration_pvalues",
          type = "pp")
  draw_ex("ex_calibration_rej", "perm_calibrate", "ex_calibration_rejection",
          type = "rejection")
}

# ---- Paper and supplementary figures
# Figures 1 and 8 are diagrams (figure1_concept.pptx, figure1_workflow.pptx).
PAPER_FIGS <- list(
  Fig02 = "fig04_timecourse_false_positives",
  Fig03 = "fig05_timecourse_power",
  Fig04 = "fig07_timecourse_interval_coverage",
  Fig05 = "fig08_gca_tests",
  Fig06 = c("fig10_families", "fig11_posthoc"),
  Fig07 = "fig13_singular_fits",
  Fig09 = "real0_data_and_model_fit",
  Fig10 = "real_acf_autocorrelation",
  Fig11 = c("real2_tests_by_method", "real3_intervals"))
SUPP_FIGS <- list(
  FigS01 = "fig01_trial_false_positives",
  FigS02 = "fig02_trial_robustness",
  FigS03 = "fig03_trial_power",
  FigS04 = "fig12_bootstrap_few_participants",
  FigS05 = "fig20_pvalue_calibration",
  FigS06 = "fig06_triple_interaction",
  FigS07 = "fig09_gca_interval_coverage",
  FigS08 = "fig15_diag_acf",
  FigS09 = "fig16_diag_perm_calibrate",
  FigS10 = "fig17_diag_timecourse",
  FigS11 = "fig18_diag_loso",
  FigS12 = "fig19_diag_agreement")
MAX_HEIGHT <- 235

if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
assemble <- function(parts, out, dir) {
  missing <- setdiff(parts, names(PLOTS))
  if (length(missing) > 0) {
    message("not assembled ", out, ": missing ", paste(missing, collapse = ", "))
    return(invisible())
  }
  if (length(parts) == 1) {
    fig <- PLOTS[[parts]]$p
    s <- PLOTS[[parts]]$s
  } else {
    heights <- vapply(PLOTS[parts], function(x) x$s$height, numeric(1))
    widths <- vapply(PLOTS[parts], function(x) x$s$width, numeric(1))
    tag_theme <- theme(plot.tag = element_text(family = FONT, face = "bold",
                                               size = 12))
    panels <- lapply(PLOTS[parts], function(x) x$p + tag_theme)
    fig <- patchwork::wrap_plots(panels, ncol = 1, heights = heights) +
      patchwork::plot_annotation(tag_levels = "A")
    s <- list(width = max(widths), height = min(sum(heights), MAX_HEIGHT))
  }
  old <- FIG_DIR
  FIG_DIR <<- dir
  on.exit(FIG_DIR <<- old)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  save_fig(fig, out, s)
}
for (out in names(PAPER_FIGS)) {
  assemble(PAPER_FIGS[[out]], out, file.path(FIG_DIR, "paper"))
}
for (out in names(SUPP_FIGS)) {
  assemble(SUPP_FIGS[[out]], out, file.path(FIG_DIR, "supplementary"))
}
message("Figures saved in ", FIG_DIR, " (paper and supplementary figures in ",
        "the subfolders paper/ and supplementary/)")
