# =============================================================================
# lmmresample: analysis of the real data set
#
# Data:     preprocessed_accepted_trials.csv.gz
#           https://doi.org/10.5281/zenodo.23160276
# Usage:    Rscript analysis_real_data.R
# Settings: LMM_DATA (data file), LMM_OUT (output folder),
#           LMM_WORKERS (parallel sessions),
#           LMM_SKIP_INSTALL (1 = packages already installed)
# =============================================================================

DATA_FILE <- Sys.getenv("LMM_DATA",
                       file.path("data", "preprocessed_accepted_trials.csv.gz"))
OUT_DIR <- Sys.getenv("LMM_OUT", "lmmresample_realdata")
WORKERS <- as.integer(Sys.getenv("LMM_WORKERS",
                                 max(1, parallel::detectCores() - 1)))
B_MAIN <- 4999
N_CAL <- 1000
B_CAL <- 99
N_CAL_GCA <- 100
B_CAL_GCA <- 19
CAL_WORKERS <- min(WORKERS, 30)

if (Sys.getenv("LMM_SKIP_INSTALL") != "1") {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pak(c("ricardo-martinez-flores/lmmresample", "lme4", "lmerTest",
             "mgcv", "future", "future.apply"), ask = FALSE)
}
suppressPackageStartupMessages({
  library(lmmresample); library(lme4); library(splines)
})
if (utils::packageVersion("lmmresample") < "0.0.0.9005") {
  stop("lmmresample 0.0.0.9005 or later is needed.")
}
future::plan("multisession", workers = WORKERS)
dir.create(file.path(OUT_DIR, "objects"), showWarnings = FALSE,
           recursive = TRUE)
LOG_FILE <- file.path(OUT_DIR, "progress.log")
log_msg <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M  "), ...)
  message(txt)
  cat(txt, "\n", file = LOG_FILE, append = TRUE, sep = "")
}
step <- function(name, expr) {
  f <- file.path(OUT_DIR, "objects", paste0(name, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  t0 <- Sys.time()
  x <- expr
  saveRDS(x, f)
  log_msg(sprintf("%-34s %s", name,
                  format(round(difftime(Sys.time(), t0, units = "mins"), 1))))
  x
}
quiet <- function(expr) suppressWarnings(suppressMessages(expr))
ctrl <- lmerControl(calc.derivs = FALSE)
log_msg("lmmresample ", utils::packageVersion("lmmresample"), ", ", WORKERS,
        " workers")

# ---- Data
if (!file.exists(DATA_FILE)) {
  stop("Data file not found: ", DATA_FILE, "\nDownload ",
       "preprocessed_accepted_trials.csv.gz from ",
       "https://doi.org/10.5281/zenodo.23160276 into the data folder, ",
       "or set LMM_DATA.")
}
raw <- utils::read.csv(DATA_FILE, stringsAsFactors = FALSE)
HAS_COHORT <- "cohort" %in% names(raw)
d <- data.frame(
  participant = factor(raw$participant),
  trial = factor(paste(raw$participant, raw$trial, sep = "_")),
  condition = factor(raw$stimulus, levels = c("Distractor", "Target")),
  time = round(raw$timestamp_ms) / 1000,
  trial_index = raw$trial,
  y = raw$pupil_bc)
if (HAS_COHORT) d$cohort <- factor(raw$cohort)
rm(raw)
d <- d[stats::complete.cases(d), ]
# LMM_STRATA=none resamples participants without stratifying by cohort, as is
# done with the public data (which has no cohort column).
STRATA <- if (HAS_COHORT && Sys.getenv("LMM_STRATA", "cohort") != "none")
  "cohort" else NULL
CASE <- if (is.null(STRATA)) "case_" else "case_strata_"
signflip <- exch_signflip("participant")

# ---- A. Trial-level analyses (one value per trial)
local({
  f_by <- if (HAS_COHORT) {
    y ~ participant + cohort + trial + trial_index + condition
  } else {
    y ~ participant + trial + trial_index + condition
  }
  tr <- stats::aggregate(f_by, d, mean)
  names(tr)[names(tr) == "y"] <- "mean"
  pk <- stats::aggregate(f_by, d, max)
  lt <- stats::aggregate(f_by, d[d$time >= 1, ], mean)
  tr$peak <- pk$y[match(tr$trial, pk$trial)]
  tr$late <- lt$y[match(tr$trial, lt$trial)]
  tr <- tr[stats::complete.cases(tr), ]
  tr$order <- as.numeric(scale(tr$trial_index))

  kt <- table(tr$participant, tr$condition)
  descriptives <- data.frame(
    quantity = c("participants", "trials", "target trials",
                 "trials per participant (median)",
                 "trials per participant (min)", "trials per participant (max)",
                 "target proportion (median)", "samples per trial",
                 "sampling rate (Hz)"),
    value = c(nlevels(tr$participant), nrow(tr), sum(tr$condition == "Target"),
              stats::median(rowSums(kt)), min(rowSums(kt)), max(rowSums(kt)),
              round(stats::median(kt[, "Target"] / rowSums(kt)), 3),
              length(unique(d$time)),
              round(1 / mean(diff(sort(unique(d$time)))), 1)))
  if (HAS_COHORT) {
    g <- table(tapply(as.character(tr$cohort), tr$participant, `[`, 1))
    descriptives <- rbind(descriptives, data.frame(
      quantity = paste("participants", names(g)), value = as.numeric(g)))
  }
  rm(pk, lt)

  # ---- Helpers
  wald_p <- function(m, cf) {
    b <- fixef(m)[cf]; V <- as.matrix(vcov(m))[cf, cf, drop = FALSE]
    stats::pchisq(as.numeric(crossprod(b, solve(V, b))), length(cf),
                  lower.tail = FALSE)
  }
  satt <- function(m, cf, term) {
    mt <- quiet(lmerTest::as_lmerModLmerTest(m))
    co <- summary(mt)$coefficients[cf, ]
    q <- stats::qt(0.975, co[["df"]])
    list(p = quiet(stats::anova(mt, ddf = "Satterthwaite"))[term, "Pr(>F)"],
         ci = co[["Estimate"]] + c(-1, 1) * q * co[["Std. Error"]])
  }
  TESTS <- list(); INTERVALS <- list(); FAMILIES <- list()
  add_test <- function(analysis, method, p, extra = NA_character_) {
    TESTS[[length(TESTS) + 1]] <<- data.frame(analysis = analysis,
                                              method = method, p = p,
                                              note = extra)
  }
  add_interval <- function(analysis, coef, method, estimate, ci) {
    INTERVALS[[length(INTERVALS) + 1]] <<- data.frame(
      analysis = analysis, coef = coef, method = method, estimate = estimate,
      lower = ci[1], upper = ci[2])
  }
  signflip <- exch_signflip("participant")
  participant_diffs <- function() {
    id <- if (HAS_COHORT) c("participant", "cohort") else "participant"
    pm <- stats::aggregate(stats::reformulate(c(id, "condition"), "mean"), tr,
                           mean)
    wd <- stats::reshape(pm, idvar = id, timevar = "condition",
                         direction = "wide")
    wd$diff <- wd$mean.Target - wd$mean.Distractor
    wd
  }

  # ---- 1. Target vs Distractor (trial means)
  cf1 <- "conditionTarget"
  m1_s <- quiet(lmer(mean ~ condition + (1 | participant), data = tr,
                     control = ctrl))
  m1_f <- quiet(lmer(mean ~ condition + (1 + condition | participant),
                     data = tr, control = ctrl))
  p1_s <- step("perm_stimulus_simple", perm_test(
    m1_s, "condition", "trial", signflip, B = B_MAIN, seed = 1, data = tr))
  p1_f <- step("perm_stimulus_full", perm_test(
    m1_f, "condition", "trial", signflip, B = B_MAIN, seed = 1, data = tr))
  p1_s2 <- step("perm_stimulus_simple_seed2", perm_test(
    m1_s, "condition", "trial", signflip, B = B_MAIN, seed = 2, data = tr))
  s1 <- satt(m1_f, cf1, "condition")
  wd <- participant_diffs()
  add_test("stimulus", "wald_simple", wald_p(m1_s, cf1))
  add_test("stimulus", "wald_full", wald_p(m1_f, cf1))
  add_test("stimulus", "satt_full", s1$p)
  add_test("stimulus", "perm_simple", p1_s$p.value)
  add_test("stimulus", "perm_full", p1_f$p.value)
  add_test("stimulus", "perm_simple", p1_s2$p.value, "seed 2")
  add_test("stimulus", "two_stage", stats::t.test(wd$diff)$p.value)

  b1_case <- step("boot_stimulus_case", boot_ci(
    m1_f, "participant", resample = "case", ci = "bca", strata = STRATA,
    terms = cf1, B = B_MAIN, seed = 1, data = tr))
  b1_wild <- step("boot_stimulus_wild", boot_ci(
    m1_f, "participant", resample = "wild", ci = "bca", terms = cf1,
    B = B_MAIN, seed = 1, data = tr))
  est1 <- fixef(m1_f)[[cf1]]
  se1 <- sqrt(vcov(m1_f)[cf1, cf1])
  add_interval("stimulus", cf1, "wald_full", est1,
               est1 + c(-1, 1) * stats::qnorm(0.975) * se1)
  add_interval("stimulus", cf1, "satt_full", est1, s1$ci)
  for (type in c("expanded", "percentile", "bca", "basic")) {
    add_interval("stimulus", cf1, paste0(CASE, type), est1,
                 confint(b1_case, type = type)[cf1, ])
    add_interval("stimulus", cf1, paste0("wild_", type), est1,
                 confint(b1_wild, type = type)[cf1, ])
  }
  add_interval("stimulus", cf1, "two_stage", mean(wd$diff),
               stats::t.test(wd$diff)$conf.int)

  # ---- 2. Cohort x stimulus
  if (HAS_COHORT) {
    cf2 <- paste0("cohort", levels(tr$cohort)[2], ":conditionTarget")
    m2_s <- quiet(lmer(mean ~ cohort * condition + (1 | participant),
                       data = tr, control = ctrl))
    m2_f <- quiet(lmer(mean ~ cohort * condition +
                         (1 + condition | participant),
                       data = tr, control = ctrl))
    p2_s <- step("perm_cohort_simple", perm_test(
      m2_s, "cohort:condition", "trial", signflip, B = B_MAIN, seed = 1,
      data = tr))
    p2_f <- step("perm_cohort_full", perm_test(
      m2_f, "cohort:condition", "trial", signflip, B = B_MAIN, seed = 1,
      data = tr))
    s2 <- satt(m2_f, cf2, "cohort:condition")
    add_test("cohort_x_stimulus", "wald_simple", wald_p(m2_s, cf2))
    add_test("cohort_x_stimulus", "wald_full", wald_p(m2_f, cf2))
    add_test("cohort_x_stimulus", "satt_full", s2$p)
    add_test("cohort_x_stimulus", "perm_simple", p2_s$p.value)
    add_test("cohort_x_stimulus", "perm_full", p2_f$p.value)
    add_test("cohort_x_stimulus", "two_stage",
             stats::t.test(diff ~ cohort, data = wd)$p.value)

    b2_case <- step("boot_cohort_case", boot_ci(
      m2_f, "participant", resample = "case", ci = "bca", strata = STRATA,
      terms = cf2, B = B_MAIN, seed = 1, data = tr))
    est2 <- fixef(m2_f)[[cf2]]
    se2 <- sqrt(vcov(m2_f)[cf2, cf2])
    add_interval("cohort_x_stimulus", cf2, "wald_full", est2,
                 est2 + c(-1, 1) * stats::qnorm(0.975) * se2)
    add_interval("cohort_x_stimulus", cf2, "satt_full", est2, s2$ci)
    for (type in c("expanded", "percentile", "bca", "basic")) {
      add_interval("cohort_x_stimulus", cf2, paste0(CASE, type), est2,
                   confint(b2_case, type = type)[cf2, ])
    }
  }

  # ---- 3. Families of trial features
  feats <- c("mean", "peak", "late")
  family_of <- function(name, rhs, term, cf) {
    fits <- lapply(feats, function(v) {
      list(simple = quiet(lmer(stats::as.formula(paste(
        v, "~", rhs, "+ (1 | participant)")), data = tr, control = ctrl)),
        full = quiet(lmer(stats::as.formula(paste(
          v, "~", rhs, "+ (1 + condition | participant)")), data = tr,
          control = ctrl)))
    })
    names(fits) <- feats
    specs <- lapply(fits, function(f) perm_spec(f$simple, term, data = tr))
    r <- step(paste0("maxt_", name), do.call(perm_maxt, c(specs, list(
      unit = "trial", exchange = signflip, B = B_MAIN, seed = 1))))
    p_w <- vapply(fits, function(f) wald_p(f$full, cf), numeric(1))
    p_s <- vapply(fits, function(f) satt(f$full, cf, term)$p, numeric(1))
    p_r <- r$results$p.value
    adj <- list(perm_raw = p_r, perm_holm = stats::p.adjust(p_r, "holm"),
                maxt_stepdown = r$results$p.adjusted,
                wald_raw = p_w, wald_holm = stats::p.adjust(p_w, "holm"),
                satt_raw = p_s, satt_holm = stats::p.adjust(p_s, "holm"))
    FAMILIES[[length(FAMILIES) + 1]] <<- do.call(rbind, lapply(
      names(adj), function(m) {
        data.frame(family = name, method = m, feature = feats,
                   p = unname(adj[[m]]))
      }))
  }
  family_of("stimulus", "condition", "condition", cf1)
  if (HAS_COHORT) {
    family_of("cohort_x_stimulus", "cohort * condition", "cohort:condition",
              cf2)
  }

  # ---- 4. Target vs Distractor adjusted for trial order (covariate)
  m4_s <- quiet(lmer(mean ~ condition + order + (1 | participant), data = tr,
                     control = ctrl))
  m4_f <- quiet(lmer(mean ~ condition + order + (1 + condition | participant),
                     data = tr, control = ctrl))
  p4_s <- step("perm_order_simple", perm_test(
    m4_s, "condition", "trial", signflip, method = "freedman-lane",
    B = B_MAIN, seed = 1, data = tr))
  p4_f <- step("perm_order_full", perm_test(
    m4_f, "condition", "trial", signflip, method = "freedman-lane",
    B = B_MAIN, seed = 1, data = tr))
  s4 <- satt(m4_f, cf1, "condition")
  b4 <- vapply(split(tr, tr$participant), function(x) {
    f <- tryCatch(stats::coef(stats::lm(mean ~ condition + order,
                                        data = x))[["conditionTarget"]],
                  error = function(e) NA_real_)
    if (is.null(f)) NA_real_ else f
  }, numeric(1))
  b4 <- b4[is.finite(b4)]
  add_test("stimulus_adjusted_order", "wald_simple", wald_p(m4_s, cf1))
  add_test("stimulus_adjusted_order", "wald_full", wald_p(m4_f, cf1))
  add_test("stimulus_adjusted_order", "satt_full", s4$p)
  add_test("stimulus_adjusted_order", "perm_simple", p4_s$p.value)
  add_test("stimulus_adjusted_order", "perm_full", p4_f$p.value)
  add_test("stimulus_adjusted_order", "two_stage", stats::t.test(b4)$p.value)
  b4_case <- step("boot_order_case", boot_ci(
    m4_f, "participant", resample = "case", ci = "bca", strata = STRATA,
    terms = cf1, B = B_MAIN, seed = 1, data = tr))
  est4 <- fixef(m4_f)[[cf1]]
  se4 <- sqrt(vcov(m4_f)[cf1, cf1])
  add_interval("stimulus_adjusted_order", cf1, "wald_full", est4,
               est4 + c(-1, 1) * stats::qnorm(0.975) * se4)
  add_interval("stimulus_adjusted_order", cf1, "satt_full", est4, s4$ci)
  for (type in c("expanded", "percentile", "bca", "basic")) {
    add_interval("stimulus_adjusted_order", cf1, paste0(CASE, type),
                 est4, confint(b4_case, type = type)[cf1, ])
  }
  add_interval("stimulus_adjusted_order", cf1, "two_stage", mean(b4),
               stats::t.test(b4)$conf.int)

  # ---- 5. Diagnostics
  lo <- step("diag_loso", diag_loso(m1_f, "participant", terms = cf1,
                                    data = tr))
  ag <- step("diag_agreement", diag_agreement(m1_f, "condition",
                                              "participant", data = tr))
  ag4 <- step("diag_agreement_order", diag_agreement(m4_f, "condition",
                                                     "participant", data = tr))
  sc <- lo$std_change[, cf1]
  diagnostics <- data.frame(
    check = c("loso: largest standardised change",
              "loso: participants with |change| > 1",
              "agreement: relative discrepancy", "agreement: flagged",
              "agreement adjusted for trial order: relative discrepancy",
              "agreement adjusted for trial order: flagged"),
    value = c(round(max(abs(sc), na.rm = TRUE), 3),
              sum(abs(sc) > 1, na.rm = TRUE), round(ag$relative, 4),
              ag$flagged, round(ag4$relative, 4), ag4$flagged))

  write_csv <- function(x, file) {
    utils::write.csv(x, file.path(OUT_DIR, file), row.names = FALSE)
  }
  write_csv(descriptives, "real_descriptives.csv")
  write_csv(do.call(rbind, TESTS), "real_tests_trial_level.csv")
  write_csv(do.call(rbind, INTERVALS), "real_intervals_trial_level.csv")
  write_csv(do.call(rbind, FAMILIES), "real_families.csv")
  write_csv(diagnostics, "real_diagnostics_trial_level.csv")
})
log_msg("Trial-level analyses finished")

# ---- B. Sample-level analyses (full time series)
d <- d[order(d$trial, d$time), ]
d$y <- d$y - stats::ave(d$y, d$time)
P <- stats::poly(d$time, 3)
d$ot1 <- P[, 1]; d$ot2 <- P[, 2]; d$ot3 <- P[, 3]
d$P <- unclass(P)[, 1:3]
rownames(d) <- NULL

# ---- Helpers
wald_p <- function(m, cf) {
  b <- fixef(m)[cf]; V <- as.matrix(vcov(m))[cf, cf, drop = FALSE]
  stats::pchisq(as.numeric(crossprod(b, solve(V, b))), length(cf),
                lower.tail = FALSE)
}
wald_ci <- function(m, cf) {
  b <- fixef(m)[cf]; se <- sqrt(diag(as.matrix(vcov(m)))[cf])
  cbind(b - stats::qnorm(0.975) * se, b + stats::qnorm(0.975) * se)
}
satt_p <- function(m, term) {
  mt <- quiet(lmerTest::as_lmerModLmerTest(m))
  quiet(stats::anova(mt, ddf = "Satterthwaite"))[term, "Pr(>F)"]
}
satt_ci <- function(m, cf) {
  mt <- quiet(lmerTest::as_lmerModLmerTest(m))
  co <- summary(mt)$coefficients[cf, , drop = FALSE]
  q <- stats::qt(0.975, co[, "df"])
  cbind(co[, "Estimate"] - q * co[, "Std. Error"],
        co[, "Estimate"] + q * co[, "Std. Error"])
}
hotelling_p <- function(D) {
  D <- D[stats::complete.cases(D), , drop = FALSE]
  n <- nrow(D); q <- ncol(D); mu <- colMeans(D); S <- stats::cov(D)
  t2 <- n * as.numeric(crossprod(mu, solve(S, mu)))
  stats::pf((n - q) / (q * (n - 1)) * t2, q, n - q, lower.tail = FALSE)
}
run_test <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  r <- tryCatch(list(p = as.numeric(expr)[1], error = NA_character_),
                error = function(e) list(p = NA_real_,
                                         error = conditionMessage(e)))
  data.frame(p = r$p, seconds = round(proc.time()[["elapsed"]] - t0, 1),
             error = r$error)
}

diff_curves <- function(d) {
  m <- stats::aggregate(y ~ participant + condition + time, d, mean)
  wd <- stats::reshape(m, idvar = c("participant", "time"),
                       timevar = "condition", direction = "wide")
  wd$diff <- wd$y.Target - wd$y.Distractor
  D <- unclass(stats::xtabs(diff ~ participant + time, wd))
  has <- unclass(stats::xtabs(~ participant + time,
                              wd[!is.na(wd$diff), ])) > 0
  D[!has] <- NA
  D[stats::complete.cases(D), , drop = FALSE]
}
cluster_masses <- function(t, thr) {
  s <- sign(t) * (abs(t) > thr)
  r <- rle(s)
  ends <- cumsum(r$lengths); starts <- ends - r$lengths + 1
  m <- mapply(function(a, b, v) if (v == 0) 0 else sum(t[a:b]),
              starts, ends, r$values)
  list(mass = m, start = starts, end = ends, value = r$values)
}
cluster_test <- function(D, times, B, seed, group = NULL) {
  n <- nrow(D)
  if (is.null(group)) {
    thr <- stats::qt(0.975, n - 1)
    stat_of <- function(M) colMeans(M) / (apply(M, 2, stats::sd) / sqrt(n))
  } else {
    g <- factor(group)
    thr <- stats::qt(0.975, n - 2)
    stat_of <- function(M, gg = g) {
      a <- M[gg == levels(gg)[1], , drop = FALSE]
      b <- M[gg == levels(gg)[2], , drop = FALSE]
      na <- nrow(a); nb <- nrow(b)
      sp <- sqrt(((na - 1) * apply(a, 2, stats::var) +
                    (nb - 1) * apply(b, 2, stats::var)) / (na + nb - 2))
      (colMeans(b) - colMeans(a)) / (sp * sqrt(1 / na + 1 / nb))
    }
  }
  obs <- cluster_masses(stat_of(D), thr)
  set.seed(seed)
  null <- vapply(seq_len(B), function(b) {
    st <- if (is.null(group)) {
      stat_of(D * sample(c(-1, 1), n, replace = TRUE))
    } else {
      stat_of(D, sample(g))
    }
    max(abs(cluster_masses(st, thr)$mass))
  }, numeric(1))
  p_each <- vapply(abs(obs$mass), function(m) (1 + sum(null >= m)) / (B + 1),
                   numeric(1))
  keep <- obs$value != 0
  list(p = min(c(1, p_each[keep])),
       clusters = data.frame(start_s = times[obs$start[keep]],
                             end_s = times[obs$end[keep]],
                             mass = obs$mass[keep], p = p_each[keep]))
}
gamm_test <- function(d, triple = FALSE, k = 10) {
  d <- d[order(d$trial, d$time), ]
  d$condO <- as.ordered(d$condition)
  stats::contrasts(d$condO) <- "contr.treatment"
  d$start <- !duplicated(d$trial)
  if (triple) {
    d$groupO <- as.ordered(d$cohort)
    stats::contrasts(d$groupO) <- "contr.treatment"
    d$gcO <- as.ordered(factor(d$cohort == levels(d$cohort)[2] &
                                 d$condition == "Target",
                               levels = c(FALSE, TRUE)))
    stats::contrasts(d$gcO) <- "contr.treatment"
    f <- y ~ groupO + condO + gcO + s(time, k = k) +
      s(time, by = groupO, k = k) + s(time, by = condO, k = k) +
      s(time, by = gcO, k = k) +
      s(time, participant, bs = "fs", m = 1, k = 5) +
      s(time, participant, by = condO, bs = "fs", m = 1, k = 5)
    pattern <- "^s\\(time\\):gcO"
  } else {
    f <- y ~ condO + s(time, k = k) + s(time, by = condO, k = k) +
      s(time, participant, bs = "fs", m = 1, k = 5) +
      s(time, participant, by = condO, bs = "fs", m = 1, k = 5)
    pattern <- "^s\\(time\\):condO"
  }
  m0 <- quiet(mgcv::bam(f, data = d, method = "fREML", discrete = TRUE))
  r <- stats::residuals(m0)
  same <- !d$start[-1]
  rho <- stats::cor(r[-1][same], r[-length(r)][same])
  m1 <- quiet(mgcv::bam(f, data = d, method = "fREML", discrete = TRUE,
                        rho = rho, AR.start = d$start))
  pv <- function(m) {
    st <- summary(m)$s.table
    min(max(st[grep(pattern, rownames(st)), "p-value"][1], 0), 1)
  }
  list(noar = pv(m0), ar1 = pv(m1), rho = rho)
}
participant_spline_coefs <- function(d, k) {
  t(vapply(split(d, d$participant), function(x) {
    f <- tryCatch(stats::lm(y ~ condition * ns(time, k), data = x),
                  error = function(e) NULL)
    v <- if (is.null(f)) rep(NA_real_, k) else {
      co <- stats::coef(f)
      unname(co[grep("^conditionTarget:", names(co))])
    }
    if (length(v) != k) rep(NA_real_, k) else v
  }, numeric(k)))
}

# ---- 1-2. Stimulus x time and cohort x stimulus x time
timecourse <- function(name, k = 3, triple = FALSE, seed = 1,
                       methods = "all") {
  fixed <- sprintf("y ~ %scondition * ns(time, %d)",
                   if (triple) "cohort * " else "", k)
  term <- sprintf("%scondition:ns(time, %d)", if (triple) "cohort:" else "", k)
  pattern <- if (triple) "^cohort.*:conditionTarget:ns" else
    "^conditionTarget:ns"
  form <- function(re) stats::as.formula(paste(fixed, "+", re))
  fit <- function(re) quiet(lmer(form(re), data = d, control = ctrl))
  all <- identical(methods, "all")
  out <- list()
  m_s <- fit("(1 | participant)")
  cf <- grep(pattern, names(fixef(m_s)), value = TRUE)
  out$perm_simple <- step(paste0(name, "_perm_simple"), run_test(perm_test(
    m_s, term, "trial", signflip, B = B_MAIN, seed = seed,
    data = d)$p.value))
  out$wald_simple <- run_test(wald_p(m_s, cf))
  if (all) {
    m_f <- fit("(1 + condition | participant)")
    out$wald_full <- run_test(wald_p(m_f, cf))
    out$satt_full <- step(paste0(name, "_satt_full"),
                          run_test(satt_p(m_f, term)))
    out$perm_full <- step(paste0(name, "_perm_full"), run_test(perm_test(
      m_f, term, "trial", signflip, B = B_MAIN, seed = seed,
      data = d)$p.value))
    if (!triple) {
      out$wald_trial <- step(paste0(name, "_wald_trial"), run_test(wald_p(
        fit("(1 + condition | participant) + (1 | trial)"), cf)))
    }
    out$wald_complete <- step(paste0(name, "_wald_complete"), {
      sing <- NA
      ci_c <- cbind(NA_real_, matrix(NA_real_, length(cf), 2))
      r <- run_test({
        m_c <- fit(sprintf(
          "(1 + condition + condition:ns(time, %d) | participant)", k))
        sing <- isSingular(m_c)
        ci_c <- cbind(fixef(m_c)[cf], wald_ci(m_c, cf))
        wald_p(m_c, cf)
      })
      r$note <- paste("singular:", sing)
      attr(r, "ci") <- ci_c
      r
    })
    INTERVALS_TC[[name]] <<- step(paste0(name, "_intervals"), {
      na_ci <- function(e) matrix(NA_real_, length(cf), 2)
      bt <- boot_ci(m_s, "participant", resample = "case", ci = "bca",
                    strata = STRATA, terms = cf, B = B_MAIN, seed = seed,
                    data = d)
      ci_c <- attr(out$wald_complete, "ci")
      iv <- list(wald_simple = cbind(fixef(m_s)[cf], wald_ci(m_s, cf)),
                 wald_full = cbind(fixef(m_f)[cf], wald_ci(m_f, cf)),
                 wald_complete = ci_c,
                 satt_simple = cbind(fixef(m_s)[cf], tryCatch(
                   satt_ci(m_s, cf), error = na_ci)),
                 satt_full = cbind(fixef(m_f)[cf], tryCatch(
                   satt_ci(m_f, cf), error = na_ci)))
      for (type in c("expanded", "percentile", "bca", "basic")) {
        iv[[paste0(CASE, type)]] <- cbind(
          fixef(m_s)[cf], confint(bt, type = type)[cf, , drop = FALSE])
      }
      do.call(rbind, lapply(names(iv), function(m) {
        data.frame(analysis = name, coef = cf, method = m,
                   estimate = unname(iv[[m]][, 1]),
                   lower = unname(iv[[m]][, 2]), upper = unname(iv[[m]][, 3]))
      }))
    })
    g_of <- if (triple) tapply(as.character(d$cohort), d$participant, `[`, 1)
    out$two_stage <- run_test({
      co <- participant_spline_coefs(d, k)
      if (triple) {
        ok <- stats::complete.cases(co)
        fm <- stats::manova(co[ok, ] ~ factor(g_of[rownames(co)][ok]))
        summary(fm, test = "Hotelling-Lawley")$stats[1, "Pr(>F)"]
      } else {
        hotelling_p(co)
      }
    })
    cl <- step(paste0(name, "_cluster"), {
      D <- diff_curves(d)
      cluster_test(D, as.numeric(colnames(D)), B_MAIN, seed,
                   group = if (triple) g_of[rownames(D)])
    })
    out$cluster <- data.frame(p = cl$p, seconds = NA, error = NA,
                              note = paste(sprintf(
                                "%.2f-%.2f s (p = %.4f)", cl$clusters$start_s,
                                cl$clusters$end_s, cl$clusters$p),
                                collapse = "; "))
    gm <- step(paste0(name, "_gamm"), tryCatch(
      gamm_test(d, triple = triple), error = function(e) conditionMessage(e)))
    if (is.character(gm)) {
      out$gamm_noar <- data.frame(p = NA, seconds = NA, error = gm)
      out$gamm_ar1 <- data.frame(p = NA, seconds = NA, error = gm)
    } else {
      out$gamm_noar <- data.frame(p = gm$noar, seconds = NA, error = NA)
      out$gamm_ar1 <- data.frame(p = gm$ar1, seconds = NA, error = NA,
                                 note = sprintf("rho = %.3f", gm$rho))
    }
  }
  res <- do.call(rbind, lapply(names(out), function(m) {
    x <- out[[m]]
    if (is.null(x$note)) x$note <- NA
    data.frame(analysis = name, method = m, x[c("p", "seconds", "error",
                                                "note")])
  }))
  rownames(res) <- NULL
  res
}

TESTS <- list()
INTERVALS_TC <- list()
TESTS$tc <- timecourse("stimulus_x_time")
TESTS$tc_df5 <- timecourse("stimulus_x_time_df5", k = 5,
                           methods = "simple")
TESTS$tc_seed2 <- timecourse("stimulus_x_time_seed2", seed = 2,
                             methods = "simple")
if (HAS_COHORT) {
  TESTS$triple <- timecourse("cohort_x_stimulus_x_time", triple = TRUE)
}

# ---- 3. Growth curve analysis
GCA_TERMS <- c(level = "condition", linear = "ot1:condition",
               quadratic = "ot2:condition", cubic = "ot3:condition")
GCA_COEFS <- c(level = "conditionTarget", linear = "ot1:conditionTarget",
               quadratic = "ot2:conditionTarget", cubic = "ot3:conditionTarget")
m_gca <- quiet(lmer(y ~ (ot1 + ot2 + ot3) * condition +
                      (ot1 + ot2 + ot3 | participant) +
                      (1 | participant:trial), data = d, control = ctrl))
m_mir <- quiet(lmer(y ~ (ot1 + ot2 + ot3) * condition +
                      (ot1 + ot2 + ot3 | participant) +
                      (ot1 + ot2 + ot3 | participant:condition),
                    data = d, control = ctrl))
m_gs <- quiet(lmer(y ~ (ot1 + ot2 + ot3) * condition + (1 | participant),
                   data = d, control = ctrl))
m_ls <- quiet(lmer(y ~ condition * P + (1 | participant), data = d,
                   control = ctrl))
fam4 <- step("gca_maxt_four_terms", do.call(perm_maxt, c(
  lapply(GCA_TERMS, function(tm) perm_spec(m_gs, tm, data = d)),
  list(unit = "trial", exchange = signflip, B = B_MAIN, seed = 1))))
fam2 <- step("gca_minp_level_shape", perm_maxt(
  level = perm_spec(m_ls, "condition", data = d),
  shape = perm_spec(m_ls, "condition:P", data = d),
  unit = "trial", exchange = signflip, B = B_MAIN, seed = 1))
co <- t(vapply(split(d, d$participant), function(x) {
  f <- tryCatch(stats::coef(stats::lm(y ~ (ot1 + ot2 + ot3) * condition,
                                      data = x))[GCA_COEFS],
                error = function(e) rep(NA_real_, 4))
  if (length(f) != 4) rep(NA_real_, 4) else unname(f)
}, numeric(4)))
p_two <- apply(co, 2, function(v) stats::t.test(v[is.finite(v)])$p.value)
wald_terms <- function(m) {
  vapply(GCA_COEFS, function(cc) tryCatch(wald_p(m, cc),
                                          error = function(e) NA_real_),
         numeric(1))
}
p_gca <- wald_terms(m_gca); p_mir <- wald_terms(m_mir)
p_gs <- wald_terms(m_gs); p_perm <- fam4$results$p.value
TESTS$gca <- data.frame(
  analysis = "gca", method = rep(c("wald_gca", "wald_mirman", "wald_simple",
                                   "perm_simple", "two_stage"), each = 4),
  term = names(GCA_TERMS), p = c(p_gca, p_mir, p_gs, p_perm, p_two),
  seconds = NA, error = NA,
  note = rep(c(paste("singular:", isSingular(m_gca)),
               paste("singular:", isSingular(m_mir)), NA, NA, NA), each = 4))

shape_p <- function(m) tryCatch(wald_p(m, GCA_COEFS[2:4]),
                                error = function(e) NA_real_)
fam_rows <- function(family, feats, adj) {
  do.call(rbind, lapply(names(adj), function(m) {
    data.frame(family = family, method = m, feature = feats,
               p = unname(adj[[m]]))
  }))
}
FAMILIES <- rbind(
  fam_rows("gca_four_terms", names(GCA_TERMS), list(
    perm_raw = p_perm, perm_holm = stats::p.adjust(p_perm, "holm"),
    maxt_stepdown = fam4$results$p.adjusted,
    wald_gca_holm = stats::p.adjust(p_gca, "holm"),
    wald_mirman_holm = stats::p.adjust(p_mir, "holm"),
    two_stage_holm = stats::p.adjust(p_two, "holm"))),
  fam_rows("gca_level_shape", c("level", "shape"), list(
    perm_raw = fam2$results$p.value,
    perm_holm = stats::p.adjust(fam2$results$p.value, "holm"),
    minp_stepdown = fam2$results$p.adjusted,
    wald_gca_holm = stats::p.adjust(c(p_gca[[1]], shape_p(m_gca)), "holm"),
    wald_mirman_holm = stats::p.adjust(c(p_mir[[1]], shape_p(m_mir)),
                                       "holm"))))

bt <- step("gca_boot_case", boot_ci(
  m_gs, "participant", resample = "case", ci = "bca", strata = STRATA,
  terms = unname(GCA_COEFS), B = B_MAIN, seed = 1, data = d))
iv <- list(wald_gca = wald_ci(m_gca, GCA_COEFS),
           wald_mirman = wald_ci(m_mir, GCA_COEFS),
           wald_simple = wald_ci(m_gs, GCA_COEFS))
for (type in c("expanded", "percentile", "bca", "basic")) {
  iv[[paste0(CASE, type)]] <- confint(bt, type = type)[GCA_COEFS, ]
}
INTERVALS <- do.call(rbind, lapply(names(iv), function(m) {
  data.frame(analysis = "gca", coef = unname(GCA_COEFS),
             term = names(GCA_COEFS), method = m,
             estimate = unname(fixef(if (m == "wald_gca") m_gca else
               if (m == "wald_mirman") m_mir else m_gs)[GCA_COEFS]),
             lower = unname(iv[[m]][, 1]), upper = unname(iv[[m]][, 2]))
}))
rm(m_gca, m_mir, m_gs, m_ls)

# ---- 4. Diagnostics
m_tc_s <- quiet(lmer(y ~ condition * ns(time, 3) + (1 | participant),
                     data = d, control = ctrl))
m_tc_f <- quiet(lmer(y ~ condition * ns(time, 3) +
                       (1 + condition | participant), data = d,
                     control = ctrl))
m_const <- quiet(lmer(y ~ condition + (1 + condition | participant),
                      data = d, control = ctrl))
ac <- step("diag_acf", diag_acf(m_tc_f, "trial", "time", data = d))
tc_spline <- step("diag_timecourse_spline", diag_timecourse(
  m_tc_f, "time", by = "condition", cluster = "participant", data = d))
tc_const <- step("diag_timecourse_constant", diag_timecourse(
  m_const, "time", by = "condition", cluster = "participant", data = d))
max_z <- function(x) {
  z <- abs(x$timecourse$mean / x$timecourse$se)
  max(z, na.rm = TRUE)
}
cal <- step("perm_calibrate", perm_calibrate(
  m_tc_s, "condition:ns(time, 3)", "trial", signflip, ar1 = ac,
  series = "trial", time = "time", n_sim = N_CAL, B = B_CAL, seed = 1,
  data = d))
mc <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(c(rate = NA, low = NA, high = NA, n = 0))
  ci <- stats::binom.test(sum(x <= 0.05), length(x))$conf.int
  c(rate = 100 * mean(x <= 0.05), low = 100 * ci[1], high = 100 * ci[2],
    n = length(x))
}
DIAGNOSTICS <- data.frame(
  check = c("autocorrelation lag 1 (bias-corrected)",
            "autocorrelation lag 1 (uncorrected)",
            "time-course misfit, spline model: largest |z|",
            "time-course misfit, constant-effect model: largest |z|",
            "time-course misfit threshold (Bonferroni, 2 x 67 points)"),
  value = round(c(ac$phi, ac$phi_raw, max_z(tc_spline), max_z(tc_const),
                  stats::qnorm(1 - 0.025 / (2 * length(unique(d$time))))),
                4))
CALIBRATION <- data.frame(
  analysis = "stimulus_x_time",
  test = c("permutation, intercept-only model", "Wald, intercept-only model"),
  rbind(mc(cal$p_perm), mc(cal$p_wald)), B = B_CAL, row.names = NULL)

m_gca <- quiet(lmer(y ~ (ot1 + ot2 + ot3) * condition +
                      (ot1 + ot2 + ot3 | participant) +
                      (1 | participant:trial), data = d, control = ctrl))
future::plan("multisession", workers = CAL_WORKERS)
for (tm in c(level = "condition", linear = "ot1:condition")) {
  nm <- if (tm == "condition") "level" else "linear"
  cg <- step(paste0("perm_calibrate_gca_", nm), perm_calibrate(
    m_gca, tm, "trial", signflip, ar1 = ac, series = "trial", time = "time",
    n_sim = N_CAL_GCA, B = B_CAL_GCA, seed = 1, data = d))
  CALIBRATION <- rbind(CALIBRATION, data.frame(
    analysis = paste0("gca_", nm),
    test = c("permutation, GCA model", "Wald, GCA model"),
    rbind(mc(cg$p_perm), mc(cg$p_wald)), B = B_CAL_GCA, row.names = NULL))
}
future::plan("multisession", workers = WORKERS)
rm(m_gca)

# ---- Export
write_csv <- function(x, file) {
  utils::write.csv(x, file.path(OUT_DIR, file), row.names = FALSE)
}
tests <- do.call(rbind, lapply(TESTS, function(x) {
  if (is.null(x$term)) x$term <- NA
  x[c("analysis", "term", "method", "p", "seconds", "error", "note")]
}))
write_csv(tests, "real_tests_sample_level.csv")
write_csv(FAMILIES, "real_families_gca.csv")
write_csv(INTERVALS, "real_intervals_gca.csv")
write_csv(do.call(rbind, INTERVALS_TC), "real_intervals_timecourse.csv")
write_csv(DIAGNOSTICS, "real_diagnostics_sample_level.csv")
write_csv(CALIBRATION, "real_perm_calibrate.csv")
write_csv(rbind(cbind(model = "spline", tc_spline$timecourse),
                cbind(model = "constant", tc_const$timecourse)),
          "real_diag_timecourse.csv")
write_csv(cbind(ac$acf, phi = ac$phi), "real_diag_acf.csv")
cluster_analyses <- c("stimulus_x_time",
                      if (HAS_COHORT) "cohort_x_stimulus_x_time")
clusters <- do.call(rbind, lapply(cluster_analyses, function(a) {
  x <- readRDS(file.path(OUT_DIR, "objects", paste0(a, "_cluster.rds")))
  if (nrow(x$clusters) == 0) return(NULL)
  cbind(analysis = a, x$clusters)
}))
if (!is.null(clusters)) write_csv(clusters, "real_clusters.csv")
writeLines(c(paste("Finished:", format(Sys.time())),
             paste("lmmresample", utils::packageVersion("lmmresample"),
                   utils::packageDescription("lmmresample")$RemoteSha),
             paste("lme4", utils::packageVersion("lme4")),
             paste("mgcv", utils::packageVersion("mgcv")),
             paste("R", R.version.string),
             sprintf("B = %d; perm_calibrate: %d simulations x B = %d",
                     B_MAIN, N_CAL, B_CAL),
             sprintf("perm_calibrate (GCA): %d simulations x B = %d",
                     N_CAL_GCA, B_CAL_GCA),
             paste("Workers:", WORKERS)),
           file.path(OUT_DIR, "run_info.txt"))
zip_file <- paste0(normalizePath(OUT_DIR), ".zip")
old_wd <- setwd(dirname(OUT_DIR))
if (file.exists(zip_file)) invisible(file.remove(zip_file))
utils::zip(zip_file, file.path(basename(OUT_DIR),
                               list.files(basename(OUT_DIR),
                                          recursive = TRUE)), flags = "-q")
setwd(old_wd)
log_msg("Done. Results: ", OUT_DIR, "; zip: ", zip_file)
