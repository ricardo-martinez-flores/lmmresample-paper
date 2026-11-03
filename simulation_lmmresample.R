# =============================================================================
# lmmresample: simulation study
#
# Usage:    Rscript simulation_lmmresample.R
# Settings: LMM_OUT (output folder), LMM_WORKERS (parallel sessions),
#           LMM_RUN (subset of parts), LMM_TEST (1 = quick check),
#           LMM_SKIP_INSTALL (1 = packages already installed)
# =============================================================================

WORKERS <- as.integer(Sys.getenv("LMM_WORKERS",
                                 max(1, parallel::detectCores() - 1)))
OUT_DIR <- Sys.getenv("LMM_OUT", "lmmresample_simulations")
PARTS <- c("example", "single", "timecourse", "triple", "bootstrap", "gca",
           "families", "posthoc", "diagnostics", "timing")
RUN <- PARTS
if (nzchar(Sys.getenv("LMM_RUN"))) {
  RUN <- intersect(trimws(strsplit(Sys.getenv("LMM_RUN"), ",")[[1]]), PARTS)
}
TEST <- Sys.getenv("LMM_TEST") == "1"

N <- list(null_base = 500, null_within = 1000, null = 300, effect = 200,
          boot = 500, boot_tc = 300, family_null = 500, family_effect = 200,
          diag = 200, calibrate = 100)
B <- list(perm = 199, cluster = 999, family = 499, boot = 1999, boot_tc = 999,
          boot_gca = 999, example = 4999, calibrate = 99, calibrate_sims = 100)
if (TEST) {
  N[] <- 2
  B <- list(perm = 9, cluster = 19, family = 19, boot = 49, boot_tc = 19,
            boot_gca = 19, example = 19, calibrate = 9, calibrate_sims = 4)
}
CHUNK <- 5
WAVE <- 4 * WORKERS

# ---- Packages
if (Sys.getenv("LMM_SKIP_INSTALL") != "1") {
  if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
  pak::pak(c("ricardo-martinez-flores/lmmresample", "lme4", "lmerTest",
             "mgcv", "future", "future.apply"), ask = FALSE)
}
suppressPackageStartupMessages({
  library(lmmresample); library(lme4); library(splines)
})
`%||%` <- function(a, b) if (is.null(a)) b else a
PKG_VERSION <- as.character(utils::packageVersion("lmmresample"))
PKG_SHA <- utils::packageDescription("lmmresample")$RemoteSha %||% "unknown"
if (utils::packageVersion("lmmresample") < "0.0.0.9004") {
  stop("lmmresample 0.0.0.9004 or later is needed. Restart R and run the ",
       "script again so that the new version is installed.")
}

if (Sys.info()[["sysname"]] == "Darwin") {
  system(sprintf("caffeinate -i -w %d", Sys.getpid()), wait = FALSE)
}
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
RAW_DIR <- file.path(OUT_DIR, "raw")
dir.create(RAW_DIR, showWarnings = FALSE)
LOG_FILE <- file.path(OUT_DIR, "progress.log")
log_msg <- function(...) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M  "), ...)
  message(txt)
  cat(txt, "\n", file = LOG_FILE, append = TRUE, sep = "")
}
log_msg("lmmresample ", PKG_VERSION, " (commit ", substr(PKG_SHA, 1, 7),
        "), ", WORKERS, " workers, parts: ", paste(RUN, collapse = ", "),
        if (TEST) " [TEST RUN]")
future::plan("multisession", workers = WORKERS)

# ---- Job runner

data_seed_base <- function(key) {
  h <- 5381
  for (v in utf8ToInt(key)) h <- (h * 33 + v) %% 500009
  h * 4000
}
second_seed <- function(seed) seed + 1000
fresh_rng <- function(seed) set.seed(seed + 2000)

JOBS <- list()
add_job <- function(.prefix, .fun, .n, .cost, .priority, ...,
                    .data_key = .prefix) {
  JOBS[[length(JOBS) + 1]] <<- list(prefix = .prefix, fun = .fun, n = .n,
                                    cost = .cost, priority = .priority,
                                    data_key = .data_key,
                                    seed_base = data_seed_base(.data_key),
                                    args = list(...))
}

bind_fill <- function(lst) {
  lst <- Filter(function(x) !is.null(x) && nrow(x) > 0, lst)
  if (length(lst) == 0) return(NULL)
  cols <- unique(unlist(lapply(lst, names)))
  do.call(rbind, lapply(lst, function(x) {
    for (cc in setdiff(cols, names(x))) x[[cc]] <- NA
    x[cols]
  }))
}

run_rep <- function(fun, i, seed, args) {
  f <- switch(fun,
              single = rep_single, timecourse = rep_timecourse,
              triple = rep_triple, triple_family = rep_triple_family,
              triple_boot = rep_triple_boot, boot_within = rep_boot_within,
              boot_groups = rep_boot_groups, gca = rep_gca,
              gca_mirman = rep_gca_mirman, triple_satt = rep_triple_satt,
              tc_boot = rep_tc_boot, posthoc = rep_posthoc,
              family = rep_family, diag_acf = rep_diag_acf,
              diag_calibrate = rep_diag_calibrate,
              diag_timecourse = rep_diag_timecourse,
              diag_loso = rep_diag_loso, diag_agreement = rep_diag_agreement,
              stop("Unknown job function: ", fun))
  tryCatch(do.call(f, c(list(i = i, seed = seed), args)),
           error = function(e) {
             list(errors = data.frame(rep = i, error = conditionMessage(e)))
           })
}

run_chunk <- function(task) {
  out <- lapply(task$reps, function(i) {
    run_rep(task$fun, i, task$seed_base + i, task$args)
  })
  parts <- unique(unlist(lapply(out, names)))
  res <- stats::setNames(lapply(parts, function(nm) {
    x <- bind_fill(lapply(out, `[[`, nm))
    if (!is.null(x)) x <- cbind(cell = task$prefix, x)
    x
  }), parts)
  tmp <- paste0(task$file, ".tmp")
  saveRDS(res, tmp)
  file.rename(tmp, task$file)
  TRUE
}

fmt_time <- function(sec) {
  sec <- max(0, round(sec))
  if (sec < 3600) return(sprintf("%d min", sec %/% 60))
  if (sec < 86400) return(sprintf("%d h %02d min", sec %/% 3600,
                                  (sec %% 3600) %/% 60))
  sprintf("%d d %02d h", sec %/% 86400, (sec %% 86400) %/% 3600)
}

run_jobs <- function(jobs) {
  if (length(jobs) == 0) return(invisible())
  keys <- unique(data.frame(
    key = vapply(jobs, `[[`, character(1), "data_key"),
    base = vapply(jobs, `[[`, numeric(1), "seed_base")))
  if (anyDuplicated(keys$base)) {
    stop("Two data keys share a seed: ",
         paste(keys$key[keys$base %in% keys$base[duplicated(keys$base)]],
               collapse = ", "))
  }
  if (anyDuplicated(vapply(jobs, `[[`, character(1), "prefix"))) {
    stop("Duplicated job names.")
  }
  todo <- list()
  for (j in jobs) {
    for (start in seq(1, j$n, by = CHUNK)) {
      f <- file.path(RAW_DIR, sprintf("%s__%04d.rds", j$prefix, start))
      if (file.exists(f)) next
      reps <- start:min(start + CHUNK - 1, j$n)
      todo[[length(todo) + 1]] <- list(
        prefix = j$prefix, fun = j$fun, file = f, reps = reps,
        seed_base = j$seed_base, args = j$args, priority = j$priority,
        weight = length(reps) * j$cost)
    }
  }
  if (length(todo) == 0) {
    log_msg("All simulation blocks are already finished.")
    return(invisible())
  }
  pr <- vapply(todo, `[[`, numeric(1), "priority")
  wt <- vapply(todo, `[[`, numeric(1), "weight")
  todo <- todo[order(pr, -wt)]
  total_w <- sum(wt)
  guess <- total_w / WORKERS * 1.2
  log_msg(sprintf("%d files to compute. First rough estimate: %s (around %s).",
                  length(todo), fmt_time(guess),
                  format(Sys.time() + guess, "%a %d %b %H:%M")))
  t0 <- Sys.time(); done_w <- 0
  waves <- split(seq_along(todo), ceiling(seq_along(todo) / WAVE))
  for (wv in waves) {
    tryCatch(future.apply::future_lapply(todo[wv], run_chunk,
                                         future.seed = TRUE,
                                         future.chunk.size = 1),
             error = function(e) log_msg("WARNING: ", conditionMessage(e)))
    done_w <- done_w + sum(vapply(todo[wv], `[[`, numeric(1), "weight"))
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    left <- (total_w - done_w) * elapsed / done_w
    log_msg(sprintf("[%5.1f%%] last: %s | elapsed %s | left ~%s | ends ~%s",
                    100 * done_w / total_w, todo[[max(wv)]]$prefix,
                    fmt_time(elapsed), fmt_time(left),
                    format(Sys.time() + left, "%a %d %b %H:%M")))
  }
  missing <- sum(!file.exists(vapply(todo, `[[`, character(1), "file")))
  log_msg("Blocks finished in ", fmt_time(as.numeric(
    difftime(Sys.time(), t0, units = "secs"))), ".",
    if (missing > 0) sprintf(" %d files are missing: run the script again.",
                             missing))
}

KEY_COLS <- c("cell", "rep", "method", "term", "coef", "feature", "test")
read_raw <- function(prefix, part) {
  files <- list.files(RAW_DIR, paste0("^", prefix, ".*__[0-9]+\\.rds$"),
                      full.names = TRUE)
  if (length(files) == 0) return(NULL)
  x <- bind_fill(lapply(files, function(f) readRDS(f)[[part]]))
  if (is.null(x)) return(NULL)
  x[!duplicated(x[intersect(KEY_COLS, names(x))]), , drop = FALSE]
}

# ---- Test helpers
run_test <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  r <- tryCatch(list(p = as.numeric(expr)[1], error = NA_character_),
                error = function(e) list(p = NA_real_,
                                         error = conditionMessage(e)))
  r$seconds <- proc.time()[["elapsed"]] - t0
  r
}
results <- function(tests, ...) {
  data.frame(..., method = names(tests),
             p = vapply(tests, `[[`, numeric(1), "p"),
             seconds = vapply(tests, `[[`, numeric(1), "seconds"),
             error = vapply(tests, `[[`, character(1), "error"),
             package = substr(PKG_SHA, 1, 7), row.names = NULL)
}
worker_setup <- function() {
  suppressPackageStartupMessages({
    library(lmmresample); library(lme4); library(splines)
  })
  list(ctrl = lmerControl(calc.derivs = FALSE),
       quiet = function(expr) suppressWarnings(suppressMessages(expr)))
}
wald_p <- function(m, cf) {
  b <- fixef(m)[cf]
  V <- as.matrix(vcov(m))[cf, cf, drop = FALSE]
  stats::pchisq(as.numeric(crossprod(b, solve(V, b))), length(cf),
                lower.tail = FALSE)
}
wald_ci <- function(m, cf, level = 0.95) {
  b <- fixef(m)[cf]
  se <- sqrt(diag(as.matrix(vcov(m)))[cf])
  z <- stats::qnorm(1 - (1 - level) / 2)
  cbind(b - z * se, b + z * se)
}
satt_p <- function(m, term) {
  mt <- suppressWarnings(suppressMessages(lmerTest::as_lmerModLmerTest(m)))
  a <- suppressWarnings(suppressMessages(
    stats::anova(mt, ddf = "Satterthwaite")))
  a[term, "Pr(>F)"]
}
satt_ci <- function(m, cf, level = 0.95) {
  mt <- suppressWarnings(suppressMessages(lmerTest::as_lmerModLmerTest(m)))
  co <- summary(mt)$coefficients[cf, , drop = FALSE]
  q <- stats::qt(1 - (1 - level) / 2, co[, "df"])
  cbind(co[, "Estimate"] - q * co[, "Std. Error"],
        co[, "Estimate"] + q * co[, "Std. Error"])
}
perm_p <- function(m, term, unit, exchange, B, seed,
                   method = "auto") {
  suppressWarnings(suppressMessages(
    perm_test(m, term, unit, exchange, method = method, B = B,
              seed = seed)))$p.value
}
hotelling_p <- function(D) {
  D <- D[stats::complete.cases(D), , drop = FALSE]
  n <- nrow(D); q <- ncol(D); mu <- colMeans(D); S <- stats::cov(D)
  t2 <- n * as.numeric(crossprod(mu, solve(S, mu)))
  stats::pf((n - q) / (q * (n - 1)) * t2, q, n - q, lower.tail = FALSE)
}

# ---- Simulated data
TRIAL_COUNTS <- c(6, 22, 23, 27, 27, 39, 51, 54, 54, 55, 56, 60, 62, 64, 65,
                  72, 76, 80, 83, 86, 87, 88, 89, 90, 92, 93, 93, 96, 96, 97,
                  98, 101, 101, 103, 104, 104, 104, 104, 104, 105, 106, 106,
                  106, 106, 107, 109, 109, 110, 110, 110, 110, 111, 111, 111,
                  111, 111, 111, 111, 111, 112, 112, 112, 112, 113, 113, 113,
                  114)
SD <- list(participant = 0.04, slope = 0.056, trial = 0.05, residual = 0.124,
           baseline = 0.105)

BASE <- list(n = 30, n_treat = NA, ar1 = 0.92, residual_df = 5,
             trials = "realistic", skew = FALSE, ar1_trials = 0, n_time = 20,
             rate = 10, gaps = "none", ar_split = NULL)
VARIANTS <- list(
  base         = list(),
  n12          = list(n = 12),
  n60          = list(n = 60),
  ar05         = list(ar1 = 0.5),
  normal       = list(residual_df = Inf),
  balanced     = list(trials = "balanced"),
  sparse       = list(trials = "sparse"),
  skewed       = list(skew = TRUE),
  trial_ar     = list(ar1_trials = 0.2),
  unbalanced   = list(n_treat = 10),
  gaps_interp  = list(gaps = "interpolate"),
  gaps_removed = list(gaps = "remove"),
  ar_hetero    = list(ar_split = c(0.80, 0.97)),
  real_res     = list(n_time = 67, rate = 33, ar1 = 0.975)
)

skew_draw <- function(n) -(stats::rchisq(n, 2) - 2) / 2

make_data <- function(seed, design, variant = "base", effect = 0) {
  v <- utils::modifyList(BASE, VARIANTS[[variant]])
  has_cond <- design != "between"
  grouped <- design %in% c("between", "mixed", "triple")
  onset <- if (design %in% c("timecourse", "triple")) 0.5 else 0
  cond_effect <- switch(design, within = effect, covariate = effect,
                        timecourse = effect, triple = 0.04, biomarker = 0.05,
                        0)
  set.seed(seed)
  k <- switch(v$trials,
              balanced = rep(80, v$n),
              sparse = c(rep(5, round(v$n / 3)),
                         sample(TRIAL_COUNTS, v$n - round(v$n / 3), TRUE)),
              sample(TRIAL_COUNTS, v$n, replace = TRUE))
  sim <- function(n, kk, ar1, s) {
    sim_blocks(if (has_cond) "within" else "between", n_participants = n,
               n_trials = kk, n_time = v$n_time, sampling_rate = v$rate,
               prop_condition = if (design == "three_level") 0.4 else 0.2,
               effect_condition = if (has_cond) cond_effect else 0,
               effect_onset = onset,
               sd_participant = if (v$skew) 0 else SD$participant,
               sd_slope = if (v$skew || !has_cond) 0 else SD$slope,
               sd_trial = if (design == "covariate") 0.02 else SD$trial,
               sd_residual = SD$residual, ar1 = ar1,
               ar1_trials = v$ar1_trials, residual_df = v$residual_df,
               seed = s)
  }
  if (is.null(v$ar_split)) {
    d <- sim(v$n, k, v$ar1, seed)
  } else {
    h <- v$n %/% 2
    a <- sim(h, k[seq_len(h)], v$ar_split[1], seed)
    b <- sim(v$n - h, k[-seq_len(h)], v$ar_split[2], second_seed(seed))
    a$participant <- paste0("a", a$participant); a$trial <- paste0("a", a$trial)
    b$participant <- paste0("b", b$participant); b$trial <- paste0("b", b$trial)
    d <- rbind(a, b)
    d$participant <- factor(d$participant, levels = unique(d$participant))
    d$trial <- factor(d$trial, levels = unique(d$trial))
  }
  d$group <- NULL

  fresh_rng(seed)
  parts <- levels(d$participant)
  n <- length(parts)
  pid <- match(d$participant, parts)
  isB <- if (has_cond) d$condition == "B" else rep(FALSE, nrow(d))
  w <- lmmresample:::effect_weight(d$time, onset)
  if (grouped) {
    n_treat <- if (is.na(v$n_treat)) n %/% 2 else v$n_treat
    trt <- seq_len(n) %in% sample(n, n_treat)
    d$group <- factor(ifelse(trt[pid], "treatment", "control"),
                      levels = c("control", "treatment"))
  }
  if (v$skew) {
    d$y <- d$y + SD$participant * skew_draw(n)[pid]
    if (has_cond) d$y <- d$y + SD$slope * skew_draw(n)[pid] * isB * w
  }
  isT <- if (grouped) d$group == "treatment" else rep(FALSE, nrow(d))
  if (design == "between") d$y <- d$y + effect * isT
  if (design %in% c("mixed", "triple")) d$y <- d$y + effect * w * (isT & isB)
  if (design == "three_level") {
    tr <- unique(d[c("trial", "condition")])
    lab <- as.character(tr$condition)
    b_tr <- which(lab == "B")
    lab[b_tr[stats::runif(length(b_tr)) < 0.5]] <- "C"
    d$condition <- factor(lab[match(d$trial, tr$trial)],
                          levels = c("A", "B", "C"))
    d$y <- d$y + effect * (d$condition == "C")
  }
  if (design == "covariate") {
    tr <- unique(d[c("participant", "trial", "condition")])
    kk <- as.vector(table(tr$participant)[parts])
    tr$baseline <- -0.03 * (tr$condition == "B") +
      lmmresample:::trial_intercepts(kk, SD$baseline, 0.21)
    d$baseline <- tr$baseline[match(d$trial, tr$trial)]
    d$y <- d$y - 0.44 * d$baseline
  }
  if (design == "biomarker") {
    raw <- stats::rlnorm(n, meanlog = log(0.3), sdlog = 0.6)
    z <- as.numeric(scale(log(raw)))
    d$biomarker <- z[pid]
    d$y <- d$y + effect * d$biomarker * isB
  }
  prop_missing <- 0
  if (v$gaps != "none") {
    rows <- split(seq_len(nrow(d)), d$trial)
    drop <- logical(nrow(d))
    for (ix in rows) {
      if (stats::runif(1) < 0.4) {
        L <- sample(2:6, 1)
        s <- sample.int(length(ix) - L + 1, 1)
        drop[ix[s:(s + L - 1)]] <- TRUE
      }
    }
    prop_missing <- mean(drop)
    if (v$gaps == "remove") {
      d <- d[!drop, ]
    } else {
      d$y[drop] <- NA
      for (ix in rows) {
        yy <- d$y[ix]
        if (anyNA(yy)) {
          ok <- !is.na(yy)
          d$y[ix] <- stats::approx(d$time[ix][ok], yy[ok], xout = d$time[ix],
                                   rule = 2)$y
        }
      }
    }
  }
  d$y <- d$y - stats::ave(d$y, d$time)
  rownames(d) <- NULL
  attr(d, "prop_missing") <- prop_missing
  d
}

trial_features <- function(d) {
  by <- intersect(c("participant", "group", "trial", "condition"), names(d))
  f <- stats::as.formula(paste("y ~", paste(by, collapse = " + ")))
  tr <- stats::aggregate(f, d, mean)
  names(tr)[names(tr) == "y"] <- "mean"
  pk <- stats::aggregate(f, d, max)
  lt <- stats::aggregate(f, d[d$time >= 1, ], mean)
  tr$peak <- pk$y[match(tr$trial, pk$trial)]
  tr$late <- lt$y[match(tr$trial, lt$trial)]
  tr
}

noise_free <- function(d, design, effect) {
  times <- sort(unique(d$time))
  ti <- match(d$time, times)
  rc <- lmmresample:::response_curve(length(times))
  w <- lmmresample:::effect_weight(d$time, 0.5)
  isB <- d$condition == "B"
  d$y <- if (design == "triple") {
    rc[ti] + w * (0.04 * isB + effect * (isB & d$group == "treatment"))
  } else {
    rc[ti] + w * effect * isB
  }
  d
}

# ---- Part "single"

two_stage_cond <- function(d) {
  tm <- stats::aggregate(y ~ participant + trial + condition, d, mean)
  pm <- stats::aggregate(y ~ participant + condition, tm, mean)
  stats::reshape(pm, idvar = "participant", timevar = "condition",
                 direction = "wide")
}

rep_single <- function(i, seed, design, variant, effect, B) {
  w <- worker_setup()
  d <- make_data(seed, design, variant, effect)
  fit <- function(f) w$quiet(lmer(f, data = d, control = w$ctrl))
  signflip <- exch_signflip("participant")
  tests <- list()

  if (design == "within") {
    m_s <- fit(y ~ condition + (1 | participant))
    m_f <- fit(y ~ condition + (1 + condition | participant))
    m_t <- fit(y ~ condition + (1 + condition | participant) + (1 | trial))
    tests <- list(
      wald_simple = run_test(wald_p(m_s, "conditionB")),
      wald_full   = run_test(wald_p(m_f, "conditionB")),
      wald_trial  = run_test(wald_p(m_t, "conditionB")),
      satt_full   = run_test(satt_p(m_f, "condition")),
      perm_simple = run_test(perm_p(m_s, "condition", "trial", signflip, B,
                                    seed)),
      perm_full   = run_test(perm_p(m_f, "condition", "trial", signflip, B,
                                    seed)),
      two_stage   = run_test({
        wd <- two_stage_cond(d)
        stats::t.test(wd$y.B - wd$y.A)$p.value
      }))
  }

  if (design == "between") {
    m_s <- fit(y ~ group + (1 | participant))
    m_t <- fit(y ~ group + (1 | participant) + (1 | trial))
    tests <- list(
      wald_simple = run_test(wald_p(m_s, "grouptreatment")),
      wald_trial  = run_test(wald_p(m_t, "grouptreatment")),
      satt_simple = run_test(satt_p(m_s, "group")),
      perm_simple = run_test(perm_p(m_s, "group", "participant", exch_free(),
                                    B, seed)),
      two_stage   = run_test({
        pm <- stats::aggregate(y ~ participant + group,
                               stats::aggregate(y ~ participant + group +
                                                  trial, d, mean), mean)
        stats::t.test(y ~ group, data = pm)$p.value
      }))
  }

  if (design == "mixed") {
    term <- "group:condition"
    cf <- "grouptreatment:conditionB"
    m_s <- fit(y ~ group * condition + (1 | participant))
    m_f <- fit(y ~ group * condition + (1 + condition | participant))
    tests <- list(
      wald_simple = run_test(wald_p(m_s, cf)),
      wald_full   = run_test(wald_p(m_f, cf)),
      satt_full   = run_test(satt_p(m_f, term)),
      perm_simple = run_test(perm_p(m_s, term, "trial", signflip, B, seed)),
      perm_full   = run_test(perm_p(m_f, term, "trial", signflip, B, seed)),
      two_stage   = run_test({
        tm <- stats::aggregate(y ~ participant + group + trial + condition, d,
                               mean)
        pm <- stats::aggregate(y ~ participant + group + condition, tm, mean)
        wd <- stats::reshape(pm, idvar = c("participant", "group"),
                             timevar = "condition", direction = "wide")
        stats::t.test(I(y.B - y.A) ~ group, data = wd)$p.value
      }))
  }

  if (design == "covariate") {
    m_s <- fit(y ~ condition + baseline + (1 | participant))
    m_f <- fit(y ~ condition + baseline + (1 + condition | participant))
    tests <- list(
      wald_simple = run_test(wald_p(m_s, "conditionB")),
      wald_full   = run_test(wald_p(m_f, "conditionB")),
      satt_full   = run_test(satt_p(m_f, "condition")),
      perm_simple = run_test(perm_p(m_s, "condition", "trial", signflip, B,
                                    seed, method = "freedman-lane")),
      perm_full   = run_test(perm_p(m_f, "condition", "trial", signflip, B,
                                    seed, method = "freedman-lane")),
      two_stage   = run_test({
        tm <- stats::aggregate(y ~ participant + trial + condition + baseline,
                               d, mean)
        b <- vapply(split(tm, tm$participant), function(x) {
          f <- tryCatch(stats::coef(stats::lm(y ~ condition + baseline,
                                              data = x))[["conditionB"]],
                        error = function(e) NA_real_)
          if (is.null(f)) NA_real_ else f
        }, numeric(1))
        stats::t.test(b[is.finite(b)])$p.value
      }),
      two_stage_unadjusted = run_test({
        wd <- two_stage_cond(d)
        stats::t.test(wd$y.B - wd$y.A)$p.value
      }))
  }

  if (design == "biomarker") {
    term <- "condition:biomarker"
    m_s <- fit(y ~ condition * biomarker + (1 | participant))
    m_f <- fit(y ~ condition * biomarker + (1 + condition | participant))
    tests <- list(
      wald_simple = run_test(wald_p(m_s, "conditionB:biomarker")),
      wald_full   = run_test(wald_p(m_f, "conditionB:biomarker")),
      satt_full   = run_test(satt_p(m_f, term)),
      perm_simple = run_test(perm_p(m_s, term, "trial", signflip, B, seed)),
      perm_full   = run_test(perm_p(m_f, term, "trial", signflip, B, seed)),
      two_stage   = run_test({
        tm <- stats::aggregate(y ~ participant + trial + condition + biomarker,
                               d, mean)
        pm <- stats::aggregate(y ~ participant + condition + biomarker, tm,
                               mean)
        wd <- stats::reshape(pm, idvar = c("participant", "biomarker"),
                             timevar = "condition", direction = "wide")
        summary(stats::lm(I(y.B - y.A) ~ biomarker,
                          data = wd))$coefficients["biomarker", 4]
      }))
  }

  if (design == "three_level") {
    cf <- c("conditionB", "conditionC")
    m_s <- fit(y ~ condition + (1 | participant))
    m_f <- fit(y ~ condition + (1 + condition | participant))
    tests <- list(
      wald_simple = run_test(wald_p(m_s, cf)),
      wald_full   = run_test(wald_p(m_f, cf)),
      satt_full   = run_test(satt_p(m_f, "condition")),
      perm_simple = run_test(perm_p(m_s, "condition", "trial", signflip, B,
                                    seed)),
      perm_full   = run_test(perm_p(m_f, "condition", "trial", signflip, B,
                                    seed)),
      two_stage   = run_test({
        wd <- two_stage_cond(d)
        hotelling_p(cbind(wd$y.B - wd$y.A, wd$y.C - wd$y.A))
      }))
  }

  list(tests = results(tests, design = design, variant = variant,
                       effect = effect, rep = i))
}

# ---- Parts "timecourse" and "triple"

diff_curves <- function(d) {
  m <- stats::aggregate(y ~ participant + condition + time, d, mean)
  wd <- stats::reshape(m, idvar = c("participant", "time"),
                       timevar = "condition", direction = "wide")
  wd$diff <- wd$y.B - wd$y.A
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
  list(mass = m, start = starts, value = r$values)
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
  obs_max <- max(abs(obs$mass))
  set.seed(seed)
  null <- vapply(seq_len(B), function(b) {
    st <- if (is.null(group)) {
      stat_of(D * sample(c(-1, 1), n, replace = TRUE))
    } else {
      stat_of(D, sample(g))
    }
    max(abs(cluster_masses(st, thr)$mass))
  }, numeric(1))
  crit <- stats::quantile(null, 0.95, names = FALSE)
  sig <- which(obs$value != 0 & abs(obs$mass) > crit)
  list(p = (1 + sum(null >= obs_max)) / (B + 1),
       onset = if (length(sig)) times[min(obs$start[sig])] else NA_real_)
}

gamm_test <- function(d, triple = FALSE, k = 10) {
  d <- d[order(d$trial, d$time), ]
  d$condO <- as.ordered(d$condition)
  stats::contrasts(d$condO) <- "contr.treatment"
  d$start <- !duplicated(d$trial)
  kk <- min(k, length(unique(d$time)) - 1)
  if (triple) {
    d$groupO <- as.ordered(d$group)
    stats::contrasts(d$groupO) <- "contr.treatment"
    d$gcO <- as.ordered(factor(d$group == "treatment" & d$condition == "B",
                               levels = c(FALSE, TRUE)))
    stats::contrasts(d$gcO) <- "contr.treatment"
    f <- y ~ groupO + condO + gcO + s(time, k = kk) +
      s(time, by = groupO, k = kk) + s(time, by = condO, k = kk) +
      s(time, by = gcO, k = kk) +
      s(time, participant, bs = "fs", m = 1, k = 5) +
      s(time, participant, by = condO, bs = "fs", m = 1, k = 5)
    pattern <- "^s\\(time\\):gcO"
  } else {
    f <- y ~ condO + s(time, k = kk) + s(time, by = condO, k = kk) +
      s(time, participant, bs = "fs", m = 1, k = 5) +
      s(time, participant, by = condO, bs = "fs", m = 1, k = 5)
    pattern <- "^s\\(time\\):condO"
  }
  t0 <- proc.time()[["elapsed"]]
  m0 <- suppressWarnings(mgcv::bam(f, data = d, method = "fREML",
                                    discrete = TRUE))
  t_noar <- proc.time()[["elapsed"]] - t0
  r <- stats::residuals(m0)
  same <- !d$start[-1]
  rho <- stats::cor(r[-1][same], r[-length(r)][same])
  t0 <- proc.time()[["elapsed"]]
  m1 <- suppressWarnings(mgcv::bam(f, data = d, method = "fREML",
                                    discrete = TRUE, rho = rho,
                                    AR.start = d$start))
  t_ar1 <- proc.time()[["elapsed"]] - t0
  pv <- function(m) {
    st <- summary(m)$s.table
    min(max(st[grep(pattern, rownames(st)), "p-value"][1], 0), 1)
  }
  list(noar = list(p = pv(m0), error = NA_character_, seconds = t_noar),
       ar1 = list(p = pv(m1), error = NA_character_,
                  seconds = t_noar + t_ar1),
       rho = rho)
}

participant_spline_coefs <- function(d) {
  t(vapply(split(d, d$participant), function(x) {
    f <- tryCatch(stats::lm(y ~ condition * ns(time, 3), data = x),
                  error = function(e) NULL)
    v <- if (is.null(f)) rep(NA_real_, 3) else {
      co <- stats::coef(f)
      unname(co[grep("^conditionB:", names(co))])
    }
    if (length(v) != 3) rep(NA_real_, 3) else v
  }, numeric(3)))
}

tc_methods <- function(d, seed, B, B_cluster, triple = FALSE,
                       full_perm = TRUE) {
  w <- worker_setup()
  fit <- function(f) w$quiet(lmer(f, data = d, control = w$ctrl))
  fixed <- if (triple) "y ~ group * condition * ns(time, 3)" else
    "y ~ condition * ns(time, 3)"
  term <- if (triple) "group:condition:ns(time, 3)" else
    "condition:ns(time, 3)"
  pattern <- if (triple) "^grouptreatment:conditionB:ns" else "^conditionB:ns"
  form <- function(re) stats::as.formula(paste(fixed, "+", re))
  signflip <- exch_signflip("participant")
  tests <- list(); extra <- list(singular = NA, rho = NA_real_,
                                 onset = NA_real_)

  m_s <- fit(form("(1 | participant)"))
  m_f <- fit(form("(1 + condition | participant)"))
  cf <- grep(pattern, names(fixef(m_f)), value = TRUE)
  tests$wald_simple <- run_test(wald_p(m_s, cf))
  tests$wald_full <- run_test(wald_p(m_f, cf))
  if (!triple) {
    tests$wald_trial <- run_test(wald_p(
      fit(form("(1 + condition | participant) + (1 | trial)")), cf))
  }
  tests$wald_complete <- run_test({
    m_c <- fit(form("(1 + condition + condition:ns(time, 3) | participant)"))
    extra$singular <- isSingular(m_c)
    wald_p(m_c, cf)
  })
  tests$satt_full <- run_test(satt_p(m_f, term))
  tests$perm_simple <- run_test(perm_p(m_s, term, "trial", signflip, B, seed))
  if (full_perm) {
    tests$perm_full <- run_test(perm_p(m_f, term, "trial", signflip, B, seed))
  }
  g_of <- if (triple) tapply(as.character(d$group), d$participant, `[`, 1)
  tests$two_stage <- run_test({
    co <- participant_spline_coefs(d)
    if (triple) {
      ok <- stats::complete.cases(co)
      fit_m <- stats::manova(co[ok, ] ~ factor(g_of[rownames(co)][ok]))
      summary(fit_m, test = "Hotelling-Lawley")$stats[1, "Pr(>F)"]
    } else {
      hotelling_p(co)
    }
  })
  tests$cluster <- run_test({
    D <- diff_curves(d)
    cl <- cluster_test(D, as.numeric(colnames(D)), B_cluster, seed,
                       group = if (triple) g_of[rownames(D)])
    extra$onset <- cl$onset
    cl$p
  })
  g <- tryCatch(gamm_test(d, triple = triple),
                error = function(e) conditionMessage(e))
  if (is.character(g)) {
    tests$gamm_noar <- list(p = NA_real_, error = g, seconds = NA_real_)
    tests$gamm_ar1 <- list(p = NA_real_, error = g, seconds = NA_real_)
  } else {
    tests$gamm_noar <- g$noar
    tests$gamm_ar1 <- g$ar1
    extra$rho <- g$rho
  }
  list(tests = tests, extra = extra)
}
tc_results <- function(out, ...) {
  r <- results(out$tests, ...)
  r$singular <- ifelse(r$method == "wald_complete", out$extra$singular, NA)
  r$rho <- ifelse(r$method == "gamm_ar1", out$extra$rho, NA_real_)
  r$onset <- ifelse(r$method == "cluster", out$extra$onset, NA_real_)
  r
}

rep_timecourse <- function(i, seed, variant, effect, B, B_cluster,
                           full_perm) {
  d <- make_data(seed, "timecourse", variant, effect)
  out <- tc_methods(d, seed, B, B_cluster, full_perm = full_perm)
  r <- tc_results(out, design = "timecourse", variant = variant,
                  effect = effect, rep = i)
  r$prop_missing <- attr(d, "prop_missing")
  list(tests = r)
}

rep_triple <- function(i, seed, variant, effect, B, B_cluster) {
  d <- make_data(seed, "triple", variant, effect)
  out <- tc_methods(d, seed, B, B_cluster, triple = TRUE)
  list(tests = tc_results(out, design = "triple", variant = variant,
                          effect = effect, rep = i))
}

rep_triple_family <- function(i, seed, variant, effect, B) {
  w <- worker_setup()
  d <- make_data(seed, "triple", variant, effect)
  tr <- trial_features(d)
  feats <- c("mean", "peak", "late")
  cf <- "grouptreatment:conditionB"
  fits <- lapply(feats, function(v) {
    m_s <- w$quiet(lmer(stats::as.formula(paste(
      v, "~ group * condition + (1 | participant)")), data = tr,
      control = w$ctrl))
    m_f <- w$quiet(lmer(stats::as.formula(paste(
      v, "~ group * condition + (1 + condition | participant)")), data = tr,
      control = w$ctrl))
    list(simple = m_s, full = m_f)
  })
  names(fits) <- feats
  p_wald <- vapply(fits, function(f) {
    tryCatch(wald_p(f$full, cf), error = function(e) NA_real_)
  }, numeric(1))
  p_satt <- vapply(fits, function(f) {
    tryCatch(satt_p(f$full, "group:condition"), error = function(e) NA_real_)
  }, numeric(1))
  specs <- lapply(fits, function(f) perm_spec(f$simple, "group:condition",
                                              data = tr))
  t0 <- proc.time()[["elapsed"]]
  r <- w$quiet(do.call(perm_maxt, c(specs, list(
    unit = "trial", exchange = exch_signflip("participant"), B = B,
    seed = seed))))
  secs <- proc.time()[["elapsed"]] - t0
  family_rows(r, list(wald = p_wald, satt = p_satt), secs,
              design = "triple", variant = variant, effect = effect, rep = i)
}

family_rows <- function(r, other, seconds, ...) {
  obs <- r$results$statistic
  nul <- r$null[stats::complete.cases(r$null), , drop = FALSE]
  p_raw <- r$results$p.value
  adj <- list(
    perm_raw      = p_raw,
    perm_holm     = stats::p.adjust(p_raw, "holm"),
    maxt_single   = lmmresample:::maxt_adjust(obs, nul, "single-step"),
    maxt_stepdown = lmmresample:::maxt_adjust(obs, nul, "step-down"),
    minp_single   = lmmresample:::minp_adjust(obs, nul, "single-step"),
    minp_stepdown = lmmresample:::minp_adjust(obs, nul, "step-down"))
  for (nm in names(other)) {
    adj[[paste0(nm, "_raw")]] <- other[[nm]]
    adj[[paste0(nm, "_holm")]] <- stats::p.adjust(other[[nm]], "holm")
  }
  feats <- r$results$test
  out <- do.call(rbind, lapply(names(adj), function(m) {
    data.frame(..., method = m, feature = feats, p = unname(adj[[m]]),
               row.names = NULL)
  }))
  out$seconds_perm <- seconds
  out$B_used <- r$B_used
  list(family = out)
}

rep_triple_boot <- function(i, seed, variant, effect, B) {
  w <- worker_setup()
  d <- make_data(seed, "triple", variant, effect)
  fixed <- "y ~ group * condition * ns(time, 3)"
  fit <- function(re) w$quiet(lmer(stats::as.formula(paste(fixed, "+", re)),
                                   data = d, control = w$ctrl))
  m_s <- fit("(1 | participant)")
  cf <- grep("^grouptreatment:conditionB:ns", names(fixef(m_s)), value = TRUE)
  truth <- stats::coef(stats::lm(stats::as.formula(fixed),
                                 data = noise_free(d, "triple", effect)))[cf]
  iv <- list(wald_simple = wald_ci(m_s, cf))
  iv$wald_full <- tryCatch(wald_ci(fit("(1 + condition | participant)"), cf),
                           error = function(e) matrix(NA_real_, 3, 2))
  iv$wald_complete <- tryCatch(wald_ci(fit(
    "(1 + condition + condition:ns(time, 3) | participant)"), cf),
    error = function(e) matrix(NA_real_, 3, 2))
  err <- NA_character_
  t0 <- proc.time()[["elapsed"]]
  bt <- tryCatch(w$quiet(boot_ci(m_s, "participant", resample = "case",
                                 ci = "bca", strata = "group", terms = cf,
                                 B = B, seed = seed)),
                 error = function(e) { err <<- conditionMessage(e); NULL })
  secs <- proc.time()[["elapsed"]] - t0
  for (type in c("expanded", "percentile", "bca", "basic")) {
    iv[[paste0("case_strata_", type)]] <- if (is.null(bt)) {
      matrix(NA_real_, 3, 2)
    } else {
      tryCatch(confint(bt, type = type)[cf, , drop = FALSE],
               error = function(e) matrix(NA_real_, 3, 2))
    }
  }
  out <- do.call(rbind, lapply(names(iv), function(m) {
    data.frame(design = "triple", variant = variant, effect = effect,
               rep = i, method = m, coef = cf, lower = unname(iv[[m]][, 1]),
               upper = unname(iv[[m]][, 2]), truth = unname(truth),
               row.names = NULL)
  }))
  out$error <- err
  out$seconds_boot <- secs
  list(intervals = out)
}

rep_triple_satt <- function(i, seed, variant, effect) {
  w <- worker_setup()
  d <- make_data(seed, "triple", variant, effect)
  fixed <- "y ~ group * condition * ns(time, 3)"
  fit <- function(re) w$quiet(lmer(stats::as.formula(paste(fixed, "+", re)),
                                   data = d, control = w$ctrl))
  m_s <- fit("(1 | participant)")
  cf <- grep("^grouptreatment:conditionB:ns", names(fixef(m_s)), value = TRUE)
  truth <- stats::coef(stats::lm(stats::as.formula(fixed),
                                 data = noise_free(d, "triple", effect)))[cf]
  iv <- list(
    satt_simple = tryCatch(satt_ci(m_s, cf),
                           error = function(e) matrix(NA_real_, 3, 2)),
    satt_full = tryCatch(satt_ci(fit("(1 + condition | participant)"), cf),
                         error = function(e) matrix(NA_real_, 3, 2)))
  out <- do.call(rbind, lapply(names(iv), function(m) {
    data.frame(design = "triple", variant = variant, effect = effect,
               rep = i, method = m, coef = cf, lower = unname(iv[[m]][, 1]),
               upper = unname(iv[[m]][, 2]), truth = unname(truth),
               row.names = NULL)
  }))
  list(intervals = out)
}

rep_tc_boot <- function(i, seed, variant, effect, B) {
  w <- worker_setup()
  d <- make_data(seed, "timecourse", variant, effect)
  fixed <- "y ~ condition * ns(time, 3)"
  fit <- function(re) w$quiet(lmer(stats::as.formula(paste(fixed, "+", re)),
                                   data = d, control = w$ctrl))
  m_s <- fit("(1 | participant)")
  m_f <- fit("(1 + condition | participant)")
  cf <- grep("^conditionB:ns", names(fixef(m_s)), value = TRUE)
  truth <- stats::coef(stats::lm(stats::as.formula(fixed),
                                 data = noise_free(d, "timecourse",
                                                   effect)))[cf]
  na3 <- function(e) matrix(NA_real_, 3, 2)
  iv <- list(wald_simple = wald_ci(m_s, cf), wald_full = wald_ci(m_f, cf))
  iv$wald_complete <- tryCatch(wald_ci(fit(
    "(1 + condition + condition:ns(time, 3) | participant)"), cf),
    error = na3)
  iv$satt_simple <- tryCatch(satt_ci(m_s, cf), error = na3)
  iv$satt_full <- tryCatch(satt_ci(m_f, cf), error = na3)
  err <- NA_character_
  t0 <- proc.time()[["elapsed"]]
  bt <- tryCatch(w$quiet(boot_ci(m_s, "participant", resample = "case",
                                 ci = "bca", terms = cf, B = B, seed = seed)),
                 error = function(e) { err <<- conditionMessage(e); NULL })
  secs <- proc.time()[["elapsed"]] - t0
  for (type in c("expanded", "percentile", "bca", "basic")) {
    iv[[paste0("case_", type)]] <- if (is.null(bt)) na3() else
      tryCatch(confint(bt, type = type)[cf, , drop = FALSE], error = na3)
  }
  out <- do.call(rbind, lapply(names(iv), function(m) {
    data.frame(design = "timecourse", variant = variant, effect = effect,
               rep = i, method = m, coef = cf, lower = unname(iv[[m]][, 1]),
               upper = unname(iv[[m]][, 2]), truth = unname(truth),
               row.names = NULL)
  }))
  out$error <- err
  out$seconds_boot <- secs
  list(intervals = out)
}

# ---- Part "bootstrap"
TRUE_EFFECT_WITHIN <- 0.3
interval_types <- c("expanded", "percentile", "bca", "basic")

boot_intervals_of <- function(w, m, cluster, resample, cf, B, seed,
                              strata = NULL) {
  err <- NA_character_
  bt <- tryCatch(w$quiet(boot_ci(m, cluster, resample = resample, ci = "bca",
                                 strata = strata, terms = cf, B = B,
                                 seed = seed)),
                 error = function(e) { err <<- conditionMessage(e); NULL })
  iv <- lapply(interval_types, function(type) {
    if (is.null(bt)) return(matrix(NA_real_, length(cf), 2))
    tryCatch(confint(bt, type = type)[cf, , drop = FALSE],
             error = function(e) matrix(NA_real_, length(cf), 2))
  })
  names(iv) <- interval_types
  list(iv = iv, error = err)
}
interval_rows <- function(iv, cf, truth, errors, ...) {
  do.call(rbind, lapply(names(iv), function(m) {
    data.frame(..., method = m, coef = cf, lower = unname(iv[[m]][, 1]),
               upper = unname(iv[[m]][, 2]), truth = unname(truth),
               error = errors[[m]] %||% NA_character_, row.names = NULL)
  }))
}

rep_boot_within <- function(i, seed, n, re, B) {
  w <- worker_setup()
  set.seed(seed)
  draw <- function(sd) {
    if (re == "normal") stats::rnorm(n, 0, sd) else
      (stats::rchisq(n, 2) - 2) / 2 * sd
  }
  u0 <- draw(0.5); u1 <- draw(0.3)
  n_tr <- 40
  pid <- rep(seq_len(n), each = n_tr)
  isB <- rep(rep(0:1, each = n_tr / 2), n)
  y <- u0[pid] + (TRUE_EFFECT_WITHIN + u1[pid]) * isB +
    stats::rt(length(pid), df = 5) / sqrt(5 / 3)
  d <- data.frame(participant = factor(pid),
                  condition = factor(ifelse(isB == 1, "B", "A")), y = y)
  m <- w$quiet(lmer(y ~ condition + (1 + condition | participant), data = d,
                    control = w$ctrl))
  cf <- "conditionB"
  iv <- list(wald = wald_ci(m, cf))
  iv$satterthwaite <- tryCatch(satt_ci(m, cf),
                               error = function(e) matrix(NA_real_, 1, 2))
  errors <- list()
  for (rs in c("case", "wild")) {
    b <- boot_intervals_of(w, m, "participant", rs, cf, B, seed)
    for (type in interval_types) {
      iv[[paste(rs, type, sep = "_")]] <- b$iv[[type]]
      errors[[paste(rs, type, sep = "_")]] <- b$error
    }
  }
  list(intervals = interval_rows(iv, cf, TRUE_EFFECT_WITHIN, errors,
                                 design = "within", n = n, re = re, rep = i))
}

rep_boot_groups <- function(i, seed, variant, effect, B) {
  w <- worker_setup()
  d <- make_data(seed, "mixed", variant, effect)
  tr <- stats::aggregate(y ~ participant + group + trial + condition, d, mean)
  m <- w$quiet(lmer(y ~ group * condition + (1 + condition | participant),
                    data = tr, control = w$ctrl))
  cf <- "grouptreatment:conditionB"
  iv <- list(wald = wald_ci(m, cf))
  iv$satterthwaite <- tryCatch(satt_ci(m, cf),
                               error = function(e) matrix(NA_real_, 1, 2))
  errors <- list()
  runs <- list(case_strata = list("case", "group"), case = list("case", NULL),
               wild = list("wild", NULL))
  for (nm in names(runs)) {
    b <- boot_intervals_of(w, m, "participant", runs[[nm]][[1]], cf, B, seed,
                           strata = runs[[nm]][[2]])
    for (type in interval_types) {
      iv[[paste(nm, type, sep = "_")]] <- b$iv[[type]]
      errors[[paste(nm, type, sep = "_")]] <- b$error
    }
  }
  list(intervals = interval_rows(iv, cf, effect, errors, design = "mixed",
                                 variant = variant, effect = effect, rep = i))
}

# ---- Part "gca"
GCA_SCENARIOS <- c(null = 0, level = 0.03, shape = 0.025, onset = 0.03)
GCA_TERMS <- c(level = "condition", linear = "ot1:condition",
               quadratic = "ot2:condition", cubic = "ot3:condition")
GCA_COEFS <- c(level = "conditionB", linear = "ot1:conditionB",
               quadratic = "ot2:conditionB", cubic = "ot3:conditionB")

gca_data <- function(seed, scenario, effect) {
  set.seed(seed)
  d <- sim_blocks("within", n_participants = 30,
                  n_trials = sample(TRIAL_COUNTS, 30, replace = TRUE),
                  n_time = 20, sampling_rate = 10, prop_condition = 0.2,
                  effect_condition = if (scenario %in% c("level", "onset"))
                    effect else 0,
                  effect_onset = if (scenario == "onset") 0.5 else 0,
                  sd_participant = SD$participant, sd_slope = SD$slope,
                  sd_trial = SD$trial, sd_residual = SD$residual, ar1 = 0.92,
                  residual_df = 5, seed = seed)
  times <- sort(unique(d$time))
  ti <- match(d$time, times)
  rc <- lmmresample:::response_curve(length(times))
  shape <- rc - mean(rc)
  isB <- d$condition == "B"
  if (scenario == "shape") d$y[isB] <- d$y[isB] + effect * shape[ti[isB]]
  d$y <- d$y - stats::ave(d$y, d$time)
  P <- stats::poly(d$time, 3)
  d$ot1 <- P[, 1]; d$ot2 <- P[, 2]; d$ot3 <- P[, 3]
  d$P <- unclass(P)[, 1:3]
  diff <- switch(scenario, null = 0 * shape, level = 0 * shape + effect,
                 shape = effect * shape,
                 onset = effect * lmmresample:::effect_weight(times, 0.5))
  d0 <- d
  d0$y <- rc[ti] + isB * diff[ti]
  truth <- stats::coef(stats::lm(y ~ (ot1 + ot2 + ot3) * condition,
                                 data = d0))[GCA_COEFS]
  truth[abs(truth) < 1e-10] <- 0
  list(d = d, truth = truth)
}

rep_gca <- function(i, seed, scenario, effect, B, B_boot) {
  w <- worker_setup()
  g <- gca_data(seed, scenario, effect)
  d <- g$d
  fit <- function(f) w$quiet(lmer(f, data = d, control = w$ctrl))
  m_gca <- fit(y ~ (ot1 + ot2 + ot3) * condition +
                 (ot1 + ot2 + ot3 | participant) + (1 | participant:trial))
  m_s <- fit(y ~ (ot1 + ot2 + ot3) * condition + (1 | participant))
  m_ls <- fit(y ~ condition * P + (1 | participant))
  signflip <- exch_signflip("participant")
  id <- list(scenario = scenario, effect = effect, rep = i)

  t0 <- proc.time()[["elapsed"]]
  specs <- lapply(GCA_TERMS, function(tm) perm_spec(m_s, tm, data = d))
  fam <- tryCatch(w$quiet(do.call(perm_maxt, c(specs, list(
    unit = "trial", exchange = signflip, B = B, seed = seed)))),
    error = function(e) conditionMessage(e))
  t_fam <- proc.time()[["elapsed"]] - t0
  co <- t(vapply(split(d, d$participant), function(x) {
    f <- tryCatch(stats::coef(stats::lm(y ~ (ot1 + ot2 + ot3) * condition,
                                        data = x))[GCA_COEFS],
                  error = function(e) rep(NA_real_, 4))
    if (length(f) != 4) rep(NA_real_, 4) else unname(f)
  }, numeric(4)))
  p_two <- apply(co, 2, function(v) {
    v <- v[is.finite(v)]
    if (length(v) > 2) stats::t.test(v)$p.value else NA_real_
  })
  p_wgca <- vapply(GCA_COEFS, function(cc) {
    tryCatch(wald_p(m_gca, cc), error = function(e) NA_real_)
  }, numeric(1))
  p_ws <- vapply(GCA_COEFS, function(cc) wald_p(m_s, cc), numeric(1))
  p_perm <- if (is.character(fam)) rep(NA_real_, 4) else fam$results$p.value
  tests <- do.call(rbind, lapply(seq_along(GCA_TERMS), function(k) {
    data.frame(id, term = names(GCA_TERMS)[k],
               method = c("wald_gca", "wald_simple", "perm_simple",
                          "two_stage"),
               p = c(p_wgca[k], p_ws[k], p_perm[k], p_two[k]),
               seconds = c(NA, NA, t_fam / 4, NA),
               error = c(NA, NA, if (is.character(fam)) fam else NA, NA),
               truth = unname(g$truth[k]), row.names = NULL)
  }))

  family <- NULL
  if (!is.character(fam)) {
    family <- family_rows(fam, list(wald_gca = unname(p_wgca),
                                    two_stage = unname(p_two)),
                          t_fam, id)$family
    family$truth <- unname(g$truth[match(family$feature, names(GCA_TERMS))])
    family$family <- "four_terms"
  }
  t0 <- proc.time()[["elapsed"]]
  ls <- tryCatch(w$quiet(perm_maxt(
    level = perm_spec(m_ls, "condition", data = d),
    shape = perm_spec(m_ls, "condition:P", data = d),
    unit = "trial", exchange = signflip, B = B, seed = seed)),
    error = function(e) conditionMessage(e))
  t_ls <- proc.time()[["elapsed"]] - t0
  if (!is.character(ls)) {
    wl <- c(level = tryCatch(wald_p(m_gca, GCA_COEFS[1]),
                             error = function(e) NA_real_),
            shape = tryCatch(wald_p(m_gca, GCA_COEFS[2:4]),
                             error = function(e) NA_real_))
    f2 <- family_rows(ls, list(wald_gca = unname(wl)), t_ls, id)$family
    f2$truth <- ifelse(f2$feature == "level", g$truth[[1]],
                       max(abs(g$truth[2:4])))
    f2$family <- "level_shape"
    family <- rbind(family, f2)
  }
  errors <- NULL
  if (is.character(fam)) {
    errors <- data.frame(rep = i, error = paste("four terms:", fam))
  }
  if (is.character(ls)) {
    errors <- rbind(errors, data.frame(rep = i,
                                       error = paste("level and shape:", ls)))
  }

  iv <- list(wald_gca = tryCatch(wald_ci(m_gca, GCA_COEFS),
                                 error = function(e) matrix(NA_real_, 4, 2)),
             wald_simple = wald_ci(m_s, GCA_COEFS))
  b <- boot_intervals_of(w, m_s, "participant", "case", unname(GCA_COEFS),
                         B_boot, seed)
  for (type in interval_types) iv[[paste0("case_", type)]] <- b$iv[[type]]
  intervals <- interval_rows(iv, unname(GCA_COEFS), g$truth,
                             list(), scenario = scenario, effect = effect,
                             rep = i)
  intervals$term <- names(GCA_COEFS)[match(intervals$coef, GCA_COEFS)]
  intervals$error <- ifelse(grepl("^case_", intervals$method), b$error, NA)
  list(tests = tests, family = family, intervals = intervals, errors = errors)
}

rep_gca_mirman <- function(i, seed, scenario, effect) {
  w <- worker_setup()
  g <- gca_data(seed, scenario, effect)
  d <- g$d
  id <- list(scenario = scenario, effect = effect, rep = i)
  t0 <- proc.time()[["elapsed"]]
  m <- tryCatch(w$quiet(lmer(y ~ (ot1 + ot2 + ot3) * condition +
                               (ot1 + ot2 + ot3 | participant) +
                               (ot1 + ot2 + ot3 | participant:condition),
                             data = d, control = w$ctrl)),
                error = function(e) conditionMessage(e))
  secs <- proc.time()[["elapsed"]] - t0
  if (is.character(m)) {
    return(list(errors = data.frame(rep = i, error = paste("Mirman model:",
                                                           m))))
  }
  p <- unname(vapply(GCA_COEFS, function(cc) {
    tryCatch(wald_p(m, cc), error = function(e) NA_real_)
  }, numeric(1)))
  truth <- unname(g$truth)
  tests <- data.frame(id, term = names(GCA_TERMS), method = "wald_mirman",
                      p = p, seconds = secs, error = NA_character_,
                      truth = truth, singular = isSingular(m),
                      row.names = NULL)
  p_ls <- c(p[1], tryCatch(wald_p(m, GCA_COEFS[2:4]),
                           error = function(e) NA_real_))
  family <- rbind(
    data.frame(id, method = rep(c("wald_mirman_raw", "wald_mirman_holm"),
                                each = 4),
               feature = names(GCA_TERMS), p = c(p, stats::p.adjust(p, "holm")),
               truth = rep(truth, 2), family = "four_terms",
               row.names = NULL),
    data.frame(id, method = rep(c("wald_mirman_raw", "wald_mirman_holm"),
                                each = 2),
               feature = c("level", "shape"),
               p = c(p_ls, stats::p.adjust(p_ls, "holm")),
               truth = rep(c(truth[1], max(abs(truth[2:4]))), 2),
               family = "level_shape", row.names = NULL))
  iv <- tryCatch(wald_ci(m, GCA_COEFS),
                 error = function(e) matrix(NA_real_, 4, 2))
  intervals <- data.frame(id, method = "wald_mirman", coef = unname(GCA_COEFS),
                          lower = unname(iv[, 1]), upper = unname(iv[, 2]),
                          truth = truth, error = NA_character_,
                          term = names(GCA_COEFS), row.names = NULL)
  list(tests = tests, family = family, intervals = intervals)
}

# ---- Part "families"
FAMILY_CELLS <- expand.grid(k = c(4, 10), rho = c(0, 0.5, 0.8))
EFFECT_FAMILY <- 0.2

rep_family <- function(i, seed, k, rho, effect, B) {
  w <- worker_setup()
  set.seed(seed)
  n <- 30
  n_tr <- sample(TRIAL_COUNTS, n, replace = TRUE)
  S <- matrix(rho, k, k); diag(S) <- 1
  L <- chol(S)
  mvn <- function(m, sd) matrix(stats::rnorm(m * k), m, k) %*% L * sd
  u0 <- mvn(n, 0.5)
  u1 <- mvn(n, 0.3)
  pid <- rep(seq_len(n), n_tr)
  cond <- unlist(lapply(n_tr, function(m) {
    x <- rep("A", m); x[sample(m, max(1, round(0.2 * m)))] <- "B"; x
  }))
  isB <- as.numeric(cond == "B")
  e <- matrix(stats::rt(length(pid) * k, df = 5) / sqrt(5 / 3),
              length(pid), k) %*% L
  beta <- c(effect, rep(0, k - 1))
  Y <- u0[pid, ] + sweep(u1[pid, ], 2, beta, "+") * isB + e
  d <- data.frame(participant = factor(pid), trial = factor(seq_along(pid)),
                  condition = factor(cond, levels = c("A", "B")))
  for (j in seq_len(k)) d[[paste0("f", j)]] <- Y[, j]
  specs <- lapply(seq_len(k), function(j) {
    m <- w$quiet(lmer(stats::as.formula(paste0(
      "f", j, " ~ condition + (1 | participant)")), data = d,
      control = w$ctrl))
    perm_spec(m, "condition", data = d)
  })
  names(specs) <- paste0("f", seq_len(k))
  p_wald <- vapply(seq_len(k), function(j) {
    m <- w$quiet(lmer(stats::as.formula(paste0(
      "f", j, " ~ condition + (1 + condition | participant)")), data = d,
      control = w$ctrl))
    tryCatch(wald_p(m, "conditionB"), error = function(e) NA_real_)
  }, numeric(1))
  t0 <- proc.time()[["elapsed"]]
  r <- w$quiet(do.call(perm_maxt, c(specs, list(
    unit = "trial", exchange = exch_signflip("participant"), B = B,
    seed = seed))))
  out <- family_rows(r, list(wald = p_wald), proc.time()[["elapsed"]] - t0,
                     k = k, rho = rho, effect = effect, rep = i)$family
  pb <- out[out$method == "perm_raw", ]
  pb$method <- "perm_bonferroni"
  pb$p <- stats::p.adjust(pb$p, "bonferroni")
  out <- rbind(out, pb)
  out$truth <- ifelse(out$feature == "f1", effect, 0)
  list(family = out)
}

# ---- Part "diagnostics"
ACF_CELLS <- expand.grid(ar1 = c(0.5, 0.8, 0.92, 0.975), n_time = c(20, 40, 67))
LOSO_SIZES <- c(0, 3, 6)
TC_CELLS <- expand.grid(scenario = c("correct", "misspecified"),
                        effect = c(0.04, 0.08), stringsAsFactors = FALSE)
TC_THRESHOLD <- stats::qnorm(1 - 0.025 / 40)

diag_base <- function(seed, ...) {
  utils::modifyList(list(
    design = "within", n_participants = 30,
    n_trials = sample(TRIAL_COUNTS, 30, replace = TRUE), sampling_rate = 10,
    prop_condition = 0.2, sd_participant = SD$participant,
    sd_slope = SD$slope, sd_trial = SD$trial, sd_residual = SD$residual,
    residual_df = 5, seed = seed), list(...))
}

rep_diag_acf <- function(i, seed, ar1, n_time) {
  w <- worker_setup()
  set.seed(seed)
  d <- do.call(sim_blocks, diag_base(seed, n_time = n_time, ar1 = ar1))
  d$time_f <- factor(d$time)
  m <- w$quiet(lmer(y ~ condition + time_f + (1 + condition | participant),
                    data = d, control = w$ctrl))
  a <- diag_acf(m, "trial", "time")
  list(diag = data.frame(check = "acf", rep = i, ar1 = ar1, n_time = n_time,
                         estimate = a$phi, estimate_raw = a$phi_raw))
}

rep_diag_calibrate <- function(i, seed, n_sim, B) {
  w <- worker_setup()
  set.seed(seed)
  d <- do.call(sim_blocks, diag_base(seed, n_participants = 15,
                                     n_trials = 20, n_time = 10, ar1 = 0.9,
                                     sd_slope = 0))
  d$sample <- factor(seq_len(nrow(d)))
  d$time_f <- factor(d$time)
  m <- w$quiet(lmer(y ~ condition + time_f + (1 | participant), data = d,
                    control = w$ctrl))
  ac <- diag_acf(m, "trial", "time")
  schemes <- list(
    trials = list(unit = "trial", exchange = exch_signflip("participant")),
    samples = list(unit = "sample", exchange = exch_within("participant")))
  out <- list()
  for (s in names(schemes)) {
    sc <- schemes[[s]]
    observed <- w$quiet(perm_test(m, "condition", sc$unit, sc$exchange,
                                  B = B, seed = seed))$p.value
    cal <- w$quiet(perm_calibrate(m, "condition", sc$unit, sc$exchange,
                                  ar1 = ac, series = "trial", time = "time",
                                  n_sim = n_sim, B = B, seed = seed))
    out[[s]] <- data.frame(
      check = "calibrate", rep = i, test = c(paste0("perm_", s), "wald"),
      observed_p = c(observed, wald_p(m, "conditionB")),
      predicted_rate = c(mean(cal$p_perm <= 0.05, na.rm = TRUE),
                         mean(cal$p_wald <= 0.05, na.rm = TRUE)))
  }
  res <- do.call(rbind, out)
  list(diag = res[!duplicated(res$test), ])
}

rep_diag_timecourse <- function(i, seed, scenario, effect) {
  w <- worker_setup()
  set.seed(seed)
  d <- do.call(sim_blocks, diag_base(seed, n_time = 20, ar1 = 0.92,
                                     effect_condition = effect,
                                     effect_onset = 0.5))
  d$time_f <- factor(d$time)
  f <- if (scenario == "correct") {
    y ~ condition * time_f + (1 + condition | participant)
  } else {
    y ~ condition + time_f + (1 + condition | participant)
  }
  m <- w$quiet(lmer(f, data = d, control = w$ctrl))
  tc <- diag_timecourse(m, "time", by = "condition",
                        cluster = "participant")$timecourse
  z <- abs(tc$mean / tc$se)
  list(diag = data.frame(check = "timecourse", rep = i, scenario = scenario,
                         effect = effect, max_z = max(z, na.rm = TRUE),
                         flagged = max(z, na.rm = TRUE) > TC_THRESHOLD))
}

rep_diag_loso <- function(i, seed, size) {
  w <- worker_setup()
  set.seed(seed)
  d <- do.call(sim_blocks, diag_base(seed, n_participants = 20,
                                     n_trials = 40, level = "trial",
                                     n_time = 20, ar1 = 0.92,
                                     effect_condition = 0.03))
  fresh_rng(seed)
  bad <- as.character(sample(unique(d$participant), 1))
  hit <- d$participant == bad & d$condition == "B"
  d$y[hit] <- d$y[hit] + size * SD$slope
  m <- w$quiet(lmer(y ~ condition + (1 + condition | participant), data = d,
                    control = w$ctrl))
  lo <- diag_loso(m, "participant", terms = "conditionB")
  sc <- abs(lo$std_change[, "conditionB"])
  list(diag = data.frame(check = "loso", rep = i, size = size,
                         rank_of_aberrant = rank(-sc)[[bad]],
                         max_std_change = max(sc, na.rm = TRUE)))
}

rep_diag_agreement <- function(i, seed, scenario) {
  w <- worker_setup()
  set.seed(seed)
  d <- do.call(sim_blocks, diag_base(seed, level = "trial", n_time = 20,
                                     ar1 = 0.92, effect_condition = 0.03))
  fresh_rng(seed)
  delta <- if (scenario == "confound") -0.03 else 0
  d$baseline <- delta * (d$condition == "B") +
    stats::rnorm(nrow(d), 0, SD$baseline)
  d$y <- d$y - 0.44 * d$baseline
  m <- w$quiet(lmer(y ~ condition + baseline + (1 + condition | participant),
                    data = d, control = w$ctrl))
  ag <- diag_agreement(m, "condition", "participant")
  list(diag = data.frame(check = "agreement", rep = i, scenario = scenario,
                         flagged = ag$flagged, relative = ag$relative))
}

# ---- Part "posthoc"
POSTHOC_LEVELS <- c("A", "B", "C", "D")

posthoc_data <- function(seed, design, effect) {
  timecourse <- design == "timecourse"
  set.seed(seed)
  k <- sample(TRIAL_COUNTS, 30, replace = TRUE)
  d <- sim_blocks("within", n_participants = 30, n_trials = k, n_time = 20,
                  sampling_rate = 10, prop_condition = 0.2,
                  sd_participant = SD$participant, sd_slope = 0, sd_trial = SD$trial,
                  sd_residual = SD$residual, ar1 = 0.92, residual_df = 5,
                  seed = seed)
  d$condition <- NULL
  fresh_rng(seed)
  tr <- unique(d[c("participant", "trial")])
  tr$stim <- factor(sample(POSTHOC_LEVELS, nrow(tr), replace = TRUE,
                           prob = c(0.4, 0.2, 0.2, 0.2)),
                    levels = POSTHOC_LEVELS)
  d$stim <- tr$stim[match(d$trial, tr$trial)]
  n <- nlevels(d$participant)
  pid <- as.integer(d$participant)
  lvl <- as.integer(d$stim)
  u <- matrix(stats::rnorm(n * 3, 0, SD$slope), n, 3)
  dev <- ifelse(lvl == 1, 0, u[cbind(pid, pmax(lvl - 1, 1))])
  w <- lmmresample:::effect_weight(d$time, if (timecourse) 0.5 else 0)
  d$y <- d$y + (dev + effect * (d$stim == "D")) * w
  d$y <- d$y - stats::ave(d$y, d$time)
  if (!timecourse) {
    d <- stats::aggregate(y ~ participant + trial + stim, d, mean)
  }
  d
}

posthoc_contrasts <- function(m, pairs, timecourse) {
  nm <- names(fixef(m))
  coef_of <- function(l) {
    if (l == POSTHOC_LEVELS[1]) return(NULL)
    if (timecourse) grep(paste0("^stim", l, ":ns"), nm, value = TRUE) else
      paste0("stim", l)
  }
  q <- if (timecourse) 3 else 1
  lapply(seq_len(nrow(pairs)), function(i) {
    L <- matrix(0, q, length(nm), dimnames = list(NULL, nm))
    cb <- coef_of(pairs[i, 2]); ca <- coef_of(pairs[i, 1])
    if (!is.null(cb)) L[cbind(seq_len(q), match(cb, nm))] <- 1
    if (!is.null(ca)) L[cbind(seq_len(q), match(ca, nm))] <- -1
    L
  })
}
contrast_wald <- function(m, L) {
  b <- fixef(m); V <- as.matrix(vcov(m))
  e <- as.vector(L %*% b)
  S <- L %*% V %*% t(L)
  list(p = stats::pchisq(as.numeric(crossprod(e, solve(S, e))), nrow(L),
                         lower.tail = FALSE),
       est = e[1], se = sqrt(S[1, 1]))
}

rep_posthoc <- function(i, seed, design, effect, B) {
  w <- worker_setup()
  timecourse <- design == "timecourse"
  d <- posthoc_data(seed, design, effect)
  fixed <- if (timecourse) "y ~ stim * ns(time, 3)" else "y ~ stim"
  term <- if (timecourse) "stim:ns(time, 3)" else "stim"
  fit <- function(re) w$quiet(lmer(stats::as.formula(paste(fixed, "+", re)),
                                   data = d, control = w$ctrl))
  m_s <- fit("(1 | participant)")
  m_f <- fit("(1 + stim | participant)")
  pairs <- t(utils::combn(POSTHOC_LEVELS, 2))
  comparisons <- paste(pairs[, 2], "-", pairs[, 1])
  truth <- ifelse(pairs[, 2] == "D" | pairs[, 1] == "D",
                  ifelse(pairs[, 2] == "D", effect, -effect), 0)
  signflip <- exch_signflip("participant")
  rows <- list(); errors <- NULL; intervals <- NULL
  add <- function(method, p) {
    rows[[length(rows) + 1]] <<- data.frame(
      design = design, effect = effect, rep = i, method = method,
      feature = comparisons, p = p, truth = truth)
  }
  for (mod in if (timecourse) "simple" else c("simple", "full")) {
    m <- if (mod == "simple") m_s else m_f
    pw <- tryCatch(w$quiet(perm_pairwise(
      m, "stim", term = term, unit = "trial", exchange = signflip,
      B = B, seed = seed, data = d,
      cluster = if (!timecourse && mod == "simple") "participant")),
      error = function(e) conditionMessage(e))
    if (is.character(pw)) {
      errors <- rbind(errors, data.frame(rep = i, error = paste(mod, pw)))
      next
    }
    add(paste0("perm_", mod, "_raw"), pw$results$p.value)
    add(paste0("perm_", mod, "_wy"), pw$results$p.adjusted)
    add(paste0("perm_", mod, "_holm"),
        stats::p.adjust(pw$results$p.value, "holm"))
    if (!is.null(pw$results$conf.low)) {
      intervals <- rbind(intervals, data.frame(
        design = design, effect = effect, rep = i, method = "case_expanded",
        feature = comparisons, lower = pw$results$conf.low,
        upper = pw$results$conf.high, truth = truth))
    }
  }
  Ls <- posthoc_contrasts(m_f, pairs, timecourse)
  wald <- lapply(Ls, function(L) tryCatch(contrast_wald(m_f, L),
                                          error = function(e) NULL))
  p_w <- vapply(wald, function(x) if (is.null(x)) NA_real_ else x$p,
                numeric(1))
  add("wald_full_raw", p_w)
  add("wald_full_holm", stats::p.adjust(p_w, "holm"))
  Ls_s <- posthoc_contrasts(m_s, pairs, timecourse)
  p_ws <- vapply(Ls_s, function(L) tryCatch(contrast_wald(m_s, L)$p,
                                            error = function(e) NA_real_),
                 numeric(1))
  add("wald_simple_holm", stats::p.adjust(p_ws, "holm"))
  mt <- tryCatch(w$quiet(lmerTest::as_lmerModLmerTest(m_f)),
                 error = function(e) NULL)
  satt <- lapply(Ls, function(L) {
    if (is.null(mt)) return(NULL)
    tryCatch(w$quiet(lmerTest::contest(mt, L, ddf = "Satterthwaite",
                                       joint = TRUE, confint = FALSE)),
             error = function(e) NULL)
  })
  p_s <- vapply(satt, function(x) if (is.null(x)) NA_real_ else
    x[["Pr(>F)"]][1], numeric(1))
  add("satt_full_holm", stats::p.adjust(p_s, "holm"))
  if (!timecourse) {
    ci_s <- t(vapply(Ls, function(L) {
      if (is.null(mt)) return(c(NA_real_, NA_real_))
      r <- tryCatch(w$quiet(lmerTest::contest(mt, L, ddf = "Satterthwaite",
                                              joint = FALSE, confint = TRUE)),
                    error = function(e) NULL)
      if (is.null(r)) c(NA_real_, NA_real_) else c(r$lower[1], r$upper[1])
    }, numeric(2)))
    est <- vapply(wald, function(x) if (is.null(x)) NA_real_ else x$est,
                  numeric(1))
    se <- vapply(wald, function(x) if (is.null(x)) NA_real_ else x$se,
                 numeric(1))
    intervals <- rbind(intervals,
      data.frame(design = design, effect = effect, rep = i,
                 method = "wald_full", feature = comparisons,
                 lower = est - 1.96 * se, upper = est + 1.96 * se,
                 truth = truth),
      data.frame(design = design, effect = effect, rep = i,
                 method = "satt_full", feature = comparisons,
                 lower = ci_s[, 1], upper = ci_s[, 2], truth = truth))
  }
  list(family = do.call(rbind, rows), intervals = intervals, errors = errors)
}

run_example <- function(dir, B, n_sim, B_cal) {
  dir.create(dir, showWarnings = FALSE)
  step <- function(file, expr) {
    f <- file.path(dir, paste0(file, ".rds"))
    if (file.exists(f)) return(readRDS(f))
    t0 <- Sys.time()
    x <- expr
    saveRDS(x, f)
    log_msg(sprintf("example: %-24s %s", file, format(round(difftime(
      Sys.time(), t0, units = "mins"), 1))))
    x
  }
  set.seed(2026)
  d <- step("data", sim_blocks(
    "within", n_participants = 30,
    n_trials = sample(TRIAL_COUNTS, 30, replace = TRUE), n_time = 20,
    sampling_rate = 10, prop_condition = 0.2, effect_condition = 0.04,
    effect_onset = 0.5, sd_participant = SD$participant, sd_slope = SD$slope,
    sd_trial = SD$trial, sd_residual = SD$residual, ar1 = 0.92,
    residual_df = 5, seed = 2026))
  utils::write.csv(d, file.path(dir, "example_data.csv"), row.names = FALSE)
  ctrl <- lmerControl(calc.derivs = FALSE)
  d$time_f <- factor(d$time)
  m <- step("model", lmer(y ~ condition + time_f +
                            (1 + condition | participant),
                          data = d, REML = FALSE, control = ctrl))
  m_linear <- step("model_linear_time", lmer(
    y ~ condition + time + (1 + condition | participant), data = d,
    REML = FALSE, control = ctrl))
  m_tc <- step("model_timecourse", lmer(
    y ~ condition * ns(time, 6) + (1 + condition | participant), data = d,
    REML = FALSE, control = ctrl))
  ac <- step("diag_acf", diag_acf(m_tc, "trial", "time"))
  step("diag_timecourse_linear", diag_timecourse(
    m_linear, "time", by = "condition", cluster = "participant"))
  step("diag_timecourse", diag_timecourse(m, "time", by = "condition",
                                          cluster = "participant"))
  step("diag_timecourse_spline", diag_timecourse(
    m_tc, "time", by = "condition", cluster = "participant"))
  step("diag_agreement", diag_agreement(m, "condition", "participant"))
  step("diag_loso", diag_loso(m, "participant", terms = "conditionB"))
  step("perm_test", perm_test(m, "condition", "trial",
                              exch_signflip("participant"), B = B, seed = 1))
  step("perm_test_timecourse", perm_test(
    m_tc, "condition:ns(time, 6)", "trial", exch_signflip("participant"),
    B = B, seed = 1))
  tr <- stats::aggregate(y ~ participant + trial + condition, d, mean)
  names(tr)[names(tr) == "y"] <- "mean"
  tr$peak <- stats::aggregate(y ~ participant + trial + condition, d, max)$y
  tr$late <- stats::aggregate(y ~ participant + trial + condition,
                              d[d$time >= 1, ], mean)$y
  feat <- lapply(c("mean", "peak", "late"), function(v) {
    f <- stats::as.formula(paste(v, "~ condition + (1 + condition | participant)"))
    perm_spec(lmer(f, data = tr, control = ctrl), "condition", data = tr)
  })
  names(feat) <- c("Mean", "Peak", "Late window")
  step("perm_maxt", do.call(perm_maxt, c(feat, list(
    unit = "trial", exchange = exch_signflip("participant"),
    combine = "max-t", B = B, seed = 1))))
  step("perm_minp", do.call(perm_maxt, c(feat, list(
    unit = "trial", exchange = exch_signflip("participant"),
    combine = "min-p", B = B, seed = 1))))
  step("boot_case", boot_ci(m, "participant", resample = "case", ci = "bca",
                            terms = "conditionB", B = B, seed = 1))
  step("boot_wild", boot_ci(m, "participant", resample = "wild", ci = "bca",
                            terms = "conditionB", B = B, seed = 1))
  step("perm_calibrate", perm_calibrate(
    m, "condition", "trial", exch_signflip("participant"), ar1 = ac,
    series = "trial", time = "time", n_sim = n_sim, B = B_cal, seed = 1))
  invisible(TRUE)
}

eff_tag <- function(effect) sprintf("e%03d", round(1000 * effect))
null_n <- function(variant, base_n = N$null_base) {
  if (variant == "base") base_n else N$null
}
size_factor <- c(base = 1, n12 = 0.45, n60 = 2, ar05 = 1, normal = 1,
                 balanced = 0.9, sparse = 0.7, skewed = 1, trial_ar = 1,
                 unbalanced = 1, gaps_interp = 1, gaps_removed = 1,
                 ar_hetero = 1, real_res = 3.5)

SINGLE <- list(
  within      = list(variants = c("base", "n12", "n60", "ar05", "normal",
                                  "balanced", "sparse", "skewed", "trial_ar"),
                     effects = c(0.015, 0.03, 0.045), cost = 200),
  between     = list(variants = c("base", "n12", "n60", "ar05", "normal",
                                  "balanced", "skewed", "trial_ar",
                                  "unbalanced"),
                     effects = c(0.02, 0.04, 0.06), cost = 60),
  mixed       = list(variants = c("base", "n12", "n60", "ar05", "normal",
                                  "balanced", "skewed", "trial_ar",
                                  "unbalanced"),
                     effects = c(0.025, 0.05, 0.075), cost = 200),
  covariate   = list(variants = c("base", "n12", "skewed", "trial_ar"),
                     effects = c(0.015, 0.03, 0.045), cost = 200),
  biomarker   = list(variants = c("base", "n12"),
                     effects = c(0.015, 0.03), cost = 250),
  three_level = list(variants = "base", effects = c(0.015, 0.03), cost = 230)
)
TC_VARIANTS <- c("base", "n12", "n60", "ar05", "normal", "balanced", "sparse",
                 "skewed", "trial_ar", "gaps_interp", "gaps_removed",
                 "ar_hetero", "real_res")
TC_EFFECTS <- c(0.02, 0.04, 0.06)
TRIPLE_VARIANTS <- c("base", "unbalanced")
TRIPLE_EFFECTS <- c(0.04, 0.08)
BOOT_GROUP_VARIANTS <- c("base", "unbalanced", "n12")
EFFECT_GROUPS_BOOT <- 0.05

if ("single" %in% RUN) {
  for (ds in names(SINGLE)) {
    s <- SINGLE[[ds]]
    for (vr in s$variants) {
      effects <- if (vr == "base") c(0, s$effects) else
        c(0, s$effects[ceiling(length(s$effects) / 2)])
      for (ef in effects) {
        n0 <- if (ds == "within" && vr == "base") N$null_within else
          null_n(vr)
        add_job(sprintf("single_%s_%s_%s", ds, vr, eff_tag(ef)), "single",
                if (ef == 0) n0 else N$effect, s$cost * size_factor[[vr]],
                if (vr == "base") (if (ef == 0) 1 else 2) else 6,
                design = ds, variant = vr, effect = ef, B = B$perm)
      }
    }
  }
}
if ("timecourse" %in% RUN) {
  for (vr in TC_VARIANTS) {
    effects <- if (vr == "base") c(0, TC_EFFECTS) else c(0, TC_EFFECTS[2])
    for (ef in effects) {
      add_job(sprintf("tc_%s_%s", vr, eff_tag(ef)), "timecourse",
              if (ef == 0) null_n(vr) else N$effect,
              (200 + 150 * (vr == "base")) * size_factor[[vr]],
              if (vr == "base") (if (ef == 0) 1 else 2) else 5,
              variant = vr, effect = ef, B = B$perm, B_cluster = B$cluster,
              full_perm = vr == "base")
    }
  }
  ef <- TC_EFFECTS[2]
  add_job(sprintf("tcboot_base_%s", eff_tag(ef)), "tc_boot", N$boot_tc, 1000,
          4, .data_key = sprintf("tc_base_%s", eff_tag(ef)),
          variant = "base", effect = ef, B = B$boot_tc)
}
if ("triple" %in% RUN) {
  for (vr in TRIPLE_VARIANTS) {
    for (ef in c(0, TRIPLE_EFFECTS)) {
      key <- sprintf("triple_%s_%s", vr, eff_tag(ef))
      add_job(key, "triple", if (ef == 0) N$null else N$effect, 500, 3,
              variant = vr, effect = ef, B = B$perm, B_cluster = B$cluster)
    }
    for (ef in c(0, max(TRIPLE_EFFECTS))) {
      add_job(sprintf("triplefamily_%s_%s", vr, eff_tag(ef)), "triple_family",
              if (ef == 0) N$family_null else N$family_effect, 80, 4,
              .data_key = sprintf("triple_%s_%s", vr, eff_tag(ef)),
              variant = vr, effect = ef, B = B$family)
    }
    ef <- max(TRIPLE_EFFECTS)
    add_job(sprintf("tripleboot_%s_%s", vr, eff_tag(ef)), "triple_boot",
            N$boot_tc, 1000, 4,
            .data_key = sprintf("triple_%s_%s", vr, eff_tag(ef)),
            variant = vr, effect = ef, B = B$boot_tc)
    add_job(sprintf("triplesatt_%s_%s", vr, eff_tag(ef)), "triple_satt",
            N$boot_tc, 30, 4,
            .data_key = sprintf("triple_%s_%s", vr, eff_tag(ef)),
            variant = vr, effect = ef)
  }
}
if ("bootstrap" %in% RUN) {
  for (n in c(8, 15, 30)) for (re in c("normal", "skewed")) {
    add_job(sprintf("bootwithin_n%02d_%s", n, re), "boot_within", N$boot,
            (60 + 0.8 * n) * B$boot / 999, 3, n = n, re = re, B = B$boot)
  }
  for (vr in BOOT_GROUP_VARIANTS) {
    add_job(sprintf("bootgroups_%s_%s", vr, eff_tag(EFFECT_GROUPS_BOOT)),
            "boot_groups", N$boot, 300 * size_factor[[vr]], 3,
            variant = vr, effect = EFFECT_GROUPS_BOOT, B = B$boot)
  }
}
if ("gca" %in% RUN) {
  for (sc in names(GCA_SCENARIOS)) {
    add_job(sprintf("gca_%s", sc), "gca",
            if (sc == "null") N$null_base else N$effect, 700, 4,
            scenario = sc, effect = GCA_SCENARIOS[[sc]], B = B$perm,
            B_boot = B$boot_gca)
    add_job(sprintf("gcamirman_%s", sc), "gca_mirman",
            if (sc == "null") N$null_base else N$effect, 120, 4,
            .data_key = sprintf("gca_%s", sc),
            scenario = sc, effect = GCA_SCENARIOS[[sc]])
  }
}
if ("posthoc" %in% RUN) {
  for (ds in c("level", "timecourse")) {
    for (ef in c(0, if (ds == "level") 0.04 else 0.05)) {
      add_job(sprintf("posthoc_%s_%s", ds, eff_tag(ef)), "posthoc",
              if (ef == 0) N$null else N$effect,
              if (ds == "level") 1000 else 800, 5,
              design = ds, effect = ef,
              B = if (ds == "level") B$family else B$perm)
    }
  }
}
if ("families" %in% RUN) {
  for (r in seq_len(nrow(FAMILY_CELLS))) {
    k <- FAMILY_CELLS$k[r]; rho <- FAMILY_CELLS$rho[r]
    for (ef in c(0, EFFECT_FAMILY)) {
      add_job(sprintf("family_k%02d_r%02d_%s", k, round(10 * rho),
                      eff_tag(ef)), "family",
              if (ef == 0) N$family_null else N$family_effect,
              (5 + 4.4 * k) * B$family / 199, 5,
              k = k, rho = rho, effect = ef, B = B$family)
    }
  }
}
if ("diagnostics" %in% RUN) {
  for (r in seq_len(nrow(ACF_CELLS))) {
    add_job(sprintf("diag_acf_ar%03d_t%02d", round(1000 * ACF_CELLS$ar1[r]),
                    ACF_CELLS$n_time[r]), "diag_acf", N$diag,
            ACF_CELLS$n_time[r] / 8, 5, ar1 = ACF_CELLS$ar1[r],
            n_time = ACF_CELLS$n_time[r])
  }
  add_job("diag_calibrate", "diag_calibrate", N$calibrate,
          550 * B$calibrate_sims / 100 * B$calibrate / 99, 5,
          n_sim = B$calibrate_sims, B = B$calibrate)
  for (r in seq_len(nrow(TC_CELLS))) {
    add_job(sprintf("diag_timecourse_%s_%s", TC_CELLS$scenario[r],
                    eff_tag(TC_CELLS$effect[r])), "diag_timecourse", N$diag,
            3, 5, scenario = TC_CELLS$scenario[r], effect = TC_CELLS$effect[r])
  }
  for (sz in LOSO_SIZES) {
    add_job(sprintf("diag_loso_size%d", sz), "diag_loso", N$diag, 1, 5,
            size = sz)
  }
  for (sc in c("no_confound", "confound")) {
    add_job(sprintf("diag_agreement_%s", sc), "diag_agreement", N$diag, 0.5,
            5, scenario = sc)
  }
}

cells <- do.call(rbind, lapply(JOBS, function(j) {
  a <- j$args
  data.frame(cell = j$prefix, job = j$fun, data_key = j$data_key,
             seed_base = j$seed_base, replicates = j$n,
             settings = paste(names(a), vapply(a, function(x) {
               paste(format(x), collapse = "/")
             }, character(1)), sep = "=", collapse = "; "))
}))

# ---- Run
if ("example" %in% RUN) {
  log_msg("Worked example (each result is saved as it finishes)")
  run_example(file.path(OUT_DIR, "example"), B = B$example,
              n_sim = B$calibrate_sims, B_cal = B$calibrate)
}
run_jobs(JOBS)

if ("timing" %in% RUN && !file.exists(file.path(OUT_DIR, "timing.csv"))) {
  log_msg("Timing analyses at the size of the real data set")
  ctrl <- lmerControl(calc.derivs = FALSE)
  quiet <- function(expr) suppressWarnings(suppressMessages(expr))
  tm <- function(expr) system.time(quiet(expr))[["elapsed"]]
  d <- sim_blocks("within", n_participants = length(TRIAL_COUNTS),
                  n_trials = TRIAL_COUNTS, n_time = 20, sampling_rate = 10,
                  prop_condition = 0.2, effect_condition = 0.04,
                  effect_onset = 0.5, sd_participant = SD$participant,
                  sd_slope = SD$slope, sd_trial = SD$trial,
                  sd_residual = SD$residual, ar1 = 0.92, residual_df = 5,
                  seed = 2027)
  d$y <- d$y - stats::ave(d$y, d$time)
  fit_time <- list()
  fit <- function(name, f) {
    t0 <- proc.time()[["elapsed"]]
    m <- quiet(lmer(f, data = d, control = ctrl))
    fit_time[[name]] <<- proc.time()[["elapsed"]] - t0
    m
  }
  m_s <- fit("simple", y ~ condition + (1 | participant))
  m_f <- fit("full", y ~ condition + (1 + condition | participant))
  m_tc <- fit("tc_simple", y ~ condition * ns(time, 3) + (1 | participant))
  fit("tc_complete", y ~ condition * ns(time, 3) +
        (1 + condition + condition:ns(time, 3) | participant))
  B_t <- if (TEST) 9 else 99
  tt <- data.frame(
    analysis = c("lmer fit, intercept-only model",
                 "lmer fit, maximal model",
                 "lmer fit, complete time-course model",
                 "GAMM fit (bam), without and with AR(1)",
                 "perm_test, intercept-only model",
                 "perm_test, maximal model",
                 "perm_test, condition x time (intercept-only model)",
                 "boot_ci, case resampling (maximal model)"),
    seconds = c(fit_time$simple, fit_time$full, fit_time$tc_complete,
                tm(gamm_test(d)),
                tm(perm_test(m_s, "condition", "trial",
                             exch_signflip("participant"), B = B_t, seed = 1)),
                tm(perm_test(m_f, "condition", "trial",
                             exch_signflip("participant"), B = B_t, seed = 1)),
                tm(perm_test(m_tc, "condition:ns(time, 3)", "trial",
                             exch_signflip("participant"), B = B_t, seed = 1)),
                tm(boot_ci(m_f, "participant", resample = "case",
                           ci = "percentile", terms = "conditionB", B = B_t,
                           seed = 1))),
    resamples = c(NA, NA, NA, NA, B_t, B_t, B_t, B_t))
  tt$minutes_B4999 <- ifelse(is.na(tt$resamples), NA,
                             round(tt$seconds * 4999 / tt$resamples / 60, 1))
  tt$rows <- nrow(d); tt$participants <- length(TRIAL_COUNTS)
  tt$workers <- WORKERS
  utils::write.csv(tt, file.path(OUT_DIR, "timing.csv"), row.names = FALSE)
}

# ---- Export: raw results and summaries as .csv
mc_ci <- function(k, n) round(100 * stats::binom.test(k, n)$conf.int, 2)
write_out <- function(x, file) {
  if (!is.null(x) && nrow(x) > 0) {
    utils::write.csv(x, file.path(OUT_DIR, file), row.names = FALSE)
  }
}
try_summary <- function(expr, file) {
  x <- tryCatch(expr, error = function(e) {
    log_msg("WARNING: summary ", file, " not written: ", conditionMessage(e))
    NULL
  })
  write_out(x, file)
}
report_failures <- function(x, name) {
  if (is.null(x)) return(invisible())
  bad <- if ("p" %in% names(x)) is.na(x$p) else rep(FALSE, nrow(x))
  if (any(bad)) {
    first <- if ("error" %in% names(x)) x$error[bad][!is.na(x$error[bad])][1]
    log_msg(sprintf("WARNING: %d missing results in '%s'. First error: %s",
                    sum(bad), name, first %||% "none recorded"))
  }
}

summarise_tests <- function(x, by) {
  cell_by <- setdiff(by, c("effect", "method"))
  s <- bind_fill(lapply(split(x, x[by], drop = TRUE), function(z) {
    ok <- !is.na(z$p); k <- sum(z$p[ok] <= 0.05); n <- sum(ok)
    ci <- if (n > 0) mc_ci(k, n) else c(NA, NA)
    out <- z[1, by, drop = FALSE]
    out$n <- n; out$failed <- sum(!ok)
    out$reject_pct <- if (n > 0) round(100 * k / n, 2) else NA
    out$ci_low <- ci[1]; out$ci_high <- ci[2]
    out$median_seconds <- if (all(is.na(z$seconds))) NA else
      round(stats::median(z$seconds, na.rm = TRUE), 3)
    if ("truth" %in% names(z)) out$truth <- signif(z$truth[1], 4)
    out$first_error <- if (any(!ok) && "error" %in% names(z)) {
      z$error[!ok][!is.na(z$error[!ok])][1] %||% NA
    } else NA
    out
  }))
  s$power_size_adjusted_pct <- NA_real_
  nul <- x[x$effect == 0, ]
  for (r in which(s$effect != 0)) {
    match_row <- Reduce(`&`, lapply(c(cell_by, "method"), function(v) {
      nul[[v]] == s[[v]][r]
    }))
    p0 <- nul$p[match_row & !is.na(nul$p)]
    if (length(p0) >= 100) {
      crit <- stats::quantile(p0, 0.05, names = FALSE, type = 1)
      z <- x[Reduce(`&`, lapply(by, function(v) x[[v]] == s[[v]][r])), ]
      s$power_size_adjusted_pct[r] <- round(100 * mean(z$p[!is.na(z$p)] <=
                                                         crit), 2)
    }
  }
  s[do.call(order, unname(as.list(s[by]))), , drop = FALSE]
}

paired_comparisons <- function(x, by, ref = "perm_simple") {
  cell_by <- setdiff(by, "method")
  out <- lapply(split(x, x[cell_by], drop = TRUE), function(z) {
    r <- z[z$method == ref & !is.na(z$p), c("rep", "p")]
    if (nrow(r) == 0) return(NULL)
    bind_fill(lapply(setdiff(unique(z$method), ref), function(m) {
      o <- z[z$method == m & !is.na(z$p), c("rep", "p")]
      mm <- merge(r, o, by = "rep", suffixes = c(".ref", ".other"))
      if (nrow(mm) == 0) return(NULL)
      a <- mm$p.ref <= 0.05; b <- mm$p.other <= 0.05
      n10 <- sum(a & !b); n01 <- sum(!a & b)
      cbind(z[1, cell_by, drop = FALSE],
            data.frame(reference = ref, method = m, n = nrow(mm),
                       reference_pct = round(100 * mean(a), 2),
                       method_pct = round(100 * mean(b), 2),
                       only_reference = n10, only_method = n01,
                       mcnemar_p = if (n10 + n01 > 0) {
                         stats::binom.test(n10, n10 + n01)$p.value
                       } else 1))
    }))
  })
  bind_fill(out)
}

summarise_intervals <- function(x, by) {
  bind_fill(lapply(split(x, x[by], drop = TRUE), function(z) {
    ok <- !is.na(z$lower) & !is.na(z$upper)
    cover <- z$lower[ok] <= z$truth[ok] & z$upper[ok] >= z$truth[ok]
    ci <- if (sum(ok) > 0) mc_ci(sum(cover), sum(ok)) else c(NA, NA)
    out <- z[1, by, drop = FALSE]
    out$truth <- signif(z$truth[1], 4)
    out$reps <- sum(ok); out$failed <- sum(!ok)
    out$coverage_pct <- round(100 * mean(cover), 2)
    out$ci_low <- ci[1]; out$ci_high <- ci[2]
    out$miss_below_pct <- round(100 * mean(z$upper[ok] < z$truth[ok]), 2)
    out$miss_above_pct <- round(100 * mean(z$lower[ok] > z$truth[ok]), 2)
    out$median_width <- round(stats::median(z$upper[ok] - z$lower[ok]), 4)
    out
  }))
}

summarise_family <- function(x, by) {
  x$null_feature <- abs(x$truth) < 1e-12
  bind_fill(lapply(split(x, x[by], drop = TRUE), function(z) {
    per_rep <- split(z, z$rep)
    any_false <- vapply(per_rep, function(r) {
      any(r$p[r$null_feature] <= 0.05, na.rm = TRUE)
    }, logical(1))
    any_hit <- vapply(per_rep, function(r) {
      any(r$p[!r$null_feature] <= 0.05, na.rm = TRUE)
    }, logical(1))
    has_null <- vapply(per_rep, function(r) any(r$null_feature), logical(1))
    has_eff <- vapply(per_rep, function(r) any(!r$null_feature), logical(1))
    out <- z[1, by, drop = FALSE]
    out$reps <- length(per_rep)
    out$fwer_pct <- if (any(has_null)) round(100 * mean(any_false[has_null]),
                                             2) else NA
    ci <- if (any(has_null)) mc_ci(sum(any_false[has_null]),
                                   sum(has_null)) else c(NA, NA)
    out$fwer_ci_low <- ci[1]; out$fwer_ci_high <- ci[2]
    out$power_any_pct <- if (any(has_eff)) round(100 * mean(any_hit[has_eff]),
                                                 2) else NA
    for (f in unique(z$feature[!z$null_feature])) {
      v <- z$p[z$feature == f & !z$null_feature]
      out[[paste0("power_", f, "_pct")]] <- round(100 * mean(v <= 0.05,
                                                           na.rm = TRUE), 2)
    }
    out
  }))
}
bind_summaries <- function(lst) {
  out <- bind_fill(lst)
  if (!is.null(out)) rownames(out) <- NULL
  out
}

# ---- Single tests and time courses
for (pt in list(c("single", "single"), c("tc_", "timecourse"),
                c("triple_", "triple"))) {
  x <- read_raw(pt[1], "tests")
  if (is.null(x)) next
  report_failures(x, pt[2])
  by <- c("design", "variant", "effect", "method")
  write_out(x, paste0(pt[2], "_raw.csv"))
  try_summary(summarise_tests(x, by), paste0(pt[2], "_summary.csv"))
  try_summary(paired_comparisons(x[x$effect != 0, ], by),
            paste0(pt[2], "_paired_comparisons.csv"))
  if (pt[2] != "single") {
    cl <- x[x$method == "cluster" & x$effect > 0 & !is.na(x$onset), ]
    if (nrow(cl) > 0) {
      write_out(bind_fill(lapply(split(cl, cl[c("design", "variant",
                                                    "effect")], drop = TRUE),
                                      function(z) {
        data.frame(design = z$design[1], variant = z$variant[1],
                   effect = z$effect[1], n_significant = nrow(z),
                   median_onset_s = round(stats::median(z$onset), 3),
                   p10_onset_s = round(stats::quantile(z$onset, 0.1), 3),
                   p90_onset_s = round(stats::quantile(z$onset, 0.9), 3),
                   pct_onset_before_0.3s = round(100 * mean(z$onset < 0.3), 1))
      })), paste0(pt[2], "_cluster_onsets.csv"))
    }
    ex <- x[x$method %in% c("wald_complete", "gamm_ar1"), ]
    if (nrow(ex) > 0) {
      write_out(bind_fill(lapply(split(ex, ex[c("design", "variant",
                                                    "effect")], drop = TRUE),
                                      function(z) {
        data.frame(design = z$design[1], variant = z$variant[1],
                   effect = z$effect[1],
                   complete_singular_pct = round(100 * mean(
                     z$singular[z$method == "wald_complete"], na.rm = TRUE), 1),
                   gamm_rho_mean = round(mean(z$rho[z$method == "gamm_ar1"],
                                              na.rm = TRUE), 3),
                   gamm_rho_sd = round(stats::sd(z$rho[z$method == "gamm_ar1"],
                                                 na.rm = TRUE), 3))
      })), paste0(pt[2], "_model_details.csv"))
    }
    if (pt[2] == "timecourse") {
      gp <- x[x$variant %in% c("gaps_interp", "gaps_removed") &
                !duplicated(x[c("cell", "rep")]), ]
      if (nrow(gp) > 0) {
        write_out(stats::aggregate(prop_missing ~ variant, gp, mean),
                  "timecourse_gaps_missing.csv")
      }
    }
  }
}

# ---- Triple: families and intervals
x <- read_raw("triplefamily", "family")
if (!is.null(x)) {
  x$truth <- x$effect
  write_out(x, "triple_families_raw.csv")
  try_summary(summarise_family(x, c("design", "variant", "effect", "method")),
            "triple_families_summary.csv")
}
for (pt in list(c("tripleboot|triplesatt", "triple_intervals"),
                c("tcboot", "timecourse_intervals"))) {
  x <- bind_fill(lapply(strsplit(pt[1], "|", fixed = TRUE)[[1]], read_raw,
                        part = "intervals"))
  if (is.null(x)) next
  write_out(x, paste0(pt[2], "_raw.csv"))
  try_summary(summarise_intervals(x, c("design", "variant", "effect", "coef",
                                     "method")),
            paste0(pt[2], "_summary.csv"))
}

# ---- Bootstrap
x <- read_raw("bootwithin", "intervals")
if (!is.null(x)) {
  write_out(x, "boot_within_raw.csv")
  try_summary(summarise_intervals(x, c("re", "n", "method")),
            "boot_within_summary.csv")
}
x <- read_raw("bootgroups", "intervals")
if (!is.null(x)) {
  write_out(x, "boot_groups_raw.csv")
  try_summary(summarise_intervals(x, c("variant", "method")),
            "boot_groups_summary.csv")
}

# ---- Growth curve analysis
x <- read_raw("gca", "tests")
if (!is.null(x)) {
  report_failures(x, "gca")
  write_out(x, "gca_tests_raw.csv")
  try_summary(summarise_tests(x, c("scenario", "term", "method")),
            "gca_tests_summary.csv")
  sg <- x[x$method == "wald_mirman" & !duplicated(x[c("cell", "rep")]) &
            !is.na(x$singular), ]
  if (nrow(sg) > 0) {
    write_out(stats::aggregate(cbind(singular_pct = 100 * singular) ~ scenario,
                               sg, function(v) round(mean(v), 1)),
              "gca_mirman_singular.csv")
  }
}
x <- read_raw("gca", "family")
if (!is.null(x)) {
  write_out(x, "gca_families_raw.csv")
  try_summary(summarise_family(x, c("family", "scenario", "method")),
            "gca_families_summary.csv")
}
x <- read_raw("gca", "intervals")
if (!is.null(x)) {
  write_out(x, "gca_intervals_raw.csv")
  try_summary(summarise_intervals(x, c("scenario", "term", "method")),
            "gca_intervals_summary.csv")
}

# ---- Families of outcomes
x <- read_raw("family_", "family")
if (!is.null(x)) {
  write_out(x, "families_raw.csv")
  try_summary(summarise_family(x, c("k", "rho", "effect", "method")),
            "families_summary.csv")
}

# ---- Pairwise comparisons
x <- read_raw("posthoc", "family")
if (!is.null(x)) {
  write_out(x, "posthoc_raw.csv")
  try_summary(summarise_family(x, c("design", "effect", "method")),
              "posthoc_summary.csv")
}
x <- read_raw("posthoc", "intervals")
if (!is.null(x)) {
  x$coef <- x$feature
  write_out(x, "posthoc_intervals_raw.csv")
  try_summary(summarise_intervals(x, c("design", "effect", "coef", "method")),
              "posthoc_intervals_summary.csv")
}

# ---- Diagnostics
for (chk in c("acf", "calibrate", "timecourse", "loso", "agreement")) {
  x <- read_raw(paste0("diag_", chk), "diag")
  if (is.null(x)) next
  write_out(x, paste0("diag_", chk, "_raw.csv"))
  grp <- switch(chk, acf = c("ar1", "n_time"), calibrate = "test",
                timecourse = c("scenario", "effect"), loso = "size",
                agreement = "scenario")
  s <- bind_fill(lapply(split(x, x[grp], drop = TRUE), function(z) {
    out <- z[1, grp, drop = FALSE]
    out$n <- nrow(z)
    if (chk == "acf") {
      out$mean_estimate <- round(mean(z$estimate), 4)
      out$sd_estimate <- round(stats::sd(z$estimate), 4)
      out$bias <- round(mean(z$estimate) - z$ar1[1], 4)
      out$mean_raw <- round(mean(z$estimate_raw), 4)
      out$bias_raw <- round(mean(z$estimate_raw) - z$ar1[1], 4)
    } else if (chk == "calibrate") {
      ok <- !is.na(z$observed_p)
      k <- sum(z$observed_p[ok] <= 0.05)
      ci <- mc_ci(k, sum(ok))
      out$true_rate_pct <- round(100 * k / sum(ok), 2)
      out$true_ci_low <- ci[1]; out$true_ci_high <- ci[2]
      out$predicted_mean_pct <- round(100 * mean(z$predicted_rate,
                                                 na.rm = TRUE), 2)
      out$predicted_p10_pct <- round(100 * stats::quantile(
        z$predicted_rate, 0.1, na.rm = TRUE), 2)
      out$predicted_p90_pct <- round(100 * stats::quantile(
        z$predicted_rate, 0.9, na.rm = TRUE), 2)
    } else if (chk == "loso") {
      ci <- mc_ci(sum(z$rank_of_aberrant == 1), nrow(z))
      out$ranked_first_pct <- round(100 * mean(z$rank_of_aberrant == 1), 2)
      out$ci_low <- ci[1]; out$ci_high <- ci[2]
      out$median_max_std_change <- round(stats::median(z$max_std_change), 3)
    } else {
      ci <- mc_ci(sum(z$flagged), nrow(z))
      out$flagged_pct <- round(100 * mean(z$flagged), 2)
      out$ci_low <- ci[1]; out$ci_high <- ci[2]
      if (chk == "timecourse") {
        out$median_max_z <- round(stats::median(z$max_z), 3)
      } else {
        out$median_relative <- round(stats::median(z$relative), 4)
      }
    }
    out
  }))
  write_out(s, paste0("diag_", chk, "_summary.csv"))
}

# ---- Replicates that failed as a whole
errs <- read_raw("", "errors")
if (!is.null(errs)) {
  log_msg("WARNING: ", nrow(errs), " replicates failed as a whole. First: ",
          errs$error[1])
  write_out(errs, "failed_replicates.csv")
}

# ---- Run information and zip
if (exists("cells") && !is.null(cells)) {
  done <- vapply(cells$cell, function(cc) {
    length(list.files(RAW_DIR, paste0("^", cc, "__[0-9]+\\.rds$")))
  }, integer(1))
  cells$files_done <- done
  cells$files_expected <- ceiling(cells$replicates / CHUNK)
  write_out(cells, "cells.csv")
}
writeLines(c(
  paste("Finished:", format(Sys.time())),
  paste("lmmresample", PKG_VERSION, "commit", PKG_SHA),
  paste("lme4", as.character(utils::packageVersion("lme4"))),
  paste("lmerTest", as.character(utils::packageVersion("lmerTest"))),
  paste("mgcv", as.character(utils::packageVersion("mgcv"))),
  paste("R", R.version.string),
  paste("Platform:", R.version$platform),
  paste("Workers:", WORKERS),
  paste("Parts:", paste(RUN, collapse = ", ")),
  paste("Test run:", TEST),
  paste("Replicates:", paste(names(N), unlist(N), sep = " = ",
                             collapse = "; ")),
  paste("Resamples:", paste(names(B), unlist(B), sep = " = ",
                            collapse = "; ")),
  paste("Replicates per file:", CHUNK)),
  file.path(OUT_DIR, "run_info.txt"))

ZIP_FILE <- paste0(normalizePath(OUT_DIR), ".zip")
old_wd <- setwd(dirname(OUT_DIR))
to_zip <- file.path(basename(OUT_DIR),
                    list.files(basename(OUT_DIR), recursive = TRUE))
to_zip <- to_zip[!grepl(paste0("^", basename(OUT_DIR), "/raw/"), to_zip)]
if (file.exists(ZIP_FILE)) invisible(file.remove(ZIP_FILE))
utils::zip(ZIP_FILE, to_zip, flags = "-q")
setwd(old_wd)
log_msg("Done. Results in ", OUT_DIR, "; zip: ", ZIP_FILE)
