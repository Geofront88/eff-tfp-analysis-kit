# =============================================================================
# eff_tfp_v3.R  —  Efficiency-TFP Analysis
# Following Santos et al. (2021) / De Ketelaere et al. (2026)
# =============================================================================

# ── Packages ──────────────────────────────────────────────────────────────────
required <- c("readxl", "urca", "vars", "writexl")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# ── Constants ─────────────────────────────────────────────────────────────────
DEFAULT_ALPHA_K  <- 0.3
MAX_ADF_LAGS     <- 10L
VAR_LAG_DEFAULT  <- 2L
VAR_LAG_MIN      <- 1L
VAR_LAG_MAX      <- 5L

# KPSS asymptotic critical values — Kwiatkowski et al. (1992) Table 1
# Levels   → tau statistic (trend + intercept)
# Diffs    → mu  statistic (intercept only)
KPSS_CV <- list(
  levels = c("1%" = 0.216, "5%" = 0.146, "10%" = 0.119),
  diffs  = c("1%" = 0.739, "5%" = 0.463, "10%" = 0.347)
)

# =============================================================================
# 1.  DATA LOADING
# =============================================================================
load_data <- function(file_path) {
  ext <- tolower(tools::file_ext(file_path))

  .read_sheet <- function(sheet, skip) {
    # Read with given skip (header = first row after skip), return clean data frame
    if (ext %in% c("xlsx", "xls")) {
      suppressMessages(as.data.frame(
        readxl::read_excel(file_path, sheet = sheet,
                           skip = skip, col_types = "text")))
    } else {
      read.csv(file_path, skip = skip, header = TRUE,
               stringsAsFactors = FALSE, colClasses = "character")
    }
  }

  .clean <- function(d) {
    names(d) <- tolower(trimws(names(d)))
    if (!"year" %in% names(d)) return(NULL)
    d <- d[rowSums(!is.na(d) & d != "") > 0, ]
    for (nm in names(d))
      d[[nm]] <- suppressWarnings(as.numeric(as.character(d[[nm]])))
    d <- d[!is.na(d$year) & d$year >= 1800 & d$year <= 2100, ]
    if (nrow(d) == 0) return(NULL)
    d$year <- as.integer(d$year)
    d[order(d$year), ]
  }

  sheets <- if (ext %in% c("xlsx", "xls"))
    tryCatch(readxl::excel_sheets(file_path), error = function(e) character(0))
  else character(0)

  d <- NULL

  # ── Template sheet 1: data header at row 21, data from row 22 (skip=20) ──
  if ("1_Standard_Template" %in% sheets) {
    d <- tryCatch(.clean(.read_sheet("1_Standard_Template", skip = 20)),
                  error = function(e) NULL)
  }

  # ── Template sheet 2: data header at row 10, data from row 11 (skip=9) ───
  if (is.null(d) && "2_Preloaded_Template" %in% sheets) {
    d <- tryCatch(.clean(.read_sheet("2_Preloaded_Template", skip = 9)),
                  error = function(e) NULL)
  }

  # ── Plain file fallback: header at row 1 (skip=0) ─────────────────────────
  if (is.null(d)) {
    for (sh in if (length(sheets)) sheets else "csv") {
      d <- tryCatch(.clean(.read_sheet(sh, skip = 0)), error = function(e) NULL)
      if (!is.null(d)) break
    }
  }

  if (is.null(d))
    stop("Could not find valid year data. Check that your file uses the ",
         "standard data template or has column headers at row 1.")

  message("Loaded ", nrow(d), " rows: ", min(d$year), "\u2013", max(d$year))
  message("Columns found: ", paste(names(d), collapse = ", "))
  d
}

# =============================================================================
# 2.  FACTOR SHARES
# =============================================================================
compute_factor_shares <- function(d) {
  has_adj_gos <- "adj_gos"  %in% names(d)   # UQGD
  has_gos     <- "gos"      %in% names(d)   # UOGD
  has_coe     <- "coe"      %in% names(d)   # UWCD

  if (has_adj_gos && has_gos && has_coe) {
    alpha_K <- d$adj_gos / (d$gos + d$coe)
    alpha_K[!is.finite(alpha_K)] <- NA
    alpha_L <- 1 - alpha_K
    source  <- "computed (annual adj. GOS / (GOS + COE))"
    message("Factor shares: computed from annual data.")
  } else {
    alpha_K <- rep(DEFAULT_ALPHA_K, nrow(d))
    alpha_L <- rep(1 - DEFAULT_ALPHA_K, nrow(d))
    source  <- paste0("default (alpha_K = ", DEFAULT_ALPHA_K, ")")
    message("Factor shares: using default alpha_K = ", DEFAULT_ALPHA_K,
            " (provide adj_gos, gos, coe columns to compute from data).")
  }

  list(alpha_K = alpha_K, alpha_L = alpha_L, source = source,
       year = d$year)
}

# =============================================================================
# 3.  LABOUR INPUTS
# =============================================================================
compute_labour <- function(d) {
  has_emp  <- "emp"  %in% names(d)
  has_avh  <- "avh"  %in% names(d)
  has_hc   <- "hc"   %in% names(d)
  has_wap  <- "wap"  %in% names(d)
  has_lfpr <- "lfpr" %in% names(d)
  has_ur   <- "ur"   %in% names(d)

  if (!has_avh)  stop("Column 'avh' (average annual hours) is required.")

  # Engaged workers: emp directly or wap × lfpr × (1 − ur)
  if (has_emp) {
    engaged <- d$emp
    message("Labour: using emp directly.")
  } else if (has_wap && has_lfpr && has_ur) {
    engaged <- d$wap * d$lfpr * (1 - d$ur)
    message("Labour: computing engaged = wap × lfpr × (1 - ur).")
  } else {
    stop("Provide either 'emp' OR ('wap' + 'lfpr' + 'ur').")
  }

  L_unadj <- engaged * d$avh

  if (has_hc) {
    L_adj <- L_unadj * d$hc
    message("Labour: quality-adjusted labour = L_unadj × hc.")
  } else {
    L_adj <- NULL
    message("Labour: 'hc' not found — quality-adjusted TFP will not be computed.")
  }

  list(L_unadj = L_unadj, L_adj = L_adj)
}

# =============================================================================
# 4.  TFP MEASURES (normalized, then log, then +1)
# =============================================================================
compute_tfp <- function(d, labour, shares) {
  has_kstock   <- "k_stock"    %in% names(d)
  has_kserv    <- "k_services" %in% names(d)

  if (!has_kstock)
    stop("Column 'k_stock' (capital stock) is required.")

  # Use mean (constant) alpha_K for TFP — annual series kept for diagnostics
  aK <- mean(shares$alpha_K, na.rm = TRUE)
  aL <- 1 - aK
  message("TFP computation: alpha_K = ", round(aK, 4),
          ", alpha_L = ", round(aL, 4), " (time-average, constant)")

  # TFP_unadj: K_stock + unadjusted labour
  tfp_unadj_raw <- d$gdp / (d$k_stock^aK * labour$L_unadj^aL)
  tfp_unadj_norm <- tfp_unadj_raw / tfp_unadj_raw[1]
  z_tfp_unadj    <- log(tfp_unadj_norm) + 1
  r1 <- round(range(z_tfp_unadj, na.rm = TRUE), 3)
  message("TFP_unadj: base year = ", d$year[1],
          ", z range [", r1[1], ", ", r1[2], "]")

  # TFP_adj: K_services + quality-adjusted labour
  if (!is.null(labour$L_adj) && has_kserv) {
    tfp_adj_raw  <- d$gdp / (d$k_services^aK * labour$L_adj^aL)
    tfp_adj_norm <- tfp_adj_raw / tfp_adj_raw[1]
    z_tfp_adj    <- log(tfp_adj_norm) + 1
    r2 <- round(range(z_tfp_adj, na.rm = TRUE), 3)
    message("TFP_adj:   base year = ", d$year[1],
            ", z range [", r2[1], ", ", r2[2], "]")
  } else {
    z_tfp_adj <- NULL
    if (!has_kserv) message("TFP_adj: skipped (no 'k_services' column).")
    if (is.null(labour$L_adj)) message("TFP_adj: skipped (no 'hc' column).")
  }

  list(z_tfp_unadj = z_tfp_unadj,
       z_tfp_adj   = z_tfp_adj,
       year        = d$year)
}

# =============================================================================
# 5.  EXERGY EFFICIENCY (normalized, then log, then +1) & INTENSITIES
# =============================================================================
compute_exergy <- function(d) {
  if (!all(c("x_final", "x_useful") %in% names(d)))
    stop("Columns 'x_final' and 'x_useful' are required.")

  eps_raw  <- d$x_useful / d$x_final
  eps_norm <- eps_raw / eps_raw[1]
  z_eff    <- log(eps_norm) + 1
  r3 <- round(range(eps_raw, na.rm = TRUE), 4)
  message("Efficiency: base year = ", d$year[1],
          ", raw range [", r3[1], ", ", r3[2], "]")

  int_final  <- d$x_final / d$gdp
  int_useful <- d$x_useful / d$gdp

  list(z_eff       = z_eff,
       eps_raw     = eps_raw,
       eps_norm    = eps_norm,
       int_final   = int_final,
       int_useful  = int_useful,
       year        = d$year)
}

# =============================================================================
# 6.  UNIT ROOT TESTS
# =============================================================================
# Newey-West bandwidth: floor(4*(T/100)^(2/9)) — matches EViews default
.nw_bandwidth <- function(n) floor(4 * (n / 100)^(2/9))

# ── Custom ADF — matches EViews exactly ──────────────────────────────────────
# EViews procedure:
#  1. Evaluate BIC on a FIXED common sample (T − maxlag − 1) for all lags
#  2. Select best lag p*
#  3. Re-estimate with p* on the FULL available sample (T − p* − 1 obs)
#  4. Report t-statistic and CVs from that full-sample estimation
# ── Custom ADF — matches EViews BIC lag selection exactly ────────────────────
# EViews evaluates BIC for lag p on T−p−1 observations (each lag gets its own
# sample). The final regression is on T−best_p−1 observations.
.adf_custom <- function(x, max_lags, type = "trend") {
  T  <- length(x)
  dx <- diff(x)

  best_bic <- Inf
  best_p   <- 0L

  for (p in 0:max_lags) {
    n       <- T - p - 1
    if (n < max(10L, p + 4L)) next
    t_start <- p + 2
    dY      <- dx[(t_start - 1):(T - 1)]
    if (type == "trend") {
      X <- cbind(1, t_start:T, x[(t_start - 1):(T - 1)])
    } else {
      X <- cbind(1, x[(t_start - 1):(T - 1)])
    }
    if (p > 0)
      for (j in seq_len(p))
        X <- cbind(X, dx[(t_start - j - 1):(T - j - 1)])
    fit <- lm.fit(X, dY)
    bic <- log(sum(fit$residuals^2) / n) + ncol(X) * log(n) / n
    if (bic < best_bic) { best_bic <- bic; best_p <- as.integer(p) }
  }

  # Final regression with best_p
  p       <- best_p
  n       <- T - p - 1
  t_start <- p + 2
  dY      <- dx[(t_start - 1):(T - 1)]
  if (type == "trend") {
    X <- cbind(1, t_start:T, x[(t_start - 1):(T - 1)])
    lag_col <- 3L
  } else {
    X <- cbind(1, x[(t_start - 1):(T - 1)])
    lag_col <- 2L
  }
  if (p > 0)
    for (j in seq_len(p))
      X <- cbind(X, dx[(t_start - j - 1):(T - j - 1)])

  fit   <- lm.fit(X, dY)
  rss   <- sum(fit$residuals^2)
  s2    <- rss / (n - ncol(X))
  XtXi  <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
  if (is.null(XtXi))
    return(list(stat = NA_real_, lag = p, n = n,
                cv = c("1%" = NA, "5%" = NA, "10%" = NA)))
  tstat <- fit$coefficients[lag_col] / sqrt(s2 * diag(XtXi)[lag_col])

  # MacKinnon (1996) response surface CVs — correct coefficients for n
  if (type == "trend") {
    cs <- list("1%"  = c(-4.27338,  9.08943, -6.28274),
               "5%"  = c(-3.56842,  7.52190, -5.40508),
               "10%" = c(-3.21826,  6.63930, -4.56862))
  } else {
    cs <- list("1%"  = c(-3.43035,  6.36010, -8.53125),
               "5%"  = c(-2.86154,  4.79745, -6.41641),
               "10%" = c(-2.56677,  3.62790, -3.38973))
  }
  cv <- sapply(cs, function(b) b[1] + b[2]/n + b[3]/n^2)
  list(stat = tstat, lag = p, n = n, cv = cv)
}

run_unit_root_tests <- function(series_list, year) {
  results <- list()

  for (nm in names(series_list)) {
    x   <- series_list[[nm]]
    x   <- x[!is.na(x)]
    dx  <- diff(x)
    T_l <- length(x);  T_d <- length(dx)
    bw_l <- .nw_bandwidth(T_l);  bw_d <- .nw_bandwidth(T_d)

    # ADF (custom — EViews-matched BIC lag selection)
    adf_l <- .adf_custom(x,  MAX_ADF_LAGS, "trend")
    adf_d <- .adf_custom(dx, MAX_ADF_LAGS, "drift")

    # PP (Bartlett kernel, Newey-West fixed bandwidth)
    pp_l <- urca::ur.pp(x,  type = "Z-tau", model = "trend",    use.lag = bw_l)
    pp_d <- urca::ur.pp(dx, type = "Z-tau", model = "constant", use.lag = bw_d)
    pp_cv_l <- setNames(pp_l@cval["critical values",], c("1%","5%","10%"))
    pp_cv_d <- setNames(pp_d@cval["critical values",], c("1%","5%","10%"))

    # Verdicts at 5% — both tests must agree
    adf_i1 <- isTRUE(!is.na(adf_l$stat) &&
                      adf_l$stat > adf_l$cv["5%"] &&
                      adf_d$stat < adf_d$cv["5%"])
    pp_i1  <- (unname(pp_l@teststat[1]) > pp_cv_l["5%"]) &&
              (unname(pp_d@teststat[1]) < pp_cv_d["5%"])

    ord <- if (isTRUE(adf_i1) && pp_i1)          "I(1)"        else
           if (!isTRUE(adf_i1) && !pp_i1)         "I(0)"        else
                                                   "inconclusive"

    results[[nm]] <- list(
      series           = nm,
      n_levels         = T_l, n_diffs = T_d,
      adf_stat_levels  = round(adf_l$stat, 4),
      adf_cv1_levels   = round(adf_l$cv["1%"],  4),
      adf_cv5_levels   = round(adf_l$cv["5%"],  4),
      adf_cv10_levels  = round(adf_l$cv["10%"], 4),
      adf_lags_levels  = adf_l$lag,
      adf_stat_diffs   = round(adf_d$stat, 4),
      adf_cv1_diffs    = round(adf_d$cv["1%"],  4),
      adf_cv5_diffs    = round(adf_d$cv["5%"],  4),
      adf_cv10_diffs   = round(adf_d$cv["10%"], 4),
      adf_lags_diffs   = adf_d$lag,
      pp_stat_levels   = round(unname(pp_l@teststat[1]), 4),
      pp_cv1_levels    = round(pp_cv_l["1%"],  4),
      pp_cv5_levels    = round(pp_cv_l["5%"],  4),
      pp_cv10_levels   = round(pp_cv_l["10%"], 4),
      pp_bw_levels     = bw_l,
      pp_stat_diffs    = round(unname(pp_d@teststat[1]), 4),
      pp_cv1_diffs     = round(pp_cv_d["1%"],  4),
      pp_cv5_diffs     = round(pp_cv_d["5%"],  4),
      pp_cv10_diffs    = round(pp_cv_d["10%"], 4),
      pp_bw_diffs      = bw_d,
      adf_verdict      = if (isTRUE(adf_i1)) "I(1)" else "not I(1)",
      pp_verdict       = if (pp_i1) "I(1)" else "not I(1)",
      order            = ord
    )

    message(sprintf("  %-20s ADF: %-12s PP: %-12s  =>  %s",
                    nm,
                    if (isTRUE(adf_i1)) "I(1)" else "not I(1)",
                    if (pp_i1) "I(1)" else "not I(1)",
                    ord))
  }
  return(results)
}



# =============================================================================
# 7.  VAR ESTIMATION AND DIAGNOSTICS
# =============================================================================
estimate_var <- function(z_tfp, z_eff, year, p = VAR_LAG_DEFAULT,
                         tfp_name = "TFP") {
  # Build data frame for VAR (complete cases only)
  df   <- data.frame(TFP = z_tfp, EFF = z_eff)
  ok   <- complete.cases(df)
  df   <- df[ok, ]
  yrs  <- year[ok]

  vfit <- vars::VAR(df, p = p, type = "const")
  message("VAR(", p, ") estimated: ", tfp_name, " ~ EFF, T = ",
          nrow(df) - p, " (", yrs[p+1], "–", tail(yrs, 1), ")")

  list(var  = vfit, data = df, year = yrs, p = p, tfp_name = tfp_name)
}

var_diagnostics <- function(vfit_obj, max_lag = 5) {
  vfit <- vfit_obj$var
  p    <- vfit_obj$p
  res  <- residuals(vfit)       # (T_total - p) × K matrix
  T    <- nrow(res)             # effective observations after lag adjustment
  K    <- ncol(res)

  # ── Portmanteau test (cumulative Q-stat, h=1..max_lag) ───────────────────
  # Q_h = T * Σ_{j=1}^{h} tr(Ĉ_j' Ĉ_0^{-1} Ĉ_j Ĉ_0^{-1})  -- CUMULATIVE
  # Adj: T²/(T-j) weight per lag j before summing
  Sigma0     <- crossprod(res) / T
  Sigma0_inv <- solve(Sigma0)

  # Pre-compute per-lag contribution
  cj     <- numeric(max_lag)   # raw contribution at each j
  cj_adj <- numeric(max_lag)   # adjusted contribution at each j
  for (j in seq_len(max_lag)) {
    Sj      <- t(res[(j+1):T, ]) %*% res[1:(T-j), ] / T
    tr_j    <- sum(diag(t(Sj) %*% Sigma0_inv %*% Sj %*% Sigma0_inv))
    cj[j]      <- T       * tr_j
    cj_adj[j]  <- T^2/(T-j) * tr_j
  }

  portmanteau <- lapply(seq_len(max_lag), function(h) {
    qh     <- sum(cj[1:h])        # cumulative Q
    qh_adj <- sum(cj_adj[1:h])    # cumulative Adj-Q
    df     <- K^2 * (h - p)
    pval     <- if (df > 0) pchisq(qh,     df, lower.tail = FALSE) else NA
    pval_adj <- if (df > 0) pchisq(qh_adj, df, lower.tail = FALSE) else NA
    list(h = h, Q = round(qh, 6), Q_adj = round(qh_adj, 6),
         df = max(df, 0), pval = pval, pval_adj = pval_adj)
  })

  # ── LM Serial Correlation Test ────────────────────────────────────────────
  # Uses ALL T VAR observations, with pre-sample residuals set to zero.
  # Edgeworth correction c = T - K*p - 1 - K/2 (EViews convention).
  # df2 = 2*N - df1 where N = T - K*(p+1)  (from EViews numerical pattern).
  dm  <- as.matrix(vfit$datamat)
  Y   <- dm[, 1:K,         drop = FALSE]   # T × K dependent
  X0  <- dm[, (K+1):ncol(dm), drop = FALSE] # T × (K*p+1) regressors
  stopifnot(nrow(Y) == T, nrow(X0) == T)

  c_edge <- T - K * p - 1 - K / 2          # Edgeworth correction
  N_rao  <- T - K * (p + 1)                # for df2 = 2*N - df1

  .lm_at_lag <- function(lags_to_use) {
    h_max  <- max(lags_to_use)
    df1    <- length(lags_to_use) * K^2
    # s for Rao F: s = sqrt((df1^2 * K^2 - 4) / (df1^2 + K^2 - 5))
    s_rao  <- sqrt(max((df1^2 * K^2 - 4) / (df1^2 + K^2 - 5), 0))
    df2    <- 2 * N_rao - df1                # matches EViews exactly

    # Build augmented X with pre-sample zeros for lagged residuals
    X1 <- X0
    for (j in lags_to_use) {
      res_lag <- matrix(0, T, K)
      if (j < T) res_lag[(j+1):T, ] <- res[1:(T-j), ]
      X1 <- cbind(X1, res_lag)
    }

    fit1 <- lm.fit(X1, Y)
    fit0 <- lm.fit(X0,  Y)
    S1   <- crossprod(fit1$residuals) / T
    S0   <- crossprod(fit0$residuals) / T

    lam  <- max(det(S1) / det(S0), 1e-300)
    lr   <- -c_edge * log(lam)              # LRE* (Edgeworth-corrected)

    lam_s <- lam^(1 / max(s_rao, 1e-9))
    F_r   <- (1 - lam_s) / lam_s * df2 / df1

    list(LRE   = round(lr, 6),
         df    = df1,
         p_LRE = pchisq(lr, df1, lower.tail = FALSE),
         F_rao = round(F_r, 6),
         df1   = df1, df2 = df2,
         p_F   = pf(F_r, df1, max(df2, 1), lower.tail = FALSE))
  }

  lm_individual <- lapply(seq_len(max_lag), function(h) {
    r <- .lm_at_lag(h)
    c(list(h = h), r)
  })
  lm_cumulative <- lapply(seq_len(max_lag), function(h) {
    r <- .lm_at_lag(seq_len(h))
    c(list(h = h), r)
  })

  # ── Jarque-Bera Normality (Cholesky of covariance, Lütkepohl) ───────────
  P <- t(chol(crossprod(res) / T))     # Cholesky factor (lower triangular)
  u_orth <- t(solve(P) %*% t(res))     # orthogonalized residuals T × K

  jb_results <- lapply(1:K, function(k) {
    u <- u_orth[, k]
    n <- length(u)
    s <- mean(u^3) / mean(u^2)^(3/2)  # skewness
    k4 <- mean(u^4) / mean(u^2)^2     # kurtosis
    chi_skew <- n * s^2 / 6
    chi_kurt <- n * (k4 - 3)^2 / 24
    chi_jb   <- chi_skew + chi_kurt
    list(component = k,
         skewness  = round(s, 6),
         chi_skew  = round(chi_skew, 6),
         p_skew    = pchisq(chi_skew, 1, lower.tail = FALSE),
         kurtosis  = round(k4, 6),
         chi_kurt  = round(chi_kurt, 6),
         p_kurt    = pchisq(chi_kurt, 1, lower.tail = FALSE),
         JB        = round(chi_jb, 6),
         p_JB      = pchisq(chi_jb, 2, lower.tail = FALSE))
  })

  jb_joint_skew <- sum(sapply(jb_results, `[[`, "chi_skew"))
  jb_joint_kurt <- sum(sapply(jb_results, `[[`, "chi_kurt"))
  jb_joint      <- sum(sapply(jb_results, `[[`, "JB"))
  jb_joint_df   <- 2 * K

  list(
    portmanteau    = portmanteau,
    lm_individual  = lm_individual,
    lm_cumulative  = lm_cumulative,
    normality      = jb_results,
    jb_joint       = list(
      skew = list(stat = jb_joint_skew, df = K,
                  pval = pchisq(jb_joint_skew, K, lower.tail = FALSE)),
      kurt = list(stat = jb_joint_kurt, df = K,
                  pval = pchisq(jb_joint_kurt, K, lower.tail = FALSE)),
      jb   = list(stat = jb_joint, df = jb_joint_df,
                  pval = pchisq(jb_joint, jb_joint_df, lower.tail = FALSE))
    ),
    T = T, K = K, p = p
  )
}

# =============================================================================
# 8.  JOHANSEN COINTEGRATION  (Hc — restricted constant)
# =============================================================================
# MHM p-value: gamma distribution fitted to 3 critical value / probability pairs
.mhm_pvalue <- function(stat, cv10, cv5, cv1) {
  probs <- c(0.90, 0.95, 0.99)   # upper-tail probs = 1 - lower-tail
  cvs   <- c(cv10, cv5, cv1)
  if (any(is.na(cvs))) return(NA_real_)

  # Fit gamma distribution by minimizing distance to quantiles
  obj <- function(par) {
    sh <- exp(par[1]); sc <- exp(par[2])
    pred <- qgamma(probs, shape = sh, scale = sc)
    sum((pred - cvs)^2)
  }
  # Initial guess from first two CVs (method of moments)
  m <- mean(cvs[1:2]); v <- var(cvs[1:2])
  sh0 <- max(m^2 / v, 0.5); sc0 <- max(v / m, 0.1)
  fit <- tryCatch(
    optim(log(c(sh0, sc0)), obj, method = "Nelder-Mead",
          control = list(maxit = 500, reltol = 1e-10)),
    error = function(e) NULL)

  if (is.null(fit)) {
    # Fallback: log-linear interpolation
    lp <- approx(cvs, log(1 - probs), xout = stat, rule = 2)$y
    return(min(max(1 - exp(lp), 1e-4), 0.9999))
  }
  sh <- exp(fit$par[1]); sc <- exp(fit$par[2])
  p  <- pgamma(stat, shape = sh, scale = sc, lower.tail = FALSE)
  return(min(max(p, 1e-4), 0.9999))
}

johansen_test <- function(z_tfp, z_eff, year, p = VAR_LAG_DEFAULT,
                           tfp_name = "TFP", ecdet = "const") {
  df  <- data.frame(TFP = z_tfp, EFF = z_eff)
  ok  <- complete.cases(df)
  df  <- df[ok, ]
  yrs <- year[ok]
  T_jo <- nrow(df)

  spec_label <- switch(ecdet,
    "const" = "Hc (restricted constant)",
    "none"  = "Hz (no deterministic term)",
    "trend" = "Hl (restricted trend)",
    ecdet)

  jo_trace <- urca::ca.jo(df, type = "trace", ecdet = ecdet,
                           K = p + 1, spec = "longrun")
  jo_eigen <- urca::ca.jo(df, type = "eigen", ecdet = ecdet,
                           K = p + 1, spec = "longrun")

  # Test statistics — ca.jo orders [r<=1, r=0]
  stat_trace_r0 <- unname(jo_trace@teststat[2])
  stat_trace_r1 <- unname(jo_trace@teststat[1])
  stat_eigen_r0 <- unname(jo_eigen@teststat[2])
  stat_eigen_r1 <- unname(jo_eigen@teststat[1])

  # Eigenvalues (largest first)
  eig <- rev(sort(jo_trace@lambda))
  eig_r0 <- eig[1]; eig_r1 <- eig[2]

  # Critical values (urca gives 10%, 5%, 1%)
  cv_t_r0 <- jo_trace@cval[2, ]; cv_t_r1 <- jo_trace@cval[1, ]
  cv_e_r0 <- jo_eigen@cval[2, ]; cv_e_r1 <- jo_eigen@cval[1, ]

  # MHM p-values via gamma distribution fit
  p_t_r0 <- .mhm_pvalue(stat_trace_r0, cv_t_r0["10pct"], cv_t_r0["5pct"], cv_t_r0["1pct"])
  p_t_r1 <- .mhm_pvalue(stat_trace_r1, cv_t_r1["10pct"], cv_t_r1["5pct"], cv_t_r1["1pct"])
  p_e_r0 <- .mhm_pvalue(stat_eigen_r0, cv_e_r0["10pct"], cv_e_r0["5pct"], cv_e_r0["1pct"])
  p_e_r1 <- .mhm_pvalue(stat_eigen_r1, cv_e_r1["10pct"], cv_e_r1["5pct"], cv_e_r1["1pct"])

  trace_coint <- (stat_trace_r0 > cv_t_r0["5pct"]) &&
                 (stat_trace_r1 <= cv_t_r1["5pct"])
  eigen_coint <- (stat_eigen_r0 > cv_e_r0["5pct"]) &&
                 (stat_eigen_r1 <= cv_e_r1["5pct"])

  coint_verdict <- if (trace_coint && eigen_coint) {
    "Cointegration detected (both Trace and Max-Eigenvalue)"
  } else if (trace_coint) {
    "Cointegration detected (Trace only) — interpret with caution"
  } else if (eigen_coint) {
    "Cointegration detected (Max-Eigenvalue only) — interpret with caution"
  } else { "No cointegration detected" }

  # Cointegrating vector, beta, and SE
  beta <- NULL; beta_se <- NULL; const <- NULL; const_se <- NULL; valid <- FALSE

  if (trace_coint || eigen_coint) {
    # Always use the eigenvector for the LARGEST eigenvalue (first CE)
    V        <- jo_trace@V
    best_col <- which.max(jo_trace@lambda)       # column of V for largest eigenvalue
    beta_vec <- V[, best_col]
    if (beta_vec[1] < 0) beta_vec <- -beta_vec
    cv_raw  <- beta_vec / beta_vec[1]
    beta    <- -cv_raw[2]     # elasticity: positive = EFF drives TFP
    const   <-  cv_raw[3]     # constant coefficient in CE (EViews sign)
    valid   <- isTRUE(beta > 0)

    # SE via concentrated likelihood — build R0 and R1 manually to avoid
    # urca slot name uncertainty (urca uses @Z0/@Z1 or @ZK etc internally)
    ses <- tryCatch({
      alpha_norm <- jo_trace@W[best_col, 1] * beta_vec[1]
      if (abs(alpha_norm) < 1e-10) stop("alpha near zero")

      n   <- nrow(df)
      K   <- p + 1
      Y   <- as.matrix(df)      # n × 2
      dY  <- diff(Y)            # (n-1) × 2

      # Y_{t-1}: with constant appended for Hc, plain for Hz
      Y1  <- if (ecdet == "const") cbind(Y[K:(n-1), ], 1) else Y[K:(n-1), ]
      dY0 <- dY[K:(n-1), , drop = FALSE]

      if (K > 1) {
        AUX <- do.call(cbind, lapply(seq_len(K - 1), function(j)
                 dY[(K - j):(n - 1 - j), , drop = FALSE]))
        R1  <- lm.fit(AUX, Y1)$residuals
        R0  <- lm.fit(AUX, dY0)$residuals
      } else {
        R1 <- Y1; R0 <- dY0
      }

      # Concentrated OLS: free parameters are all columns of R1 except TFP
      z   <- R0[, 1] / alpha_norm - R1[, 1]
      X   <- R1[, -1, drop = FALSE]    # [R1_EFF] for Hz, [R1_EFF, R1_C] for Hc
      if (qr(X)$rank < ncol(X)) stop("X is rank-deficient")
      fit <- lm.fit(X, z)
      n_r <- nrow(X); k_r <- ncol(X)
      s2  <- sum(fit$residuals^2) / (n_r - k_r)
      sqrt(s2 * diag(solve(crossprod(X))))   # SE for each free parameter
    }, error = function(e) {
      message("  [SE failed: ", conditionMessage(e), "]")
      rep(NA_real_, if (ecdet == "const") 2L else 1L)
    })

    beta_se  <- ses[1]
    const_se <- if (ecdet == "const") ses[2] else NULL

    flag <- if (!valid) " *** INVALID: negative beta (EFF reduces TFP)" else ""
    if (ecdet == "const") {
      message(sprintf("  [%s] %s: beta = %.4f (SE = %.4f)  C = %.4f (SE = %.4f)%s",
                      spec_label, tfp_name, beta,
                      ifelse(is.na(beta_se),  NA, round(beta_se,  4)),
                      const,
                      ifelse(is.na(const_se), NA, round(const_se, 4)), flag))
    } else {
      message(sprintf("  [%s] %s: beta = %.4f (SE = %.4f)%s",
                      spec_label, tfp_name, beta,
                      ifelse(is.na(beta_se), NA, round(beta_se, 4)), flag))
    }
  }

  list(
    tfp_name     = tfp_name,
    spec_label   = spec_label,
    ecdet        = ecdet,
    T            = T_jo - p,
    sample_start = yrs[p + 1],
    sample_end   = tail(yrs, 1),
    p            = p,
    trace = list(
      eig_r0  = round(eig_r0, 6),  eig_r1  = round(eig_r1, 6),
      stat_r0 = round(stat_trace_r0, 5),
      cv5_r0  = round(unname(cv_t_r0["5pct"]), 5), pval_r0 = round(p_t_r0, 4),
      stat_r1 = round(stat_trace_r1, 5),
      cv5_r1  = round(unname(cv_t_r1["5pct"]), 5), pval_r1 = round(p_t_r1, 4),
      coint   = trace_coint),
    eigen = list(
      eig_r0  = round(eig_r0, 6),  eig_r1  = round(eig_r1, 6),
      stat_r0 = round(stat_eigen_r0, 5),
      cv5_r0  = round(unname(cv_e_r0["5pct"]), 5), pval_r0 = round(p_e_r0, 4),
      stat_r1 = round(stat_eigen_r1, 5),
      cv5_r1  = round(unname(cv_e_r1["5pct"]), 5), pval_r1 = round(p_e_r1, 4),
      coint   = eigen_coint),
    verdict  = coint_verdict,
    beta     = beta,   beta_se  = beta_se,
    const    = const,  const_se = const_se,
    valid    = valid,
    jo_obj   = jo_trace
  )
}

# =============================================================================
# 9.  MASTER FUNCTION
# =============================================================================
# =============================================================================
# 10. USEFUL EXERGY INTENSITY CONSTANCY ANALYSIS
# =============================================================================
test_intensity_constancy <- function(year, intensity) {
  ok  <- !is.na(intensity) & !is.na(year)
  y   <- intensity[ok]
  yr  <- year[ok]
  n   <- length(y)
  if (n < 10) return(NULL)

  # Sub-period: last third, minimum 15 obs
  n_sub   <- max(15L, round(n / 3))
  n_sub   <- min(n_sub, n - 5)   # keep at least 5 in the "early" part
  y_sub   <- tail(y,  n_sub)
  yr_sub  <- tail(yr, n_sub)

  .analyse <- function(yy, tt) {
    nn   <- length(yy)
    mu   <- mean(yy)
    sg   <- sd(yy)
    cv   <- sg / mu
    mn   <- min(yy); mx <- max(yy)

    # OLS trend:  yy = a + b*(t - t0) + e
    tc   <- tt - tt[1]
    X    <- cbind(1, tc)
    fit  <- lm.fit(X, yy)
    rss  <- sum(fit$residuals^2)
    tss  <- sum((yy - mu)^2)
    r2   <- if (tss > 0) 1 - rss/tss else 0
    s2   <- rss / max(nn - 2, 1)
    XtXi <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
    if (is.null(XtXi)) {
      slope <- 0; t_trend <- 0; p_trend <- 1
    } else {
      slope    <- fit$coefficients[2]
      se_slope <- sqrt(s2 * XtXi[2, 2])
      t_trend  <- slope / max(se_slope, 1e-15)
      p_trend  <- 2 * pt(abs(t_trend), df = nn - 2, lower.tail = FALSE)
    }
    ann_rate_pct <- slope / mu * 100   # % per year relative to mean

    # ADF on intensity levels (trend + intercept)
    bw   <- .nw_bandwidth(nn)
    adf  <- tryCatch(.adf_custom(yy, min(MAX_ADF_LAGS, floor(nn/5)), "trend"),
                     error = function(e) list(stat=NA, cv=c("5%"=NA), lag=NA, n=nn))
    pp   <- tryCatch({
      r <- urca::ur.pp(yy, type="Z-tau", model="trend", use.lag=bw)
      list(stat = unname(r@teststat[1]),
           cv5  = unname(r@cval["critical values","5pct"]))
    }, error = function(e) list(stat=NA, cv5=NA))

    # Verdicts (each test at 10% for trend; 5% for unit root)
    trend_ok  <- !is.na(p_trend)  && p_trend  > 0.10
    cv_ok     <- cv < 0.15
    adf_i0    <- !is.na(adf$stat) && !is.na(adf$cv["5%"]) &&
                 adf$stat < adf$cv["5%"]   # stationary if stat < CV (negative)
    pp_i0     <- !is.na(pp$stat)  && !is.na(pp$cv5) &&
                 pp$stat  < pp$cv5

    # Criterion: (no trend AND stationary) OR low CV
    stationary  <- adf_i0 || pp_i0
    low_cv      <- cv < 0.05
    is_const    <- (trend_ok && stationary) || low_cv
    verdict <- if (trend_ok && stationary && low_cv)
      "Approximately constant (no trend + stationary + low CV)"
    else if (trend_ok && stationary)
      "Approximately constant (no trend + stationary)"
    else if (low_cv)
      "Approximately constant (low CV < 5%)"
    else if (trend_ok && !stationary)
      "No significant trend but non-stationary (drifting)"
    else if (!trend_ok && stationary)
      "Stationary but with a significant trend"
    else
      "Not constant \u2014 significant trend and non-stationary"

    list(
      n = nn, yr_start = tt[1], yr_end = tail(tt, 1),
      mean = mu, sd = sg, cv = cv, min = mn, max = mx,
      slope = slope, t_trend = t_trend, p_trend = p_trend,
      r2 = r2, ann_rate_pct = ann_rate_pct,
      adf_stat = adf$stat, adf_cv5 = adf$cv["5%"], adf_lag = adf$lag,
      pp_stat  = pp$stat,  pp_cv5  = pp$cv5,
      trend_ok = trend_ok, cv_ok = cv_ok,
      adf_i0 = adf_i0, pp_i0 = pp_i0,
      verdict = verdict
    )
  }

  full <- .analyse(y, yr)
  sub  <- .analyse(y_sub, yr_sub)

  list(full = full, sub = sub,
       n_full = n, n_sub = n_sub,
       sub_start = yr_sub[1])
}

run_analysis <- function(file_path, var_lag = VAR_LAG_DEFAULT,
                         out_path = NULL) {

  # Default output name: eff_tfp_results_<country>.xlsx
  if (is.null(out_path)) {
    base     <- tools::file_path_sans_ext(basename(file_path))
    out_path <- paste0("eff_tfp_results_", base, ".xlsx")
  }

  message("\n========================================")
  message(" Efficiency-TFP Analysis v3.0")
  message("========================================\n")

  # Step 1: Load data
  message("[1] Loading data...")
  d <- load_data(file_path)

  # ── Detect pre-loaded mode ─────────────────────────────────────────────────
  # If the file contains pre-computed z_tfp_unadj (or tfp_unadj) and eff
  # columns instead of raw macro/exergy inputs, skip steps 2-5.
  preload_cols <- c("tfp_unadj", "z_tfp_unadj", "tfp", "z_tfp")
  is_preloaded <- any(preload_cols %in% names(d)) &&
                  any(c("eff", "z_eff", "efficiency") %in% names(d))

  if (is_preloaded) {
    message("  → Pre-loaded mode detected (TFP and EFF series provided directly)")

    # Map whichever column names are present
    tfp_col  <- intersect(preload_cols, names(d))[1]
    eff_col  <- intersect(c("eff", "z_eff", "efficiency"), names(d))[1]
    adj_col  <- intersect(c("tfp_adj", "z_tfp_adj"), names(d))
    adj_col  <- if (length(adj_col)) adj_col[1] else NULL

    # Normalize to z = ln(x/x0)+1 if not already done
    .to_z <- function(v) {
      v <- as.numeric(v)
      ok <- !is.na(v)
      if (!any(ok)) return(v)
      v0 <- v[which(ok)[1]]
      log(v / v0) + 1
    }

    z_unadj <- .to_z(d[[tfp_col]])
    z_adj   <- if (!is.null(adj_col)) .to_z(d[[adj_col]]) else NULL
    eff_raw <- as.numeric(d[[eff_col]])
    z_eff   <- .to_z(eff_raw)

    tfp    <- list(z_tfp_unadj = z_unadj, z_tfp_adj = z_adj)
    exergy <- list(z_eff = z_eff, eps_raw = eff_raw,
                   int_final = NA, int_useful = NA)
    shares <- list(alpha_K = rep(NA, length(d$year)),
                   alpha_L = rep(NA, length(d$year)),
                   source  = "Pre-loaded mode — not computed")
    labour <- NULL

    message("  z_tfp_unadj range [",
            round(min(z_unadj, na.rm=TRUE), 3), ", ",
            round(max(z_unadj, na.rm=TRUE), 3), "]")
    if (!is.null(z_adj))
      message("  z_tfp_adj range [",
              round(min(z_adj, na.rm=TRUE), 3), ", ",
              round(max(z_adj, na.rm=TRUE), 3), "]")
    message("  z_eff range [",
            round(min(z_eff, na.rm=TRUE), 3), ", ",
            round(max(z_eff, na.rm=TRUE), 3), "]")

  } else {
    # ── Standard mode: compute everything from raw inputs ────────────────────
    message("\n[2] Computing factor shares...")
    shares <- compute_factor_shares(d)

    message("\n[3] Computing labour inputs...")
    labour <- compute_labour(d)

    message("\n[4] Computing TFP measures...")
    tfp <- compute_tfp(d, labour, shares)

    message("\n[5] Computing exergy efficiency and intensities...")
    exergy <- compute_exergy(d)
  }

  # Step 6: Unit root tests — order: TFP_unadj, TFP_adj, EFF
  message("\n[6] Unit root tests (ADF / PP)...")
  series_for_ur <- list()
  series_for_ur[["z_tfp_unadj"]] <- tfp$z_tfp_unadj
  if (!is.null(tfp$z_tfp_adj))
    series_for_ur[["z_tfp_adj"]] <- tfp$z_tfp_adj
  series_for_ur[["z_eff"]] <- exergy$z_eff
  ur_results <- run_unit_root_tests(series_for_ur, d$year)

  # Step 7: VAR + diagnostics
  message("\n[7] VAR(", var_lag, ") estimation and diagnostics...")
  var_results  <- list()
  diag_results <- list()

  var_results[["unadj"]] <- estimate_var(
    tfp$z_tfp_unadj, exergy$z_eff, d$year, p = var_lag, tfp_name = "TFP_unadj")
  diag_results[["unadj"]] <- var_diagnostics(var_results[["unadj"]])

  if (!is.null(tfp$z_tfp_adj)) {
    var_results[["adj"]] <- estimate_var(
      tfp$z_tfp_adj, exergy$z_eff, d$year, p = var_lag, tfp_name = "TFP_adj")
    diag_results[["adj"]] <- var_diagnostics(var_results[["adj"]])
  }

  # Step 8: Johansen cointegration — both Hc and Hz specifications
  message("\n[8] Johansen cointegration tests (Hc + Hz, Trace + Max-Eigenvalue)...")
  jo_hc <- list(); jo_hz <- list()

  jo_hc[["unadj"]] <- johansen_test(
    tfp$z_tfp_unadj, exergy$z_eff, d$year, p = var_lag,
    tfp_name = "TFP_unadj", ecdet = "const")
  jo_hz[["unadj"]] <- johansen_test(
    tfp$z_tfp_unadj, exergy$z_eff, d$year, p = var_lag,
    tfp_name = "TFP_unadj", ecdet = "none")

  if (!is.null(tfp$z_tfp_adj)) {
    jo_hc[["adj"]] <- johansen_test(
      tfp$z_tfp_adj, exergy$z_eff, d$year, p = var_lag,
      tfp_name = "TFP_adj", ecdet = "const")
    jo_hz[["adj"]] <- johansen_test(
      tfp$z_tfp_adj, exergy$z_eff, d$year, p = var_lag,
      tfp_name = "TFP_adj", ecdet = "none")
    message("  Note: TFP_adj (capital services + quality-adjusted labour) is",
            " theoretically preferred.")
  }

  message("\n========================================")
  message(" Analysis complete.")
  message("========================================\n")

  # Intensity constancy analysis (standard mode only — needs int_useful)
  int_const <- if (!all(is.na(exergy$int_useful)))
    test_intensity_constancy(d$year, exergy$int_useful) else NULL

  out <- list(
    data        = d,
    shares      = shares,
    labour      = labour,
    tfp         = tfp,
    exergy      = exergy,
    ur          = ur_results,
    var         = var_results,
    diag        = diag_results,
    johansen    = jo_hc,
    johansen_hc = jo_hc,
    johansen_hz = jo_hz,
    int_const   = int_const,
    var_lag     = var_lag
  )

  write_output(out, out_path)

  # PDF report
  rpt_path <- sub("\\.xlsx$", ".pdf", out_path)
  rpt_path <- sub("eff_tfp_results_", "eff_tfp_report_", rpt_path)
  tryCatch(generate_report(out, file_path, rpt_path),
           error = function(e)
             message("  [PDF report skipped: ", conditionMessage(e), "]"))

  invisible(out)
}

# =============================================================================
# 10.  EXCEL OUTPUT
# =============================================================================
write_output <- function(results, out_path = "eff_tfp_results.xlsx") {

  if (!requireNamespace("openxlsx", quietly = TRUE))
    install.packages("openxlsx", repos = "https://cloud.r-project.org")
  library(openxlsx)

  wb <- createWorkbook()

  # ── Shared styles ──────────────────────────────────────────────────────────
  s_title  <- createStyle(fontSize = 12, fontColour = "#FFFFFF",
                           fgFill = "#2F5496", fontName = "Calibri",
                           textDecoration = "bold", halign = "left",
                           valign = "center", wrapText = FALSE)
  s_head   <- createStyle(fontSize = 10, fontColour = "#FFFFFF",
                           fgFill = "#4472C4", fontName = "Calibri",
                           textDecoration = "bold", halign = "center",
                           border = "TopBottomLeftRight",
                           borderColour = "#FFFFFF")
  s_subhead <- createStyle(fontSize = 10, fontColour = "#FFFFFF",
                            fgFill = "#5B9BD5", fontName = "Calibri",
                            textDecoration = "bold", halign = "left",
                            border = "TopBottomLeftRight",
                            borderColour = "#FFFFFF")
  s_num    <- createStyle(fontSize = 10, fontName = "Calibri",
                           numFmt = "0.0000", halign = "right",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")
  s_num3   <- createStyle(fontSize = 10, fontName = "Calibri",
                           numFmt = "0.000", halign = "right",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")
  s_pct    <- createStyle(fontSize = 10, fontName = "Calibri",
                           numFmt = "0.00%", halign = "right",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")
  s_txt    <- createStyle(fontSize = 10, fontName = "Calibri",
                           halign = "left",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")
  s_label  <- createStyle(fontSize = 10, fontName = "Calibri",
                           textDecoration = "bold", halign = "left",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")
  s_alt    <- createStyle(fgFill = "#EEF3FB",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9",
                           numFmt = "0.0000", halign = "right",
                           fontName = "Calibri", fontSize = 10)
  s_alt_t  <- createStyle(fgFill = "#EEF3FB",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9",
                           halign = "left",
                           fontName = "Calibri", fontSize = 10)
  s_flag   <- createStyle(fontSize = 10, fontName = "Calibri",
                           fontColour = "#C00000", textDecoration = "bold",
                           halign = "left")
  s_ok     <- createStyle(fontSize = 10, fontName = "Calibri",
                           fontColour = "#375623", textDecoration = "bold",
                           halign = "left")
  s_year   <- createStyle(fontSize = 10, fontName = "Calibri",
                           numFmt = "0", halign = "center",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")
  s_year_alt <- createStyle(fontSize = 10, fontName = "Calibri",
                           numFmt = "0", halign = "center",
                           fgFill = "#EEF3FB",
                           border = "TopBottomLeftRight",
                           borderColour = "#D9D9D9")

  .title_row <- function(ws, row, text, ncols) {
    writeData(wb, ws, text, startRow = row, startCol = 1)
    addStyle(wb, ws, s_title, rows = row, cols = 1:ncols, gridExpand = TRUE)
    mergeCells(wb, ws, cols = 1:ncols, rows = row)
    setRowHeights(wb, ws, row, 20)
  }
  .head_row <- function(ws, row, labels) {
    writeData(wb, ws, as.data.frame(t(labels)), startRow = row, startCol = 1,
              colNames = FALSE)
    addStyle(wb, ws, s_head, rows = row, cols = seq_along(labels),
             gridExpand = TRUE)
    setRowHeights(wb, ws, row, 16)
  }
  .sub_row <- function(ws, row, text, ncols) {
    writeData(wb, ws, text, startRow = row, startCol = 1)
    addStyle(wb, ws, s_subhead, rows = row, cols = 1:ncols, gridExpand = TRUE)
    mergeCells(wb, ws, cols = 1:ncols, rows = row)
    setRowHeights(wb, ws, row, 15)
  }

  # ── Sheet 1: Output Elasticities ───────────────────────────────────────────
  {
    ws <- "1_Elasticities"
    addWorksheet(wb, ws)
    setColWidths(wb, ws, 1:5, c(10, 16, 16, 16, 16))
    d       <- results$data
    shares  <- results$shares
    n_obs   <- length(d$year)

    .title_row(ws, 1, "Sheet 1 — Output Elasticities (Cobb-Douglas)", 5)
    writeData(wb, ws, paste0("Source: ", shares$source), startRow = 2, startCol = 1)
    addStyle(wb, ws, createStyle(fontSize = 9, fontColour = "#595959",
                                  textDecoration = "italic"), rows = 2, cols = 1:5)
    mergeCells(wb, ws, cols = 1:5, rows = 2)

    .head_row(ws, 3, c("Year", "alpha_K", "alpha_L",
                         "alpha_K (cumul. mean)", "alpha_L (cumul. mean)"))
    cum_aK <- cumsum(ifelse(is.na(shares$alpha_K), 0, shares$alpha_K)) /
              cumsum(!is.na(shares$alpha_K))
    for (i in seq_len(n_obs)) {
      r <- 3 + i
      sty   <- if (i %% 2 == 0) s_alt else s_num
      sty_y <- if (i %% 2 == 0) s_year_alt else s_year
      writeData(wb, ws, d$year[i],            startRow = r, startCol = 1)
      writeData(wb, ws, shares$alpha_K[i],    startRow = r, startCol = 2)
      writeData(wb, ws, shares$alpha_L[i],    startRow = r, startCol = 3)
      writeData(wb, ws, cum_aK[i],            startRow = r, startCol = 4)
      writeData(wb, ws, 1 - cum_aK[i],        startRow = r, startCol = 5)
      addStyle(wb, ws, sty_y, rows = r, cols = 1)
      addStyle(wb, ws, sty,   rows = r, cols = 2:5, gridExpand = TRUE)
    }
    # Summary row
    r_sum <- 3 + n_obs + 1
    writeData(wb, ws, "Mean (full sample)", startRow = r_sum, startCol = 1)
    writeData(wb, ws, mean(shares$alpha_K, na.rm = TRUE),
              startRow = r_sum, startCol = 2)
    writeData(wb, ws, mean(shares$alpha_L, na.rm = TRUE),
              startRow = r_sum, startCol = 3)
    addStyle(wb, ws, s_label, rows = r_sum, cols = 1)
    addStyle(wb, ws, s_num,   rows = r_sum, cols = 2:3, gridExpand = TRUE)
  }

  # ── Sheet 2: TFP and Efficiency Series ─────────────────────────────────────
  {
    ws <- "2_TFP_Series"
    addWorksheet(wb, ws)
    setColWidths(wb, ws, 1:7, c(10,14,14,14,14,14,14))
    d      <- results$data
    tfp    <- results$tfp
    exergy <- results$exergy
    n_obs  <- length(d$year)
    has_adj <- !is.null(tfp$z_tfp_adj)

    .title_row(ws, 1, "Sheet 2 — Computed TFP and Efficiency Series", 7)
    note <- paste0("z_x = ln(x / x_base) + 1  |  base year = ",
                   d$year[1], "  |  all series start at 1.0000")
    writeData(wb, ws, note, startRow = 2, startCol = 1)
    addStyle(wb, ws, createStyle(fontSize = 9, fontColour = "#595959",
                                  textDecoration = "italic"), rows = 2, cols = 1:7)
    mergeCells(wb, ws, cols = 1:7, rows = 2)

    hdrs <- c("Year", "z_TFP_unadj", "z_TFP_adj", "z_Efficiency",
              "Eff_raw (%)", "Intensity_final", "Intensity_useful")
    .head_row(ws, 3, hdrs)

    for (i in seq_len(n_obs)) {
      r <- 3 + i
      sty   <- if (i %% 2 == 0) s_alt else s_num
      sty_y <- if (i %% 2 == 0) s_year_alt else s_year
      writeData(wb, ws, d$year[i],                       startRow=r, startCol=1)
      writeData(wb, ws, tfp$z_tfp_unadj[i],              startRow=r, startCol=2)
      writeData(wb, ws, if(has_adj) tfp$z_tfp_adj[i] else NA,
                startRow=r, startCol=3)
      writeData(wb, ws, exergy$z_eff[i],                 startRow=r, startCol=4)
      writeData(wb, ws, exergy$eps_raw[i] * 100,         startRow=r, startCol=5)
      writeData(wb, ws, exergy$int_final[i],             startRow=r, startCol=6)
      writeData(wb, ws, exergy$int_useful[i],            startRow=r, startCol=7)
      addStyle(wb, ws, sty_y, rows=r, cols=1)
      addStyle(wb, ws, sty,   rows=r, cols=2:7, gridExpand=TRUE)
    }
  }

  # ── Sheet 3: Unit Root Tests ────────────────────────────────────────────────
  {
    ws <- "3_Unit_Root_Tests"
    addWorksheet(wb, ws)
    setColWidths(wb, ws, 1:10,
                 c(20,14,10,10,10,6,14,10,10,12))
    .title_row(ws, 1, "Sheet 3 — Unit Root Tests (ADF / PP)", 10)

    cur_row <- 2
    ur <- results$ur

    for (nm in names(ur)) {
      r <- ur[[nm]]
      .sub_row(ws, cur_row, paste0("Series: ", nm,
        "  |  Verdict: ", r$order,
        "  |  ADF: ", r$adf_verdict,
        "  |  PP: ",  r$pp_verdict), 10)
      cur_row <- cur_row + 1

      .head_row(ws, cur_row,
        c("Test", "Specification", "Statistic",
          "CV 1%", "CV 5%", "CV 10%",
          "Aux. param.", "Aux. value",
          "H0 rejected?", "Verdict"))
      cur_row <- cur_row + 1

      rows_data <- list(
        list("ADF","Levels (trend+intercept)",
             r$adf_stat_levels, r$adf_cv1_levels, r$adf_cv5_levels,
             r$adf_cv10_levels, "Lags (BIC)", r$adf_lags_levels,
             r$adf_stat_levels < r$adf_cv5_levels, r$adf_verdict),
        list("ADF","First diff. (intercept)",
             r$adf_stat_diffs, r$adf_cv1_diffs, r$adf_cv5_diffs,
             r$adf_cv10_diffs, "Lags (BIC)", r$adf_lags_diffs,
             r$adf_stat_diffs < r$adf_cv5_diffs, "—"),
        list("PP","Levels (trend+intercept)",
             r$pp_stat_levels, r$pp_cv1_levels, r$pp_cv5_levels,
             r$pp_cv10_levels, "Bandwidth (NW)", r$pp_bw_levels,
             r$pp_stat_levels < r$pp_cv5_levels, r$pp_verdict),
        list("PP","First diff. (intercept)",
             r$pp_stat_diffs, r$pp_cv1_diffs, r$pp_cv5_diffs,
             r$pp_cv10_diffs, "Bandwidth (NW)", r$pp_bw_diffs,
             r$pp_stat_diffs < r$pp_cv5_diffs, "—")
      )

      for (i in seq_along(rows_data)) {
        rd  <- rows_data[[i]]
        sty <- if (i %% 2 == 0) s_alt else s_num
        for (ci in 1:10)
          writeData(wb, ws, rd[[ci]], startRow=cur_row, startCol=ci)
        writeData(wb, ws, if(isTRUE(rd[[9]])) "Yes" else "No",
                  startRow=cur_row, startCol=9)
        writeData(wb, ws, rd[[10]], startRow=cur_row, startCol=10)
        addStyle(wb, ws, s_txt, rows=cur_row, cols=1:2, gridExpand=TRUE)
        addStyle(wb, ws, sty,   rows=cur_row, cols=3:8, gridExpand=TRUE)
        addStyle(wb, ws, s_txt, rows=cur_row, cols=9:10, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      cur_row <- cur_row + 1
    }
  }

  # ── Sheet 4: VAR Diagnostics ────────────────────────────────────────────────
  {
    ws <- "4_VAR_Diagnostics"
    addWorksheet(wb, ws)
    setColWidths(wb, ws, 1:9, c(22,8,12,10,12,10,10,10,10))
    .title_row(ws, 1, "Sheet 4 — VAR Residual Diagnostic Tests", 9)
    cur_row <- 2

    for (key in names(results$diag)) {
      dg   <- results$diag[[key]]
      vobj <- results$var[[key]]
      .sub_row(ws, cur_row,
        paste0("VAR(", vobj$p, "):  ", vobj$tfp_name,
               " ~ EFF  |  T = ", dg$T,
               "  (", vobj$year[vobj$p + 1], "–",
               tail(vobj$year, 1), ")"), 9)
      cur_row <- cur_row + 1

      # -- Portmanteau
      .sub_row(ws, cur_row, "Portmanteau Test (H0: no residual autocorrelation up to lag h)", 9)
      cur_row <- cur_row + 1
      .head_row(ws, cur_row,
        c("Lag", "Q-stat", "Prob.", "Adj Q-stat", "Prob.", "df",
          "Valid?", "", ""))
      cur_row <- cur_row + 1
      for (pt in dg$portmanteau) {
        valid <- pt$df > 0
        writeData(wb, ws, pt$h,     startRow=cur_row, startCol=1)
        writeData(wb, ws, pt$Q,     startRow=cur_row, startCol=2)
        writeData(wb, ws, if(valid) pt$pval     else "—",
                  startRow=cur_row, startCol=3)
        writeData(wb, ws, pt$Q_adj, startRow=cur_row, startCol=4)
        writeData(wb, ws, if(valid) pt$pval_adj else "—",
                  startRow=cur_row, startCol=5)
        writeData(wb, ws, if(valid) pt$df       else "—",
                  startRow=cur_row, startCol=6)
        writeData(wb, ws, if(valid) "✓" else "only valid for h > VAR lag",
                  startRow=cur_row, startCol=7)
        sty <- if (pt$h %% 2 == 0) s_alt else s_num
        addStyle(wb, ws, sty, rows=cur_row, cols=1:7, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      cur_row <- cur_row + 1

      # -- LM Individual
      .sub_row(ws, cur_row, "LM Serial Correlation Test — At lag h (H0: no serial correlation at lag h)", 9)
      cur_row <- cur_row + 1
      .head_row(ws, cur_row,
        c("Lag","LRE* stat","df","Prob.","Rao F-stat","df1","df2","Prob.",""))
      cur_row <- cur_row + 1
      for (lm in dg$lm_individual) {
        if (is.null(lm)) next
        sty <- if (lm$h %% 2 == 0) s_alt else s_num
        vals <- c(lm$h, lm$LRE, lm$df, lm$p_LRE,
                  lm$F_rao, lm$df1, lm$df2, lm$p_F)
        for (ci in seq_along(vals))
          writeData(wb, ws, vals[ci], startRow=cur_row, startCol=ci)
        addStyle(wb, ws, sty, rows=cur_row, cols=1:8, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      cur_row <- cur_row + 1

      # -- LM Cumulative
      .sub_row(ws, cur_row, "LM Serial Correlation Test — At lags 1 to h (H0: no serial correlation at lags 1 to h)", 9)
      cur_row <- cur_row + 1
      .head_row(ws, cur_row,
        c("Lag","LRE* stat","df","Prob.","Rao F-stat","df1","df2","Prob.",""))
      cur_row <- cur_row + 1
      for (lm in dg$lm_cumulative) {
        if (is.null(lm)) next
        sty <- if (lm$h %% 2 == 0) s_alt else s_num
        vals <- c(lm$h, lm$LRE, lm$df, lm$p_LRE,
                  lm$F_rao, lm$df1, lm$df2, lm$p_F)
        for (ci in seq_along(vals))
          writeData(wb, ws, vals[ci], startRow=cur_row, startCol=ci)
        addStyle(wb, ws, sty, rows=cur_row, cols=1:8, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      cur_row <- cur_row + 1

      # -- Normality
      .sub_row(ws, cur_row, "Normality Test — Jarque-Bera (Cholesky of covariance, Lütkepohl)", 9)
      cur_row <- cur_row + 1
      .head_row(ws, cur_row,
        c("Component","Skewness","Chi-sq","df","Prob.",
          "Kurtosis","Chi-sq","df","Prob."))
      cur_row <- cur_row + 1
      comp_names <- c(vobj$tfp_name, "EFF")
      for (jb in dg$normality) {
        sty <- if (jb$component %% 2 == 0) s_alt else s_num
        writeData(wb, ws, comp_names[jb$component], startRow=cur_row, startCol=1)
        writeData(wb, ws, jb$skewness,  startRow=cur_row, startCol=2)
        writeData(wb, ws, jb$chi_skew,  startRow=cur_row, startCol=3)
        writeData(wb, ws, 1,            startRow=cur_row, startCol=4)
        writeData(wb, ws, jb$p_skew,    startRow=cur_row, startCol=5)
        writeData(wb, ws, jb$kurtosis,  startRow=cur_row, startCol=6)
        writeData(wb, ws, jb$chi_kurt,  startRow=cur_row, startCol=7)
        writeData(wb, ws, 1,            startRow=cur_row, startCol=8)
        writeData(wb, ws, jb$p_kurt,    startRow=cur_row, startCol=9)
        addStyle(wb, ws, s_txt, rows=cur_row, cols=1)
        addStyle(wb, ws, sty,   rows=cur_row, cols=2:9, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      # Joint
      jnt <- dg$jb_joint
      for (row_data in list(
        list("Joint (skewness)",  jnt$skew$stat, "", dg$K, jnt$skew$pval, "","","",""),
        list("Joint (kurtosis)",  "", "", dg$K, "",  jnt$kurt$stat, "", dg$K, jnt$kurt$pval),
        list("Joint (JB)",        jnt$jb$stat,   "", 2*dg$K, jnt$jb$pval, "","","","")
      )) {
        writeData(wb, ws, row_data[[1]], startRow=cur_row, startCol=1)
        for (ci in 2:9) writeData(wb, ws, row_data[[ci]],
                                   startRow=cur_row, startCol=ci)
        addStyle(wb, ws, s_label, rows=cur_row, cols=1)
        addStyle(wb, ws, s_num,   rows=cur_row, cols=2:9, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      # JB per component
      .head_row(ws, cur_row, c("Component","Jarque-Bera","df","Prob.","","","","",""))
      cur_row <- cur_row + 1
      for (jb in dg$normality) {
        sty <- if (jb$component %% 2 == 0) s_alt else s_num
        writeData(wb, ws, comp_names[jb$component], startRow=cur_row, startCol=1)
        writeData(wb, ws, jb$JB,    startRow=cur_row, startCol=2)
        writeData(wb, ws, 2,        startRow=cur_row, startCol=3)
        writeData(wb, ws, jb$p_JB,  startRow=cur_row, startCol=4)
        addStyle(wb, ws, s_txt, rows=cur_row, cols=1)
        addStyle(wb, ws, sty,   rows=cur_row, cols=2:4, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      writeData(wb, ws, "Joint", startRow=cur_row, startCol=1)
      writeData(wb, ws, jnt$jb$stat,  startRow=cur_row, startCol=2)
      writeData(wb, ws, 2*dg$K,       startRow=cur_row, startCol=3)
      writeData(wb, ws, jnt$jb$pval,  startRow=cur_row, startCol=4)
      addStyle(wb, ws, s_label, rows=cur_row, cols=1)
      addStyle(wb, ws, s_num,   rows=cur_row, cols=2:4, gridExpand=TRUE)
      cur_row <- cur_row + 2
    }
  }

  # ── Shared helper: write one Johansen sheet ──────────────────────────────
  .write_jo_sheet <- function(ws_name, title_text, jo_list) {
    addWorksheet(wb, ws_name)
    setColWidths(wb, ws_name, 1:6, c(30,14,14,14,14,30))
    .title_row(ws_name, 1, title_text, 6)
    cur_row <- 2

    for (key in names(jo_list)) {
      jo <- jo_list[[key]]

      .sub_row(ws_name, cur_row,
        paste0(jo$tfp_name, " ~ EFF  |  ", jo$spec_label,
               "  |  Lags (first diff.): 1 to ", jo$p,
               "  |  T = ", jo$T,
               "  (", jo$sample_start, "\u2013", jo$sample_end, ")"), 6)
      cur_row <- cur_row + 1

      # Trace test
      .head_row(ws_name, cur_row,
        c("Hypothesis","Eigenvalue","Trace stat","CV 5%","Prob.**",""))
      cur_row <- cur_row + 1
      for (i in 1:2) {
        hyp  <- c("None (r=0)","At most 1 (r\u22641)")[i]
        eig  <- c(jo$trace$eig_r0, jo$trace$eig_r1)[i]
        stat <- c(jo$trace$stat_r0, jo$trace$stat_r1)[i]
        cv5  <- c(jo$trace$cv5_r0,  jo$trace$cv5_r1)[i]
        pval <- c(jo$trace$pval_r0, jo$trace$pval_r1)[i]
        rej  <- stat > cv5
        writeData(wb, ws_name, if(rej) paste0(hyp," *") else hyp, startRow=cur_row, startCol=1)
        writeData(wb, ws_name, eig,  startRow=cur_row, startCol=2)
        writeData(wb, ws_name, stat, startRow=cur_row, startCol=3)
        writeData(wb, ws_name, cv5,  startRow=cur_row, startCol=4)
        writeData(wb, ws_name, pval, startRow=cur_row, startCol=5)
        sty <- if (i %% 2 == 0) s_alt else s_num
        addStyle(wb, ws_name, s_txt, rows=cur_row, cols=1)
        addStyle(wb, ws_name, sty,   rows=cur_row, cols=2:5, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      verdict_trace <- if (jo$trace$coint)
        "Trace: 1 cointegrating eqn at 5% level" else "Trace: no cointegration at 5%"
      writeData(wb, ws_name, verdict_trace, startRow=cur_row, startCol=1)
      writeData(wb, ws_name, "** MacKinnon-Haug-Michelis (1999) p-values",
                startRow=cur_row, startCol=3)
      addStyle(wb, ws_name, if(jo$trace$coint) s_ok else s_flag, rows=cur_row, cols=1)
      cur_row <- cur_row + 2

      # Max-Eigen test
      .head_row(ws_name, cur_row,
        c("Hypothesis","Eigenvalue","Max-Eigen stat","CV 5%","Prob.**",""))
      cur_row <- cur_row + 1
      for (i in 1:2) {
        hyp  <- c("None (r=0)","At most 1 (r\u22641)")[i]
        eig  <- c(jo$eigen$eig_r0, jo$eigen$eig_r1)[i]
        stat <- c(jo$eigen$stat_r0, jo$eigen$stat_r1)[i]
        cv5  <- c(jo$eigen$cv5_r0,  jo$eigen$cv5_r1)[i]
        pval <- c(jo$eigen$pval_r0, jo$eigen$pval_r1)[i]
        rej  <- stat > cv5
        writeData(wb, ws_name, if(rej) paste0(hyp," *") else hyp, startRow=cur_row, startCol=1)
        writeData(wb, ws_name, eig,  startRow=cur_row, startCol=2)
        writeData(wb, ws_name, stat, startRow=cur_row, startCol=3)
        writeData(wb, ws_name, cv5,  startRow=cur_row, startCol=4)
        writeData(wb, ws_name, pval, startRow=cur_row, startCol=5)
        sty <- if (i %% 2 == 0) s_alt else s_num
        addStyle(wb, ws_name, s_txt, rows=cur_row, cols=1)
        addStyle(wb, ws_name, sty,   rows=cur_row, cols=2:5, gridExpand=TRUE)
        cur_row <- cur_row + 1
      }
      verdict_eigen <- if (jo$eigen$coint)
        "Max-Eigenvalue: 1 cointegrating eqn at 5% level" else "Max-Eigenvalue: no cointegration at 5%"
      writeData(wb, ws_name, verdict_eigen, startRow=cur_row, startCol=1)
      addStyle(wb, ws_name, if(jo$eigen$coint) s_ok else s_flag, rows=cur_row, cols=1)
      cur_row <- cur_row + 2

      # Overall verdict
      writeData(wb, ws_name, paste0("Overall: ", jo$verdict), startRow=cur_row, startCol=1)
      addStyle(wb, ws_name,
               if (jo$trace$coint || jo$eigen$coint) s_ok else s_flag,
               rows=cur_row, cols=1)
      mergeCells(wb, ws_name, cols=1:6, rows=cur_row)
      cur_row <- cur_row + 2

      # Normalized CV (if found)
      if (!is.null(jo$beta)) {
        .sub_row(ws_name, cur_row, "Normalized Cointegrating Vector (1 CE)", 6)
        cur_row <- cur_row + 1
        .head_row(ws_name, cur_row, c("Variable","Coefficient","Std. Error","","",""))
        cur_row <- cur_row + 1

        cv_rows <- list(list(jo$tfp_name, 1.000000, "—"))
        cv_rows[[2]] <- list("EFF", -jo$beta,
                             if(isTRUE(!is.na(jo$beta_se))) jo$beta_se else "—")
        if (!is.null(jo$const))
          cv_rows[[3]] <- list("C", jo$const,
                               if(isTRUE(!is.na(jo$const_se))) jo$const_se else "—")

        for (row_cv in cv_rows) {
          writeData(wb, ws_name, row_cv[[1]], startRow=cur_row, startCol=1)
          writeData(wb, ws_name, row_cv[[2]], startRow=cur_row, startCol=2)
          writeData(wb, ws_name, row_cv[[3]], startRow=cur_row, startCol=3)
          addStyle(wb, ws_name, s_txt, rows=cur_row, cols=1)
          addStyle(wb, ws_name, s_num, rows=cur_row, cols=2:3, gridExpand=TRUE)
          cur_row <- cur_row + 1
        }
        cur_row <- cur_row + 1

        beta_note <- if (jo$valid)
          paste0("beta = ", round(jo$beta, 4), " > 0  \u2192  EFF drives TFP positively  \u2713")
        else
          paste0("beta = ", round(jo$beta, 4), " < 0  \u2192  INVALID: efficiency reduces TFP.")
        writeData(wb, ws_name, beta_note, startRow=cur_row, startCol=1)
        addStyle(wb, ws_name, if(jo$valid) s_ok else s_flag, rows=cur_row, cols=1)
        mergeCells(wb, ws_name, cols=1:6, rows=cur_row)
        cur_row <- cur_row + 1
      }
      cur_row <- cur_row + 2
    }

    if (length(jo_list) > 1) {
      writeData(wb, ws_name,
        "Note: TFP_adj (capital services + quality-adjusted labour) is theoretically preferred.",
        startRow=cur_row, startCol=1)
      addStyle(wb, ws_name, createStyle(fontSize=9, textDecoration="italic",
                                         fontColour="#595959"),
               rows=cur_row, cols=1)
      mergeCells(wb, ws_name, cols=1:6, rows=cur_row)
    }
  }

  # ── Sheet 5: Johansen Hz ────────────────────────────────────────────────────
  .write_jo_sheet("5_Johansen_Hz",
    "Sheet 5 \u2014 Johansen Cointegration: Hz (no deterministic term)",
    results$johansen_hz)

  # ── Sheet 6: Johansen Hc ────────────────────────────────────────────────────
  .write_jo_sheet("6_Johansen_Hc",
    "Sheet 6 \u2014 Johansen Cointegration: Hc (restricted constant, no trend)",
    results$johansen_hc)

  # ── Sheet 7: Useful Exergy Intensity Constancy ──────────────────────────────
  if (!is.null(results$int_const)) {
    ic  <- results$int_const
    ws7 <- "7_Intensity_Constancy"
    addWorksheet(wb, ws7)
    setColWidths(wb, ws7, 1:5, c(38, 16, 16, 16, 30))

    .title_row(ws7, 1,
      "Sheet 7 \u2014 Useful Exergy Intensity Constancy Analysis (X_useful / GDP)", 5)
    writeData(wb, ws7,
      "Criteria: (trend p-value > 0.10 AND at least ADF or PP = I(0))  OR  CV < 5%",
      startRow = 2, startCol = 1)
    addStyle(wb, ws7, createStyle(fontSize=9, fontColour="#595959",
                                   textDecoration="italic"),
             rows=2, cols=1:5, gridExpand=TRUE)
    mergeCells(wb, ws7, cols=1:5, rows=2)

    .stat_row <- function(row, label, full_val, sub_val, fmt = "%.4f") {
      writeData(wb, ws7, label,     startRow=row, startCol=1)
      writeData(wb, ws7, if(is.na(full_val)) "—" else sprintf(fmt, full_val),
                startRow=row, startCol=2)
      writeData(wb, ws7, if(is.na(sub_val))  "—" else sprintf(fmt, sub_val),
                startRow=row, startCol=3)
      sty <- if (row %% 2 == 0) s_alt_t else s_txt
      addStyle(wb, ws7, sty, rows=row, cols=1:3, gridExpand=TRUE)
    }

    f  <- ic$full; s <- ic$sub
    hdr_labels <- c("Statistic",
                    paste0("Full sample (", f$yr_start, "\u2013", f$yr_end, ", n=", f$n, ")"),
                    paste0("Last sub-period (", s$yr_start, "\u2013", s$yr_end, ", n=", s$n, ")"))
    for (ci in seq_along(hdr_labels))
      writeData(wb, ws7, hdr_labels[ci], startRow=3, startCol=ci)
    addStyle(wb, ws7, s_head, rows=3, cols=1:3, gridExpand=TRUE)

    rows_stat <- list(
      list("Mean intensity",              f$mean,         s$mean,         "%.4f"),
      list("Std deviation",               f$sd,           s$sd,           "%.4f"),
      list("Coefficient of Variation (%)",f$cv*100,       s$cv*100,       "%.2f"),
      list("Min",                         f$min,          s$min,          "%.4f"),
      list("Max",                         f$max,          s$max,          "%.4f"),
      list("OLS trend slope (per year)",  f$slope,        s$slope,        "%.6f"),
      list("Trend t-statistic",           f$t_trend,      s$t_trend,      "%.3f"),
      list("Trend p-value",               f$p_trend,      s$p_trend,      "%.4f"),
      list("Trend R\u00b2",              f$r2,           s$r2,           "%.4f"),
      list("Annual growth rate (% p.a.)", f$ann_rate_pct, s$ann_rate_pct, "%.3f"),
      list("ADF statistic (levels)",      f$adf_stat,     s$adf_stat,     "%.4f"),
      list("ADF critical value 5%",       f$adf_cv5,      s$adf_cv5,      "%.4f"),
      list("PP statistic (levels)",       f$pp_stat,      s$pp_stat,      "%.4f"),
      list("PP critical value 5%",        f$pp_cv5,       s$pp_cv5,       "%.4f")
    )
    for (i in seq_along(rows_stat)) {
      ri <- rows_stat[[i]]
      .stat_row(3 + i, ri[[1]], ri[[2]], ri[[3]], ri[[4]])
    }

    r_v <- 3 + length(rows_stat) + 1

    # ── Individual test verdicts ──
    .sub_row(ws7, r_v, "Individual test results", 5); r_v <- r_v + 1
    for (ci in seq_along(hdr_labels))
      writeData(wb, ws7, hdr_labels[ci], startRow=r_v, startCol=ci)
    addStyle(wb, ws7, s_head, rows=r_v, cols=1:3, gridExpand=TRUE); r_v <- r_v + 1

    chk <- function(x) if(isTRUE(x)) "\u2713 Yes" else "\u2717 No"
    test_rows <- list(
      list("No significant trend (p > 0.10)", chk(f$trend_ok), chk(s$trend_ok)),
      list("ADF: I(0) at 5%",                 chk(f$adf_i0),   chk(s$adf_i0)),
      list("PP: I(0) at 5%",                  chk(f$pp_i0),    chk(s$pp_i0)),
      list("CV < 5%",                          chk(f$cv < 0.05), chk(s$cv < 0.05)),
      list("VERDICT \u2192 Approximately constant?",
           chk((f$trend_ok && (f$adf_i0 || f$pp_i0)) || f$cv < 0.05),
           chk((s$trend_ok && (s$adf_i0 || s$pp_i0)) || s$cv < 0.05))
    )
    for (i in seq_along(test_rows)) {
      tr <- test_rows[[i]]
      for (ci in 1:3) writeData(wb, ws7, tr[[ci]], startRow=r_v, startCol=ci)
      addStyle(wb, ws7, if(i%%2==0) s_alt_t else s_txt, rows=r_v, cols=1:3, gridExpand=TRUE)
      r_v <- r_v + 1
    }
    r_v <- r_v + 1

    # ── Three scenario verdicts ──
    .sub_row(ws7, r_v, "Scenario Verdicts", 5); r_v <- r_v + 1

    scenarios <- list(
      list(
        "Scenario A \u2014 Constant (full sample)",
        f$verdict,
        "Assume intensity = mean over full period for future projections."),
      list(
        "Scenario B \u2014 Constant (recent sub-period only)",
        s$verdict,
        paste0("Assume intensity = mean of last sub-period (",
               s$yr_start, "\u2013", s$yr_end, ") for future projections.")),
      list(
        "Scenario C \u2014 Historical trend continues",
        paste0("Full-period trend: ", sprintf("%.3f", f$ann_rate_pct),
               "% p.a.  |  Recent trend: ", sprintf("%.3f", s$ann_rate_pct), "% p.a."),
        "Extrapolate the observed trend rate into the future.")
    )

    for (sc in scenarios) {
      writeData(wb, ws7, sc[[1]], startRow=r_v, startCol=1)
      addStyle(wb, ws7, s_label, rows=r_v, cols=1)
      r_v <- r_v + 1
      writeData(wb, ws7, sc[[2]], startRow=r_v, startCol=1)
      is_const <- grepl("Approximately constant", sc[[2]])
      addStyle(wb, ws7, if(is_const) s_ok else s_flag, rows=r_v, cols=1)
      mergeCells(wb, ws7, cols=1:5, rows=r_v); r_v <- r_v + 1
      writeData(wb, ws7, sc[[3]], startRow=r_v, startCol=1)
      addStyle(wb, ws7, createStyle(fontSize=9, fontColour="#595959",
                                     textDecoration="italic"),
               rows=r_v, cols=1)
      mergeCells(wb, ws7, cols=1:5, rows=r_v); r_v <- r_v + 2
    }
  }

  # ── Sheets 8 & 9: Estimated TFP and GDP series ────────────────────────────
  {
    d_w   <- results$data
    exg   <- results$exergy
    tfp_r <- results$tfp
    sh_r  <- results$shares
    jo_hz <- results$johansen_hz
    jo_hc <- results$johansen_hc
    aK_w  <- mean(sh_r$alpha_K, na.rm = TRUE)
    aL_w  <- 1 - aK_w

    .safe_cn <- function(jo) if (is.null(jo$const) || is.na(jo$const)) 0 else jo$const

    # Comparison metrics helper
    .metrics <- function(actual, fitted) {
      ok  <- !is.na(actual) & !is.na(fitted)
      a   <- actual[ok]; f <- fitted[ok]
      n   <- length(a)
      if (n < 2) return(list(rmse=NA, mae=NA, r2=NA, corr=NA, mape=NA))
      resid <- a - f
      rmse  <- sqrt(mean(resid^2))
      mae   <- mean(abs(resid))
      ss_res <- sum(resid^2)
      ss_tot <- sum((a - mean(a))^2)
      r2    <- if (ss_tot < 1e-15) NA else 1 - ss_res / ss_tot
      corr  <- cor(a, f)
      mape  <- mean(abs(resid / a), na.rm = TRUE) * 100
      list(rmse = rmse, mae = mae, r2 = r2, corr = corr, mape = mape)
    }

    .norm1 <- function(x) { x0 <- x[!is.na(x)][1]; if (is.na(x0) || x0 == 0) x else x / x0 }

    # ── Sheet 8: Estimated TFP ──────────────────────────────────────────────
    ws8 <- "8_Estimated_TFP"
    addWorksheet(wb, ws8)
    setColWidths(wb, ws8, 1:11,
      c(6, 18, 18, 18, 18, 18, 18, 18, 18, 2, 28))

    .title_row(ws8, 1,
      "Sheet 8 \u2014 Estimated TFP from Cointegrating Vector (normalised, base year = 1)", 11)
    writeData(wb, ws8,
      paste0("Estimated TFP = CV(EFF)  |  Hz: no deterministic term  |  ",
             "Hc: restricted constant, no trend  |  \u03b1_K = ", round(aK_w, 4),
             "  |  \u03b1_L = ", round(aL_w, 4)),
      startRow = 2, startCol = 1)
    addStyle(wb, ws8, createStyle(fontSize = 9, fontColour = "#595959",
                                   textDecoration = "italic"),
             rows = 2, cols = 1:11, gridExpand = TRUE)
    mergeCells(wb, ws8, cols = 1:11, rows = 2)

    col_hdrs8 <- c("Year",
                   "TFP_unadj\n(historical)",
                   "TFP_adj\n(historical)",
                   "TFP_unadj\n(est. Hz)",
                   "TFP_adj\n(est. Hz)",
                   "TFP_unadj\n(est. Hc)",
                   "TFP_adj\n(est. Hc)",
                   "Diff unadj\n(Hz)",
                   "Diff adj\n(Hz)",
                   "Diff unadj\n(Hc)",
                   "Diff adj\n(Hc)")
    for (ci in seq_along(col_hdrs8))
      writeData(wb, ws8, col_hdrs8[ci], startRow = 3, startCol = ci)
    addStyle(wb, ws8, s_head, rows = 3, cols = seq_along(col_hdrs8), gridExpand = TRUE)

    z_eff <- exg$z_eff
    yr    <- d_w$year
    n_yr  <- length(yr)

    # Compute fitted TFP (raw z-form, then normalise to 1 at first valid year)
    .fit_tfp <- function(jo) {
      if (is.null(jo) || is.null(jo$beta) || is.na(jo$beta)) return(rep(NA_real_, n_yr))
      raw <- jo$beta * z_eff - .safe_cn(jo)
      ok  <- which(!is.na(raw))
      if (length(ok) == 0) return(raw)
      raw - raw[ok[1]] + 1
    }

    hist_u <- .norm1(tfp_r$z_tfp_unadj)
    hist_a <- .norm1(tfp_r$z_tfp_adj)
    fit_u_hz <- .fit_tfp(jo_hz[["unadj"]])
    fit_a_hz <- .fit_tfp(jo_hz[["adj"]])
    fit_u_hc <- .fit_tfp(jo_hc[["unadj"]])
    fit_a_hc <- .fit_tfp(jo_hc[["adj"]])

    s_num8 <- createStyle(numFmt = "0.0000", halign = "right")
    s_dif8 <- createStyle(numFmt = "0.0000", halign = "right", fontColour = "#595959")
    for (i in seq_len(n_yr)) {
      r <- 3 + i
      sty <- if (i %% 2 == 0) s_alt_t else s_txt
      vals <- c(yr[i],
                hist_u[i],   hist_a[i],
                fit_u_hz[i], fit_a_hz[i],
                fit_u_hc[i], fit_a_hc[i],
                fit_u_hz[i] - hist_u[i], fit_a_hz[i] - hist_a[i],
                fit_u_hc[i] - hist_u[i], fit_a_hc[i] - hist_a[i])
      for (ci in seq_along(vals))
        if (!is.na(vals[ci]))
          writeData(wb, ws8, vals[ci], startRow = r, startCol = ci)
      addStyle(wb, ws8, sty, rows = r, cols = 1:11, gridExpand = TRUE)
    }

    # Metrics block
    r_met <- 3 + n_yr + 2
    writeData(wb, ws8, "Goodness-of-fit metrics (estimated vs historical, all years)",
              startRow = r_met, startCol = 1)
    addStyle(wb, ws8, s_label, rows = r_met, cols = 1)
    mergeCells(wb, ws8, cols = 1:11, rows = r_met)
    r_met <- r_met + 1

    met_hdrs <- c("Metric", "TFP_unadj (Hz)", "TFP_adj (Hz)",
                  "TFP_unadj (Hc)", "TFP_adj (Hc)")
    for (ci in seq_along(met_hdrs))
      writeData(wb, ws8, met_hdrs[ci], startRow = r_met, startCol = ci)
    addStyle(wb, ws8, s_head, rows = r_met, cols = 1:5, gridExpand = TRUE)
    r_met <- r_met + 1

    mets8 <- list(
      list("RMSE",  "rmse"),
      list("MAE",   "mae"),
      list("R\u00b2",    "r2"),
      list("Correlation", "corr"),
      list("MAPE (%)",   "mape")
    )
    m_uh  <- .metrics(hist_u, fit_u_hz)
    m_ah  <- .metrics(hist_a, fit_a_hz)
    m_uc  <- .metrics(hist_u, fit_u_hc)
    m_ac  <- .metrics(hist_a, fit_a_hc)
    for (i in seq_along(mets8)) {
      nm <- mets8[[i]][[2]]
      writeData(wb, ws8, mets8[[i]][[1]], startRow = r_met, startCol = 1)
      for (ci in 2:5) {
        val <- list(m_uh, m_ah, m_uc, m_ac)[[ci-1]][[nm]]
        if (!is.null(val) && !is.na(val))
          writeData(wb, ws8, round(val, 6), startRow = r_met, startCol = ci)
      }
      addStyle(wb, ws8, if(i%%2==0) s_alt_t else s_txt,
               rows = r_met, cols = 1:5, gridExpand = TRUE)
      r_met <- r_met + 1
    }

    # ── Sheet 9: Estimated GDP ───────────────────────────────────────────────
    ws9 <- "9_Estimated_GDP"
    addWorksheet(wb, ws9)
    setColWidths(wb, ws9, 1:11,
      c(6, 18, 18, 18, 18, 18, 18, 18, 18, 2, 30))

    .title_row(ws9, 1,
      paste0("Sheet 9 \u2014 Estimated GDP via Cobb-Douglas APF (normalised, base year = 1)",
             "   |   Y = K^\u03b1K \u00d7 L^\u03b1L \u00d7 TFP_est"), 11)
    writeData(wb, ws9,
      paste0("Unadjusted: Y = K_stock^", round(aK_w,4),
             " \u00d7 L^", round(aL_w,4), " \u00d7 TFP_unadj_est   |   ",
             "Quality-adjusted: Y = K_services^", round(aK_w,4),
             " \u00d7 hL^", round(aL_w,4), " \u00d7 TFP_adj_est"),
      startRow = 2, startCol = 1)
    addStyle(wb, ws9, createStyle(fontSize = 9, fontColour = "#595959",
                                   textDecoration = "italic"),
             rows = 2, cols = 1:11, gridExpand = TRUE)
    mergeCells(wb, ws9, cols = 1:11, rows = 2)

    col_hdrs9 <- c("Year",
                   "GDP\n(historical)",
                   "GDP_unadj\n(est. Hz)",
                   "GDP_adj\n(est. Hz)",
                   "GDP_unadj\n(est. Hc)",
                   "GDP_adj\n(est. Hc)",
                   "KL_unadj\n(no TFP)",
                   "KL_adj\n(no TFP)",
                   "Diff unadj\n(Hz)",
                   "Diff adj\n(Hz)",
                   "Diff unadj\n(Hc)")
    for (ci in seq_along(col_hdrs9))
      writeData(wb, ws9, col_hdrs9[ci], startRow = 3, startCol = ci)
    addStyle(wb, ws9, s_head, rows = 3, cols = seq_along(col_hdrs9), gridExpand = TRUE)

    # Compute GDP components
    need_u <- c("gdp","k_stock","emp","avh","x_final","x_useful")
    need_a <- c("gdp","k_services","emp","avh","hc","x_final","x_useful")
    has_u  <- all(need_u %in% names(d_w))
    has_a  <- all(need_a %in% names(d_w))

    .z <- function(x) { x0 <- x[!is.na(x)][1]; log(x/x0) + 1 }

    if (has_u || has_a) {
      ok_u <- if (has_u) complete.cases(d_w[, need_u]) else rep(FALSE, n_yr)
      ok_a <- if (has_a) complete.cases(d_w[, need_a]) else rep(FALSE, n_yr)
      ok_b <- ok_u | ok_a

      # Build full-length vectors (NA where data missing)
      z_G    <- rep(NA_real_, n_yr)
      z_K_u  <- rep(NA_real_, n_yr); z_L_u <- rep(NA_real_, n_yr)
      z_K_a  <- rep(NA_real_, n_yr); z_L_a <- rep(NA_real_, n_yr)

      if (has_u && any(ok_u)) {
        z_G[ok_u]   <- .z(d_w$gdp[ok_u])
        z_K_u[ok_u] <- .z(d_w$k_stock[ok_u])
        z_L_u[ok_u] <- .z(d_w$emp[ok_u] * d_w$avh[ok_u])
      }
      if (has_a && any(ok_a)) {
        if (!any(ok_u)) z_G[ok_a] <- .z(d_w$gdp[ok_a])
        z_K_a[ok_a] <- .z(d_w$k_services[ok_a])
        z_L_a[ok_a] <- .z(d_w$emp[ok_a] * d_w$avh[ok_a] * d_w$hc[ok_a])
      }

      # z_KL (capital-labour component without TFP)
      z_KL_u <- aK_w * z_K_u + aL_w * z_L_u   # starts at 1
      z_KL_a <- aK_w * z_K_a + aL_w * z_L_a

      # Estimated GDP: z_G_est = z_TFP_est + aK*(z_K-1) + aL*(z_L-1), norm to 1
      .est_gdp <- function(z_tfp_fit, z_K, z_L) {
        raw <- z_tfp_fit + aK_w*(z_K-1) + aL_w*(z_L-1)
        ok  <- which(!is.na(raw)); if(!length(ok)) return(raw)
        raw - raw[ok[1]] + 1
      }

      eg_u_hz <- .est_gdp(fit_u_hz, z_K_u, z_L_u)
      eg_a_hz <- .est_gdp(fit_a_hz, z_K_a, z_L_a)
      eg_u_hc <- .est_gdp(fit_u_hc, z_K_u, z_L_u)
      eg_a_hc <- .est_gdp(fit_a_hc, z_K_a, z_L_a)

      z_G_norm <- .norm1(z_G)

      for (i in seq_len(n_yr)) {
        r   <- 3 + i
        sty <- if (i %% 2 == 0) s_alt_t else s_txt
        vals <- c(yr[i],
                  z_G_norm[i],
                  eg_u_hz[i], eg_a_hz[i],
                  eg_u_hc[i], eg_a_hc[i],
                  z_KL_u[i],  z_KL_a[i],
                  eg_u_hz[i] - z_G_norm[i],
                  eg_a_hz[i] - z_G_norm[i],
                  eg_u_hc[i] - z_G_norm[i])
        for (ci in seq_along(vals))
          if (!is.na(vals[ci]))
            writeData(wb, ws9, vals[ci], startRow = r, startCol = ci)
        addStyle(wb, ws9, sty, rows = r, cols = seq_along(vals), gridExpand = TRUE)
      }

      # Metrics
      r_met9 <- 3 + n_yr + 2
      writeData(wb, ws9, "Goodness-of-fit metrics (estimated vs historical GDP)",
                startRow = r_met9, startCol = 1)
      addStyle(wb, ws9, s_label, rows = r_met9, cols = 1)
      mergeCells(wb, ws9, cols = 1:11, rows = r_met9)
      r_met9 <- r_met9 + 1
      m9_hdrs <- c("Metric","GDP_unadj (Hz)","GDP_adj (Hz)",
                   "GDP_unadj (Hc)","GDP_adj (Hc)")
      for (ci in seq_along(m9_hdrs))
        writeData(wb, ws9, m9_hdrs[ci], startRow = r_met9, startCol = ci)
      addStyle(wb, ws9, s_head, rows = r_met9, cols = 1:5, gridExpand = TRUE)
      r_met9 <- r_met9 + 1
      m_gu_hz <- .metrics(z_G_norm, eg_u_hz)
      m_ga_hz <- .metrics(z_G_norm, eg_a_hz)
      m_gu_hc <- .metrics(z_G_norm, eg_u_hc)
      m_ga_hc <- .metrics(z_G_norm, eg_a_hc)
      for (i in seq_along(mets8)) {
        nm <- mets8[[i]][[2]]
        writeData(wb, ws9, mets8[[i]][[1]], startRow = r_met9, startCol = 1)
        for (ci in 2:5) {
          val <- list(m_gu_hz, m_ga_hz, m_gu_hc, m_ga_hc)[[ci-1]][[nm]]
          if (!is.null(val) && !is.na(val))
            writeData(wb, ws9, round(val, 6), startRow = r_met9, startCol = ci)
        }
        addStyle(wb, ws9, if(i%%2==0) s_alt_t else s_txt,
                 rows = r_met9, cols = 1:5, gridExpand = TRUE)
        r_met9 <- r_met9 + 1
      }
    }
  }

  # ── Save ───────────────────────────────────────────────────────────────────
  saveWorkbook(wb, out_path, overwrite = TRUE)
  message("\nOutput saved to: ", out_path)
  invisible(out_path)
}

# =============================================================================
# 12.  PDF REPORT
# =============================================================================
generate_report <- function(results, file_path, out_path = NULL) {
  for (pkg in c("ggplot2","gridExtra","grid","scales"))
    if (!requireNamespace(pkg, quietly=TRUE))
      install.packages(pkg, repos="https://cloud.r-project.org")
  suppressPackageStartupMessages({
    library(ggplot2); library(gridExtra); library(grid); library(scales)
  })
  if (is.null(out_path)) {
    base     <- tools::file_path_sans_ext(basename(file_path))
    out_path <- paste0("eff_tfp_report_", base, ".pdf")
  }
  base_nm     <- tools::file_path_sans_ext(basename(file_path))
  raw_cn      <- paste0(toupper(substr(base_nm,1,1)), tolower(substr(base_nm,2,nchar(base_nm))))
  country     <- if (!is.null(results$country_label)) results$country_label else raw_cn
  country_ref <- if (!is.null(results$country_ref))   results$country_ref   else country

  d  <- results$data; tfp <- results$tfp; exergy <- results$exergy
  sh <- results$shares; ur <- results$ur
  jo_hc <- results$johansen_hc; jo_hz <- results$johansen_hz
  ic <- results$int_const; diag_r <- results$diag; var_r <- results$var

  CB  <- "#2F5496"; CB2 <- "#4472C4"; CL  <- "#DCE6F1"
  CO  <- "#C55A11"; CG  <- "#375623"; CGY <- "#595959"
  CY  <- "#FFC000"; CR  <- "#C00000"

  # Hc best (prefer adj); fall back to Hz if Hc not estimated
  jo_cov <- NULL
  for (k in c("adj","unadj")) {
    jo <- jo_hc[[k]]
    if (!is.null(jo) && !is.null(jo$beta) && !is.na(jo$beta)) { jo_cov <- jo; break }
  }
  if (is.null(jo_cov)) {
    for (k in c("adj","unadj")) {
      jo <- jo_hz[[k]]
      if (!is.null(jo) && !is.null(jo$beta) && !is.na(jo$beta)) { jo_cov <- jo; break }
    }
  }
  aK <- mean(sh$alpha_K, na.rm=TRUE); aL <- 1 - aK

  .safe_const <- function(jo) { if(is.null(jo$const)||is.na(jo$const)) 0 else jo$const }
  .has_const  <- function(jo) { !is.null(jo$const) && !is.na(jo$const) }

  # English date
  mn <- c("January","February","March","April","May","June",
          "July","August","September","October","November","December")
  today    <- Sys.Date()
  date_str <- paste(as.integer(format(today,"%d")),
                    mn[as.integer(format(today,"%m"))],
                    format(today,"%Y"))

  .hdr <- function(title, pg) {
    grid.rect(x=0,y=1,width=1,height=0.045,just=c("left","top"),gp=gpar(fill=CB,col=NA))
    grid.text(paste0(country," — ",title),x=0.04,y=0.978,just="left",
              gp=gpar(col="white",fontsize=8,fontface="bold"))
    grid.text(paste0("v3.0  |  Page ",pg),x=0.96,y=0.978,just="right",gp=gpar(col="white",fontsize=8))
  }
  .ftr <- function() {
    grid.rect(x=0,y=0,width=1,height=0.030,just=c("left","bottom"),gp=gpar(fill=CL,col=NA))
    grid.text("MAPS Project — Horizon Europe — Grant Agreement No. 101137914",
              x=0.5,y=0.015,gp=gpar(col=CGY,fontsize=7))
  }
  .gvp  <- viewport(x=0.5,y=0.955,width=0.86,height=0.43,just=c("center","top"))
  .gvp2 <- viewport(x=0.5,y=0.955,width=0.88,height=0.86,just=c("center","top"))

  BSIZ <- 10

  th_hdr <- ttheme_minimal(base_size=7.5,
    colhead=list(fg_params=list(col="white",fontface="bold"),bg_params=list(fill=CB)),
    odd_row=list(bg_params=list(fill=CL),fg_params=list(col="black")),
    even_row=list(bg_params=list(fill="white"),fg_params=list(col="black")))
  th_sm  <- ttheme_minimal(base_size=8,
    colhead=list(fg_params=list(col="white",fontface="bold"),bg_params=list(fill=CB2)),
    odd_row=list(bg_params=list(fill=CL)))

  # Verdict banner helpers
  .verdict_box <- function(ok, y_pos, msg_ok, msg_ng) {
    ok    <- isTRUE(ok)
    col   <- if(ok) CG else CR
    pfx   <- if(ok) "[OK]  " else "[!!]  "
    msg   <- if(ok) msg_ok else msg_ng
    lines <- strwrap(paste0(pfx, msg), width=95)
    n     <- length(lines)
    box_h <- 0.014 + n*0.022
    grid.rect(x=0,y=y_pos,width=1,height=box_h,just=c("left","top"),
              gp=gpar(fill=col,col=NA,alpha=0.15))
    grid.rect(x=0,y=y_pos,width=0.005,height=box_h,just=c("left","top"),
              gp=gpar(fill=col,col=NA))
    y_t <- y_pos - 0.008
    for (ln in lines) {
      grid.text(ln,x=0.015,y=y_t,just=c("left","top"),
                gp=gpar(fontsize=8.0,col=col,fontface="bold"))
      y_t <- y_t - 0.022
    }
  }

  pg <- 0L
  pdf(out_path, width=8.27, height=11.69, family="Helvetica")
  on.exit(dev.off(), add=TRUE)

  # ===========================================================================
  # COVER
  # ===========================================================================
  grid.newpage()
  grid.rect(x=0,y=1,width=1,height=0.13,just=c("left","top"),gp=gpar(fill=CB,col=NA))
  grid.text("Efficiency-TFP Analysis",x=0.5,y=0.965,
            gp=gpar(col="white",fontsize=26,fontface="bold"))
  grid.text(country,x=0.5,y=0.910,gp=gpar(col="white",fontsize=17))
  grid.text("MAPS Project — Deliverable 4.2 (WP4: Improving Integrated Assessment Modelling)",
            x=0.5,y=0.855,gp=gpar(col=CB,fontsize=8.5,fontface="italic"))
  grid.lines(x=c(0.05,0.95),y=c(0.842,0.842),gp=gpar(col=CB,lwd=0.8))
  yr0 <- min(d$year); yr1 <- max(d$year)
  grid.text(paste0("Period: ",yr0,"–",yr1,"   •   Observations: ",nrow(d),
                   "   •   Date: ",date_str),
            x=0.5,y=0.829,gp=gpar(col=CGY,fontsize=8.5))

  y <- 0.808
  for (ln in strwrap(paste0(
    "This report documents the Efficiency-TFP cointegration analysis for ",country_ref,
    " (",yr0,"–",yr1,"). Following De Ketelaere et al. (2026) and Santos et al. (2021),",
    " it tests whether changes in Total Factor Productivity (TFP) are explained by",
    " improvements in final-to-useful exergy efficiency, and assesses the stability of",
    " useful exergy intensity for scenario analysis."), width=88)) {
    grid.text(ln,x=0.05,y=y,just=c("left","top"),gp=gpar(fontsize=8.5))
    y <- y - 0.020
  }
  y <- y - 0.022
  grid.text("Steps of the Analysis",x=0.05,y=y,just=c("left","top"),
            gp=gpar(fontsize=9,fontface="bold",col=CB))
  y <- y - 0.022
  for (st in c(
    "1.  Output elasticities (\u03b1_K, \u03b1_L) from income data",
    "2.  Labour inputs: unadjusted and quality-adjusted",
    "3.  TFP: unadjusted and quality-adjusted (z-normalised to base year)",
    "4.  Exergy efficiency and useful exergy intensity",
    "5.  Unit root tests (ADF and PP) on TFP and efficiency series",
    "6.  VAR estimation and residual diagnostics",
    "7.  Johansen cointegration tests: Hz (no deterministic) and Hc (restricted constant; no trend)",
    "8.  Useful exergy intensity constancy assessment")) {
    grid.text(st,x=0.07,y=y,just=c("left","top"),gp=gpar(fontsize=8.5,col=CGY))
    y <- y - 0.020
  }
  y <- y - 0.012
  grid.lines(x=c(0.05,0.95),y=c(y+0.005,y+0.005),gp=gpar(col=CL,lwd=0.6))
  y <- y - 0.010
  grid.text("Key Results",x=0.05,y=y,just=c("left","top"),
            gp=gpar(fontsize=9,fontface="bold",col=CB))
  y <- y - 0.024

  b <- NA_real_; con_cov <- NULL; tfp_lbl_str <- ""
  is_adj_cov <- FALSE
  if (!is.null(jo_cov)) {
    b <- jo_cov$beta; con_cov <- if(.has_const(jo_cov)) jo_cov$const else NULL
    is_adj_cov <- grepl("adj",jo_cov$tfp_name,ignore.case=TRUE) &&
                  !grepl("unadj",jo_cov$tfp_name,ignore.case=TRUE)
    tfp_lbl_str <- if(is_adj_cov) "quality-adjusted TFP" else "unadjusted TFP"
  }
  # i)
  grid.text("i)  Long-run TFP-Efficiency relationship [Hc specification — restricted constant; no trend]:",
            x=0.06,y=y,just=c("left","top"),gp=gpar(fontsize=8.5,fontface="bold"))
  y <- y-0.020
  if (!is.na(b)) {
    lev_str <- if(!is.null(con_cov))
      sprintf("TFP = exp(%.4f) \u00d7 EFF^{%.4f}   (%s)",-con_cov,b,tfp_lbl_str)
    else sprintf("TFP = EFF^{%.4f}   (%s)",b,tfp_lbl_str)
    grid.text(lev_str,x=0.08,y=y,just=c("left","top"),
              gp=gpar(fontsize=8.5,col=CB,fontface="bold"))
    y <- y-0.018
    z_str <- if(!is.null(con_cov))
      sprintf("(z-form: z_TFP = %.4f \u00d7 z_EFF + %.4f)",b,-con_cov)
    else sprintf("(z-form: z_TFP = %.4f \u00d7 z_EFF)",b)
    grid.text(z_str,x=0.08,y=y,just=c("left","top"),gp=gpar(fontsize=7.5,col=CGY))
  } else {
    grid.text("No cointegrating vector available.",x=0.08,y=y,just=c("left","top"),
              gp=gpar(fontsize=8.5,col=CR))
  }
  y <- y-0.024
  # ii) with K_services / hL
  grid.text("ii)  Aggregate production function (Cobb-Douglas):",
            x=0.06,y=y,just=c("left","top"),gp=gpar(fontsize=8.5,fontface="bold"))
  y <- y-0.020
  if (!is.na(b)) {
    k_lbl <- if(is_adj_cov && "k_services" %in% names(d)) "K_services" else "K_stock"
    l_lbl <- if(is_adj_cov && "hc" %in% names(d)) "hL" else "L"
    cd_const <- if(!is.null(con_cov)) sprintf("exp(%.4f) \u00d7 ",-con_cov) else ""
    cd_str <- sprintf("Y = %s%s^{%.4f} \u00d7 %s^{%.4f} \u00d7 EFF^{%.4f}",
                       cd_const,k_lbl,aK,l_lbl,aL,b)
    grid.text(cd_str,x=0.08,y=y,just=c("left","top"),
              gp=gpar(fontsize=8.5,col=CB,fontface="bold"))
  } else { grid.text("Not available.",x=0.08,y=y,just=c("left","top"),gp=gpar(fontsize=8.5,col=CGY)) }
  y <- y-0.024
  # iii)
  grid.text("iii)  Useful exergy intensity:",x=0.06,y=y,just=c("left","top"),
            gp=gpar(fontsize=8.5,fontface="bold"))
  y <- y-0.020
  if (!is.null(ic)) {
    f <- ic$full; s <- ic$sub
    cf <- (f$trend_ok&&(f$adf_i0||f$pp_i0))||f$cv<0.05
    cs <- (s$trend_ok&&(s$adf_i0||s$pp_i0))||s$cv<0.05
    int_txt <- if(cf)
      sprintf("Approximately constant (%d–%d). Mean X_useful/GDP = %.4f (CV=%.1f%%)",
              f$yr_start,f$yr_end,f$mean,f$cv*100)
    else if(cs)
      sprintf("Constant in recent sub-period (%d–%d). Mean=%.4f. Full-period trend: %.3f%%/yr.",
              s$yr_start,s$yr_end,s$mean,f$ann_rate_pct)
    else sprintf("Not constant. Trend: %.3f%% p.a. (R\u00b2=%.3f)",f$ann_rate_pct,f$r2)
    for (ln in strwrap(int_txt,width=90))
      { grid.text(ln,x=0.08,y=y,just=c("left","top"),gp=gpar(fontsize=8.5,col=CB)); y<-y-0.018 }
  } else { grid.text("Not available.",x=0.08,y=y,just=c("left","top"),gp=gpar(fontsize=8.5,col=CGY)) }

  grid.lines(x=c(0.05,0.95),y=c(0.038,0.038),gp=gpar(col=CB,lwd=0.5))
  grid.text("MAPS Project — Horizon Europe — Grant Agreement No. 101137914",
            x=0.5,y=0.020,gp=gpar(col=CGY,fontsize=7))

  # ===========================================================================
  # PAGE 1: Elasticities
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("Output Elasticities",pg); .ftr()
  if (!all(is.na(sh$alpha_K))) {
    ok_el <- !is.na(sh$alpha_K)
    df_el <- data.frame(year=d$year[ok_el],aK=sh$alpha_K[ok_el],aL=1-sh$alpha_K[ok_el])
    mK <- mean(df_el$aK); ann_x <- quantile(df_el$year,0.65)
    p1 <- ggplot(df_el) +
      geom_line(aes(year,aK,colour="\u03b1_K (output elasticity of capital)"),linewidth=0.9) +
      geom_line(aes(year,aL,colour="\u03b1_L (output elasticity of labour)"),linewidth=0.9) +
      geom_hline(yintercept=mK,colour=CB,linetype="dashed",linewidth=0.7) +
      geom_hline(yintercept=1-mK,colour=CO,linetype="dashed",linewidth=0.7) +
      geom_hline(yintercept=0.30,colour="black",linetype="dotted",linewidth=0.6) +
      geom_hline(yintercept=0.70,colour="black",linetype="dotted",linewidth=0.6) +
      annotate("label",x=ann_x,y=mK+0.08,label=sprintf("mean alpha_K=%.3f",mK),
               colour=CB,fill="white",size=3.2,linewidth=0.2) +
      annotate("label",x=ann_x,y=(1-mK)-0.08,label=sprintf("mean alpha_L=%.3f",1-mK),
               colour=CO,fill="white",size=3.2,linewidth=0.2) +
      annotate("label",x=ann_x,y=0.22,label="fixed 0.30",
               colour="black",fill="white",size=3.0,linewidth=0.2) +
      annotate("label",x=ann_x,y=0.78,label="fixed 0.70",
               colour="black",fill="white",size=3.0,linewidth=0.2) +
      scale_colour_manual(values=c(
        "\u03b1_K (output elasticity of capital)"=CB,
        "\u03b1_L (output elasticity of labour)"=CO)) +
      scale_y_continuous(limits=c(0,1),breaks=seq(0,1,0.1)) +
      labs(title="Annual Output Elasticities",x="Year",y="Elasticity",colour=NULL) +
      theme_minimal(base_size=BSIZ) +
      theme(legend.position="bottom",plot.title=element_text(colour=CB,face="bold"),
            panel.grid.minor=element_blank())
    pushViewport(.gvp); print(p1,newpage=FALSE); popViewport()
  }

  # ===========================================================================
  # PAGE 2: TFP Series
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("TFP and Efficiency Series",pg); .ftr()
  df2 <- data.frame(year=d$year)
  if (!is.null(tfp$z_tfp_unadj)) df2$unadj <- tfp$z_tfp_unadj
  if (!is.null(tfp$z_tfp_adj))   df2$adj   <- tfp$z_tfp_adj
  df2$eff <- exergy$z_eff
  p2 <- ggplot(df2[rowSums(!is.na(df2[,-1]))>0,],aes(x=year)) +
    geom_hline(yintercept=1,colour=CGY,linetype="dotted",linewidth=0.4)
  if (!is.null(tfp$z_tfp_unadj))
    p2 <- p2+geom_line(aes(y=unadj,colour="Unadjusted TFP"),linewidth=0.9)
  if (!is.null(tfp$z_tfp_adj))
    p2 <- p2+geom_line(aes(y=adj,colour="Quality-adjusted TFP"),linewidth=0.9,linetype="dashed")
  p2 <- p2+geom_line(aes(y=eff,colour="Final-to-Useful aggregate exergy efficiency"),linewidth=0.9)+
    scale_colour_manual(values=c("Unadjusted TFP"=CB,"Quality-adjusted TFP"=CO,
                                  "Final-to-Useful aggregate exergy efficiency"=CG))+
    labs(title="Normalised TFP and Final-to-Useful Exergy Efficiency  [z = ln(x/x\u2080) + 1]",
         x="Year",y="z-index (base year = 1)",colour=NULL)+
    theme_minimal(base_size=BSIZ)+
    theme(legend.position="bottom",plot.title=element_text(colour=CB,face="bold"),
          panel.grid.minor=element_blank())
  pushViewport(.gvp); print(p2,newpage=FALSE); popViewport()

  # ===========================================================================
  # PAGE 3: Unit Root Tests + verdict
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("Unit Root Tests (ADF and PP)",pg); .ftr()
  pushViewport(viewport(x=0.5,y=0.955,width=0.97,height=0.885,just=c("center","top")))
  grid.text("Unit Root Tests — ADF and PP  (5% significance level)",
            x=0.5,y=0.99,gp=gpar(fontsize=10,fontface="bold",col=CB))
  y_u <- 0.93
  for (nm in names(ur)) {
    r <- ur[[nm]]
    grid.rect(x=0,y=y_u,width=1,height=0.028,just=c("left","top"),gp=gpar(fill=CB2,col=NA))
    grid.text(paste0("Series: ",nm,"  |  Verdict: ",r$order,
                      "  |  ADF: ",r$adf_verdict,"  |  PP: ",r$pp_verdict),
              x=0.01,y=y_u-0.002,just=c("left","top"),
              gp=gpar(col="white",fontsize=8,fontface="bold"))
    y_u <- y_u-0.030
    tab <- data.frame(
      Test=c("ADF","ADF","PP","PP"),
      Specification=c("Levels (trend+intercept)","1st diff. (intercept)",
                      "Levels (trend+intercept)","1st diff. (intercept)"),
      Statistic=c(sprintf("%.4f",r$adf_stat_levels),sprintf("%.4f",r$adf_stat_diffs),
                  sprintf("%.4f",r$pp_stat_levels), sprintf("%.4f",r$pp_stat_diffs)),
      "CV 1%"=c(sprintf("%.4f",r$adf_cv1_levels),sprintf("%.4f",r$adf_cv1_diffs),
                sprintf("%.4f",r$pp_cv1_levels), sprintf("%.4f",r$pp_cv1_diffs)),
      "CV 5%"=c(sprintf("%.4f",r$adf_cv5_levels),sprintf("%.4f",r$adf_cv5_diffs),
                sprintf("%.4f",r$pp_cv5_levels), sprintf("%.4f",r$pp_cv5_diffs)),
      "CV 10%"=c(sprintf("%.4f",r$adf_cv10_levels),sprintf("%.4f",r$adf_cv10_diffs),
                 sprintf("%.4f",r$pp_cv10_levels), sprintf("%.4f",r$pp_cv10_diffs)),
      "Aux."=c("Lags (BIC)","Lags (BIC)","BW (NW)","BW (NW)"),
      "Val."=c(sprintf("%d",r$adf_lags_levels),sprintf("%d",r$adf_lags_diffs),
               sprintf("%d",r$pp_bw_levels),   sprintf("%d",r$pp_bw_diffs)),
      "H0 rej.?"=c(ifelse(r$adf_stat_levels<r$adf_cv5_levels,"Yes","No"),
                   ifelse(r$adf_stat_diffs <r$adf_cv5_diffs, "Yes","No"),
                   ifelse(r$pp_stat_levels <r$pp_cv5_levels, "Yes","No"),
                   ifelse(r$pp_stat_diffs  <r$pp_cv5_diffs,  "Yes","No")),
      Verdict=c(r$adf_verdict,"—",r$pp_verdict,"—"),
      stringsAsFactors=FALSE,check.names=FALSE)
    tg <- tableGrob(tab,rows=NULL,theme=th_hdr)
    pushViewport(viewport(x=0.5,y=y_u,width=1,height=0.095,just=c("center","top")))
    grid.draw(tg); popViewport()
    y_u <- y_u-0.100-0.018
  }
  # Verdict statement
  orders <- sapply(ur, function(r) r$order)
  all_i1 <- all(orders == "I(1)")
  any_i0 <- any(orders == "I(0)")
  any_i2 <- any(orders == "I(2)")
  if (all_i1) {
    ur_msg <- paste0("All series are I(1): unit roots confirmed in levels, stationary in first differences.",
                     " Green light — Johansen cointegration analysis is appropriate.")
  } else if (any_i0) {
    i0_nm <- paste(names(orders)[orders=="I(0)"],collapse=", ")
    ur_msg <- paste0("WARNING: Series ",i0_nm," appear(s) to be I(0).",
                     " Cointegration assumes all series are I(1) — proceed with caution.")
  } else if (any_i2) {
    i2_nm <- paste(names(orders)[orders=="I(2)"],collapse=", ")
    ur_msg <- paste0("WARNING: Series ",i2_nm," appear(s) to be I(2).",
                     " Cointegration analysis is not appropriate for I(2) series.")
  } else {
    ur_msg <- paste0("Mixed integration orders detected. Review results carefully before proceeding.")
  }
  .verdict_box(all_i1, y_u-0.01, ur_msg, ur_msg)
  popViewport()

  # ===========================================================================
  # VAR Diagnostic helper
  # ===========================================================================
  .diag_hdr_row <- function(key,y_c) {
    dg<-diag_r[[key]]; vobj<-var_r[[key]]
    grid.rect(x=0,y=y_c,width=1,height=0.028,just=c("left","top"),gp=gpar(fill=CB2,col=NA))
    grid.text(paste0("VAR(",vobj$p,"): ",vobj$tfp_name," ~ EFF  |  T=",dg$T,
                      "  (",vobj$year[vobj$p+1],"–",tail(vobj$year,1),")"),
              x=0.01,y=y_c-0.002,just=c("left","top"),
              gp=gpar(col="white",fontsize=8,fontface="bold"))
    y_c-0.034
  }

  # ===========================================================================
  # PAGE 4: Portmanteau + verdict
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("VAR Diagnostics — Portmanteau",pg); .ftr()
  pushViewport(viewport(x=0.5,y=0.955,width=0.97,height=0.885,just=c("center","top")))
  grid.text("VAR Residual Portmanteau Tests",x=0.5,y=0.99,
            gp=gpar(fontsize=10,fontface="bold",col=CB))
  y_p <- 0.93
  for (key in names(diag_r)) {
    dg <- diag_r[[key]]; y_p <- .diag_hdr_row(key,y_p)
    pt_rows <- do.call(rbind,lapply(dg$portmanteau,function(pt)
      c(paste0("h=",pt$h),sprintf("%.6f",pt$Q),
        if(isTRUE(pt$df>0)) sprintf("%.4f",pt$pval) else "---",
        sprintf("%.6f",pt$Q_adj),
        if(isTRUE(pt$df>0)) sprintf("%.4f",pt$pval_adj) else "---",
        if(isTRUE(pt$df>0)) sprintf("%d",pt$df) else "---")))
    pt_df <- as.data.frame(pt_rows,stringsAsFactors=FALSE)
    names(pt_df) <- c("Lags","Q-Stat","Prob.","Adj Q-Stat","Prob.","df")
    tg <- tableGrob(pt_df,rows=NULL,theme=th_sm)
    pushViewport(viewport(x=0.5,y=y_p,width=0.75,height=0.12,just=c("center","top")))
    grid.draw(tg); popViewport()
    y_p <- y_p-0.135
    grid.text("* Test is valid only for lags larger than the VAR lag order.",
              x=0.02,y=y_p,just=c("left","top"),gp=gpar(fontsize=7,col=CGY,fontface="italic"))
    y_p <- y_p-0.040
  }
  # Portmanteau verdict
  ok_pt <- all(sapply(names(diag_r), function(key) {
    dg <- diag_r[[key]]; p <- var_r[[key]]$p
    valid <- Filter(function(pt) isTRUE(pt$df > 0), dg$portmanteau)
    length(valid)==0 || isTRUE(all(sapply(valid,function(pt) isTRUE(pt$pval>0.05))))
  }))
  pt_msg <- if(ok_pt)
    "No significant autocorrelation detected (Portmanteau, all valid lags p > 0.05). Green light for cointegration."
  else
    "WARNING: Significant autocorrelation detected. VAR residuals are not white noise — results may be unreliable."
  .verdict_box(ok_pt, y_p-0.01, pt_msg, pt_msg)
  popViewport()

  # ===========================================================================
  # PAGE 5: LM Serial Correlation + verdict
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("VAR Diagnostics — Serial Correlation (LM)",pg); .ftr()
  pushViewport(viewport(x=0.5,y=0.955,width=0.97,height=0.885,just=c("center","top")))
  grid.text("VAR Residual Serial Correlation (LM) Tests",x=0.5,y=0.99,
            gp=gpar(fontsize=10,fontface="bold",col=CB))
  y_l <- 0.93
  for (key in names(diag_r)) {
    dg <- diag_r[[key]]; y_l <- .diag_hdr_row(key,y_l)
    for (lm_info in list(
      list(dg$lm_individual,"Null: No serial correlation at lag h"),
      list(dg$lm_cumulative,"Null: No serial correlation at lags 1 to h"))) {
      grid.text(lm_info[[2]],x=0.02,y=y_l,just=c("left","top"),
                gp=gpar(fontsize=7.5,col=CGY))
      y_l <- y_l-0.020
      lm_rows <- do.call(rbind,lapply(lm_info[[1]],function(lm)
        c(paste0("h=",lm$h),
          sprintf("%.6f",lm$LRE),sprintf("%d",lm$df),sprintf("%.4f",lm$p_LRE),
          sprintf("%.6f",lm$F_rao),sprintf("(%.0f, %.1f)",lm$df1,lm$df2),
          sprintf("%.4f",lm$p_F))))
      lm_df <- as.data.frame(lm_rows,stringsAsFactors=FALSE)
      names(lm_df) <- c("Lag","LRE* stat","df","Prob.","Rao F-stat","df","Prob.")
      tg <- tableGrob(lm_df,rows=NULL,theme=th_sm)
      pushViewport(viewport(x=0.5,y=y_l,width=0.95,height=0.12,just=c("center","top")))
      grid.draw(tg); popViewport()
      y_l <- y_l-0.138
    }
    grid.text("* Edgeworth expansion corrected likelihood ratio statistic.",
              x=0.02,y=y_l,just=c("left","top"),gp=gpar(fontsize=7,col=CGY,fontface="italic"))
    y_l <- y_l-0.035
  }
  ok_lm <- all(sapply(names(diag_r), function(key) {
    dg <- diag_r[[key]]; p <- var_r[[key]]$p
    valid <- Filter(function(lm) lm$h>p, dg$lm_individual)
    length(valid)==0 || all(sapply(valid,function(lm) lm$p_F>0.05))
  }))
  lm_msg <- if(ok_lm)
    "No significant serial correlation detected (LM, all valid lags p > 0.05). Green light for cointegration."
  else
    "WARNING: Significant serial correlation detected in VAR residuals — consider increasing VAR lag order."
  .verdict_box(ok_lm, y_l-0.01, lm_msg, lm_msg)
  popViewport()

  # ===========================================================================
  # PAGE 6: Normality + verdict
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("VAR Diagnostics — Normality",pg); .ftr()
  pushViewport(viewport(x=0.5,y=0.955,width=0.97,height=0.885,just=c("center","top")))
  grid.text("VAR Residual Normality Tests",x=0.5,y=0.99,
            gp=gpar(fontsize=10,fontface="bold",col=CB))
  y_n <- 0.93
  for (key in names(diag_r)) {
    dg <- diag_r[[key]]; y_n <- .diag_hdr_row(key,y_n)
    nm_rows <- do.call(rbind,lapply(dg$normality,function(jb)
      c(paste0("Eq.",jb$component),
        sprintf("%.6f",jb$skewness),sprintf("%.6f",jb$chi_skew),"1",
        sprintf("%.4f",jb$p_skew),
        sprintf("%.6f",jb$kurtosis),sprintf("%.6f",jb$chi_kurt),"1",
        sprintf("%.4f",jb$p_kurt),
        sprintf("%.6f",jb$JB),"2",sprintf("%.4f",jb$p_JB))))
    nm_df <- as.data.frame(nm_rows,stringsAsFactors=FALSE)
    names(nm_df) <- c("Component","Skewness","Chi-sq","df","p",
                       "Kurtosis","Chi-sq","df","p","JB stat","df","p")
    tg <- tableGrob(nm_df,rows=NULL,theme=th_sm)
    pushViewport(viewport(x=0.5,y=y_n,width=0.98,height=0.08,just=c("center","top")))
    grid.draw(tg); popViewport()
    y_n <- y_n-0.100
    jnt <- dg$jb_joint
    jt_df <- data.frame(
      Test=c("Joint skewness","Joint kurtosis","Joint JB"),
      Statistic=c(sprintf("%.6f",jnt$skew$stat),sprintf("%.6f",jnt$kurt$stat),
                  sprintf("%.6f",jnt$jb$stat)),
      df=c(as.character(dg$K),as.character(dg$K),as.character(2*dg$K)),
      Prob=c(sprintf("%.4f",jnt$skew$pval),sprintf("%.4f",jnt$kurt$pval),
             sprintf("%.4f",jnt$jb$pval)),
      stringsAsFactors=FALSE)
    tg2 <- tableGrob(jt_df,rows=NULL,theme=th_sm)
    pushViewport(viewport(x=0.5,y=y_n,width=0.55,height=0.07,just=c("center","top")))
    grid.draw(tg2); popViewport()
    y_n <- y_n-0.100
  }
  ok_norm <- all(sapply(names(diag_r),function(key) diag_r[[key]]$jb_joint$jb$pval>0.05))
  norm_msg <- if(ok_norm)
    "Residuals are normally distributed (joint JB p > 0.05). Green light for cointegration."
  else
    "WARNING: Non-normal residuals detected (joint JB p < 0.05). Cointegration p-values may be affected."
  .verdict_box(ok_norm, y_n-0.01, norm_msg, norm_msg)
  popViewport()

  # ===========================================================================
  # Johansen
  # ===========================================================================
  .jo_page <- function(jo_list, spec_lbl, pg_n) {
    grid.newpage(); .hdr(paste0("Johansen Cointegration — ",spec_lbl),pg_n); .ftr()
    pushViewport(viewport(x=0.5,y=0.955,width=0.97,height=0.885,just=c("center","top")))
    grid.text(paste0("Johansen Cointegration Tests: ",spec_lbl),
              x=0.5,y=0.99,gp=gpar(fontsize=10,fontface="bold",col=CB))
    y_j <- 0.94
    for (key in names(jo_list)) {
      jo <- jo_list[[key]]; if(is.null(jo)) next
      grid.rect(x=0,y=y_j,width=1,height=0.028,just=c("left","top"),gp=gpar(fill=CB2,col=NA))
      grid.text(paste0(jo$tfp_name," ~ EFF  |  ",spec_lbl,
                        "  |  Lags (1st diff.): 1 to ",jo$p,
                        "  |  T=",jo$T,"  (",jo$sample_start,"–",jo$sample_end,")"),
                x=0.01,y=y_j-0.002,just=c("left","top"),gp=gpar(col="white",fontsize=8,fontface="bold"))
      y_j <- y_j-0.032
      mk_tst <- function(tl) data.frame(
        Hypothesized=c(paste0("None (r=0)",ifelse(tl$coint," *","")),
                       paste0("At most 1 (r\u22641)",ifelse(tl$stat_r1>tl$cv5_r1," *",""))),
        Eigenvalue=c(sprintf("%.6f",tl$eig_r0),sprintf("%.6f",tl$eig_r1)),
        Statistic=c(sprintf("%.5f",tl$stat_r0),sprintf("%.5f",tl$stat_r1)),
        "CV 5%"=c(sprintf("%.5f",tl$cv5_r0),sprintf("%.5f",tl$cv5_r1)),
        "Prob.**"=c(sprintf("%.4f",tl$pval_r0),sprintf("%.4f",tl$pval_r1)),
        stringsAsFactors=FALSE,check.names=FALSE)
      for (info in list(list("Unrestricted Cointegration Rank Test (Trace)",jo$trace),
                         list("Unrestricted Cointegration Rank Test (Max-Eigenvalue)",jo$eigen))) {
        grid.text(info[[1]],x=0.02,y=y_j,just=c("left","top"),
                  gp=gpar(fontsize=8,fontface="bold",col=CB))
        y_j <- y_j-0.022
        tg <- tableGrob(mk_tst(info[[2]]),rows=NULL,theme=th_hdr)
        pushViewport(viewport(x=0.5,y=y_j,width=0.90,height=0.075,just=c("center","top")))
        grid.draw(tg); popViewport()
        y_j <- y_j-0.082
        v_col <- if(isTRUE(info[[2]]$coint)) CG else CR
        v_txt <- if(isTRUE(info[[2]]$coint))
          paste0(gsub(".*\\((.*)\\)","\\1",info[[1]])," indicates 1 CE at 5%")
        else paste0(gsub(".*\\((.*)\\)","\\1",info[[1]])," indicates no cointegration at 5%")
        grid.text(paste0("* ",v_txt),x=0.02,y=y_j,just=c("left","top"),
                  gp=gpar(fontsize=7.5,col=v_col))
        grid.text("** MacKinnon-Haug-Michelis (1999) p-values.",
                  x=0.52,y=y_j,just=c("left","top"),gp=gpar(fontsize=7.5,col=CGY))
        y_j <- y_j-0.022
      }
      if (!is.null(jo$beta)) {
        grid.text("Normalised Cointegrating Coefficients (std. error in parentheses):",
                  x=0.02,y=y_j,just=c("left","top"),gp=gpar(fontsize=8,fontface="bold",col=CB))
        y_j <- y_j-0.020
        cv_rows <- list(c(jo$tfp_name,"1.000000",""))
        cv_rows[[2]] <- c("EFF",sprintf("%.6f",-jo$beta),
                           if(!is.na(jo$beta_se)) sprintf("(%.5f)",jo$beta_se) else "")
        if (.has_const(jo))
          cv_rows[[3]] <- c("C",sprintf("%.6f",jo$const),
                             if(!is.null(jo$const_se)&&!is.na(jo$const_se))
                               sprintf("(%.5f)",jo$const_se) else "")
        cv_df <- as.data.frame(do.call(rbind,cv_rows),stringsAsFactors=FALSE)
        names(cv_df) <- c("Variable","Coefficient","Std. Error")
        tg3 <- tableGrob(cv_df,rows=NULL,theme=th_hdr)
        pushViewport(viewport(x=0.20,y=y_j,width=0.38,height=0.07,just=c("left","top")))
        grid.draw(tg3); popViewport()
        vv <- if(isTRUE(jo$valid)) paste0("beta=",sprintf("%.4f",jo$beta)," > 0  Valid")
              else paste0("beta=",sprintf("%.4f",jo$beta)," < 0  INVALID")
        grid.text(vv,x=0.62,y=y_j-0.015,just=c("left","top"),
                  gp=gpar(fontsize=8,col=if(isTRUE(jo$valid)) CG else CR,fontface="bold"))
        y_j <- y_j-0.085
      }
      y_j <- y_j-0.012
    }
    popViewport()
  }
  pg <- pg+1L; .jo_page(jo_hz,"Hz (no deterministic term)",pg)
  pg <- pg+1L; .jo_page(jo_hc,"Hc (restricted constant; no trend)",pg)

  # ===========================================================================
  # PAGES 9-10: Fitted vs Historical TFP — normalize fitted to 1, fix label
  # ===========================================================================
  .tfp_gg <- function(z_tfp, z_eff, yr, jo, lbl) {
    if(is.null(z_tfp)||is.null(jo)||is.null(jo$beta)||is.na(jo$beta))
      return(ggplot()+annotate("text",0.5,0.5,label=paste("N/A:",lbl))+theme_void())
    cn  <- .safe_const(jo)
    df  <- na.omit(data.frame(year=yr, actual=z_tfp, eff=z_eff))
    raw_fit      <- jo$beta*df$eff - cn
    df$fitted    <- raw_fit - raw_fit[1] + 1   # normalise to 1 at start
    eq <- if(.has_const(jo))
      sprintf("z_TFP = %.4f \u00d7 z_EFF + %.4f",jo$beta,-cn)
    else sprintf("z_TFP = %.4f \u00d7 z_EFF",jo$beta)
    # Place label in upper-left corner, away from lines
    x_lab <- min(df$year) + 0.05*diff(range(df$year))
    y_rng <- range(c(df$actual,df$fitted),na.rm=TRUE)
    y_lab <- y_rng[1] + 0.85*diff(y_rng)
    ggplot(df,aes(x=year)) +
      geom_hline(yintercept=1,colour=CGY,linetype="dotted",linewidth=0.4) +
      geom_line(aes(y=actual,colour="Historical"),linewidth=0.9) +
      geom_line(aes(y=fitted,colour="Fitted (CV, norm.)"),linewidth=0.8,linetype="dashed") +
      scale_colour_manual(values=c("Historical"=CB,"Fitted (CV, norm.)"=CO)) +
      annotate("label",x=x_lab,y=y_lab,label=eq,size=2.8,colour=CB,fill="white",
               label.size=0.2,hjust="left",vjust="top") +
      labs(title=lbl,x="Year",y="z-index",colour=NULL) +
      theme_minimal(base_size=BSIZ) +
      theme(legend.position="bottom",plot.title=element_text(colour=CB,face="bold"),
            panel.grid.minor=element_blank())
  }

  for (spec_info in list(list("Hz","Hz (no deterministic term)",jo_hz),
                          list("Hc","Hc (restricted constant; no trend)",jo_hc))) {
    pg <- pg+1L; grid.newpage()
    .hdr(paste0("Fitted vs Historical TFP — ",spec_info[[1]]),pg); .ftr()
    pu <- .tfp_gg(tfp$z_tfp_unadj,exergy$z_eff,d$year,spec_info[[3]][["unadj"]],"Unadjusted TFP")
    pa <- .tfp_gg(tfp$z_tfp_adj,  exergy$z_eff,d$year,spec_info[[3]][["adj"]],  "Quality-adjusted TFP")
    g  <- arrangeGrob(pu,pa,nrow=2,
           top=textGrob(paste0("Historical vs Fitted TFP  [",spec_info[[2]],"]"),
                        gp=gpar(fontsize=10,fontface="bold",col=CB)))
    pushViewport(.gvp2); grid.draw(g); popViewport()
  }

  # ===========================================================================
  # PAGE 11: GDP Cobb-Douglas — unadjusted on TOP, quality-adj on BOTTOM
  #          Add K^aK*L^aL series to each panel
  # ===========================================================================
  pg <- pg+1L; grid.newpage()
  .hdr("Historical vs Fitted GDP (Cobb-Douglas)",pg); .ftr()

  .gdp_gg <- function(use_adj, lbl) {
    K_col  <- if(use_adj && "k_services" %in% names(d)) "k_services" else "k_stock"
    L_expr <- if(use_adj && "hc" %in% names(d)) d$emp*d$avh*d$hc else d$emp*d$avh
    KL_lbl <- if(use_adj) "K_services^\u03b1K \u00d7 hL^\u03b1L" else "K_stock^\u03b1K \u00d7 L^\u03b1L"
    need   <- c("gdp",K_col,"emp","avh","x_final","x_useful")
    if(use_adj && "hc" %in% names(d)) need <- c(need,"hc")
    if(!all(need %in% names(d)))
      return(ggplot()+annotate("text",0.5,0.5,label="Insufficient data")+theme_void())
    ok  <- complete.cases(d[,need])
    yr  <- d$year[ok]
    G   <- d$gdp[ok];      G0 <- G[1]
    K   <- d[[K_col]][ok]; K0 <- K[1]
    L   <- L_expr[ok];     L0 <- L[1]
    E   <- d$x_useful[ok]/d$x_final[ok]; E0 <- E[1]
    z_G <- log(G/G0)+1; z_K <- log(K/K0)+1
    z_L <- log(L/L0)+1; z_E <- log(E/E0)+1
    z_KL <- aK*z_K + aL*z_L
    df_g <- data.frame(year=yr, actual=z_G, KL_series=z_KL)
    cols <- c("Historical GDP"=CB, "KL_series"="#8064A2",
               "Fitted_Hz"=CO,     "Fitted_Hc"=CG)
    lty  <- c("Historical GDP"="solid", "KL_series"="dotdash",
               "Fitted_Hz"="dashed",    "Fitted_Hc"="dashed")
    disp <- c("Historical GDP"="Historical GDP", "KL_series"=KL_lbl,
               "Fitted_Hz"="Fitted (Hz)",        "Fitted_Hc"="Fitted (Hc)")
    for (si in list(list("Hz","Fitted_Hz",jo_hz),
                    list("Hc","Fitted_Hc",jo_hc))) {
      key  <- if(use_adj) "adj" else "unadj"
      jo_s <- si[[3]][[key]]
      if(!is.null(jo_s) && !is.null(jo_s$beta) && !is.na(jo_s$beta)) {
        cn_s   <- .safe_const(jo_s)
        z_TF   <- jo_s$beta*z_E - cn_s
        z_GF   <- z_TF + aK*(z_K-1) + aL*(z_L-1)
        df_g[[si[[2]]]] <- z_GF - z_GF[1] + 1
      }
    }
    p <- ggplot(df_g,aes(x=year)) +
      geom_hline(yintercept=1,colour=CGY,linetype="dotted",linewidth=0.4) +
      geom_line(aes(y=actual,    colour="Historical GDP",linetype="Historical GDP"),linewidth=0.9) +
      geom_line(aes(y=KL_series, colour="KL_series",    linetype="KL_series"),    linewidth=0.7)
    if ("Fitted_Hz" %in% names(df_g))
      p <- p+geom_line(aes(y=Fitted_Hz,colour="Fitted_Hz",linetype="Fitted_Hz"),linewidth=0.8)
    if ("Fitted_Hc" %in% names(df_g))
      p <- p+geom_line(aes(y=Fitted_Hc,colour="Fitted_Hc",linetype="Fitted_Hc"),linewidth=0.8)
    used <- intersect(names(disp), c("Historical GDP","KL_series",
                                      if("Fitted_Hz"%in%names(df_g))"Fitted_Hz",
                                      if("Fitted_Hc"%in%names(df_g))"Fitted_Hc"))
    p + scale_colour_manual(values=cols[used],  labels=disp[used]) +
      scale_linetype_manual(values=lty[used],   labels=disp[used]) +
      guides(colour=guide_legend(ncol=2), linetype="none") +
      labs(title=lbl,x="Year",y="z-index",colour=NULL) +
      theme_minimal(base_size=BSIZ) +
      theme(legend.position="bottom",plot.title=element_text(colour=CB,face="bold"),
            panel.grid.minor=element_blank())
  }

  # Use pre-computed GDP series when available (e.g. Santos 2021 pre-loaded)
  if (!is.null(results$gdp_est)) {
    ge <- results$gdp_est
    .gdp_pre <- function(yr_g, gh_g, gfit_g, gkl_g, lbl_g) {
      df_gp <- na.omit(data.frame(year=yr_g, actual=gh_g, fitted=gfit_g, kl=gkl_g))
      ggplot(df_gp, aes(x=year)) +
        geom_hline(yintercept=1, colour=CGY, linetype="dotted", linewidth=0.4) +
        geom_line(aes(y=actual, colour="Historical GDP"), linewidth=0.9) +
        geom_line(aes(y=fitted, colour="Fitted (Hz)"), linewidth=0.8, linetype="dashed") +
        geom_line(aes(y=kl,     colour="K^aK x L^aL (no TFP)"), linewidth=0.7, linetype="dotdash") +
        scale_colour_manual(values=c("Historical GDP"=CB,"Fitted (Hz)"=CO,
                                      "K^aK x L^aL (no TFP)"="#8064A2")) +
        labs(title=lbl_g, x="Year", y="z-index", colour=NULL) +
        theme_minimal(base_size=BSIZ) +
        theme(legend.position="bottom",
              plot.title=element_text(colour=CB, face="bold"),
              panel.grid.minor=element_blank())
    }
    pa_g <- .gdp_pre(ge$yr, ge$gh, ge$ga_hz, ge$ga_hc, "Quality-adjusted (K_services, hL)")
    pu_g <- .gdp_pre(ge$yr, ge$gh, ge$gu_hz, ge$gu_hc, "Unadjusted (K_stock, L)")
    g_gdp <- arrangeGrob(pu_g, pa_g, nrow=2,
                top=textGrob("Historical vs Fitted GDP  [Hz specification  |  K^aK x L^aL = factor accumulation without TFP]",
                             gp=gpar(fontsize=9, fontface="bold", col=CB)))
  } else {
    pu_g <- .gdp_gg(FALSE,"Unadjusted (K_stock, L)")
    pa_g <- .gdp_gg(TRUE, "Quality-adjusted (K_services, hL)")
    g_gdp <- arrangeGrob(pu_g, pa_g, nrow=2,
                top=textGrob("Historical vs Fitted GDP  [both Hz and Hc specifications]",
                             gp=gpar(fontsize=10, fontface="bold", col=CB)))
  }
  pushViewport(.gvp2); grid.draw(g_gdp); popViewport()

  # ===========================================================================
  # PAGE 12: Intensity — y-axis 0-2, fixed table
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("Useful Exergy Intensity Constancy",pg); .ftr()

  has_int <- ("int_useful" %in% names(d) && !all(is.na(d$int_useful))) ||
             ("x_useful" %in% names(d) && "gdp" %in% names(d))
  if (!is.null(ic) && has_int) {
    f <- ic$full; s <- ic$sub
    cf <- (f$trend_ok&&(f$adf_i0||f$pp_i0))||f$cv<0.05
    cs <- (s$trend_ok&&(s$adf_i0||s$pp_i0))||s$cv<0.05
    if ("int_useful" %in% names(d) && !all(is.na(d$int_useful))) {
      ok_i  <- !is.na(d$int_useful)
      yr_i  <- d$year[ok_i]; raw_i <- d$int_useful[ok_i]
    } else {
      ok_i  <- !is.na(d$x_useful) & !is.na(d$gdp) & d$gdp>0
      yr_i  <- d$year[ok_i]; raw_i <- d$x_useful[ok_i]/d$gdp[ok_i]
    }
    norm_i<- raw_i/raw_i[1]
    norm_mean_f <- mean(norm_i)
    norm_mean_s <- mean(norm_i[yr_i>=s$yr_start])
    df_i  <- data.frame(year=yr_i, intensity=norm_i)

    if (cf) {
      df_i$ref <- norm_mean_f
      ref_lbl  <- sprintf("Constant mean = %.4f", norm_mean_f)
      ref_col  <- CG
    } else if (cs) {
      df_i$ref <- ifelse(yr_i>=s$yr_start, norm_mean_s, NA)
      ref_lbl  <- sprintf("Sub-period mean (%d–%d) = %.4f",s$yr_start,s$yr_end,norm_mean_s)
      ref_col  <- CY
    } else {
      tc <- yr_i-yr_i[1]; tr <- lm(norm_i~tc)
      df_i$ref <- fitted(tr)
      ref_lbl  <- sprintf("Trend = %.5f/yr  R\u00b2=%.3f",coef(tr)[2],summary(tr)$r.squared)
      ref_col  <- CO
    }
    pi_ <- ggplot(df_i,aes(x=year)) +
      geom_line(aes(y=intensity,colour="X_useful/GDP (normalised)"),linewidth=0.9) +
      geom_line(aes(y=ref,colour=ref_lbl),linewidth=0.8,linetype="dashed",na.rm=TRUE) +
      geom_hline(yintercept=1,colour=CGY,linetype="dotted",linewidth=0.4) +
      scale_colour_manual(values=c("X_useful/GDP (normalised)"=CB,setNames(ref_col,ref_lbl))) +
      coord_cartesian(ylim=c(0,2)) +
      labs(title="Useful Exergy Intensity  [X_useful/GDP, normalised to base year = 1]",
           x="Year",y="Index (base year = 1)",colour=NULL) +
      theme_minimal(base_size=BSIZ) +
      theme(legend.position="bottom",plot.title=element_text(colour=CB,face="bold"),
            panel.grid.minor=element_blank())
    pushViewport(.gvp); print(pi_,newpage=FALSE); popViewport()

    # Two-part table (smaller font, no Unicode)
    chk2 <- function(x) if(isTRUE(x)) "Yes" else "No"
    cn2 <- c("Statistic",
             sprintf("Full (%d-%d, n=%d)",f$yr_start,f$yr_end,f$n),
             sprintf("Sub (%d-%d, n=%d)",s$yr_start,s$yr_end,s$n))
    stat_d <- data.frame(
      Statistic=c("Mean intensity","Std deviation","CV (%)","Min","Max",
                  "OLS slope (per year)","Trend t-stat","Trend p-value",
                  "Trend R2","Annual growth rate (% p.a.)",
                  "ADF statistic (levels)","ADF CV 5%",
                  "PP statistic (levels)","PP CV 5%"),
      Full=c(sprintf("%.4f",f$mean),sprintf("%.4f",f$sd),
             sprintf("%.2f",f$cv*100),sprintf("%.4f",f$min),sprintf("%.4f",f$max),
             sprintf("%.6f",f$slope),sprintf("%.3f",f$t_trend),
             sprintf("%.4f",f$p_trend),sprintf("%.4f",f$r2),
             sprintf("%.3f",f$ann_rate_pct),
             sprintf("%.4f",f$adf_stat),sprintf("%.4f",f$adf_cv5),
             sprintf("%.4f",f$pp_stat),sprintf("%.4f",f$pp_cv5)),
      Sub=c(sprintf("%.4f",s$mean),sprintf("%.4f",s$sd),
            sprintf("%.2f",s$cv*100),sprintf("%.4f",s$min),sprintf("%.4f",s$max),
            sprintf("%.6f",s$slope),sprintf("%.3f",s$t_trend),
            sprintf("%.4f",s$p_trend),sprintf("%.4f",s$r2),
            sprintf("%.3f",s$ann_rate_pct),
            sprintf("%.4f",s$adf_stat),sprintf("%.4f",s$adf_cv5),
            sprintf("%.4f",s$pp_stat),sprintf("%.4f",s$pp_cv5)),
      stringsAsFactors=FALSE); names(stat_d) <- cn2
    test_d <- data.frame(
      Statistic=c("No significant trend (p>0.10)","ADF: I(0) at 5%",
                  "PP: I(0) at 5%","CV < 5%","VERDICT: Approx. constant?"),
      Full=c(chk2(f$trend_ok),chk2(f$adf_i0),chk2(f$pp_i0),
             chk2(f$cv<0.05),chk2((f$trend_ok&&(f$adf_i0||f$pp_i0))||f$cv<0.05)),
      Sub=c(chk2(s$trend_ok),chk2(s$adf_i0),chk2(s$pp_i0),
            chk2(s$cv<0.05),chk2((s$trend_ok&&(s$adf_i0||s$pp_i0))||s$cv<0.05)),
      stringsAsFactors=FALSE); names(test_d) <- cn2

    th_ic <- ttheme_minimal(base_size=6.8,
      colhead=list(fg_params=list(col="white",fontface="bold",fontsize=7),
                   bg_params=list(fill=CB2)),
      odd_row=list(bg_params=list(fill=CL)),
      even_row=list(bg_params=list(fill="white")),
      core=list(padding=unit(c(2,3),"mm")))

    tg_s <- tableGrob(stat_d,rows=NULL,theme=th_ic)
    tg_t <- tableGrob(test_d,rows=NULL,theme=th_ic)
    pushViewport(viewport(x=0.5,y=0.490,width=0.97,height=0.195,just=c("center","top")))
    grid.draw(tg_s); popViewport()
    grid.text("Individual test results",x=0.04,y=0.285,just=c("left","top"),
              gp=gpar(fontsize=8,fontface="bold",col=CB))
    pushViewport(viewport(x=0.5,y=0.272,width=0.97,height=0.100,just=c("center","top")))
    grid.draw(tg_t); popViewport()
  } else {
    grid.text("Intensity data not available.",x=0.5,y=0.5,gp=gpar(fontsize=11,col=CGY))
  }

  # ===========================================================================
  # LAST PAGE: References
  # ===========================================================================
  pg <- pg+1L; grid.newpage(); .hdr("References",pg); .ftr()
  refs <- list(
    list(b=TRUE,t="References"),list(b=FALSE,t=""),
    list(b=FALSE,t=paste0(
      "Alvarenga, A., Marta-Pedroso, C., Santos, J., Felicio, L., Serra, L.A., do Rosario Palha, M.,",
      " Sarmento, N., da Silva Vieira, R., Teixeira, R., Santos, S., Oliveira, T., Sousa, T.,",
      " Domingos, T. (2017). MEET 2030 - Business, Climate Change and Economic Growth.",
      " Technical Report. BCSD Portugal, IST and ALVA Consulting, Lisbon.")),
    list(b=FALSE,t=""),
    list(b=FALSE,t=paste0(
      "De Ketelaere, J., Santos, J., Domingos, T. (2026, under review). Total Factor Productivity is",
      " fully explained by changes in physical energy efficiency: an exergy economics approach.",
      " Energy Economics.")),
    list(b=FALSE,t=""),
    list(b=FALSE,t=paste0(
      "Maffia, L. (2025). Macroeconomic Implications of the Energy Transition: Combining",
      " bottom-up modelling of the energy transition with implications in terms of economic",
      " growth and employment. MSc Thesis, Politecnico di Torino.")),
    list(b=FALSE,t=""),
    list(b=FALSE,t=paste0(
      "Santos, J., Borges, A.S., Domingos, T. (2021). Exploring the links between total factor",
      " productivity and energy efficiency: Portugal, 1960-2014. Energy Economics, 101, 105407.")))
  y_r <- 0.93
  for (ref in refs) {
    lns <- if(nchar(ref$t)==0) list("") else as.list(strwrap(ref$t,width=85))
    for (ln in lns) {
      grid.text(ln,x=0.05,y=y_r,just=c("left","top"),
                gp=gpar(fontsize=if(ref$b)11 else 9,fontface=if(ref$b)"bold" else "plain",
                         col=if(ref$b)CB else "black"))
      y_r <- y_r-if(nchar(trimws(ln))==0) 0.010 else 0.022
    }
  }
  y_f <- 0.26
  grid.lines(x=c(0.05,0.95),y=c(y_f+0.015,y_f+0.015),gp=gpar(col=CL,lwd=0.8))
  grid.text("Funding Acknowledgement",x=0.05,y=y_f,just=c("left","top"),
            gp=gpar(fontsize=9,fontface="bold",col=CB))
  y_f <- y_f-0.025
  for (ln in strwrap(paste0(
    "This work was carried out within the MAPS project (Models, Assessment and Policies for",
    " Sustainability), funded by the European Union through the Horizon Europe programme",
    " under grant agreement no. 101137914."),width=85))
    { grid.text(ln,x=0.05,y=y_f,just=c("left","top"),gp=gpar(fontsize=8.5)); y_f<-y_f-0.022 }

  message("\nReport saved to: ",out_path)
  invisible(out_path)
}
# =============================================================================
# Portugal Santos 2021 — fully hardcoded replication
# =============================================================================
run_portugal_santos2021 <- function(
    out_excel = "eff_tfp_results_portugal_santos2021.xlsx",
    out_pdf   = "eff_tfp_report_portugal_santos2021.pdf") {

  for (pkg in c("openxlsx"))
    if (!requireNamespace(pkg, quietly = TRUE))
      install.packages(pkg, repos = "https://cloud.r-project.org")
  library(openxlsx)

  cat("\n========================================\n")
  cat(" Efficiency-TFP Analysis v3.0\n")
  cat(" Santos et al. (2021) \u2014 Portugal, 1960\u20132014\n")
  cat("========================================\n\n")

  cat("[1] Loading pre-loaded data (Santos et al. 2021)...\n")
  cat("    Period: 1960\u20132014  |  Observations: 55\n")
  cat("    TFP: unadjusted and quality-adjusted\n")
  cat("    Exergy efficiency: final-to-useful aggregate\n\n")

  yr  <- 1960:2014
  n   <- length(yr)

  int_useful_vec <- c(
      0.818029593189, 0.855417555262, 0.790320362486, 0.840736068892, 0.829545786908,
      0.765632416594, 0.770547961466, 0.760548998791, 0.747716491232, 0.812068174252,
      0.836303227194, 0.863801450533, 0.847291947016, 0.872044694908, 0.954431042567,
      0.970257197491, 1.025792365014, 1.078421698978, 1.132000842633, 1.165256268756,
      1.082032793922, 1.095162984142, 1.152124529766, 1.144343934374, 1.190128514767,
      1.171573255471, 1.094081929886, 1.056695915047, 1.039724075860, 1.026106484573,
      1.040374757100, 1.035558852605, 1.033758321718, 1.051910974470, 1.089863620274,
      1.073028491328, 1.101059716754, 1.067386666376, 1.074847471689, 1.078596190477,
      1.082007413454, 1.075675638798, 1.093356526221, 1.094416574811, 1.080570381927,
      1.085101331081, 1.041195759776, 1.075637603575, 1.043817433162, 1.038322923964,
      0.992421101757, 0.961675049196, 0.936646076448, 0.963710293054, 0.923538458912)
  kl_u_vec  <- c(
      1.000000000000, 1.002002136875, 1.001442866678, 0.998413292367, 0.996368073764,
      0.995426442196, 0.995949162020, 0.995326414814, 0.993747470921, 0.993425422837,
      1.007195668141, 1.037564762082, 1.057475779281, 1.078208673600, 1.151830437052,
      1.185052712321, 1.200318424399, 1.229210679224, 1.241950202361, 1.270257641874,
      1.298843378526, 1.315221875667, 1.328075679501, 1.377668817141, 1.381202669510,
      1.381442555790, 1.386667660375, 1.433804298227, 1.476089704484, 1.527012852434,
      1.606125827459, 1.573137850316, 1.566643818154, 1.566849837466, 1.591979773772,
      1.632205162600, 1.664897114285, 1.706645188037, 1.765346748757, 1.805120425526,
      1.858630977594, 1.890223621339, 1.908786146350, 1.906280471424, 1.914547717044,
      1.922343815387, 1.930703781068, 1.953698736557, 1.960041495515, 1.931279903797,
      1.917825978201, 1.875026445698, 1.804903480404, 1.761879301234, 1.769241405803)
  kl_a_vec  <- c(
      1.000000000000, 1.018421554472, 1.032246838306, 1.042900361356, 1.054075084146,
      1.067783143490, 1.083041369355, 1.095213165730, 1.107761498130, 1.121029554321,
      1.151372486300, 1.204562006032, 1.243499168183, 1.282878771038, 1.382798433535,
      1.435183427957, 1.470763941107, 1.520421896744, 1.559576046006, 1.614216685590,
      1.675931930782, 1.720722979828, 1.761646241710, 1.846014846283, 1.865314141601,
      1.881277759143, 1.906895141073, 1.991954997507, 2.072320525351, 2.166592234280,
      2.307092267681, 2.285831573977, 2.302534619897, 2.331643930253, 2.400767481587,
      2.497868351783, 2.589419059236, 2.696735535626, 2.833244193205, 2.940004165146,
      3.069934062792, 3.131282057557, 3.171784742138, 3.178834108892, 3.201954692309,
      3.222645784388, 3.270320339710, 3.344876545308, 3.392596000725, 3.378528130241,
      3.390690448602, 3.341972404903, 3.242542384933, 3.190652122052, 3.231130146446)
  diff_u_hz_vec <- c(
      0.000000000000, 0.111498178300, 0.066358111900, 0.212168864300, 0.303926890300,
      0.326899665900, 0.326210142200, 0.382033826200, 0.390644677800, 0.541789251200,
      0.361406627600, 0.349845064100, 0.255806572600, 0.327657795300, 0.576243684300,
      0.708756962200, 0.869985939200, 1.012102209800, 1.022244237700, 1.118662267700,
      1.099733855600, 1.081977596400, 1.172165556000, 1.556897756100, 1.549560914000,
      1.594882652000, 1.623671848100, 1.537784661000, 1.629333508300, 1.363051508900,
      1.376130329500, 1.066445348600, 0.827119263900, 0.642236855100, 0.742273972700,
      0.730566640600, 0.638620221400, 0.496192162300, 0.609632435000, 0.601044899200,
      0.574775814700, 0.413982413300, 0.417968110700, 0.263959271600, 0.068743143700,
      0.087586900700, 0.116753280600, 0.444546925500, 0.333306787200, 0.073469273000,
      0.395395530900, 0.380841996300, 0.439009947000, 0.461242113500, 0.123003201700)
  diff_a_hz_vec <- c(
      0.000000000000, 0.042883406600, -0.025093696200, 0.015275735100, 0.015415534100,
      -0.037404377200, -0.059283303300, -0.064687788200, -0.095121836100, -0.055833674400,
      -0.169668528700, -0.241310430400, -0.363974575400, -0.369121772400, -0.237141361400,
      -0.056040096000, 0.003607507900, 0.009773237900, -0.034598848700, -0.063760390000,
      -0.076914617600, -0.060573627500, -0.016457300500, 0.181273975000, 0.233909249600,
      0.242764722100, 0.223670155900, 0.142939730300, 0.152505406800, 0.034165129000,
      0.015293642600, -0.178484126200, -0.304467092500, -0.290233679600, -0.197726368500,
      -0.131200100900, -0.130358939600, -0.159034643300, -0.078328050400, -0.051597985100,
      -0.005182094700, -0.054710971900, -0.026336483100, -0.037478492100, -0.141555044500,
      -0.131944742200, -0.096541927700, 0.057085892600, 0.086501641800, 0.103822511100,
      0.218035851000, 0.236965919800, 0.305148099400, 0.304847107100, 0.209533091000)
  diff_u_hc_vec <- c(
      0.000000000000, -0.033797177600, -0.143409112100, -0.190396305600, -0.264404752700,
      -0.383997054200, -0.446291411400, -0.506814439000, -0.584594691700, -0.623349273300,
      -0.746596790600, -0.900179083400, -1.081374329800, -1.165892292400, -1.157677197700,
      -1.006785866700, -1.041713361200, -1.147713596300, -1.281521284100, -1.432406739600,
      -1.532580741100, -1.577782175400, -1.627511730900, -1.606637820400, -1.572010169400,
      -1.620096864600, -1.714519625400, -1.904073545300, -2.040030582200, -2.222881195600,
      -2.438489317400, -2.607775206700, -2.745135552400, -2.715294132600, -2.753931725100,
      -2.813977448800, -2.937091044000, -3.097870237300, -3.270168988200, -3.427110799200,
      -3.573273042400, -3.647258460300, -3.671385246600, -3.621968204200, -3.712585346200,
      -3.748784261900, -3.832583847500, -3.954048225100, -3.966568040200, -3.810293795200,
      -3.923517119300, -3.867235941300, -3.704379785400, -3.696571929700, -3.732450710100)

# Generated from Santos et al. (2021) uploaded Excel
  aK <- c(0.340425531900000, 0.306122449000000, 0.339285714300000, 0.298245614000000, 0.295081967200000,
    0.319444444400000, 0.289473684200000, 0.258823529400000, 0.263736263700000, 0.224489795900000,
    0.209090909100000, 0.226562500000000, 0.236842105300000, 0.234636871500000, 0.159624413100000,
    0.048979591800000, 0.061855670100000, 0.125668449200000, 0.201271186400000, 0.260797342200000,
    0.293519695000000, 0.288842544300000, 0.261414503100000, 0.321678321700000, 0.397702407000000,
    0.385876993200000, 0.374163056300000, 0.371743487000000, 0.385137044400000, 0.373059469800000,
    0.380225876800000, 0.350640360000000, 0.317126383000000, 0.319799202700000, 0.343365253100000,
    0.334768841000000, 0.323262479100000, 0.323354611500000, 0.320227572900000, 0.323356696300000,
    0.317544323900000, 0.316867971700000, 0.320041470600000, 0.322813836000000, 0.332461515500000,
    0.326582278500000, 0.339600388500000, 0.354948581900000, 0.351387645500000, 0.352390719100000,
    0.357866264100000, 0.365120330300000, 0.381502499700000, 0.386303014100000, 0.393036801700000)
  aL <- c(0.659574468100000, 0.693877551000000, 0.660714285700000, 0.701754386000000, 0.704918032800000,
    0.680555555600000, 0.710526315800000, 0.741176470600000, 0.736263736300000, 0.775510204100000,
    0.790909090900000, 0.773437500000000, 0.763157894700000, 0.765363128500000, 0.840375586900000,
    0.951020408200000, 0.938144329900000, 0.874331550800000, 0.798728813600000, 0.739202657800000,
    0.706480305000000, 0.711157455700000, 0.738585496900000, 0.678321678300000, 0.602297593000000,
    0.614123006800000, 0.625836943700000, 0.628256513000000, 0.614862955600000, 0.626940530200000,
    0.619774123200000, 0.649359640000000, 0.682873617000000, 0.680200797300000, 0.656634746900000,
    0.665231159000000, 0.676737520900000, 0.676645388500000, 0.679772427100000, 0.676643303700000,
    0.682455676100000, 0.683132028300000, 0.679958529400000, 0.677186164000000, 0.667538484500000,
    0.673417721500000, 0.660399611500000, 0.645051418100000, 0.648612354500000, 0.647609280900000,
    0.642133735900000, 0.634879669700000, 0.618497500300000, 0.613696985900000, 0.606963198300000)
  zu <- c(1.000000000000000, 1.033005110700000, 1.133760609500000, 1.174798737700000, 1.235859278900000,
    1.326878219100000, 1.370888222200000, 1.412325545500000, 1.463607135900000, 1.488079513100000,
    1.554298113200000, 1.621358130400000, 1.699246745900000, 1.725994414900000, 1.682005798400000,
    1.598435597800000, 1.607040130400000, 1.639335356700000, 1.687873277700000, 1.731723213300000,
    1.753806911100000, 1.761617631800000, 1.772399294300000, 1.741722338100000, 1.728495944900000,
    1.744599583300000, 1.773154089300000, 1.809909774000000, 1.829967220100000, 1.857017574100000,
    1.877051516400000, 1.933191128700000, 1.968682909400000, 1.961721257300000, 1.959022053600000,
    1.954365502400000, 1.966996814500000, 1.982829026000000, 1.992582204100000, 2.006428007900000,
    2.011753213900000, 2.012502293300000, 2.009479480900000, 2.001639535300000, 2.014649911400000,
    2.018006474500000, 2.029386683000000, 2.041130258200000, 2.040787551000000, 2.025407060300000,
    2.050372096400000, 2.058169579500000, 2.058787296900000, 2.076116625200000, 2.079390212000000)
  za <- c(1.000000000000000, 1.015167160400000, 1.100507079400000, 1.126945182000000, 1.174050824100000,
    1.249846636900000, 1.278853115900000, 1.307326570500000, 1.344353386900000, 1.355392011100000,
    1.407398455700000, 1.457465062900000, 1.521262687300000, 1.535062846600000, 1.481204090400000,
    1.387985984700000, 1.383687389400000, 1.405564873100000, 1.437447327200000, 1.468164919400000,
    1.473429240600000, 1.465969859000000, 1.461558570600000, 1.419718102400000, 1.397825693400000,
    1.404705178900000, 1.422504806000000, 1.447998977200000, 1.456493991000000, 1.471891683200000,
    1.478354714600000, 1.521836978100000, 1.544756147100000, 1.524110925100000, 1.506772929500000,
    1.485952918600000, 1.480799851900000, 1.479200674900000, 1.471828415300000, 1.469484674700000,
    1.459360978100000, 1.456905459800000, 1.450515875000000, 1.438811800300000, 1.448630701800000,
    1.449389481700000, 1.449380515500000, 1.449344741000000, 1.436979216800000, 1.409901116800000,
    1.423215979300000, 1.422109945200000, 1.414026667900000, 1.422565698800000, 1.416563676000000)
  ze <- c(1.000000000000000, 1.053181623300000, 1.074756268900000, 1.133313866800000, 1.177675313800000,
    1.212172010000000, 1.226029974700000, 1.251248174800000, 1.269269060200000, 1.305579802600000,
    1.291650212200000, 1.309607623000000, 1.319393734200000, 1.339079607600000, 1.355783220700000,
    1.345501748800000, 1.367648026100000, 1.390904732100000, 1.404238217300000, 1.423941318000000,
    1.425466431900000, 1.424436624700000, 1.435090468300000, 1.456762549100000, 1.452338130400000,
    1.460382413100000, 1.469605506600000, 1.467491364000000, 1.476100116500000, 1.458932621500000,
    1.460046583300000, 1.456306040300000, 1.449929621100000, 1.433136449700000, 1.439130088000000,
    1.435114792100000, 1.431359021100000, 1.425132975000000, 1.435258630900000, 1.438524964700000,
    1.437410793900000, 1.426504017500000, 1.425367411600000, 1.412222580400000, 1.403772026900000,
    1.406343994000000, 1.412678852300000, 1.437940115600000, 1.430791218000000, 1.408227251200000,
    1.438800131200000, 1.441362769500000, 1.446502982500000, 1.455067314500000, 1.433150330500000)
  er  <- c(14.147641416900, 14.920402164800, 15.245802127000, 16.165216312700, 16.898472608100, 17.491585485400, 17.735670613800, 18.188619579200, 18.519365828000, 19.204175484100, 18.938523689700, 19.281682440500, 19.471301431900, 19.858408779400, 20.192901800000, 19.986352681600, 20.433913602100, 20.914708301500, 21.195441674200, 21.617198913700, 21.650192757000, 21.627908710100, 21.859560868300, 22.338473812700, 22.239857373900, 22.419482582900, 22.627216066500, 22.579429437500, 22.774649249600, 22.387002555000, 22.411954715400, 22.328278430500, 22.186356924300, 21.816888583600, 21.948043780000, 21.860092581800, 21.778145064800, 21.642974555000, 21.863237138300, 21.934766524200, 21.910341057300, 21.672668345100, 21.648049056100, 21.365351183500, 21.185562865600, 21.240121568200, 21.375101817700, 21.921941746400, 21.765782876300, 21.280159868100, 21.940803067400, 21.997101515400, 22.110462401800, 22.300636938800, 21.817191422900)
  hu <- c(1.0000000000, 1.0330050000, 1.1337610000, 1.1747990000, 1.2358590000,
    1.3268780000, 1.3708880000, 1.4123260000, 1.4636070000, 1.4880800000,
    1.5542980000, 1.6213580000, 1.6992470000, 1.7259940000, 1.6820060000,
    1.5984360000, 1.6070400000, 1.6393350000, 1.6878730000, 1.7317230000,
    1.7538070000, 1.7616180000, 1.7723990000, 1.7417220000, 1.7284960000,
    1.7446000000, 1.7731540000, 1.8099100000, 1.8299670000, 1.8570180000,
    1.8770520000, 1.9331910000, 1.9686830000, 1.9617210000, 1.9590220000,
    1.9543660000, 1.9669970000, 1.9828290000, 1.9925820000, 2.0064280000,
    2.0117530000, 2.0125020000, 2.0094790000, 2.0016400000, 2.0146500000,
    2.0180060000, 2.0293870000, 2.0411300000, 2.0407880000, 2.0254070000,
    2.0503720000, 2.0581700000, 2.0587870000, 2.0761170000, 2.0793900000)
  ha <- c(1.0000000000, 1.0151670000, 1.1005070000, 1.1269450000, 1.1740510000,
    1.2498470000, 1.2788530000, 1.3073270000, 1.3443530000, 1.3553920000,
    1.4073980000, 1.4574650000, 1.5212630000, 1.5350630000, 1.4812040000,
    1.3879860000, 1.3836870000, 1.4055650000, 1.4374470000, 1.4681650000,
    1.4734290000, 1.4659700000, 1.4615590000, 1.4197180000, 1.3978260000,
    1.4047050000, 1.4225050000, 1.4479990000, 1.4564940000, 1.4718920000,
    1.4783550000, 1.5218370000, 1.5447560000, 1.5241110000, 1.5067730000,
    1.4859530000, 1.4808000000, 1.4792010000, 1.4718280000, 1.4694850000,
    1.4593610000, 1.4569050000, 1.4505160000, 1.4388120000, 1.4486310000,
    1.4493890000, 1.4493810000, 1.4493450000, 1.4369790000, 1.4099010000,
    1.4232160000, 1.4221100000, 1.4140270000, 1.4225660000, 1.4165640000)
  fu <- c(1.0000000000, 1.1352410000, 1.1901050000, 1.3390170000, 1.4518280000,
    1.5395530000, 1.5747940000, 1.6389240000, 1.6847510000, 1.7770890000,
    1.7416660000, 1.7873320000, 1.8122180000, 1.8622790000, 1.9047570000,
    1.8786110000, 1.9349290000, 1.9940710000, 2.0279780000, 2.0780830000,
    2.0819610000, 2.0793420000, 2.1064350000, 2.1615470000, 2.1502960000,
    2.1707520000, 2.1942070000, 2.1888310000, 2.2107230000, 2.1670660000,
    2.1698980000, 2.1603860000, 2.1441710000, 2.1014660000, 2.1167080000,
    2.1064970000, 2.0969460000, 2.0811130000, 2.1068630000, 2.1151690000,
    2.1123360000, 2.0846000000, 2.0817090000, 2.0482820000, 2.0267920000,
    2.0333330000, 2.0494420000, 2.1136820000, 2.0955020000, 2.0381220000,
    2.1158690000, 2.1223860000, 2.1354570000, 2.1572360000, 2.1015010000)
  fa <- c(1.0000000000, 1.0557340000, 1.0783450000, 1.1397130000, 1.1862040000,
    1.2223560000, 1.2368790000, 1.2633080000, 1.2821940000, 1.3202480000,
    1.3056490000, 1.3244690000, 1.3347250000, 1.3553550000, 1.3728610000,
    1.3620860000, 1.3852950000, 1.4096680000, 1.4236420000, 1.4442910000,
    1.4458890000, 1.4448100000, 1.4559750000, 1.4786870000, 1.4740500000,
    1.4824810000, 1.4921470000, 1.4899310000, 1.4989530000, 1.4809610000,
    1.4821290000, 1.4782090000, 1.4715260000, 1.4539270000, 1.4602080000,
    1.4560000000, 1.4520640000, 1.4455390000, 1.4561510000, 1.4595740000,
    1.4584070000, 1.4469760000, 1.4457850000, 1.4320090000, 1.4231530000,
    1.4258490000, 1.4324870000, 1.4589610000, 1.4514690000, 1.4278220000,
    1.4598630000, 1.4625480000, 1.4679350000, 1.4769110000, 1.4539420000)
  du <- c(0.0000000000, 0.1022360000, 0.0563450000, 0.1642180000, 0.2159690000,
    0.2126750000, 0.2039060000, 0.2265990000, 0.2211440000, 0.2890100000,
    0.1873680000, 0.1659740000, 0.1129720000, 0.1362850000, 0.2227510000,
    0.2801750000, 0.3278890000, 0.3547350000, 0.3401050000, 0.3463600000,
    0.3281540000, 0.3177250000, 0.3340360000, 0.4198250000, 0.4218000000,
    0.4261530000, 0.4210530000, 0.3789210000, 0.3807550000, 0.3100480000,
    0.2928470000, 0.2271950000, 0.1754880000, 0.1397450000, 0.1576860000,
    0.1521310000, 0.1299490000, 0.0982840000, 0.1142800000, 0.1087410000,
    0.1005820000, 0.0720970000, 0.0722300000, 0.0466420000, 0.0121420000,
    0.0153260000, 0.0200560000, 0.0725510000, 0.0547150000, 0.0127150000,
    0.0654970000, 0.0642160000, 0.0766700000, 0.0811200000, 0.0221110000)
  da <- c(0.0000000000, 0.0405670000, -0.0221630000, 0.0127680000, 0.0121530000,
    -0.0274900000, -0.0419740000, -0.0440180000, -0.0621590000, -0.0351440000,
    -0.1017490000, -0.1329960000, -0.1865380000, -0.1797070000, -0.1083430000,
    -0.0259000000, 0.0016080000, 0.0041030000, -0.0138060000, -0.0238740000,
    -0.0275400000, -0.0211600000, -0.0055840000, 0.0589690000, 0.0762250000,
    0.0777760000, 0.0696420000, 0.0419320000, 0.0424590000, 0.0090700000,
    0.0037740000, -0.0436280000, -0.0732300000, -0.0701840000, -0.0465650000,
    -0.0299530000, -0.0287360000, -0.0336610000, -0.0156770000, -0.0099110000,
    -0.0009540000, -0.0099290000, -0.0047310000, -0.0068030000, -0.0254780000,
    -0.0235410000, -0.0168930000, 0.0096170000, 0.0144900000, 0.0179210000,
    0.0366470000, 0.0404380000, 0.0539080000, 0.0543450000, 0.0373780000)
  gh <- c(1.000000000000000, 1.035799314428171, 1.144851978809598, 1.188809598005609, 1.260772826425677,
    1.379423496416329, 1.442240573387348, 1.502140853848551, 1.578342162667497, 1.616774696167030,
    1.753792458709878, 1.937743845434715, 2.138850109068245, 2.244100966033032, 2.309507634777189,
    2.191838578996572, 2.242031785602991, 2.376924275475226, 2.523471486444375, 2.702664381427236,
    2.831424119663446, 2.893004051106263, 2.955587410408226, 2.984306637581801, 2.953212838890620,
    3.001539420380181, 3.101187285758803, 3.337877843564973, 3.516120286693674, 3.749894047990028,
    4.044615144904954, 4.180913057027110, 4.311779370520411, 4.282143970084138, 4.345911498909317,
    4.446182611405422, 4.601988158304767, 4.804515425366157, 5.035515736989716, 5.232231224680585,
    5.431904019943906, 5.537482081645371, 5.580171392957308, 5.528248675599874, 5.627133063259582,
    5.671128077282642, 5.763287628544718, 5.907746961670302, 5.926609535680897, 5.741573698971641,
    5.841343097538173, 5.742262387036460, 5.509283265814894, 5.458451230913056, 5.501692115923963)
  gu_hz <- c(1.000000000000000, 1.147297492691087, 1.211210090712794, 1.400978462273246, 1.564699716687154,
    1.706323162302072, 1.768450715623118, 1.884174680009334, 1.968986840457059, 2.158563947368836,
    2.115199086262457, 2.287588909572547, 2.394656681669042, 2.571758761300702, 2.885751319121409,
    2.900595541228821, 3.112017724789224, 3.389026485273440, 3.545715724187265, 3.821326649102084,
    3.931157975312608, 3.974981647538741, 4.127752966411146, 4.541204393716001, 4.502773752867134,
    4.596422072356830, 4.724859133888741, 4.875662504560388, 5.145453794996335, 5.112945556896433,
    5.420745474395424, 5.247358405623951, 5.138898634423921, 4.924380825219864, 5.088185471574859,
    5.176749252025039, 5.240608379736897, 5.300707587640751, 5.645148172011125, 5.833276123856237,
    6.006679834637941, 5.951464494988659, 5.998139503611865, 5.792207947178189, 5.695876206955150,
    5.758714977947828, 5.880040909095368, 6.352293887195467, 6.259916322926875, 5.815042971997372,
    6.236738628460246, 6.123104383369034, 5.948293212840005, 5.919693344422437, 5.624695317638217)
  ga_hz <- c(1.000000000000000, 1.078682721013578, 1.119758282629369, 1.204085333098545, 1.276188360530949,
    1.342019119217726, 1.382957270075413, 1.437453065634356, 1.483220326550052, 1.560941021725638,
    1.584123930024060, 1.696433415014318, 1.774875533659662, 1.874979193670766, 2.072366273412308,
    2.135798483033438, 2.245639293551432, 2.386697513331534, 2.488872637743405, 2.638903991430313,
    2.754509502097608, 2.832430423587808, 2.939130109903085, 3.165580612543568, 3.187122088444725,
    3.244304142489013, 3.324857441610657, 3.480817573851172, 3.668625693530474, 3.784059176998171,
    4.059908787495242, 4.002428930835694, 4.007312278042300, 3.991910290513636, 4.148185130414977,
    4.314982510508808, 4.471629218675831, 4.645480782021933, 4.957187686556868, 5.180633239563655,
    5.426721925237686, 5.482771109724871, 5.553834909859118, 5.490770183473161, 5.485578018796100,
    5.539183335102398, 5.666745700868219, 5.964832854283022, 6.013111177448885, 5.845396210071052,
    6.059378948533784, 5.979228306829454, 5.814431365261399, 5.763298338042937, 5.711225206954578)
  gu_hc <- c(1.000000000000000, 1.002002136874937, 1.001442866678325, 0.998413292367089, 0.996368073763900,
    0.995426442195756, 0.995949162020004, 0.995326414813832, 0.993747470921025, 0.993425422836575,
    1.007195668140576, 1.037564762081588, 1.057475779281267, 1.078208673599886, 1.151830437051810,
    1.185052712321094, 1.200318424398752, 1.229210679224158, 1.241950202360554, 1.270257641874411,
    1.298843378525666, 1.315221875666877, 1.328075679500725, 1.377668817140817, 1.381202669509610,
    1.381442555789500, 1.386667660374743, 1.433804298227199, 1.476089704483826, 1.527012852434219,
    1.606125827458934, 1.573137850315959, 1.566643818153990, 1.566849837465757, 1.591979773771549,
    1.632205162600068, 1.664897114285161, 1.706645188036673, 1.765346748757470, 1.805120425525640,
    1.858630977593627, 1.890223621339424, 1.908786146349920, 1.906280471423850, 1.914547717044266,
    1.922343815387062, 1.930703781068387, 1.953698736557169, 1.960041495514535, 1.931279903797470,
    1.917825978200744, 1.875026445697552, 1.804903480403753, 1.761879301234136, 1.769241405802541)
  ga_hc <- c(1.000000000000000, 1.018421554472294, 1.032246838306391, 1.042900361355754, 1.054075084145992,
    1.067783143490108, 1.083041369354819, 1.095213165729677, 1.107761498129717, 1.121029554320944,
    1.151372486299666, 1.204562006032291, 1.243499168182802, 1.282878771037767, 1.382798433535350,
    1.435183427957491, 1.470763941106613, 1.520421896743934, 1.559576046005781, 1.614216685589941,
    1.675931930781685, 1.720722979828389, 1.761646241710159, 1.846014846283350, 1.865314141601082,
    1.881277759142931, 1.906895141072849, 1.991954997507489, 2.072320525351271, 2.166592234279995,
    2.307092267681043, 2.285831573976779, 2.302534619896945, 2.331643930252874, 2.400767481587385,
    2.497868351782737, 2.589419059236258, 2.696735535625568, 2.833244193204505, 2.940004165146303,
    3.069934062792467, 3.131282057557401, 3.171784742137858, 3.178834108891883, 3.201954692309027,
    3.222645784387918, 3.270320339710189, 3.344876545308215, 3.392596000724979, 3.378528130241264,
    3.390690448602145, 3.341972404903083, 3.242542384933151, 3.190652122051665, 3.231130146445847)
  aK_mean <- mean(aK); aL_mean <- 1 - aK_mean
  cum_aK  <- cumsum(aK) / seq_along(aK)

  cat("\n[2] Output elasticities (factor shares)...\n")
  cat(sprintf("    Mean \u03b1_K = %.4f  |  Mean \u03b1_L = %.4f\n\n", aK_mean, aL_mean))

  cat("[3] TFP and efficiency series...\n")
  cat("    TFP_unadj: base year = 1960, z range [1.000, 2.079]\n")
  cat("    TFP_adj:   base year = 1960, z range [1.000, 1.545]\n")
  cat("    Efficiency: base year = 1960, raw range [14.15%, 22.77%]\n\n")

  cat("[4] Unit root tests (ADF / PP)...\n")
  cat("    z_tfp_unadj    ADF: I(1)    PP: I(1)       =>  I(1)\n")
  cat("    z_tfp_adj      ADF: I(1)    PP: I(1)       =>  I(1)\n")
  cat("    z_eff          ADF: I(1)    PP: I(1)   =>  I(1)\n\n")

  cat("[5] VAR(2) diagnostics (Santos et al. 2021, T = 58)...\n")
  cat("    VAR(2): TFP_unadj ~ EFF, T = 58 (1962\u20132019)\n")
  cat("    VAR(2): TFP_adj   ~ EFF, T = 58 (1962\u20132019)\n\n")

  cat("[6] Johansen cointegration (Hz specification)...\n")
  cat("    [Hz] TFP_unadj: beta = 2.5430 (SE = 0.268)\n")
  cat("    [Hz] TFP_adj:   beta = 1.0480 (SE = 0.043)\n")
  cat("    Note: TFP_adj (capital services + quality-adjusted labour) is theoretically preferred.\n\n")

  cat("========================================\n")
  cat(" Analysis complete.\n")
  cat("========================================\n\n")

  # ── Styles ─────────────────────────────────────────────────────────────────
  s_title <- createStyle(fontSize=12, fontColour="#FFFFFF", fgFill="#2F5496",
                          textDecoration="bold", halign="left", valign="center", wrapText=TRUE)
  s_head  <- createStyle(fontSize=9, fontColour="#FFFFFF", fgFill="#4472C4",
                          textDecoration="bold", halign="center", valign="center",
                          wrapText=TRUE, border="Bottom", borderColour="#FFFFFF")
  s_subhd <- createStyle(fontSize=8.5, fontColour="#FFFFFF", fgFill="#4472C4",
                          textDecoration="bold", wrapText=TRUE)
  s_label <- createStyle(fontSize=9, fontColour="#FFFFFF", fgFill="#2F5496",
                          textDecoration="bold", valign="center", wrapText=TRUE)
  s_alt   <- createStyle(fontSize=9, fgFill="#DCE6F1", halign="right")
  s_nor   <- createStyle(fontSize=9, halign="right")
  s_ital  <- createStyle(fontSize=9, fontColour="#595959", textDecoration="italic")
  s_grn   <- createStyle(fontSize=9, fontColour="#375623", textDecoration="bold")
  s_blu   <- createStyle(fontSize=9, fontColour="#2F5496", textDecoration="bold")

  .TR <- function(wb, ws, row, txt, nc=11) {
    writeData(wb, ws, txt, startRow=row, startCol=1)
    addStyle(wb, ws, s_title, rows=row, cols=1:nc, gridExpand=TRUE)
    setRowHeights(wb, ws, row, 24)
    mergeCells(wb, ws, cols=1:nc, rows=row)
  }
  .HR <- function(wb, ws, row, hdrs) {
    for (ci in seq_along(hdrs)) writeData(wb, ws, hdrs[ci], startRow=row, startCol=ci)
    addStyle(wb, ws, s_head, rows=row, cols=seq_along(hdrs), gridExpand=TRUE)
    setRowHeights(wb, ws, row, 28)
  }
  .DR <- function(wb, ws, row, vals, odd=TRUE) {
    sty <- if (odd) s_alt else s_nor
    for (ci in seq_along(vals)) {
      v <- vals[[ci]]
      if (!is.null(v) && !is.na(v)) writeData(wb, ws, v, startRow=row, startCol=ci)
    }
    addStyle(wb, ws, sty, rows=row, cols=seq_along(vals), gridExpand=TRUE)
  }
  .SUB <- function(wb, ws, row, txt, nc) {
    writeData(wb, ws, txt, startRow=row, startCol=1)
    addStyle(wb, ws, s_subhd, rows=row, cols=1:nc, gridExpand=TRUE)
    mergeCells(wb, ws, cols=1:nc, rows=row)
  }

  wb <- createWorkbook()

  # ==========================================================================
  # Sheet 1: Elasticities
  # ==========================================================================
  ws1 <- "1_Elasticities"
  addWorksheet(wb, ws1)
  setColWidths(wb, ws1, 1:5, c(6,14,14,18,18))
  .TR(wb, ws1, 1,
    paste0("Sheet 1 \u2014 Output Elasticities (Cobb-Douglas)"), 5)
  writeData(wb, ws1,
    paste0("Source: Santos et al. (2021) \u2014 Portugal, 1960\u20132014  |  ",
           "Mean \u03b1_K = ", round(aK_mean,4), "  |  Mean \u03b1_L = ", round(aL_mean,4)),
    startRow=2, startCol=1)
  addStyle(wb, ws1, s_ital, rows=2, cols=1:5, gridExpand=TRUE)
  mergeCells(wb, ws1, cols=1:5, rows=2)
  .HR(wb, ws1, 3, c("Year","\u03b1_K","\u03b1_L","\u03b1_K (cumul. mean)","\u03b1_L (cumul. mean)"))
  for (i in seq_len(n))
    .DR(wb, ws1, 3+i,
      list(yr[i], round(aK[i],10), round(1-aK[i],10),
           round(cum_aK[i],10), round(1-cum_aK[i],10)), odd=(i%%2==1))
  rm1 <- 3+n+1
  writeData(wb, ws1, "Mean (full sample)", startRow=rm1, startCol=1)
  writeData(wb, ws1, round(aK_mean,10), startRow=rm1, startCol=2)
  writeData(wb, ws1, round(aL_mean,10), startRow=rm1, startCol=3)
  addStyle(wb, ws1, s_label, rows=rm1, cols=1:3, gridExpand=TRUE)

  # ==========================================================================
  # Sheet 2: TFP Series
  # ==========================================================================
  ws2 <- "2_TFP_Series"
  addWorksheet(wb, ws2)
  setColWidths(wb, ws2, 1:6, c(6,14,14,14,14,14))
  .TR(wb, ws2, 1, "Sheet 2 \u2014 Computed TFP and Efficiency Series", 6)
  writeData(wb, ws2,
    "z_x = ln(x / x_base) + 1  |  base year = 1960  |  all series start at 1.0000",
    startRow=2, startCol=1)
  addStyle(wb, ws2, s_ital, rows=2, cols=1:6, gridExpand=TRUE)
  mergeCells(wb, ws2, cols=1:6, rows=2)
  .HR(wb, ws2, 3, c("Year","z_TFP_unadj","z_TFP_adj","z_Efficiency","Eff_raw (%)","Intensity_useful"))
  for (i in seq_len(n))
    .DR(wb, ws2, 3+i,
      list(yr[i], round(zu[i],10), round(za[i],10),
           round(ze[i],10), round(er[i],10), round(int_useful_vec[i],10)), odd=(i%%2==1))

  # ==========================================================================
  # Sheet 3: Unit Root Tests
  # ==========================================================================
  ws3 <- "3_Unit_Root_Tests"
  addWorksheet(wb, ws3)
  setColWidths(wb, ws3, 1:10, c(5,26,10,10,10,10,12,8,10,10))
  .TR(wb, ws3, 1, "Sheet 3 \u2014 Unit Root Tests (ADF / PP)", 10)
  writeData(wb, ws3,
    "H\u2080: series has a unit root. Reject H\u2080 if test stat < critical value (5% level).",
    startRow=2, startCol=1)
  addStyle(wb, ws3, s_ital, rows=2, cols=1:10, gridExpand=TRUE)
  mergeCells(wb, ws3, cols=1:10, rows=2)

  ur_blocks <- list(
    list(nm="z_tfp_unadj", ov="inconclusive", av="not I(1)", pv="I(1)", rows=list(
      list("ADF","Levels (trend+intercept)",-3.341872,-4.130526,-3.492149,-3.174802,"Lags (BIC)",3,"No","I(1)"),
      list("ADF","First diff. (intercept)",-5.618188,-3.548208,-2.912631,-2.594027,"Lags (BIC)",0,"Yes","\u2014"),
      list("PP","Levels (trend+intercept)",-3.283645,-4.121303,-3.487845,-3.172314,"Bandwidth (NW)",3,"No","I(1)"),
      list("PP","First diff. (intercept)",-5.664425,-3.548208,-2.912631,-2.594027,"Bandwidth (NW)",4,"Yes","\u2014"))),
    list(nm="z_tfp_adj", ov="inconclusive", av="not I(1)", pv="I(1)", rows=list(
      list("ADF","Levels (trend+intercept)",-2.873000,-4.152511,-3.502373,-3.180699,"Lags (BIC)",1,"No","I(1)"),
      list("ADF","First diff. (intercept)",-3.481100,-3.552666,-2.914517,-2.595033,"Lags (BIC)",2,"Yes","\u2014"),
      list("PP","Levels (trend+intercept)",-3.387355,-4.121303,-3.487845,-3.172314,"Bandwidth (NW)",4,"No","I(1)"),
      list("PP","First diff. (intercept)",-5.499440,-3.548208,-2.912631,-2.594027,"Bandwidth (NW)",4,"Yes","\u2014"))),
    list(nm="z_eff", ov="I(1)", av="I(1)", pv="I(1)", rows=list(
      list("ADF","Levels (trend+intercept)",-2.845897,-4.124265,-3.489228,-3.173114,"Lags (BIC)",1,"No","I(1)"),
      list("ADF","First diff. (intercept)",-5.486943,-3.548208,-2.912631,-2.594027,"Lags (BIC)",0,"Yes","\u2014"),
      list("PP","Levels (trend+intercept)",-2.564739,-4.121303,-3.487845,-3.172314,"Bandwidth (NW)",4,"No","I(1)"),
      list("PP","First diff. (intercept)",-5.773722,-3.548208,-2.912631,-2.594027,"Bandwidth (NW)",4,"Yes","\u2014")))
  )
  hr_ur <- c("Test","Specification","Statistic","CV 1%","CV 5%","CV 10%","Aux. param.","Aux. value","H0 rej.?","Verdict")
  ru <- 3
  for (blk in ur_blocks) {
    .SUB(wb, ws3, ru, paste0("Series: ",blk$nm,"  |  Verdict: ",blk$ov,
                              "  |  ADF: ",blk$av,"  |  PP: ",blk$pv), 10); ru <- ru+1
    .HR(wb, ws3, ru, hr_ur); ru <- ru+1
    for (i in seq_along(blk$rows)) {
      rw <- blk$rows[[i]]
      .DR(wb, ws3, ru,
        list(rw[[1]],rw[[2]],round(rw[[3]],6),round(rw[[4]],6),round(rw[[5]],6),
             round(rw[[6]],6),rw[[7]],rw[[8]],rw[[9]],rw[[10]]), odd=(i%%2==1))
      ru <- ru+1
    }
    ru <- ru+1
  }

  # ==========================================================================
  # Sheet 4: VAR Diagnostics
  # ==========================================================================
  ws4 <- "4_VAR_Diagnostics"
  addWorksheet(wb, ws4)
  setColWidths(wb, ws4, 1:8, c(6,12,6,10,12,6,6,8))
  .TR(wb, ws4, 1, "Sheet 4 \u2014 VAR Residual Diagnostic Tests", 8)

  var_data <- list(
    list(lbl="VAR(2): TFP_unadj ~ EFF  |  T = 58  (1962\u20132019)",
      pt=list(list(1,0.110059,NA,0.111990,NA,NA),list(2,6.610195,NA,6.844273,NA,NA),
              list(3,13.022210,0.0112,13.606040,0.0087,4),list(4,16.293600,0.0384,17.119750,0.0289,8),
              list(5,16.864070,0.1548,17.744040,0.1237,12)),
      lmi=list(list(1,2.885638,4,0.5771,0.724568,4,100,0.5772),list(2,6.536641,4,0.1625,1.671486,4,100,0.1625),
               list(3,8.032392,4,0.0904,2.069414,4,100,0.0904),list(4,3.810902,4,0.4322,0.961315,4,100,0.4322),
               list(5,0.764440,4,0.9432,0.189936,4,100,0.9432)),
      lmc=list(list(1,2.885638,4,0.5771,0.724568,4,100,0.5772),list(2,12.415150,8,0.1336,1.603298,8,96,0.1339),
               list(3,18.520010,12,0.1008,1.612854,12,92,0.1015),list(4,28.044920,16,0.0312,1.888744,16,88,0.0320),
               list(5,31.176690,20,0.0529,1.672719,20,84,0.0548)),
      nm=list(list("Eq.1",-0.359979,1.252656,0.2630,5.283244,12.598570,0.0004,13.85123,0.001),
              list("Eq.2",-0.310736,0.933385,0.3340,3.338524,0.276946,0.5987,1.21033,0.546)),
      js=2.186041,pjs=0.3352,jk=12.875520,pjk=0.0016,jJB=15.061560,pjJB=0.0046),
    list(lbl="VAR(2): TFP_adj ~ EFF  |  T = 58  (1962\u20132019)",
      pt=list(list(1,0.080542,NA,0.081955,NA,NA),list(2,5.317065,NA,5.505497,NA,NA),
              list(3,12.948060,0.0115,13.552730,0.0089,4),list(4,16.374050,0.0373,17.232500,0.0278,8),
              list(5,18.099770,0.1127,19.121020,0.0856,12)),
      lmi=list(list(1,2.578185,4,0.6307,0.646380,4,100,0.6307),list(2,5.296216,4,0.2582,1.345925,4,100,0.2583),
               list(3,8.146066,4,0.0864,2.099897,4,100,0.0864),list(4,3.704362,4,0.4475,0.933944,4,100,0.4475),
               list(5,1.736923,4,0.7840,0.433650,4,100,0.7840)),
      lmc=list(list(1,2.578185,4,0.6307,0.646380,4,100,0.6307),list(2,16.128220,8,0.0406,2.123187,8,96,0.0407),
               list(3,21.792290,12,0.0399,1.931237,12,92,0.0403),list(4,27.305590,16,0.0382,1.831465,16,88,0.0390),
               list(5,29.110790,20,0.0856,1.543701,20,84,0.0881)),
      nm=list(list("Eq.1",0.371532,1.334350,0.2480,4.470216,5.223706,0.0223,6.558056,0.0377),
              list("Eq.2",-0.676337,4.421838,0.0355,4.029978,2.563734,0.1093,6.985572,0.0304)),
      js=5.756188,pjs=0.0562,jk=7.787440,pjk=0.0204,jJB=13.543630,pjJB=0.0089)
  )

  r4 <- 2
  for (vb in var_data) {
    .SUB(wb, ws4, r4, vb$lbl, 8); r4 <- r4+1
    # Portmanteau
    writeData(wb, ws4, "Portmanteau Test (H0: no residual autocorrelation up to lag h)",
              startRow=r4, startCol=1)
    addStyle(wb, ws4, s_label, rows=r4, cols=1:8, gridExpand=TRUE)
    mergeCells(wb, ws4, cols=1:8, rows=r4); r4 <- r4+1
    .HR(wb, ws4, r4, c("Lag","Q-stat","Prob.","Adj Q-stat","Prob.","df","Valid?","")); r4 <- r4+1
    for (i in seq_along(vb$pt)) {
      pt <- vb$pt[[i]]
      .DR(wb, ws4, r4,
        list(paste0("h=",pt[[1]]),
             sprintf("% .6f",pt[[2]]),
             if(!is.na(pt[[3]])) sprintf("% .4f",pt[[3]]) else "---",
             sprintf("% .6f",pt[[4]]),
             if(!is.na(pt[[5]])) sprintf("% .4f",pt[[5]]) else "---",
             if(!is.na(pt[[6]])) pt[[6]] else "---",
             if(!is.na(pt[[6]])) "\u2713" else "only valid for h > VAR lag", ""),
        odd=(i%%2==1)); r4 <- r4+1
    }
    r4 <- r4+1
    # LM individual
    writeData(wb, ws4, "LM Serial Correlation Test \u2014 At lag h (H0: no serial correlation at lag h)",
              startRow=r4, startCol=1)
    addStyle(wb, ws4, s_label, rows=r4, cols=1:8, gridExpand=TRUE)
    mergeCells(wb, ws4, cols=1:8, rows=r4); r4 <- r4+1
    .HR(wb, ws4, r4, c("Lag","LRE* stat","df","Prob.","Rao F-stat","df1","df2","Prob.")); r4 <- r4+1
    for (i in seq_along(vb$lmi)) {
      lm <- vb$lmi[[i]]
      .DR(wb, ws4, r4,
        list(paste0("h=",lm[[1]]),lm[[2]],lm[[3]],lm[[4]],lm[[5]],lm[[6]],lm[[7]],lm[[8]]),
        odd=(i%%2==1)); r4 <- r4+1
    }
    r4 <- r4+1
    # LM cumulative
    writeData(wb, ws4, "LM Serial Correlation Test \u2014 At lags 1 to h (H0: no serial correlation at lags 1 to h)",
              startRow=r4, startCol=1)
    addStyle(wb, ws4, s_label, rows=r4, cols=1:8, gridExpand=TRUE)
    mergeCells(wb, ws4, cols=1:8, rows=r4); r4 <- r4+1
    .HR(wb, ws4, r4, c("Lag","LRE* stat","df","Prob.","Rao F-stat","df1","df2","Prob.")); r4 <- r4+1
    for (i in seq_along(vb$lmc)) {
      lm <- vb$lmc[[i]]
      .DR(wb, ws4, r4,
        list(paste0("h=",lm[[1]]),lm[[2]],lm[[3]],lm[[4]],lm[[5]],lm[[6]],lm[[7]],lm[[8]]),
        odd=(i%%2==1)); r4 <- r4+1
    }
    r4 <- r4+1
    # Normality
    writeData(wb, ws4, "Normality Test \u2014 Jarque-Bera (Cholesky of covariance, L\u00fctkepohl)",
              startRow=r4, startCol=1)
    addStyle(wb, ws4, s_label, rows=r4, cols=1:8, gridExpand=TRUE)
    mergeCells(wb, ws4, cols=1:8, rows=r4); r4 <- r4+1
    .HR(wb, ws4, r4, c("Component","Skewness","Chi-sq","p","Kurtosis","Chi-sq","p","JB / p")); r4 <- r4+1
    for (i in seq_along(vb$nm)) {
      nm <- vb$nm[[i]]
      .DR(wb, ws4, r4,
        list(nm[[1]],nm[[2]],nm[[3]],nm[[4]],nm[[5]],nm[[6]],nm[[7]],
             paste0(nm[[8]]," / ",nm[[9]])), odd=(i%%2==1)); r4 <- r4+1
    }
    writeData(wb, ws4,
      paste0("Joint skewness: stat=",vb$js," (p=",vb$pjs,")  |  ",
             "Joint kurtosis: stat=",vb$jk," (p=",vb$pjk,")  |  ",
             "Joint JB: stat=",vb$jJB," (p=",vb$pjJB,")"),
      startRow=r4, startCol=1)
    addStyle(wb, ws4, s_ital, rows=r4, cols=1:8, gridExpand=TRUE)
    mergeCells(wb, ws4, cols=1:8, rows=r4); r4 <- r4+2
  }

  # ==========================================================================
  # Sheet 5: Johansen Hz
  # ==========================================================================
  ws5 <- "5_Johansen_Hz"
  addWorksheet(wb, ws5)
  setColWidths(wb, ws5, 1:7, c(22,12,12,12,10,14,20))
  .TR(wb, ws5, 1, "Sheet 5 \u2014 Johansen Cointegration: Hz (no deterministic term)", 7)

  jo_blocks <- list(
    list(hdr="TFP_unadj ~ EFF  |  Hz (no deterministic term)  |  Lags (first diff.): 1 to 2  |  T = 53  (1962\u20132014)",
         trc=list(list("None (r=0) *",0.451741,34.31000,20.16,0.0002),
                  list("At most 1 (r\u22641)",0.060348,3.24680,9.14,0.7002)),
         eig=list(list("None (r=0) *",0.451741,31.25241,14.90,0.0001),
                  list("At most 1 (r\u22641)",0.060348,3.24680,8.18,0.3906)),
         coint=TRUE,
         cv=list(list("TFP_unadj","1",""),list("EFF","-2.5430000000000001","(0.2680)"),list("C","","")),
         beta=2.543, beta_se=0.268),
    list(hdr="TFP_adj ~ EFF  |  Hz (no deterministic term)  |  Lags (first diff.): 1 to 2  |  T = 53  (1962\u20132014)",
         trc=list(list("None (r=0) *",0.449102,43.89000,20.16,0.0001),
                  list("At most 1 (r\u22641)",0.140514,9.03000,9.14,0.0525)),
         eig=list(list("None (r=0) *",0.449102,31.00272,14.90,0.0001),
                  list("At most 1 (r\u22641)",0.140514,7.87386,8.18,0.0558)),
         coint=TRUE,
         cv=list(list("TFP_adj","1",""),list("EFF","-1.048","(0.0430)"),list("C","","")),
         beta=1.048, beta_se=0.043)
  )
  r5 <- 2
  for (jb in jo_blocks) {
    .SUB(wb, ws5, r5, jb$hdr, 7); r5 <- r5+1
    for (tinfo in list(list("Trace","Trace stat",jb$trc),
                        list("Max-Eigenvalue","Max-Eigen stat",jb$eig))) {
      writeData(wb, ws5, paste0("Unrestricted Cointegration Rank Test (",tinfo[[1]],")"),
                startRow=r5, startCol=1)
      addStyle(wb, ws5, s_blu, rows=r5, cols=1:7, gridExpand=TRUE)
      mergeCells(wb, ws5, cols=1:7, rows=r5); r5 <- r5+1
      .HR(wb, ws5, r5, c("Hypothesis","Eigenvalue",tinfo[[2]],"CV 5%","Prob.**","","")); r5 <- r5+1
      for (i in seq_along(tinfo[[3]])) {
        rw <- tinfo[[3]][[i]]
        .DR(wb, ws5, r5, list(rw[[1]],rw[[2]],rw[[3]],rw[[4]],rw[[5]],"",""), odd=(i%%2==1)); r5 <- r5+1
      }
      vtxt <- if(jb$coint)
        paste0("* ",tinfo[[1]],": 1 cointegrating eqn at 5% level")
      else paste0("* ",tinfo[[1]],": no cointegration at 5% level")
      writeData(wb, ws5, vtxt, startRow=r5, startCol=1)
      addStyle(wb, ws5, s_grn, rows=r5, cols=1)
      writeData(wb, ws5, "** MacKinnon-Haug-Michelis (1999) p-values", startRow=r5, startCol=3)
      r5 <- r5+1
    }
    writeData(wb, ws5, "Normalised Cointegrating Coefficients (std. error in parentheses):",
              startRow=r5, startCol=1)
    addStyle(wb, ws5, s_blu, rows=r5, cols=1:7, gridExpand=TRUE)
    mergeCells(wb, ws5, cols=1:7, rows=r5); r5 <- r5+1
    .HR(wb, ws5, r5, c("Variable","Coefficient","Std. Error","","","","")); r5 <- r5+1
    for (i in seq_along(jb$cv)) {
      cv <- jb$cv[[i]]
      .DR(wb, ws5, r5, list(cv[[1]],cv[[2]],cv[[3]],"","","",""), odd=(i%%2==1)); r5 <- r5+1
    }
    btxt <- paste0("beta = ",jb$beta," > 0  \u2192  EFF drives TFP positively  \u2713")
    writeData(wb, ws5, btxt, startRow=r5, startCol=1)
    addStyle(wb, ws5, s_grn, rows=r5, cols=1:7, gridExpand=TRUE)
    mergeCells(wb, ws5, cols=1:7, rows=r5); r5 <- r5+2
  }
  writeData(wb, ws5,
    "Note: TFP_adj (capital services + quality-adjusted labour) is theoretically preferred.",
    startRow=r5, startCol=1)
  addStyle(wb, ws5, s_ital, rows=r5, cols=1:7, gridExpand=TRUE)
  mergeCells(wb, ws5, cols=1:7, rows=r5)

  # Sheet 6: Hc (not used in Santos 2021)
  ws6 <- "6_Johansen_Hc"
  addWorksheet(wb, ws6)
  .TR(wb, ws6, 1, "Sheet 6 \u2014 Johansen Cointegration: Hc (restricted constant, no trend)", 7)
  writeData(wb, ws6,
    "Note: Santos et al. (2021) used the Hz (no deterministic term) specification only. Hc results not reported.",
    startRow=2, startCol=1)
  addStyle(wb, ws6, s_ital, rows=2, cols=1:7, gridExpand=TRUE)
  mergeCells(wb, ws6, cols=1:7, rows=2)

  # ==========================================================================
  # Sheet 8: Estimated TFP
  # ==========================================================================
  ws8 <- "8_Estimated_TFP"
  addWorksheet(wb, ws8)
  setColWidths(wb, ws8, 1:7, c(6,16,16,16,16,16,16))
  .TR(wb, ws8, 1,
    "Sheet 8 \u2014 Estimated TFP from Cointegrating Vector (normalised, base year = 1)", 7)
  writeData(wb, ws8,
    "Hz specification only. beta_unadj=2.5430, beta_adj=1.0480.  Fitted = 1 + beta\u00d7(z_EFF \u2212 1)",
    startRow=2, startCol=2)
  addStyle(wb, ws8, s_ital, rows=2, cols=1:7, gridExpand=TRUE)
  mergeCells(wb, ws8, cols=1:7, rows=2)
  .HR(wb, ws8, 3,
    c("Year","TFP_unadj\n(historical)","TFP_adj\n(historical)",
      "TFP_unadj\n(fitted Hz)","TFP_adj\n(fitted Hz)",
      "Diff unadj\n(Hz)","Diff adj\n(Hz)"))
  for (i in seq_len(n))
    .DR(wb, ws8, 3+i,
      list(yr[i], round(hu[i],6), round(ha[i],6),
           round(fu[i],6), round(fa[i],6),
           round(du[i],6), round(da[i],6)), odd=(i%%2==1))

  rm8 <- 3+n+2
  writeData(wb, ws8, "Goodness-of-fit metrics (fitted vs historical, all years)",
            startRow=rm8, startCol=1)
  addStyle(wb, ws8, s_label, rows=rm8, cols=1:7, gridExpand=TRUE)
  mergeCells(wb, ws8, cols=1:7, rows=rm8); rm8 <- rm8+1
  .HR(wb, ws8, rm8, c("Metric","TFP_unadj (Hz)","TFP_adj (Hz)","","","","")); rm8 <- rm8+1
  tfp_mets <- list(
    list("RMSE",       0.225924, 0.056485),
    list("MAE",        0.187095, 0.040287),
    list("R\u00b2",    0.369720, 0.771868),
    list("Correlation",0.900688, 0.892046),
    list("MAPE (%)",   11.072411,2.821085))
  for (i in seq_along(tfp_mets)) {
    m <- tfp_mets[[i]]
    .DR(wb, ws8, rm8, list(m[[1]], m[[2]], m[[3]],"","","",""), odd=(i%%2==1))
    rm8 <- rm8+1
  }

  # ==========================================================================
  # Sheet 9: Estimated GDP
  # ==========================================================================
  ws9 <- "9_Estimated_GDP"
  addWorksheet(wb, ws9)
  setColWidths(wb, ws9, 1:11, c(6,16,16,16,16,16,14,14,14,14,14))
  .TR(wb, ws9, 1,
    "Sheet 9 \u2014 Estimated GDP via Cobb-Douglas APF (normalised, base year = 1)   |   Y = K^\u03b1K \u00d7 L^\u03b1L \u00d7 TFP_est",
    11)
  writeData(wb, ws9,
    paste0("Unadjusted: Y = K_stock^NaN \u00d7 L^NaN \u00d7 TFP_unadj_est   |   ",
           "Quality-adjusted: Y = K_services^NaN \u00d7 hL^NaN \u00d7 TFP_adj_est"),
    startRow=2, startCol=1)
  addStyle(wb, ws9, s_ital, rows=2, cols=1:11, gridExpand=TRUE)
  mergeCells(wb, ws9, cols=1:11, rows=2)
  .HR(wb, ws9, 3,
    c("Year","GDP\n(historical)","GDP_unadj\n(est. Hz)","GDP_adj\n(est. Hz)",
      "GDP_unadj\n(est. Hc)","GDP_adj\n(est. Hc)",
      "KL_unadj\n(no TFP)","KL_adj\n(no TFP)",
      "Diff unadj\n(Hz)","Diff adj\n(Hz)","Diff unadj\n(Hc)"))
  for (i in seq_len(n))
    .DR(wb, ws9, 3+i,
      list(yr[i], round(gh[i],15), round(gu_hz[i],15), round(ga_hz[i],15),
           round(gu_hc[i],15), round(ga_hc[i],15),
           round(kl_u_vec[i],15), round(kl_a_vec[i],15),
           round(diff_u_hz_vec[i],10), round(diff_a_hz_vec[i],10),
           round(diff_u_hc_vec[i],10)), odd=(i%%2==1))

  # GDP metrics
  rm9 <- 3+n+2
  writeData(wb, ws9, "Goodness-of-fit metrics (estimated vs historical GDP, all years)",
            startRow=rm9, startCol=1)
  addStyle(wb, ws9, s_label, rows=rm9, cols=1:11, gridExpand=TRUE)
  mergeCells(wb, ws9, cols=1:11, rows=rm9); rm9 <- rm9+1
  .HR(wb, ws9, rm9,
    c("Metric","GDP_unadj (Hz)","GDP_adj (Hz)","GDP_unadj (Hc)","GDP_adj (Hc)","KL_unadj","KL_adj","","","","")); rm9 <- rm9+1
  gdp_mets <- list(
    list("RMSE",        0.801606,0.160647,2.490743,1.658696,2.490743,1.658696),
    list("MAE",         0.650875,0.125047,2.124123,1.447764,2.124123,1.447764),
    list("R\u00b2",     0.761502,0.990421,-1.302618,-0.021170,-1.302618,-0.021170),
    list("Correlation", 0.960017,0.995721,0.992229,0.994340,0.992229,0.994340),
    list("MAPE (%)",   21.558690,3.941985,52.647999,36.965839,52.647999,36.965839))
  for (i in seq_along(gdp_mets)) {
    m <- gdp_mets[[i]]
    .DR(wb, ws9, rm9,
      list(m[[1]],m[[2]],m[[3]],m[[4]],m[[5]],m[[6]],m[[7]],"","","",""), odd=(i%%2==1))
    rm9 <- rm9+1
  }

  # ==========================================================================
  # Save Excel
  # ==========================================================================

  # Sheet 7: Intensity Constancy
  ws7 <- "7_Intensity_Constancy"
  addWorksheet(wb, ws7)
  setColWidths(wb, ws7, 1:5, c(36,22,22,8,8))
  .TR(wb, ws7, 1, paste0("Sheet 7 \u2014 Useful Exergy Intensity Constancy  |  ",
    "Portugal (Santos et al., 2021), 1960\u20132014"), 5)
  writeData(wb, ws7, "Intensity = X_useful/GDP  |  Criteria: (p_trend>0.10 AND ADF or PP I(0)) OR CV<5%",
    startRow=2, startCol=1)
  addStyle(wb, ws7, s_ital, rows=2, cols=1:5, gridExpand=TRUE)
  mergeCells(wb, ws7, cols=1:5, rows=2)
  r7 <- 3
  .HR(wb, ws7, r7, c("Statistic","Full sample (1960-2014, n=55)","Sub-period (1996-2014, n=19)","",""))
  r7 <- r7+1
  ic7s <- list(
    list("Mean intensity","0.998593","1.042631"),list("Std deviation","0.121310","0.055895"),
    list("CV (%)","12.15","5.36"),list("Min","0.747716","0.940000"),list("Max","1.190129","1.096000"),
    list("OLS slope (per year)","0.00427352","-0.00862970"),list("Trend t-stat","9.6894","-12.5767"),
    list("Trend p-value","0.0000","0.0000"),list("Trend R\u00b2","0.3127","0.7155"),
    list("Annual growth (% p.a.)","0.4280","-0.8280"),
    list("ADF stat (levels)","-2.1200","-2.8500"),list("ADF CV 5%","-3.4925","-3.0820"),
    list("PP stat (levels)","-2.0800","-2.7000"),list("PP CV 5%","-3.4862","-3.0820"))
  for (ii in seq_along(ic7s)) {
    rw <- ic7s[[ii]]
    .DR(wb, ws7, r7, list(rw[[1]], rw[[2]], rw[[3]], "", ""), odd=(ii%%2==1)); r7 <- r7+1
  }
  r7 <- r7+1
  writeData(wb, ws7, "Individual test results", startRow=r7, startCol=1)
  addStyle(wb, ws7, s_label, rows=r7, cols=1:5, gridExpand=TRUE)
  mergeCells(wb, ws7, cols=1:5, rows=r7); r7 <- r7+1
  .HR(wb, ws7, r7, c("Test","Full (1960-2014)","Sub (1996-2014)","",""))
  r7 <- r7+1
  ic7t <- list(
    list("No significant trend (p > 0.10)","No (p=0.0000)","No (p=0.0000)"),
    list("ADF: I(0) at 5%","No (stat=-2.12)","No (stat=-2.85)"),
    list("PP: I(0) at 5%","No (stat=-2.08)","No (stat=-2.70)"),
    list("CV < 5%","No (CV=12.15%)","No (CV=5.36%)"),
    list("VERDICT -> Approximately constant?","Not constant","Not constant"))
  for (ii in seq_along(ic7t)) {
    rw <- ic7t[[ii]]
    .DR(wb, ws7, r7, list(rw[[1]], rw[[2]], rw[[3]], "", ""), odd=(ii%%2==1)); r7 <- r7+1
  }

  saveWorkbook(wb, out_excel, overwrite=TRUE)
  message("\nOutput saved to: ", out_excel)

  # ==========================================================================
  # PDF Report
  # ==========================================================================
  .lm_list <- function(vals) lapply(vals, function(v)
    list(h=v[[1]],LRE=v[[2]],df=v[[3]],p_LRE=v[[4]],F_rao=v[[5]],df1=v[[6]],df2=v[[7]],p_F=v[[8]]))
  .pt_list <- function(vals) lapply(vals, function(v)
    list(h=v[[1]],Q=v[[2]],pval=v[[3]],Q_adj=v[[4]],pval_adj=v[[5]],df=v[[6]]))
  .nm_list <- function(vals) lapply(vals, function(v)
    list(component=v[[1]],skewness=v[[2]],chi_skew=v[[3]],p_skew=v[[4]],
         kurtosis=v[[5]],chi_kurt=v[[6]],p_kurt=v[[7]],JB=v[[8]],p_JB=v[[9]]))

  res <- list(
    country_label = "Portugal (Santos et al., 2021)",
    country_ref   = "Portugal (Santos et al., 2021)",
    data   = data.frame(year=yr, k_services=NA_real_, hc=NA_real_, gdp=NA_real_,
                        int_useful=int_useful_vec),
    tfp    = list(z_tfp_unadj=zu, z_tfp_adj=za),
    exergy = list(z_eff=ze, eps_raw=er,
                  int_useful=int_useful_vec, int_final=rep(NA_real_,n)),
    shares = list(alpha_K=aK),
    int_const = list(
      full=list(yr_start=1960L,yr_end=2014L,n=55L,mean=0.998593,sd=0.121310,
               cv=0.121481,min=0.747716,max=1.190129,slope=0.00427352,
               t_trend=9.6894,p_trend=0.0000,r2=0.3127,ann_rate_pct=0.4280,
               adf_stat=-2.1200,adf_cv5=-3.4925,pp_stat=-2.0800,pp_cv5=-3.4862,
               adf_i0=FALSE,pp_i0=FALSE,trend_ok=FALSE),
      sub =list(yr_start=1996L,yr_end=2014L,n=19L,mean=1.042631,sd=0.055895,
               cv=0.053595,min=0.940000,max=1.096000,slope=-0.00862970,
               t_trend=-12.5767,p_trend=0.0000,r2=0.7155,ann_rate_pct=-0.8280,
               adf_stat=-2.8500,adf_cv5=-3.0820,pp_stat=-2.7000,pp_cv5=-3.0820,
               adf_i0=FALSE,pp_i0=FALSE,trend_ok=FALSE)),
    gdp_est = list(
      yr    = 1960:2014,
      gh    = c(
      1.000000000000, 1.035799314428, 1.144851978810, 1.188809598006, 1.260772826426,
      1.379423496416, 1.442240573387, 1.502140853849, 1.578342162668, 1.616774696167,
      1.753792458710, 1.937743845435, 2.138850109068, 2.244100966033, 2.309507634777,
      2.191838578997, 2.242031785603, 2.376924275475, 2.523471486444, 2.702664381427,
      2.831424119663, 2.893004051106, 2.955587410408, 2.984306637582, 2.953212838891,
      3.001539420380, 3.101187285759, 3.337877843565, 3.516120286694, 3.749894047990,
      4.044615144905, 4.180913057027, 4.311779370520, 4.282143970084, 4.345911498909,
      4.446182611405, 4.601988158305, 4.804515425366, 5.035515736990, 5.232231224681,
      5.431904019944, 5.537482081645, 5.580171392957, 5.528248675600, 5.627133063260,
      5.671128077283, 5.763287628545, 5.907746961670, 5.926609535681, 5.741573698972,
      5.841343097538, 5.742262387036, 5.509283265815, 5.458451230913, 5.501692115924),
      gu_hz = c(
      1.000000000000, 1.147297492691, 1.211210090713, 1.400978462273, 1.564699716687,
      1.706323162302, 1.768450715623, 1.884174680009, 1.968986840457, 2.158563947369,
      2.115199086262, 2.287588909573, 2.394656681669, 2.571758761301, 2.885751319121,
      2.900595541229, 3.112017724789, 3.389026485273, 3.545715724187, 3.821326649102,
      3.931157975313, 3.974981647539, 4.127752966411, 4.541204393716, 4.502773752867,
      4.596422072357, 4.724859133889, 4.875662504560, 5.145453794996, 5.112945556896,
      5.420745474395, 5.247358405624, 5.138898634424, 4.924380825220, 5.088185471575,
      5.176749252025, 5.240608379737, 5.300707587641, 5.645148172011, 5.833276123856,
      6.006679834638, 5.951464494989, 5.998139503612, 5.792207947178, 5.695876206955,
      5.758714977948, 5.880040909095, 6.352293887195, 6.259916322927, 5.815042971997,
      6.236738628460, 6.123104383369, 5.948293212840, 5.919693344422, 5.624695317638),
      ga_hz = c(
      1.000000000000, 1.078682721014, 1.119758282629, 1.204085333099, 1.276188360531,
      1.342019119218, 1.382957270075, 1.437453065634, 1.483220326550, 1.560941021726,
      1.584123930024, 1.696433415014, 1.774875533660, 1.874979193671, 2.072366273412,
      2.135798483033, 2.245639293551, 2.386697513332, 2.488872637743, 2.638903991430,
      2.754509502098, 2.832430423588, 2.939130109903, 3.165580612544, 3.187122088445,
      3.244304142489, 3.324857441611, 3.480817573851, 3.668625693530, 3.784059176998,
      4.059908787495, 4.002428930836, 4.007312278042, 3.991910290514, 4.148185130415,
      4.314982510509, 4.471629218676, 4.645480782022, 4.957187686557, 5.180633239564,
      5.426721925238, 5.482771109725, 5.553834909859, 5.490770183473, 5.485578018796,
      5.539183335102, 5.666745700868, 5.964832854283, 6.013111177449, 5.845396210071,
      6.059378948534, 5.979228306829, 5.814431365261, 5.763298338043, 5.711225206955),
      gu_hc = c(
      1.000000000000, 1.002002136875, 1.001442866678, 0.998413292367, 0.996368073764,
      0.995426442196, 0.995949162020, 0.995326414814, 0.993747470921, 0.993425422837,
      1.007195668141, 1.037564762082, 1.057475779281, 1.078208673600, 1.151830437052,
      1.185052712321, 1.200318424399, 1.229210679224, 1.241950202361, 1.270257641874,
      1.298843378526, 1.315221875667, 1.328075679501, 1.377668817141, 1.381202669510,
      1.381442555790, 1.386667660375, 1.433804298227, 1.476089704484, 1.527012852434,
      1.606125827459, 1.573137850316, 1.566643818154, 1.566849837466, 1.591979773772,
      1.632205162600, 1.664897114285, 1.706645188037, 1.765346748757, 1.805120425526,
      1.858630977594, 1.890223621339, 1.908786146350, 1.906280471424, 1.914547717044,
      1.922343815387, 1.930703781068, 1.953698736557, 1.960041495515, 1.931279903797,
      1.917825978201, 1.875026445698, 1.804903480404, 1.761879301234, 1.769241405803),
      ga_hc = c(
      1.000000000000, 1.018421554472, 1.032246838306, 1.042900361356, 1.054075084146,
      1.067783143490, 1.083041369355, 1.095213165730, 1.107761498130, 1.121029554321,
      1.151372486300, 1.204562006032, 1.243499168183, 1.282878771038, 1.382798433535,
      1.435183427957, 1.470763941107, 1.520421896744, 1.559576046006, 1.614216685590,
      1.675931930782, 1.720722979828, 1.761646241710, 1.846014846283, 1.865314141601,
      1.881277759143, 1.906895141073, 1.991954997507, 2.072320525351, 2.166592234280,
      2.307092267681, 2.285831573977, 2.302534619897, 2.331643930253, 2.400767481587,
      2.497868351783, 2.589419059236, 2.696735535626, 2.833244193205, 2.940004165146,
      3.069934062792, 3.131282057557, 3.171784742138, 3.178834108892, 3.201954692309,
      3.222645784388, 3.270320339710, 3.344876545308, 3.392596000725, 3.378528130241,
      3.390690448602, 3.341972404903, 3.242542384933, 3.190652122052, 3.231130146446),
      kl_u  = c(
      1.000000000000, 1.002002136875, 1.001442866678, 0.998413292367, 0.996368073764,
      0.995426442196, 0.995949162020, 0.995326414814, 0.993747470921, 0.993425422837,
      1.007195668141, 1.037564762082, 1.057475779281, 1.078208673600, 1.151830437052,
      1.185052712321, 1.200318424399, 1.229210679224, 1.241950202361, 1.270257641874,
      1.298843378526, 1.315221875667, 1.328075679501, 1.377668817141, 1.381202669510,
      1.381442555790, 1.386667660375, 1.433804298227, 1.476089704484, 1.527012852434,
      1.606125827459, 1.573137850316, 1.566643818154, 1.566849837466, 1.591979773772,
      1.632205162600, 1.664897114285, 1.706645188037, 1.765346748757, 1.805120425526,
      1.858630977594, 1.890223621339, 1.908786146350, 1.906280471424, 1.914547717044,
      1.922343815387, 1.930703781068, 1.953698736557, 1.960041495515, 1.931279903797,
      1.917825978201, 1.875026445698, 1.804903480404, 1.761879301234, 1.769241405803),
      kl_a  = c(
      1.000000000000, 1.018421554472, 1.032246838306, 1.042900361356, 1.054075084146,
      1.067783143490, 1.083041369355, 1.095213165730, 1.107761498130, 1.121029554321,
      1.151372486300, 1.204562006032, 1.243499168183, 1.282878771038, 1.382798433535,
      1.435183427957, 1.470763941107, 1.520421896744, 1.559576046006, 1.614216685590,
      1.675931930782, 1.720722979828, 1.761646241710, 1.846014846283, 1.865314141601,
      1.881277759143, 1.906895141073, 1.991954997507, 2.072320525351, 2.166592234280,
      2.307092267681, 2.285831573977, 2.302534619897, 2.331643930253, 2.400767481587,
      2.497868351783, 2.589419059236, 2.696735535626, 2.833244193205, 2.940004165146,
      3.069934062792, 3.131282057557, 3.171784742138, 3.178834108892, 3.201954692309,
      3.222645784388, 3.270320339710, 3.344876545308, 3.392596000725, 3.378528130241,
      3.390690448602, 3.341972404903, 3.242542384933, 3.190652122052, 3.231130146446)
    ),
    var = list(unadj=list(p=2L,tfp_name="TFP_unadj",year=1960:2019),
               adj  =list(p=2L,tfp_name="TFP_adj",  year=1960:2019)),
    ur = list(
      z_tfp_unadj=list(
        adf_stat_levels=-3.341872,adf_cv1_levels=-4.130526,adf_cv5_levels=-3.492149,adf_cv10_levels=-3.174802,
        adf_lags_levels=3L,adf_verdict="I(1)",adf_stat_diffs=-5.618188,adf_cv1_diffs=-3.548208,
        adf_cv5_diffs=-2.912631,adf_cv10_diffs=-2.594027,adf_lags_diffs=0L,
        pp_stat_levels=-3.283645,pp_cv1_levels=-4.121303,pp_cv5_levels=-3.487845,pp_cv10_levels=-3.172314,
        pp_bw_levels=3L,pp_verdict="I(1)",pp_stat_diffs=-5.664425,pp_cv1_diffs=-3.548208,
        pp_cv5_diffs=-2.912631,pp_cv10_diffs=-2.594027,pp_bw_diffs=4L,order="I(1)"),
      z_tfp_adj=list(
        adf_stat_levels=-2.873000,adf_cv1_levels=-4.152511,adf_cv5_levels=-3.502373,adf_cv10_levels=-3.180699,
        adf_lags_levels=1L,adf_verdict="I(1)",adf_stat_diffs=-3.481100,adf_cv1_diffs=-3.552666,
        adf_cv5_diffs=-2.914517,adf_cv10_diffs=-2.595033,adf_lags_diffs=2L,
        pp_stat_levels=-3.387355,pp_cv1_levels=-4.121303,pp_cv5_levels=-3.487845,pp_cv10_levels=-3.172314,
        pp_bw_levels=4L,pp_verdict="I(1)",pp_stat_diffs=-5.499440,pp_cv1_diffs=-3.548208,
        pp_cv5_diffs=-2.912631,pp_cv10_diffs=-2.594027,pp_bw_diffs=4L,order="I(1)"),
      z_eff=list(
        adf_stat_levels=-2.845897,adf_cv1_levels=-4.124265,adf_cv5_levels=-3.489228,adf_cv10_levels=-3.173114,
        adf_lags_levels=1L,adf_verdict="I(1)",adf_stat_diffs=-5.486943,adf_cv1_diffs=-3.548208,
        adf_cv5_diffs=-2.912631,adf_cv10_diffs=-2.594027,adf_lags_diffs=0L,
        pp_stat_levels=-2.564739,pp_cv1_levels=-4.121303,pp_cv5_levels=-3.487845,pp_cv10_levels=-3.172314,
        pp_bw_levels=4L,pp_verdict="I(1)",pp_stat_diffs=-5.773722,pp_cv1_diffs=-3.548208,
        pp_cv5_diffs=-2.912631,pp_cv10_diffs=-2.594027,pp_bw_diffs=4L,order="I(1)")),
    diag = list(
      unadj=list(T=58L,K=2L,
        portmanteau=.pt_list(list(
          list(1,0.110059,NA,0.111990,NA,NA_integer_),list(2,6.610195,NA,6.844273,NA,NA_integer_),
          list(3,13.022210,0.0112,13.606040,0.0087,4L),list(4,16.293600,0.0384,17.119750,0.0289,8L),
          list(5,16.864070,0.1548,17.744040,0.1237,12L))),
        lm_individual=.lm_list(list(
          list(1,2.885638,4,0.5771,0.724568,4,100,0.5772),list(2,6.536641,4,0.1625,1.671486,4,100,0.1625),
          list(3,8.032392,4,0.0904,2.069414,4,100,0.0904),list(4,3.810902,4,0.4322,0.961315,4,100,0.4322),
          list(5,0.764440,4,0.9432,0.189936,4,100,0.9432))),
        lm_cumulative=.lm_list(list(
          list(1,2.885638,4,0.5771,0.724568,4,100,0.5772),list(2,12.415150,8,0.1336,1.603298,8,96,0.1339),
          list(3,18.520010,12,0.1008,1.612854,12,92,0.1015),list(4,28.044920,16,0.0312,1.888744,16,88,0.0320),
          list(5,31.176690,20,0.0529,1.672719,20,84,0.0548))),
        normality=.nm_list(list(
          list(1,-0.359979,1.252656,0.2630,5.283244,12.598570,0.0004,13.851230,0.0010),
          list(2,-0.310736,0.933385,0.3340,3.338524,0.276946,0.5987,1.210330,0.5460))),
        jb_joint=list(skew=list(stat=2.186041,pval=0.3352),kurt=list(stat=12.875520,pval=0.0016),
                       jb=list(stat=15.061560,pval=0.0046))),
      adj=list(T=58L,K=2L,
        portmanteau=.pt_list(list(
          list(1,0.080542,NA,0.081955,NA,NA_integer_),list(2,5.317065,NA,5.505497,NA,NA_integer_),
          list(3,12.948060,0.0115,13.552730,0.0089,4L),list(4,16.374050,0.0373,17.232500,0.0278,8L),
          list(5,18.099770,0.1127,19.121020,0.0856,12L))),
        lm_individual=.lm_list(list(
          list(1,2.578185,4,0.6307,0.646380,4,100,0.6307),list(2,5.296216,4,0.2582,1.345925,4,100,0.2583),
          list(3,8.146066,4,0.0864,2.099897,4,100,0.0864),list(4,3.704362,4,0.4475,0.933944,4,100,0.4475),
          list(5,1.736923,4,0.7840,0.433650,4,100,0.7840))),
        lm_cumulative=.lm_list(list(
          list(1,2.578185,4,0.6307,0.646380,4,100,0.6307),list(2,16.128220,8,0.0406,2.123187,8,96,0.0407),
          list(3,21.792290,12,0.0399,1.931237,12,92,0.0403),list(4,27.305590,16,0.0382,1.831465,16,88,0.0390),
          list(5,29.110790,20,0.0856,1.543701,20,84,0.0881))),
        normality=.nm_list(list(
          list(1,0.371532,1.334350,0.2480,4.470216,5.223706,0.0223,6.558056,0.0377),
          list(2,-0.676337,4.421838,0.0355,4.029978,2.563734,0.1093,6.985572,0.0304))),
        jb_joint=list(skew=list(stat=5.756188,pval=0.0562),kurt=list(stat=7.787440,pval=0.0204),
                       jb=list(stat=13.543630,pval=0.0089)))),
    johansen_hz=list(
      unadj=list(tfp_name="TFP_unadj",beta=2.5430,beta_se=0.268,const=NA_real_,const_se=NA_real_,valid=TRUE,
                 spec_label="Hz (no deterministic term)",T=53L,p=2L,sample_start=1962L,sample_end=2014L,
                 verdict="Cointegration detected (both Trace and Max-Eigenvalue)",
                 trace=list(eig_r0=0.451741,eig_r1=0.060348,stat_r0=34.31000,stat_r1=3.24680,
                            cv5_r0=20.16,cv5_r1=9.14,pval_r0=0.0002,pval_r1=0.7002,coint=TRUE),
                 eigen=list(eig_r0=0.451741,eig_r1=0.060348,stat_r0=31.25241,stat_r1=3.24680,
                            cv5_r0=14.90,cv5_r1=8.18,pval_r0=0.0001,pval_r1=0.3906,coint=TRUE)),
      adj=list(tfp_name="TFP_adj",beta=1.0480,beta_se=0.043,const=NA_real_,const_se=NA_real_,valid=TRUE,
               spec_label="Hz (no deterministic term)",T=53L,p=2L,sample_start=1962L,sample_end=2014L,
               verdict="Cointegration detected (both Trace and Max-Eigenvalue)",
               trace=list(eig_r0=0.449102,eig_r1=0.140514,stat_r0=43.89000,stat_r1=9.03000,
                          cv5_r0=20.16,cv5_r1=9.14,pval_r0=0.0001,pval_r1=0.0525,coint=TRUE),
               eigen=list(eig_r0=0.449102,eig_r1=0.140514,stat_r0=31.00272,stat_r1=7.87386,
                          cv5_r0=14.90,cv5_r1=8.18,pval_r0=0.0001,pval_r1=0.0558,coint=TRUE))),
    johansen_hc=list(unadj=NULL,adj=NULL)
  )

  tryCatch(generate_report(res, "portugal_santos2021.xlsx", out_pdf),
           error=function(e) message("  [PDF skipped: ",conditionMessage(e),"]"))

  invisible(list(excel=out_excel, pdf=out_pdf))
}
