# Native CRAN SHELF::fitdist — same science as the Plumber sidecar in the React app.

shelf_available <- function() {
  requireNamespace("SHELF", quietly = TRUE)
}

shelf_status <- function() {
  ok <- shelf_available()
  ver <- if (ok) as.character(utils::packageVersion("SHELF")) else NA_character_
  list(
    engine = if (ok) "shelf_native" else "none",
    rAvailable = TRUE,
    shelfPackage = ok,
    shelfVersion = ver,
    message = if (ok) paste("CRAN SHELF", ver, "ready") else
      "Install SHELF: install.packages('SHELF')"
  )
}

shelf_family_id <- function(label) {
  alt <- tolower(gsub("[_ ]", ".", label %||% "beta"))
  if (grepl("best", alt)) return("best")
  if (grepl("mirror.*log.*t", alt)) return("mirror_log_t")
  if (grepl("mirror.*log", alt)) return("mirror_log_normal")
  if (grepl("mirror.*gamma", alt)) return("mirror_gamma")
  if (grepl("log.*t", alt)) return("log_t")
  if (grepl("log", alt) && grepl("normal", alt)) return("log_normal")
  if (grepl("skew", alt)) return("skew_normal")
  if (grepl("student", alt) || alt == "t") return("student_t")
  if (grepl("gamma", alt)) return("gamma")
  if (grepl("beta", alt)) return("beta")
  if (grepl("normal", alt)) return("normal")
  "beta"
}

shelf_to_d_arg <- function(family_id) {
  fam <- shelf_family_id(family_id)
  switch(fam,
    "student_t" = "t",
    "log_normal" = "lognormal",
    "log_t" = "logt",
    "mirror_log_normal" = "mirrorlognormal",
    "mirror_gamma" = "mirrorgamma",
    "mirror_log_t" = "mirrorlogt",
    "normal" = "normal",
    "gamma" = "gamma",
    "beta" = "beta",
    "best" = "best",
    "best"
  )
}

shelf_pdf_grid <- function(fit, expert_index, family_id, lo, hi, n = 81) {
  xs <- seq(lo, hi, length.out = n)
  eps <- (hi - lo) * 1e-6
  x_eval <- pmin(pmax(xs, lo + eps), hi - eps)
  pdf <- rep(0, n)
  if (family_id == "normal" && !is.null(fit$Normal)) {
    mu <- fit$Normal[expert_index, "mean"]
    sig <- fit$Normal[expert_index, "sd"]
    pdf <- stats::dnorm(x_eval, mu, sig)
  } else if (family_id == "student_t" && !is.null(fit$Student.t)) {
    mu <- fit$Student.t[expert_index, "location"]
    sig <- fit$Student.t[expert_index, "scale"]
    nu <- fit$Student.t[expert_index, "df"]
    pdf <- stats::dt((x_eval - mu) / sig, df = nu) / sig
  } else if (family_id == "skew_normal" && !is.null(fit$Skewnormal)) {
    xi <- fit$Skewnormal[expert_index, "location"]
    omega <- fit$Skewnormal[expert_index, "scale"]
    alpha <- fit$Skewnormal[expert_index, "slant"]
    pdf <- (2 / omega) * stats::dnorm((x_eval - xi) / omega) * stats::pnorm(alpha * (x_eval - xi) / omega)
  } else if (family_id == "gamma" && !is.null(fit$Gamma)) {
    sh <- fit$Gamma[expert_index, "shape"]
    ra <- fit$Gamma[expert_index, "rate"]
    pdf <- stats::dgamma(pmax(x_eval - lo, 0), shape = sh, rate = ra)
  } else if (family_id == "log_normal" && !is.null(fit$Log.normal)) {
    mu <- fit$Log.normal[expert_index, "mean.log.X"]
    sig <- fit$Log.normal[expert_index, "sd.log.X"]
    pdf <- stats::dlnorm(pmax(x_eval - lo, 1e-12), mu, sig)
  } else if (family_id == "log_t" && !is.null(fit$Log.Student.t)) {
    mu <- fit$Log.Student.t[expert_index, "location.log.X"]
    sig <- fit$Log.Student.t[expert_index, "scale.log.X"]
    nu <- fit$Log.Student.t[expert_index, "df.log.X"]
    y <- pmax(x_eval - lo, 1e-12)
    pdf <- (stats::dt((log(y) - mu) / sig, df = nu) / sig) / y
  } else if (family_id == "mirror_gamma" && !is.null(fit$mirrorgamma)) {
    sh <- fit$mirrorgamma[expert_index, "shape"]
    ra <- fit$mirrorgamma[expert_index, "rate"]
    pdf <- stats::dgamma(pmax(hi - x_eval, 0), shape = sh, rate = ra)
  } else if (family_id == "mirror_log_normal" && !is.null(fit$mirrorlognormal)) {
    mu <- fit$mirrorlognormal[expert_index, "mean.log.X"]
    sig <- fit$mirrorlognormal[expert_index, "sd.log.X"]
    pdf <- stats::dlnorm(pmax(hi - x_eval, 1e-12), mu, sig)
  } else if (!is.null(fit$Beta)) {
    a <- fit$Beta[expert_index, "shape1"]
    b <- fit$Beta[expert_index, "shape2"]
    u <- (x_eval - lo) / max(hi - lo, 1e-9)
    pdf <- stats::dbeta(pmin(pmax(u, 1e-6), 1 - 1e-6), a, b) / max(hi - lo, 1e-9)
    family_id <- "beta"
  }
  list(x = as.numeric(xs), pdf = as.numeric(pdf), family = family_id)
}

best_fit_label <- function(fit, j) {
  if (is.null(fit$best.fitting)) return("Beta")
  bf <- fit$best.fitting
  if (is.data.frame(bf) || is.matrix(bf)) {
    col <- if ("best.fit" %in% colnames(bf)) "best.fit" else 1
    return(as.character(bf[j, col]))
  }
  as.character(bf[[j]])
}

shelf_distribution_mean <- function(fit, expert_index, family_id, lo = 0, hi = 1, curves = NULL) {
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  fam <- shelf_family_id(family_id)

  val <- tryCatch({
    if (fam == "normal" && !is.null(fit$Normal)) {
      as.numeric(fit$Normal[expert_index, "mean"])
    } else if (fam == "student_t" && !is.null(fit$Student.t)) {
      df <- as.numeric(fit$Student.t[expert_index, "df"])
      if (is.finite(df) && df > 1) as.numeric(fit$Student.t[expert_index, "location"]) else NA_real_
    } else if (fam == "skew_normal" && !is.null(fit$Skewnormal)) {
      xi <- as.numeric(fit$Skewnormal[expert_index, "location"])
      omega <- as.numeric(fit$Skewnormal[expert_index, "scale"])
      alpha <- as.numeric(fit$Skewnormal[expert_index, "slant"])
      delta <- alpha / sqrt(1 + alpha^2)
      as.numeric(xi + omega * delta * sqrt(2 / pi))
    } else if (fam == "beta" && !is.null(fit$Beta)) {
      a <- as.numeric(fit$Beta[expert_index, "shape1"])
      b <- as.numeric(fit$Beta[expert_index, "shape2"])
      as.numeric(lo + (hi - lo) * (a / (a + b)))
    } else if (fam == "gamma" && !is.null(fit$Gamma)) {
      sh <- as.numeric(fit$Gamma[expert_index, "shape"])
      ra <- as.numeric(fit$Gamma[expert_index, "rate"])
      as.numeric(lo + (sh / ra))
    } else if (fam == "log_normal" && !is.null(fit$Log.normal)) {
      mu <- as.numeric(fit$Log.normal[expert_index, "mean.log.X"])
      sig <- as.numeric(fit$Log.normal[expert_index, "sd.log.X"])
      as.numeric(lo + exp(mu + (sig^2) / 2))
    } else if (fam == "mirror_gamma" && !is.null(fit$mirrorgamma)) {
      sh <- as.numeric(fit$mirrorgamma[expert_index, "shape"])
      ra <- as.numeric(fit$mirrorgamma[expert_index, "rate"])
      as.numeric(hi - (sh / ra))
    } else if (fam == "mirror_log_normal" && !is.null(fit$mirrorlognormal)) {
      mu <- as.numeric(fit$mirrorlognormal[expert_index, "mean.log.X"])
      sig <- as.numeric(fit$mirrorlognormal[expert_index, "sd.log.X"])
      as.numeric(hi - exp(mu + (sig^2) / 2))
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)

  if (is.finite(val)) return(val)

  # Fallback: numerical expectation over evaluated curve grid if available
  if (!is.null(curves) && !is.null(curves$x) && !is.null(curves$pdf)) {
    xs <- as.numeric(curves$x)
    ps <- as.numeric(curves$pdf)
    if (length(xs) > 1 && length(xs) == length(ps)) {
      dx <- diff(xs)
      mid_x <- (xs[-1] + xs[-length(xs)]) / 2
      mid_p <- (ps[-1] + ps[-length(ps)]) / 2
      area <- sum(mid_p * dx)
      if (is.finite(area) && area > 0) {
        grid_m <- sum(mid_x * mid_p * dx) / area
        if (is.finite(grid_m)) return(grid_m)
      }
    }
  }

  NA_real_
}

# Experts sometimes enter 0–100 (percent) while question is bounded 0–1, or vice versa.
shelf_maybe_percent_to_unit <- function(vals, lo, hi) {
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  mx <- max(vals, na.rm = TRUE)
  if (is.finite(hi) && hi <= 1.0001 && is.finite(mx) && mx > hi + 1e-9 && mx <= 100) {
    return(vals / 100)
  }
  if (is.finite(hi) && hi > 1.0001 && is.finite(mx) && mx <= 1.0001 && min(vals, na.rm = TRUE) >= 0) {
    return(vals * 100)
  }
  vals
}

shelf_validate_vals <- function(vals, lo, hi, names_e = NULL) {
  if (!is.finite(lo) || !is.finite(hi) || !(lo < hi)) {
    stop("Invalid plausible limits: the lower limit must be less than the upper limit.")
  }
  for (j in seq_len(ncol(vals))) {
    v <- as.numeric(vals[, j])
    label <- if (!is.null(names_e) && nzchar(names_e[[j]])) names_e[[j]] else paste0("Expert ", j)
    if (any(!is.finite(v))) {
      stop(sprintf("Missing or non-numeric quantile values for %s.", label))
    }
    if (any(v <= lo) || any(v >= hi)) {
      stop(sprintf(
        "Quantile values for %s must be strictly between %.6g and %.6g.",
        label, lo, hi
      ))
    }
    if (any(diff(v) <= 0)) {
      stop(sprintf("Quantiles for %s must be strictly increasing.", label))
    }
  }
  invisible(TRUE)
}

# SHELF::fitdist requires values strictly inside (lower, upper) and increasing.
shelf_interior_vals <- function(vals, lo, hi) {
  span <- max(as.numeric(hi) - as.numeric(lo), 1e-9)
  n <- nrow(vals)
  eps <- span * 1e-4
  lo_i <- as.numeric(lo) + eps
  hi_i <- as.numeric(hi) - eps
  if (lo_i >= hi_i) {
    lo_i <- as.numeric(lo) + span * 1e-6
    hi_i <- as.numeric(hi) - span * 1e-6
  }
  min_step <- min(eps, (hi_i - lo_i) / max(n + 1, 2))
  out <- vals
  for (j in seq_len(ncol(vals))) {
    v <- as.numeric(vals[, j])
    v[!is.finite(v)] <- (lo_i + hi_i) / 2
    v <- pmin(pmax(v, lo_i), hi_i)
    for (i in seq_len(n)) {
      floor_i <- lo_i + (i - 1) * min_step
      ceil_i <- hi_i - (n - i) * min_step
      if (i > 1) floor_i <- max(floor_i, v[i - 1] + min_step)
      v[i] <- min(max(v[i], floor_i), ceil_i)
    }
    out[, j] <- v
  }
  out
}

run_shelf_fit <- function(experts, lo = 0, hi = 1, probs = c(0.1, 0.5, 0.9), preferred = "best", weights = NULL) {
  if (!shelf_available()) stop("Package SHELF is not installed")
  n_e <- length(experts)
  if (n_e < 1) stop("No expert judgments to fit")
  vals <- matrix(NA_real_, nrow = length(probs), ncol = n_e)
  names_e <- character(n_e)
  for (j in seq_len(n_e)) {
    ex <- experts[[j]]
    names_e[j] <- ex$name %||% paste0("Expert", j)
    q <- ex$quantiles
    for (i in seq_along(probs)) {
      key <- as.character(probs[i])
      v <- q[[key]]
      if (is.null(v)) v <- q[[sprintf("%.1f", probs[i])]]
      if (is.null(v)) stop(sprintf("Missing quantile %s for %s", key, names_e[j]))
      vals[i, j] <- as.numeric(v)
    }
  }
  colnames(vals) <- make.unique(as.character(names_e), sep = " ")
  names_e <- colnames(vals)
  vals <- shelf_maybe_percent_to_unit(vals, lo, hi)

  # Adaptively ensure plausible bounds enclose all expert values with safety margin
  all_v <- vals[is.finite(vals)]
  if (length(all_v)) {
    min_v <- min(all_v)
    max_v <- max(all_v)
    span <- max(hi - lo, 1e-4)
    if (min_v <= lo) {
      margin <- max(0.05 * span, (lo - min_v) + 0.02 * span)
      lo <- if (lo >= 0 && (min_v - margin) < 0 && min_v >= 0) 0 else min_v - margin
    }
    if (max_v >= hi) {
      margin <- max(0.05 * span, (max_v - hi) + 0.02 * span)
      hi <- max_v + margin
    }
  }

  shelf_validate_vals(vals, lo, hi, names_e)
  vals <- shelf_interior_vals(vals, lo, hi)

  w <- if (!is.null(weights) && length(weights) == n_e) {
    w_num <- suppressWarnings(as.numeric(weights))
    w_num[!is.finite(w_num) | w_num < 0] <- 0
    if (sum(w_num) > 0) w_num / sum(w_num) else rep(1 / n_e, n_e)
  } else {
    rep(1 / n_e, n_e)
  }

  fit <- SHELF::fitdist(
    vals = vals,
    probs = probs,
    lower = lo,
    upper = hi,
    expertnames = names_e
  )
  preferred_id <- shelf_family_id(preferred)
  expert_outs <- lapply(seq_len(n_e), function(j) {
    ex <- experts[[j]]
    best_raw <- best_fit_label(fit, j)
    target_fam <- if (preferred_id %in% c("best", "")) shelf_family_id(best_raw) else preferred_id
    curves <- tryCatch(
      shelf_pdf_grid(fit, j, target_fam, lo, hi),
      error = function(e) shelf_pdf_grid(fit, j, shelf_family_id(best_raw), lo, hi)
    )
    if (all(curves$pdf == 0) && !identical(target_fam, shelf_family_id(best_raw))) {
      curves <- tryCatch(
        shelf_pdf_grid(fit, j, shelf_family_id(best_raw), lo, hi),
        error = function(e) shelf_pdf_grid(fit, j, "beta", lo, hi)
      )
    }
    ex_mean <- shelf_distribution_mean(fit, j, curves$family, lo, hi, curves = curves)
    list(
      name = names_e[j],
      id = ex$id %||% names_e[j],
      rationale = ex$rationale %||% "",
      bestFitting = best_raw,
      family = curves$family,
      mean = ex_mean,
      weight = w[j],
      quantiles = setNames(as.list(vals[, j]), as.character(probs)),
      x = curves$x,
      pdf = curves$pdf
    )
  })

  d_arg <- shelf_to_d_arg(preferred_id)
  lp_dens <- tryCatch(
    SHELF::linearPoolDensity(fit, xl = lo, xu = hi, d = d_arg, lpw = w, nx = 81),
    error = function(e) NULL
  )
  lp_q <- tryCatch(
    as.numeric(SHELF::qlinearpool(fit, q = probs, d = d_arg, w = w)),
    error = function(e) apply(vals, 1, function(row) sum(row * w))
  )
  expert_means <- vapply(expert_outs, function(e) as.numeric(e$mean %||% NA_real_), numeric(1))
  lp_mean <- if (all(is.finite(expert_means))) {
    sum(expert_means * w)
  } else if (!is.null(lp_dens) && !is.null(lp_dens$x) && !is.null(lp_dens$f)) {
    xs <- as.numeric(lp_dens$x)
    ps <- as.numeric(lp_dens$f)
    dx <- diff(xs)
    mid_x <- (xs[-1] + xs[-length(xs)]) / 2
    mid_p <- (ps[-1] + ps[-length(ps)]) / 2
    area <- sum(mid_p * dx)
    if (is.finite(area) && area > 0) sum(mid_x * mid_p * dx) / area else NA_real_
  } else {
    sum(expert_means * w, na.rm = TRUE) / max(sum(w[is.finite(expert_means)]), 1e-6)
  }
  linear_pool <- list(
    name = "Linear Pool",
    family = if (preferred_id %in% c("best", "")) "Linear Pool" else paste0("Linear Pool (", preferred_id, ")"),
    bestFitting = "SHELF Linear Opinion Pool",
    shelfBestFitting = "linear_pool",
    mean = lp_mean,
    weights = w,
    quantiles = setNames(as.list(lp_q), as.character(probs)),
    x = if (is.null(lp_dens)) seq(lo, hi, length.out = 81) else as.numeric(lp_dens$x),
    pdf = if (is.null(lp_dens)) rep(0, 81) else as.numeric(lp_dens$f)
  )
  list(
    engine = "shelf_native",
    shelfVersion = as.character(utils::packageVersion("SHELF")),
    lower = lo,
    upper = hi,
    probs = probs,
    experts = expert_outs,
    weights = w,
    pool = linear_pool,
    linearPool = linear_pool,
    selectedFamily = preferred_id
  )
}

quantile_triple <- function(qlist, probs = c(0.1, 0.5, 0.9)) {
  if (is.null(qlist) || !length(qlist)) return(c(NA_real_, NA_real_, NA_real_))
  nms <- names(qlist)
  if (is.null(nms) || !any(nzchar(nms))) nms <- as.character(probs)
  num_vals <- as.numeric(unlist(qlist, use.names = FALSE))
  v <- as.list(stats::setNames(num_vals, nms))
  low <- v[["0.1"]] %||% v[["0.25"]] %||% v[["0.05"]] %||% num_vals[[1]]
  mid <- v[["0.5"]] %||% v[["0.50"]] %||% num_vals[[min(2L, length(num_vals))]]
  high <- v[["0.9"]] %||% v[["0.75"]] %||% v[["0.95"]] %||% num_vals[[length(num_vals)]]
  c(as.numeric(low), as.numeric(mid), as.numeric(high))
}

build_conclusion <- function(fit_result, question_title, n) {
  lop <- quantile_triple(fit_result$linearPool$quantiles %||% fit_result$pool$quantiles, fit_result$probs)
  lop_mean <- fit_result$linearPool$mean %||% fit_result$pool$mean
  lop_mean_str <- if (!is.null(lop_mean) && is.finite(lop_mean)) sprintf(" (mean = %.2f)", lop_mean) else ""
  sprintf(
    "SHELF Linear Pool consensus for “%s” (%d expert%s): P10 = %.2f, Median (P50) = %.2f, P90 = %.2f%s.",
    question_title,
    n,
    if (n == 1) "" else "s",
    lop[[1]], lop[[2]], lop[[3]], lop_mean_str
  )
}

shelf_plot_df <- function(fit_result, show_pool = TRUE) {
  rows <- list()
  add_series <- function(name, role, x, y) {
    if (is.null(x) || is.null(y)) return()
    rows[[length(rows) + 1]] <<- data.frame(
      name = name, role = role, x = as.numeric(x), pdf = as.numeric(y),
      stringsAsFactors = FALSE
    )
  }
  for (ex in fit_result$experts) add_series(ex$name, "expert", ex$x, ex$pdf)
  if (!identical(show_pool, FALSE) && !identical(show_pool, "none")) {
    pool_obj <- fit_result$linearPool %||% fit_result$pool
    if (!is.null(pool_obj)) {
      add_series(pool_obj$name %||% "Linear Pool", "pool", pool_obj$x, pool_obj$pdf)
    }
  }
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

shelf_params_table <- function(fit_result) {
  rows <- list()
  add <- function(name, role, qlist, mean_val, family, extra = "", weight = NA_real_) {
    trip <- quantile_triple(qlist, fit_result$probs)
    rows[[length(rows) + 1]] <<- data.frame(
      series = name,
      role = role,
      weight = as.numeric(weight),
      mean = as.numeric(mean_val %||% NA_real_),
      q_low = trip[[1]],
      q_mid = trip[[2]],
      q_high = trip[[3]],
      family = family %||% "",
      note = extra,
      stringsAsFactors = FALSE
    )
  }
  for (ex in fit_result$experts) {
    add(
      ex$name, "expert", ex$quantiles, ex$mean, ex$family,
      extra = ex$bestFitting %||% "",
      weight = ex$weight %||% NA_real_
    )
  }
  pool_obj <- fit_result$linearPool %||% fit_result$pool
  if (!is.null(pool_obj)) {
    add(
      pool_obj$name %||% "Linear Pool", "linear_pool", pool_obj$quantiles,
      pool_obj$mean, "Linear Pool",
      extra = pool_obj$bestFitting %||% "SHELF Linear Opinion Pool",
      weight = 1.0
    )
  }
  do.call(rbind, rows)
}
