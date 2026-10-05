ee_header <- function(user = NULL, extra = NULL) {
  htmltools::tags$header(
    class = "site-header",
    htmltools::div(
      class = "header-inner",
      htmltools::tags$img(src = "srlogo.jpg", alt = "SCTIMST", class = "brand-logo"),
      htmltools::div(
        class = "brand-text",
        htmltools::tags$h1("STRUCTURED EXPERT ELICITATION-AMCHSS"),
        htmltools::tags$p(
          class = "institute",
          "Sree Chitra Tirunal Institute for Medical Sciences & Technology, Trivandrum"
        ),
        htmltools::tags$p(
          class = "team",
          htmltools::tags$a(
            href = "https://hta-amchss-sctimst.github.io/RRC/",
            target = "_blank",
            rel = "noopener noreferrer",
            aria_label = "Achutha Menon Centre for Health Science Studies website",
            "Achutha Menon Centre for Health Science Studies (AMCHSS)"
          )
        )
      ),
      if (!is.null(user)) {
        initial <- toupper(substr(user$displayName %||% user$email %||% "U", 1, 1))
        htmltools::div(
          class = "header-actions",
          htmltools::div(
            class = "user-chip",
            htmltools::span(class = "avatar-circle", initial),
            htmltools::div(
              class = "user-details",
              htmltools::tags$strong(user$displayName %||% user$email),
              htmltools::tags$span(class = "user-role-badge", role_label(user$platformRole))
            )
          ),
          extra
        )
      }
    )
  )
}

status_pill <- function(status) {
  st_str <- as.character(status %||% "")
  css_cls <- switch(
    st_str,
    "Round 1 Submitted" = "submitted",
    "Round 2 Submitted" = "submitted",
    "Round 2 In Progress" = "ongoing",
    "Round 2 Pending" = "draft",
    gsub("[^a-zA-Z0-9_-]", "-", tolower(st_str))
  )
  lab <- switch(
    st_str,
    draft = "Draft",
    recruiting = "Published",
    eliciting = "Published",
    workshop = "Deliberation",
    submitted = "Submitted",
    ongoing = "Ongoing",
    not_started = "Not started",
    completed = "Completed",
    ready = "Ready for SHELF",
    active = "Active",
    st_str
  )
  htmltools::span(
    class = paste("pill", css_cls),
    htmltools::span(class = "status-dot"),
    lab
  )
}

notice <- function(msg, kind = "info") {
  if (!nzchar(msg %||% "")) return(NULL)
  htmltools::div(class = paste("notice", kind), msg)
}

single_expert_preview_fit <- function(lo, hi, p10, p50, p90, mode = "percentile") {
  p10 <- as.numeric(p10)
  p50 <- as.numeric(p50)
  p90 <- as.numeric(p90)
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)

  if (!is.finite(p10) || !is.finite(p50) || !is.finite(p90) || !is.finite(lo) || !is.finite(hi)) {
    return(list(ok = FALSE, status = "empty", msg = "Enter your values above to generate your live distribution preview."))
  }
  if (p10 <= lo) {
    return(list(ok = FALSE, status = "invalid", msg = sprintf("Lower value (%s) must be greater than lowest plausible limit (%s).", p10, lo)))
  }
  if (p90 >= hi) {
    return(list(ok = FALSE, status = "invalid", msg = sprintf("Upper value (%s) must be less than upper plausible limit (%s).", p90, hi)))
  }
  if (p10 >= p50) {
    return(list(ok = FALSE, status = "invalid", msg = sprintf("Lower value (%s) must be less than Best Estimate (%s).", p10, p50)))
  }
  if (p50 >= p90) {
    return(list(ok = FALSE, status = "invalid", msg = sprintf("Best Estimate (%s) must be less than Upper value (%s).", p50, p90)))
  }

  probs <- if (identical(mode, "quartile")) c(0.25, 0.50, 0.75) else c(0.10, 0.50, 0.90)

  res <- tryCatch({
    q_map <- stats::setNames(list(p10, p50, p90), as.character(probs))
    s_fit <- run_shelf_fit(
      experts = list(list(name = "Your Estimate", quantiles = q_map)),
      lo = lo, hi = hi, probs = probs, preferred = "best"
    )
    ex <- s_fit$experts[[1]]
    if (length(ex$x) > 5 && max(ex$pdf, na.rm = TRUE) > 0) {
      list(
        ok = TRUE,
        status = "ready",
        x = ex$x,
        pdf = ex$pdf,
        family = ex$family,
        mean = ex$mean,
        p10 = p10, p50 = p50, p90 = p90,
        probs = probs,
        lo = lo, hi = hi,
        mode = mode
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (!is.null(res)) return(res)

  # Robust fallback: normal approximation within bounds
  xs <- seq(lo, hi, length.out = 101)
  denom <- stats::qnorm(probs[3]) - stats::qnorm(probs[1])
  if (!is.finite(denom) || denom <= 0) denom <- 2.56
  est_sd <- max((p90 - p10) / denom, 1e-4)
  dens <- stats::dnorm(xs, mean = p50, sd = est_sd)
  dens[xs < lo | xs > hi] <- 0
  dx <- xs[2] - xs[1]
  area <- sum(dens) * dx
  if (is.finite(area) && area > 0) dens <- dens / area

  list(
    ok = TRUE,
    status = "ready",
    x = xs,
    pdf = dens,
    family = "normal",
    mean = p50,
    p10 = p10, p50 = p50, p90 = p90,
    probs = probs,
    lo = lo, hi = hi,
    mode = mode
  )
}

draw_expert_quantile_preview_plot <- function(fit_res, unit_label = "") {
  if (is.null(fit_res) || !isTRUE(fit_res$ok)) return(NULL)

  xs <- fit_res$x
  ys <- fit_res$pdf
  lo <- fit_res$lo
  hi <- fit_res$hi
  p10 <- fit_res$p10
  p50 <- fit_res$p50
  p90 <- fit_res$p90
  mode <- fit_res$mode %||% "percentile"
  lbl_p10 <- if (identical(mode, "quartile")) "Q1" else "P10"
  lbl_p90 <- if (identical(mode, "quartile")) "Q3" else "P90"

  graphics::par(mar = c(3.4, 1.2, 1.5, 1.2), bg = "#ffffff")
  y_max <- max(ys, na.rm = TRUE)
  if (!is.finite(y_max) || y_max <= 0) y_max <- 1
  plot(xs, ys, type = "n", xlab = "", ylab = "", axes = FALSE,
       xlim = c(lo, hi), ylim = c(0, y_max * 1.18))

  # Shaded central 80% (or 50%) region
  idx_shade <- which(xs >= p10 & xs <= p90)
  if (length(idx_shade) > 1) {
    poly_x <- c(p10, xs[idx_shade], p90)
    poly_y <- c(0, ys[idx_shade], 0)
    graphics::polygon(poly_x, poly_y, col = "#e0e7ff", border = NA)
  }

  # Smooth density line
  graphics::lines(xs, ys, col = "#4f46e5", lwd = 2.8)

  # Baseline
  graphics::abline(h = 0, col = "#e2e8f0", lwd = 1)

  # Vertical reference lines
  y_p10 <- tryCatch(stats::approx(xs, ys, xout = p10)$y, error = function(e) 0) %||% 0
  y_p50 <- tryCatch(stats::approx(xs, ys, xout = p50)$y, error = function(e) 0) %||% 0
  y_p90 <- tryCatch(stats::approx(xs, ys, xout = p90)$y, error = function(e) 0) %||% 0

  graphics::segments(p10, 0, p10, y_p10, col = "#6366f1", lty = 2, lwd = 1.5)
  graphics::segments(p50, 0, p50, y_p50, col = "#4338ca", lty = 1, lwd = 2.2)
  graphics::segments(p90, 0, p90, y_p90, col = "#6366f1", lty = 2, lwd = 1.5)

  # Small anchor points
  graphics::points(c(p10, p50, p90), c(y_p10, y_p50, y_p90), pch = 21,
                   bg = c("#6366f1", "#4338ca", "#6366f1"), col = "#ffffff", cex = 1.2, lwd = 1.5)

  # Custom horizontal axis with clear clinical markers
  ticks <- c(lo, p10, p50, p90, hi)
  unit_str <- if (nzchar(unit_label)) paste0(" ", unit_label) else ""
  labels <- c(
    sprintf("L\n(%s)", lo),
    sprintf("%s\n(%s%s)", lbl_p10, p10, unit_str),
    sprintf("Median\n(%s%s)", p50, unit_str),
    sprintf("%s\n(%s%s)", lbl_p90, p90, unit_str),
    sprintf("U\n(%s)", hi)
  )

  graphics::axis(1, at = ticks, labels = labels, col = "#cbd5e1", col.axis = "#1e293b",
                 cex.axis = 0.82, padj = 0.6, lwd = 1)
}

render_expert_quantile_preview_ui <- function(fit_res, unit_label = "", mode = "percentile") {
  if (is.null(fit_res)) return(NULL)

  if (identical(fit_res$status, "empty")) {
    return(shiny::div(
      style = "background: #f8fafc; border: 1px dashed #cbd5e1; border-radius: 8px; padding: 14px 18px; margin: 16px 0; text-align: center;",
      htmltools::tags$span(style = "font-size: 1.1rem; margin-right: 6px;", "💡"),
      htmltools::tags$span(
        style = "font-size: 0.88rem; color: #475569; font-weight: 500;",
        "Enter your Lower, Central (Best estimate), and Upper values above to view your real-time implied probability curve and clinical interpretation."
      )
    ))
  }

  if (identical(fit_res$status, "invalid")) {
    return(shiny::div(
      style = "background: #fffbeb; border: 1px solid #fde68a; border-radius: 8px; padding: 12px 16px; margin: 16px 0; display: flex; align-items: center; gap: 10px;",
      htmltools::tags$span(style = "font-size: 1.1rem;", "⚠️"),
      htmltools::tags$span(
        style = "font-size: 0.88rem; color: #92400e; font-weight: 500;",
        fit_res$msg
      )
    ))
  }

  p10 <- fit_res$p10
  p50 <- fit_res$p50
  p90 <- fit_res$p90
  u_str <- if (nzchar(unit_label)) paste0(" ", unit_label) else ""
  is_quartile <- identical(mode, "quartile")
  mid_pct <- if (is_quartile) "50%" else "80%"
  tail_pct <- if (is_quartile) "25%" else "10%"
  fam_display <- switch(
    fit_res$family %||% "",
    normal = "Normal distribution",
    skew_normal = "Skew-Normal distribution",
    beta = "Beta distribution",
    gamma = "Gamma distribution",
    log_normal = "Log-Normal distribution",
    student_t = "Student-t distribution",
    log_t = "Log-Student-t distribution",
    mirror_gamma = "Mirror Gamma distribution",
    mirror_log_normal = "Mirror Log-Normal distribution",
    paste(toupper(substr(fit_res$family, 1, 1)), substr(fit_res$family, 2, nchar(fit_res$family)), sep = "")
  )

  shiny::div(
    class = "preview-curve-card",
    style = "background: #f8fafc; border: 1px solid #cbd5e1; border-radius: 10px; padding: 16px; margin: 18px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.04);",

    htmltools::div(
      style = "display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid #e2e8f0; padding-bottom: 10px; margin-bottom: 12px; flex-wrap: wrap; gap: 8px;",
      htmltools::div(
        style = "display: flex; align-items: center; gap: 8px;",
        htmltools::tags$span(style = "font-size: 1.1rem;", "👁️"),
        htmltools::strong(style = "font-size: 0.95rem; color: #0f172a;", "Visual Check: Your Implied Probability Distribution")
      ),
      htmltools::span(
        class = "status-pill status-ready",
        style = "font-size: 0.78rem; text-transform: none; font-weight: 600;",
        fam_display
      )
    ),

    shiny::plotOutput("expert_quantile_preview_plot", height = "190px"),

    htmltools::div(
      style = "background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px 16px; margin-top: 14px;",
      htmltools::tags$h5(style = "margin: 0 0 6px 0; font-size: 0.85rem; font-weight: 700; color: #334155; text-transform: uppercase; letter-spacing: 0.03em;", "💬 What this means in plain words:"),
      htmltools::tags$ul(
        style = "margin: 0; padding-left: 18px; font-size: 0.86rem; color: #334155; line-height: 1.55;",
        htmltools::tags$li(
          htmltools::tags$strong(sprintf("Central Range (%s probability): ", mid_pct)),
          sprintf("You believe there is a %s chance the true value lies between ", mid_pct),
          htmltools::strong(sprintf("%s%s", p10, u_str)),
          " and ",
          htmltools::strong(sprintf("%s%s", p90, u_str)),
          " (shaded area)."
        ),
        htmltools::tags$li(
          htmltools::tags$strong("Median / Best Estimate: "),
          htmltools::strong(sprintf("%s%s", p50, u_str)),
          " is your central value (50% probability the true value is higher, 50% probability it is lower)."
        ),
        htmltools::tags$li(
          htmltools::tags$strong(sprintf("Tail Uncertainty (%s each): ", tail_pct)),
          sprintf("You consider only a %s chance the value is below ", tail_pct),
          htmltools::strong(sprintf("%s%s", p10, u_str)),
          sprintf(", and a %s chance it exceeds ", tail_pct),
          htmltools::strong(sprintf("%s%s", p90, u_str)),
          "."
        )
      ),
      htmltools::tags$p(
        style = "margin: 8px 0 0 0; font-size: 0.82rem; color: #64748b; font-style: italic;",
        "💡 Review this curve carefully. If the spread feels too narrow (overconfident) or too wide (underconfident), you can adjust your numbers above before continuing."
      )
    )
  )
}
