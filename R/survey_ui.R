primary_question <- function(questions) {
  if (!length(questions)) return(NULL)
  qn <- Filter(function(q) !is_chips_question(q), questions)
  if (length(qn)) qn[[1]] else questions[[1]]
}

survey_pages <- function(study, questions, round_num = 1L) {
  rnd <- suppressWarnings(as.integer(round_num %||% 1L))
  if (is.na(rnd) || rnd < 1L) rnd <- 1L

  pages <- list()
  if (rnd == 1L) {
    pages[[length(pages) + 1]] <- list(kind = "welcome", title = "Welcome", subtitle = study$title %||% "Survey", round_num = 1L)
    pages[[length(pages) + 1]] <- list(kind = "onboarding", title = "Participant Consent", subtitle = "Consent declaration", round_num = 1L)
    pages[[length(pages) + 1]] <- list(kind = "bounds", title = "Plausible bounds", subtitle = "Review facilitator limits", round_num = 1L)
    for (i in seq_along(questions)) {
      q <- questions[[i]]
      if (is_chips_question(q)) {
        pages[[length(pages) + 1]] <- list(
          kind = "chips",
          title = sprintf("Question %d of %d — %s", i, length(questions), q$title),
          subtitle = "Chips-N-Bins (Round 1)",
          question = q,
          q_idx = i,
          round_num = 1L
        )
      } else {
        pages[[length(pages) + 1]] <- list(
          kind = "quantile",
          title = sprintf("Question %d of %d — %s", i, length(questions), q$title),
          subtitle = "Low-High-Best (Round 1)",
          question = q,
          q_idx = i,
          round_num = 1L
        )
      }
    }
    pages[[length(pages) + 1]] <- list(kind = "submit", title = "Submission", subtitle = "Submit Round 1 Judgments", round_num = 1L)
    pages[[length(pages) + 1]] <- list(kind = "done", title = "Completed", subtitle = "Round 1 Recorded", round_num = 1L)
  } else {
    pages[[length(pages) + 1]] <- list(
      kind = "welcome_round2",
      title = sprintf("Round %d Briefing", rnd),
      subtitle = "Delphi Deliberation",
      round_num = rnd
    )
    pages[[length(pages) + 1]] <- list(kind = "bounds", title = "Plausible bounds", subtitle = "Review facilitator limits", round_num = rnd)
    for (i in seq_along(questions)) {
      q <- questions[[i]]
      pages[[length(pages) + 1]] <- list(
        kind = "review",
        title = sprintf("Q%d Review — %s", i, q$title),
        subtitle = "Consensus & Feedback",
        question = q,
        q_idx = i,
        round_num = rnd
      )
      if (is_chips_question(q)) {
        pages[[length(pages) + 1]] <- list(
          kind = "chips",
          title = sprintf("Q%d Revision — %s", i, q$title),
          subtitle = sprintf("Chips-N-Bins (Round %d)", rnd),
          question = q,
          q_idx = i,
          round_num = rnd
        )
      } else {
        pages[[length(pages) + 1]] <- list(
          kind = "quantile",
          title = sprintf("Q%d Revision — %s", i, q$title),
          subtitle = sprintf("Low-High-Best (Round %d)", rnd),
          question = q,
          q_idx = i,
          round_num = rnd
        )
      }
    }
    pages[[length(pages) + 1]] <- list(kind = "submit", title = "Submission", subtitle = sprintf("Submit Round %d Judgments", rnd), round_num = rnd)
    pages[[length(pages) + 1]] <- list(kind = "done", title = "Completed", subtitle = sprintf("Round %d Recorded", rnd), round_num = rnd)
  }
  pages
}

survey_root_ui <- function(input, rv, study_key, ping, token_key = "") {
  if (!isTRUE(ping$ok)) {
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1("Database not connected"), htmltools::tags$p(ping$message)
    )))
  }
  if (!nzchar(study_key) && !nzchar(token_key) && is.null(rv$survey_study)) {
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1("Survey"),
      htmltools::tags$p("Open a survey link that includes ?study=<slug> or a personal invite token.")
    )))
  }
  st <- if (nzchar(study_key)) find_study(study_key) else rv$survey_study
  if (is.null(st) && nzchar(token_key)) {
    tok <- find_invite_token(token_key)
    if (!is.null(tok)) st <- find_study(tok$studyId)
  }
  if (is.null(st)) {
    msg <- if (nzchar(token_key)) {
      "This invite link is invalid, expired, or has been revoked. Please ask the facilitator to share a fresh link."
    } else {
      paste("No study matches", study_key)
    }
    return(shiny::div(class = "login-page", shiny::div(class = "login-card",
      htmltools::tags$h1(if (nzchar(token_key)) "Invalid Link" else "Case study not found"),
      htmltools::tags$p(msg)
    )))
  }
  if (nzchar(token_key) && !isTRUE(rv$token_tried) && is.null(rv$survey_user)) {
    return(shiny::div(
      class = "survey-shell",
      ee_header(),
      shiny::div(
        class = "login-page",
        style = "min-height: calc(100vh - 120px); padding: 2rem 1rem;",
        shiny::div(
          class = "login-card",
          style = "max-width: 520px; margin: 0 auto; text-align: center;",
          htmltools::tags$h2(style = "color: var(--primary); margin-bottom: 0.5rem;", st$title),
          htmltools::tags$p(class = "muted", "Connecting your expert session..."),
          htmltools::tags$div(
            style = "margin: 2rem 0; font-size: 1rem; color: #4f46e5; font-weight: 600;",
            "Verifying invite token..."
          )
        )
      )
    ))
  }
  if (is.null(rv$survey_user)) {
    welcome <- (st$protocolConfig %||% list())$surveyWelcome %||% list()
    return(shiny::div(
      class = "survey-shell",
      ee_header(),
      shiny::div(
        class = "login-page",
        style = "min-height: calc(100vh - 120px); padding: 2rem 1rem;",
        shiny::div(
          class = "login-card",
          style = "max-width: 520px; margin: 0 auto; text-align: center;",
          htmltools::tags$h2(style = "color: var(--primary); margin-bottom: 0.25rem;", st$title),
          htmltools::tags$p(class = "muted", style = "font-weight: 500;", welcome$conductedBy %||% welcome$institution %||% "Achutha Menon Centre for Health Science Studies (AMCHSS), SCTIMST"),
          if (nzchar(welcome$contactEmail %||% "")) {
            htmltools::tags$p(style = "font-size: 0.8rem; color: #64748b; margin-top: -0.2rem;", sprintf("Lead Contact: %s", welcome$contactEmail))
          } else NULL,
          htmltools::tags$p(
            style = "margin: 1.25rem 0 1.5rem; color: #475569; font-size: 0.95rem; line-height: 1.5;",
            "Enter the email address invited by the facilitator to access your elicitation survey."
          ),
          shiny::div(
            class = "form-grid",
            style = "max-width: 420px; margin: 0 auto; text-align: center;",
            shiny::textInput("survey_email", "Invited Email Address", placeholder = "expert@hospital.org", width = "100%"),
            shiny::actionButton("survey_enter", "Continue to Survey →", class = "btn-primary", style = "width: 100%; margin-top: 0.75rem;")
          ),
          notice(rv$err, "error")
        )
      )
    ))
  }

  # Stable survey shell with granular reactive outputs to prevent whole-page redraws
  shiny::div(
    class = "survey-shell",
    ee_header(rv$survey_user),
    shiny::uiOutput("survey_wizard_header"),
    shiny::div(
      class = "survey-layout",
      shiny::uiOutput("survey_sidebar_nav"),
      shiny::div(class = "panel survey-main", shiny::uiOutput("survey_page_body"))
    )
  )
}

survey_page_body_content <- function(input, rv, st, qs, r_num, pages, idx) {
  pg <- pages[[idx]]
  lo0 <- rv$bound_lo %||% 0
  hi0 <- rv$bound_hi %||% 1
  body <- switch(
    pg$kind,
    welcome = shiny::tagList(
      shiny::div(
        class = "survey-welcome-card",
        style = "max-width: 680px; margin: 1.5rem auto; text-align: left; background: #fff; border: 1px solid #e2e8f0; border-radius: 12px; padding: 2.25rem 2.5rem; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05);",
        htmltools::tags$h2(style = "font-size: 1.85rem; font-weight: 800; color: #0f172a; margin-top: 0; margin-bottom: 0.75rem;", "Welcome!"),
        htmltools::tags$p(
          style = "font-size: 1.05rem; line-height: 1.6; color: #334155; margin-bottom: 1.25rem;",
          {
            desc_val <- trimws(as.character(st$description %||% ""))
            if (!nzchar(desc_val) || identical(desc_val, "NA")) {
              sprintf("This survey contains a series of questions to collect your expert judgements for %s.", st$title)
            } else {
              desc_val
            }
          }
        ),
        htmltools::div(
          class = "welcome-details-box",
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 18px; margin: 18px 0; text-align: left;",
          htmltools::tags$h4(style = "margin: 0 0 10px 0; font-size: 0.95rem; font-weight: 700; color: #0f172a;", "Study Background & Conduct"),
          {
            w_cfg <- (st$protocolConfig %||% list())$surveyWelcome %||% list()
            cond_inst <- w_cfg$conductedBy %||% w_cfg$institution %||% "Achutha Menon Centre for Health Science Studies (AMCHSS), Sree Chitra Tirunal Institute for Medical Sciences & Technology (SCTIMST), Trivandrum"
            lead_email <- w_cfg$contactEmail %||% ""
            htmltools::tagList(
              htmltools::tags$p(style = "margin: 0 0 6px 0; font-size: 0.9rem; color: #475569;",
                htmltools::strong("Conducted by: "), cond_inst
              ),
              if (nzchar(lead_email)) {
                htmltools::tags$p(style = "margin: 0 0 6px 0; font-size: 0.9rem; color: #475569;",
                  htmltools::strong("Lead Contact: "), lead_email
                )
              } else NULL
            )
          },
          htmltools::tags$p(style = "margin: 0 0 6px 0; font-size: 0.9rem; color: #475569;",
            htmltools::strong("Progress saving: "), "Your progress is continuously saved, so you can pause the survey at any time and return to it later."
          ),
          htmltools::tags$p(style = "margin: 0; font-size: 0.9rem; color: #475569;",
            htmltools::strong("Finalization: "), "Once you have answered all the questions, please submit your responses on the final page."
          )
        ),
        shiny::div(
          style = "display: flex; justify-content: flex-end; margin-top: 1.75rem;",
          shiny::actionButton("survey_next", "Continue →", class = "btn-primary", style = "padding: 0.65rem 2rem; font-size: 1rem; font-weight: 700;")
        )
      )
    ),
    welcome_round2 = shiny::tagList(
      shiny::div(
        class = "survey-welcome-card",
        style = "max-width: 700px; margin: 1.5rem auto; text-align: left; background: #fff; border: 1px solid #e2e8f0; border-radius: 12px; padding: 2.25rem 2.5rem; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05);",
        htmltools::div(
          style = "display: inline-flex; align-items: center; gap: 8px; background: #e0e7ff; color: #3730a3; padding: 4px 12px; border-radius: 9999px; font-weight: 700; font-size: 0.82rem; margin-bottom: 1rem;",
          "DELPHI DELIBERATION · ROUND 2"
        ),
        htmltools::tags$h2(style = "font-size: 1.85rem; font-weight: 800; color: #0f172a; margin-top: 0; margin-bottom: 0.75rem;",
          "Welcome to Delphi Round 2"
        ),
        htmltools::tags$p(
          style = "font-size: 1.05rem; line-height: 1.6; color: #334155; margin-bottom: 1.25rem;",
          sprintf("The initial round of expert judgments for %s has been compiled and analyzed using the Sheffield Elicitation Framework (SHELF) Linear Pool method.", st$title)
        ),
        htmltools::tags$div(
          style = "border-top: 1px solid #f1f5f9; padding-top: 1.25rem; margin-top: 1.25rem;",
          htmltools::tags$h4(style = "font-size: 1.05rem; font-weight: 700; color: #0f172a; margin-bottom: 0.75rem;", "Round 2 Deliberation Protocol"),
          htmltools::tags$ul(
            style = "padding-left: 1.25rem; color: #475569; font-size: 0.95rem; line-height: 1.7;",
            htmltools::tags$li(htmltools::tags$strong("Review Anonymized Group Consensus: "), "For each question, you will examine your Round 1 estimate alongside the pooled group consensus distribution and blinded peer rationales."),
            htmltools::tags$li(htmltools::tags$strong("Re-elicitation & Re-evaluation: "), "You will be invited to re-assess each parameter. Your previous Round 1 answers are pre-filled as your baseline."),
            htmltools::tags$li(htmltools::tags$strong("Freedom to Revise or Affirm: "), "You may revise your numbers based on the peer evidence, or keep your original values if you remain confident in your assessment."),
            htmltools::tags$li(htmltools::tags$strong("Continuous Saving: "), "Your responses are continuously saved as you navigate through the questions.")
          )
        ),
        htmltools::tags$div(
          style = "margin-top: 2rem; display: flex; justify-content: flex-end;",
          shiny::actionButton(
            "survey_next", "Begin Round 2 Deliberation →",
            class = "btn-primary",
            style = "background: #4f46e5; color: #ffffff; border: none; padding: 0.85rem 2rem; font-size: 1.05rem; font-weight: 700; border-radius: 8px; box-shadow: 0 4px 6px -1px rgba(79, 70, 229, 0.25);"
          )
        )
      )
    ),
    onboarding = shiny::tagList(
      shiny::div(
        class = "survey-welcome-card",
        style = "max-width: 680px; margin: 1.5rem auto; text-align: left; background: #fff; border: 1px solid #e2e8f0; border-radius: 12px; padding: 2.25rem 2.5rem; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05);",
        htmltools::tags$div(
          style = "font-size: 0.82rem; font-weight: 700; text-transform: uppercase; color: #4f46e5; letter-spacing: 0.05em; margin-bottom: 6px;",
          "BEFORE YOU BEGIN"
        ),
        htmltools::tags$h2(style = "font-size: 1.85rem; font-weight: 800; color: #0f172a; margin-top: 0; margin-bottom: 1rem;", "Participant Consent"),
        htmltools::tags$p(
          style = "font-size: 1.05rem; line-height: 1.6; color: #334155; margin-bottom: 1.5rem;",
          "This is a structured expert elicitation exercise for HTA decision support. Your judgments will be recorded, analyzed, and may be pooled with other experts' responses to inform a health technology assessment."
        ),
        htmltools::div(
          style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 14px 18px; margin-bottom: 1.5rem;",
          shiny::checkboxInput(
            "tou_accept",
            "I understand my responses will be recorded and used for this HTA elicitation exercise, and I agree to participate.",
            value = FALSE,
            width = "100%"
          )
        ),
        notice(rv$err, "error"),
        shiny::div(
          class = "btn-row",
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: 1.5rem;",
          shiny::actionButton("survey_prev", "← Back", class = "btn-secondary"),
          shiny::actionButton("save_onboarding", "I Agree & Continue →", class = "btn-primary", style = "padding: 0.65rem 1.75rem; font-weight: 700;")
        )
      )
    ),
    review = survey_review_body(rv, pg),
    bounds = shiny::tagList(
      htmltools::tags$h2("Facilitator-defined plausible bounds"),
      htmltools::tags$p(
        "The facilitator has set specific plausible parameter limits for each question in this study. All elicited values must remain strictly within their respective ranges:"
      ),
      htmltools::tags$table(
        class = "data",
        htmltools::tags$thead(htmltools::tags$tr(
          htmltools::tags$th("#"),
          htmltools::tags$th("Question / Parameter"),
          htmltools::tags$th("Lower limit (L)"),
          htmltools::tags$th("Upper limit (U)"),
          htmltools::tags$th("Unit")
        )),
        htmltools::tags$tbody(lapply(seq_along(qs), function(i) {
          q_item <- qs[[i]]
          htmltools::tags$tr(
            htmltools::tags$td(htmltools::strong(sprintf("Q%d", i))),
            htmltools::tags$td(q_item$title),
            htmltools::tags$td(htmltools::tags$code(as.character(q_item$lowerBound %||% 0))),
            htmltools::tags$td(htmltools::tags$code(as.character(q_item$upperBound %||% 1))),
            htmltools::tags$td(q_item$unit %||% "probability")
          )
        }))
      ),
      notice(rv$err, "error"),
      shiny::div(
        class = "btn-row",
        style = "margin-top: 16px;",
        shiny::actionButton("survey_prev", "Back"),
        shiny::actionButton("survey_next", "Continue to Questions →", class = "btn-primary")
      )
    ),
    chips = {
      q_obj <- pg$question
      lo_q <- as.numeric(q_obj$lowerBound %||% lo0)
      hi_q <- as.numeric(q_obj$upperBound %||% hi0)
      pid_str <- as.character(rv$survey_user$personId %||% rv$survey_user$id %||% "")
      prev_chips_rat <- ""
      if (nzchar(pid_str)) {
        key <- sprintf("%s_%s", doc_id(q_obj), pg$round_num %||% 1L)
        cj <- if (!is.null(rv$survey_judgments)) rv$survey_judgments[[key]] else NULL
        if (is.null(cj)) {
          cj <- get_expert_judgment(doc_id(st), doc_id(q_obj), pid_str, round_number = pg$round_num)
        }
        if (!is.null(cj)) prev_chips_rat <- cj$rationale %||% cj$payload$rationale %||% ""
      }
      shiny::tagList(
        htmltools::tags$div(
          class = "task-badge",
          style = "display: inline-block; font-size: 0.82rem; font-weight: 700; text-transform: uppercase; color: #4f46e5; margin-bottom: 4px;",
          sprintf("Task %d", pg$q_idx %||% 1)
        ),
        htmltools::tags$h2(style = "margin-top: 0; margin-bottom: 8px;", q_obj$title),
        htmltools::tags$p(
          style = "font-size: 1.05rem; font-weight: 500; color: #1e293b; margin-bottom: 12px;",
          "Place 20 chips on the grid. The number of chips in each bin shows how likely you think the value is to be in that range."
        ),
        if (nzchar(q_obj$prompt %||% "")) {
          htmltools::tags$p(style = "font-size: 0.92rem; color: #475569; margin-bottom: 14px;", q_obj$prompt)
        },
        mod_chips_ui("chips"),
        shiny::div(
          style = "margin-top: 18px;",
          shiny::textAreaInput(
            "chips_rationale",
            "Please provide a brief explanation for your choices and any additional comments.",
            value = prev_chips_rat,
            rows = 3,
            width = "100%"
          )
        ),
        notice(rv$err, "error"),
        notice(rv$msg, "ok"),
        shiny::div(
          class = "btn-row",
          style = "display: flex; justify-content: space-between; margin-top: 18px;",
          shiny::actionButton("survey_prev", "← Back", class = "btn-secondary"),
          shiny::actionButton("save_chips", "Continue →", class = "btn-primary")
        )
      )
    },
    quantile = {
      q_obj <- pg$question
      lo_q <- as.numeric(q_obj$lowerBound %||% lo0)
      hi_q <- as.numeric(q_obj$upperBound %||% hi0)
      unit_q <- q_obj$unit %||% "probability"
      step_q <- if ((hi_q - lo_q) <= 1) 0.01 else if ((hi_q - lo_q) <= 10) 0.1 else 1

      pid_str <- as.character(rv$survey_user$personId %||% rv$survey_user$id %||% "")
      is_r2 <- isTRUE((pg$round_num %||% 1L) >= 2L)
      prev_p10 <- NA
      prev_p50 <- NA
      prev_p90 <- NA
      prev_rat <- ""

      if (nzchar(pid_str)) {
        key_curr <- sprintf("%s_%s", doc_id(q_obj), pg$round_num %||% 1L)
        curr_j <- if (!is.null(rv$survey_judgments)) rv$survey_judgments[[key_curr]] else NULL
        if (is.null(curr_j)) {
          curr_j <- get_expert_judgment(doc_id(st), doc_id(q_obj), pid_str, round_number = pg$round_num)
        }
        prior_j <- if (!is.null(curr_j)) {
          curr_j
        } else if (is_r2) {
          prev_r <- max(1L, (pg$round_num %||% 2L) - 1L)
          key_prev <- sprintf("%s_%s", doc_id(q_obj), prev_r)
          pj <- if (!is.null(rv$survey_judgments)) rv$survey_judgments[[key_prev]] else NULL
          if (is.null(pj)) get_expert_judgment(doc_id(st), doc_id(q_obj), pid_str, round_number = prev_r) else pj
        } else NULL
        if (!is.null(prior_j)) {
          pq <- quantiles_from_payload(prior_j$payload)
          if (!is.null(pq)) {
            ptrip <- quantile_triple(pq)
            prev_p10 <- ptrip[[1]]
            prev_p50 <- ptrip[[2]]
            prev_p90 <- ptrip[[3]]
          }
          prev_rat <- prior_j$rationale %||% prior_j$payload$rationale %||% ""
        }
      }

      val_p10 <- prev_p10
      val_p50 <- prev_p50
      val_p90 <- prev_p90
      val_rat <- prev_rat

      shiny::tagList(
        htmltools::tags$div(
          class = "task-badge",
          style = "display: inline-block; font-size: 0.82rem; font-weight: 700; text-transform: uppercase; color: #4f46e5; margin-bottom: 4px;",
          if (is_r2) sprintf("Delphi Round %d Re-elicitation · Task %d", pg$round_num %||% 2L, pg$q_idx %||% 1) else sprintf("Task %d", pg$q_idx %||% 1)
        ),
        htmltools::tags$h2(style = "margin-top: 0;", pg$title),
        htmltools::tags$p(style = "font-size: 1.05rem; font-weight: 500; color: #1e293b;", q_obj$prompt),
        htmltools::tags$div(
          class = "status-pill status-ready",
          style = "display: inline-block; margin-bottom: 12px;",
          sprintf("Plausible Range: L = %s to U = %s (%s)", lo_q, hi_q, unit_q)
        ),
        if (is_r2 && any(!is.na(c(prev_p10, prev_p50, prev_p90)))) {
          htmltools::div(
            style = "background: #eff6ff; border: 1px solid #bfdbfe; border-radius: 8px; padding: 10px 14px; margin-bottom: 14px; font-size: 0.9rem; color: #1e40af;",
            "ℹ️ Your inputs below are pre-filled with your Round 1 judgments. You may adjust them based on group consensus, or keep them if your assessment is unchanged."
          )
        },
        shiny::radioButtons(
          "q_mode", "Elicitation method",
          choiceNames = c("Percentiles (P10, P50, P90)", "Quartiles (Q1, median, Q3)"),
          choiceValues = c("percentile", "quartile"),
          selected = "percentile",
          inline = TRUE
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput("p10", "P10 / Q1 (Lower)", value = val_p10, min = lo_q, max = hi_q, step = step_q)),
          shiny::column(4, shiny::numericInput("p50", "P50 / Median (Best estimate)", value = val_p50, min = lo_q, max = hi_q, step = step_q)),
          shiny::column(4, shiny::numericInput("p90", "P90 / Q3 (Upper)", value = val_p90, min = lo_q, max = hi_q, step = step_q))
        ),
        shiny::uiOutput("expert_quantile_preview_card"),
        shiny::textAreaInput(
          "q_rationale",
          if (is_r2) sprintf("Clinical / scientific rationale for %s (Round %d)", q_obj$title, pg$round_num %||% 2L)
          else sprintf("Clinical / scientific rationale for %s (required)", q_obj$title),
          value = val_rat,
          rows = 3
        ),
        notice(rv$err, "error"),
        shiny::div(
          class = "btn-row",
          style = "display: flex; justify-content: space-between; margin-top: 18px;",
          shiny::actionButton("survey_prev", "← Back", class = "btn-secondary"),
          shiny::actionButton("save_quantile", "Continue →", class = "btn-primary")
        )
      )
    },
    submit = {
      is_r2 <- isTRUE((pg$round_num %||% 1L) >= 2L)
      shiny::tagList(
        shiny::div(
          class = "submission-card",
          style = "max-width: 620px; margin: 2rem auto; text-align: center; padding: 2.25rem; background: #fff; border: 1px solid #e2e8f0; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05);",
          htmltools::tags$h2(
            style = "font-size: 1.85rem; font-weight: 800; color: #0f172a; margin-top: 0; margin-bottom: 1rem;",
            if (is_r2) "Ready to submit your Round 2 responses?" else "Ready to submit your Round 1 responses?"
          ),
          htmltools::tags$p(
            style = "font-size: 1.05rem; color: #475569; margin-bottom: 1.5rem; line-height: 1.6;",
            if (is_r2) {
              "You have completed all deliberation questions for Round 2. Once submitted, your finalized judgments will be locked."
            } else {
              "After you have completed all questions, please submit your responses to conclude Round 1."
            }
          ),
          htmltools::div(
            style = "display: inline-flex; align-items: center; gap: 8px; background: #fffbeb; border: 1px solid #fde68a; border-radius: 8px; padding: 10px 18px; margin-bottom: 2.25rem; color: #92400e; font-size: 0.95rem; font-weight: 600;",
            "⚠️ Note: You won't be able to change your responses afterwards."
          ),
          htmltools::div(
            style = "margin-bottom: 2.5rem;",
            shiny::actionButton(
              "survey_final_submit",
              if (is_r2) "Submit Round 2 Judgments" else "Submit Round 1 Judgments",
              class = "btn-primary",
              style = "background: #0f172a; color: #ffffff; border: none; padding: 0.85rem 3.5rem; font-size: 1.15rem; font-weight: 700; border-radius: 6px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.1);"
            )
          ),
          notice(rv$err, "error"),
          notice(rv$msg, "ok"),
          shiny::div(
            style = "display: flex; justify-content: space-between; border-top: 1px solid #e2e8f0; padding-top: 1.25rem;",
            shiny::actionButton("survey_prev", "← Back", class = "btn-secondary"),
            shiny::actionButton("survey_next", "Continue →", class = "btn-ghost")
          )
        )
      )
    },
    done = {
      is_r2 <- isTRUE((pg$round_num %||% 1L) >= 2L)
      shiny::tagList(
        shiny::div(
          class = "submission-card",
          style = "max-width: 600px; margin: 3rem auto; text-align: center; padding: 2.5rem; background: #fff; border: 1px solid #e2e8f0; border-radius: 12px; box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05);",
          htmltools::tags$div(
            style = "display: inline-flex; align-items: center; justify-content: center; width: 64px; height: 64px; border-radius: 50%; background: #ecfdf5; color: #10b981; font-size: 2rem; margin-bottom: 1rem;",
            "✓"
          ),
          if (is_r2) {
            shiny::tagList(
              htmltools::tags$h2(style = "color: #0f172a; font-weight: 800; margin-top: 0; font-size: 1.85rem;", "Round 2 Completed"),
              htmltools::tags$p(style = "font-size: 1.05rem; color: #334155; margin: 1.25rem 0; line-height: 1.6;",
                "Thank you! Your Round 2 deliberation judgments have been successfully recorded and locked."
              ),
              htmltools::tags$div(
                style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 14px 18px; margin: 1.5rem 0; text-align: left; font-size: 0.92rem; color: #475569; line-height: 1.6;",
                htmltools::tags$strong(style = "color: #0f172a; display: block; margin-bottom: 4px;", "Deliberation Completed:"),
                "The study facilitator will aggregate all finalized responses using the SHELF Linear Pool method to generate the final HTA evidence synthesis report."
              )
            )
          } else {
            shiny::tagList(
              htmltools::tags$h2(style = "color: #0f172a; font-weight: 800; margin-top: 0; font-size: 1.85rem;", "Round 1 Responses Submitted"),
              htmltools::tags$p(style = "font-size: 1.05rem; color: #334155; margin: 1.25rem 0; line-height: 1.6;",
                "Thank you! Your Round 1 expert judgments have been securely recorded and locked."
              ),
              htmltools::tags$div(
                style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 14px 18px; margin: 1.5rem 0; text-align: left; font-size: 0.92rem; color: #475569; line-height: 1.6;",
                htmltools::tags$strong(style = "color: #0f172a; display: block; margin-bottom: 4px;", "Facilitator Review in Progress:"),
                "The study facilitator is currently reviewing the collective responses and applying the SHELF Linear Pool aggregation. The facilitator will determine whether consensus has been reached or if a Delphi Round 2 deliberation is needed.",
                htmltools::tags$p(style = "margin: 8px 0 0; color: #64748b; font-size: 0.88rem;",
                  "If Round 2 is initiated, you will be notified and can return using your survey link to view the group distribution and refine your assessment."
                )
              )
            )
          },
          if (identical(as.character(st$slug %||% ""), "infection-rate-demo")) {
            shiny::div(
              style = "margin-top: 1.5rem; padding-top: 1.25rem; border-top: 1px dashed #cbd5e1;",
              shiny::actionButton(
                "retake_demo_survey",
                "🔄 Retake / Re-explore Demo Survey",
                class = "btn-secondary btn-sm",
                style = "font-weight: 600; padding: 6px 14px;"
              )
            )
          } else NULL,
          htmltools::tags$p(class = "muted", style = "font-size: 0.9rem;", paste("Signed in as", rv$survey_user$email)),
          htmltools::tags$p(class = "muted", style = "font-size: 0.82rem; margin-top: 2rem;",
            "AMCHSS · SCTIMST Structured Expert Elicitation Platform"
          )
        )
      )
    }
  )
  body
}

survey_review_body <- function(rv, pg) {
  pid <- as.character(rv$survey_user$personId %||% rv$survey_user$id %||% "")
  qid <- doc_id(pg$question %||% list())
  sid <- doc_id(rv$survey_study %||% list())
  prev_r <- max(1L, (pg$round_num %||% 2L) - 1L)
  key_prev <- sprintf("%s_%s", qid, prev_r)
  prior_j <- if (!is.null(rv$survey_judgments)) rv$survey_judgments[[key_prev]] else NULL
  if (is.null(prior_j) && nzchar(pid) && nzchar(qid) && nzchar(sid)) {
    prior_j <- db_one("judgments", sprintf(
      '{"studyId": %s, "questionId": %s, "expertId": %s, "roundNumber": %s, "isCurrent": true}',
      json_escape(sid), json_escape(qid), json_escape(pid), as.integer(prev_r)
    ))
  }

  comparison_card <- if (!is.null(prior_j) && !is.null(rv$review_result$fit$linearPool)) {
    pq <- quantiles_from_payload(prior_j$payload)
    ptrip <- if (!is.null(pq)) quantile_triple(pq) else c(NA, NA, NA)
    gq <- rv$review_result$fit$linearPool$quantiles
    gtrip <- if (!is.null(gq)) quantile_triple(gq) else c(NA, NA, NA)
    gmean <- rv$review_result$fit$linearPool$mean

    shiny::div(
      class = "deliberation-comparison",
      htmltools::tags$h3(sprintf("Your Round %d vs. Group Consensus", prev_r)),
      shiny::div(
        class = "two-col",
        shiny::div(
          class = "panel comparison-box personal-box",
          htmltools::tags$h4("Your Previous Judgments"),
          htmltools::tags$p(sprintf("P10: %.2f  ·  P50: %.2f  ·  P90: %.2f", ptrip[[1]], ptrip[[2]], ptrip[[3]])),
          if (nzchar(prior_j$rationale %||% "")) htmltools::tags$p(class = "muted", paste("Rationale:", prior_j$rationale))
        ),
        shiny::div(
          class = "panel comparison-box group-box",
          htmltools::tags$h4("Group Consensus (True Mean Fit)"),
          htmltools::tags$p(sprintf("P10: %.2f  ·  P50: %.2f  ·  P90: %.2f", gtrip[[1]], gtrip[[2]], gtrip[[3]])),
          if (is.finite(gmean)) htmltools::tags$p(class = "muted", sprintf("Overall True Mean: %.2f", gmean))
        )
      )
    )
  } else NULL

  fit_ui <- if (!is.null(rv$review_result)) {
    shiny::tagList(
      comparison_card,
      htmltools::tags$p(class = "ok", rv$review_result$conclusion),
      plotly::plotlyOutput("review_plot", height = "360px"),
      htmltools::tags$h3("Peer rationales (anonymized)"),
      review_rationale_list(rv$review_result),
      htmltools::tags$h3("Blinded commentary"),
      review_comment_list(rv, pg),
      shiny::textAreaInput("peer_comment", "Your comment (no names)", rows = 3),
      shiny::actionButton("save_comment", "Post comment", class = "btn-secondary")
    )
  } else {
    htmltools::tags$p(class = "muted", "No prior-round overlay is available yet. You can still continue.")
  }
  shiny::tagList(
    htmltools::tags$h2("Blinded peer review"),
    htmltools::tags$p("Individual distributions are labelled Expert A, B, C. Comment on the evidence, not on people."),
    if (!is.null(rv$review_updated_at)) {
      htmltools::tags$p(
        class = "muted live-feedback-status",
        sprintf("Live feedback updates automatically · last distribution update %s",
                format(rv$review_updated_at, "%H:%M:%S"))
      )
    },
    fit_ui,
    notice(rv$err, "error"),
    notice(rv$msg, "ok"),
    shiny::div(
      class = "btn-row",
      shiny::actionButton("survey_prev", "Back"),
      shiny::actionButton("survey_next", sprintf("Continue to Q%d revisions →", pg$q_idx %||% 1), class = "btn-primary")
    )
  )
}

review_rationale_list <- function(shelf_result) {
  items <- lapply(shelf_result$fit$experts %||% list(), function(ex) {
    htmltools::tags$li(
      htmltools::tags$strong(ex$name),
      htmltools::tags$span(ex$rationale %||% "")
    )
  })
  htmltools::tags$ul(items)
}

review_comment_list <- function(rv, pg = NULL) {
  st <- rv$survey_study
  qs <- rv$survey_questions %||% study_questions(doc_id(st))
  q <- (pg$question %||% primary_question(qs))
  if (is.null(q)) return(NULL)
  r_active <- (pg$round_num %||% rv$survey_round %||% 1L)
  comments <- list_peer_comments(doc_id(st), doc_id(q), r_active)
  if (!length(comments)) return(htmltools::tags$p(class = "muted", "No comments yet."))
  js <- current_judgments(doc_id(st), doc_id(q), max(1L, r_active - 1L))
  mapping <- blind_labels_for_ids(c(
    vapply(js, function(j) as.character(j$expertId %||% ""), character(1)),
    vapply(comments, function(c) as.character(c$authorPersonId %||% ""), character(1))
  ))
  htmltools::tags$ul(lapply(comments, function(cmt) {
    htmltools::tags$li(
      htmltools::tags$strong(blind_label(cmt$authorPersonId, mapping)),
      htmltools::tags$span(cmt$body)
    )
  }))
}
