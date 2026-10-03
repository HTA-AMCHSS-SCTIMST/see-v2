staff_shell <- function(user, body) {
  extra <- shiny::tagList(
    shiny::actionButton("goto_dash", "Surveys", class = "btn-ghost"),
    if (can_provision_people(user)) shiny::actionButton("goto_people", "People", class = "btn-ghost"),
    shiny::actionButton("logout", "Sign out", class = "btn-secondary")
  )
  shiny::div(class = "app-shell", ee_header(user, extra), shiny::div(class = "page-body", body))
}

staff_dash_ui <- function(rv) {
  user <- rv$user
  if (is_expert_role(user)) {
    return(staff_shell(user, shiny::div(
      class = "panel",
      htmltools::tags$h2("Expert access"),
      htmltools::tags$p("Open the personal survey link from your facilitator. Staff tools are not used for elicitation.")
    )))
  }
  studies <- list_studies_for_user(user)
  total_surveys <- length(studies)
  total_experts <- db_count("people", '{"isActive": true}')
  total_judgments <- db_count("judgments", '{"isCurrent": true}')

  kpi_banner <- shiny::div(
    class = "kpi-banner",
    shiny::div(
      class = "kpi-card",
      htmltools::div(class = "kpi-icon", "📊"),
      htmltools::div(
        class = "kpi-content",
        htmltools::span(class = "kpi-label", "Active Case Studies"),
        htmltools::strong(class = "kpi-value", as.character(total_surveys))
      )
    ),
    shiny::div(
      class = "kpi-card",
      htmltools::div(class = "kpi-icon", "👥"),
      htmltools::div(
        class = "kpi-content",
        htmltools::span(class = "kpi-label", "Engaged Experts"),
        htmltools::strong(class = "kpi-value", as.character(total_experts))
      )
    ),
    shiny::div(
      class = "kpi-card",
      htmltools::div(class = "kpi-icon", "📈"),
      htmltools::div(
        class = "kpi-content",
        htmltools::span(class = "kpi-label", "Judgments Elicited"),
        htmltools::strong(class = "kpi-value", as.character(total_judgments))
      )
    )
  )

  cards <- if (!length(studies)) {
    htmltools::p(class = "muted", if (is_viewer(user)) {
      "No completed consensus models are assigned to you yet."
    } else {
      "No surveys yet. Seed the HTA demo or create one."
    })
  } else {
    lapply(studies, function(s) {
      nq <- db_count("questions", sprintf('{"studyId": %s, "isActive": true}', json_escape(doc_id(s))))
      r_num <- current_round(s)
      rc <- study_response_count(doc_id(s), r_num)
      progress_badge <- if (rc$total > 0 && rc$submitted >= rc$total) {
        status_pill("ready")
      } else if (rc$submitted > 0) {
        status_pill("active")
      } else {
        status_pill("recruiting")
      }
      pct <- if (rc$total > 0) min(100, round((rc$submitted / rc$total) * 100)) else 0

      shiny::div(
        class = "study-card",
        htmltools::div(
          class = "study-card-top",
          htmltools::tags$h3(s$title),
          htmltools::div(
            class = "study-pill-row",
            status_pill(s$status %||% "draft"),
            progress_badge
          )
        ),
        htmltools::p(class = "study-card-desc", s$description %||% "Structured expert elicitation for decision modeling."),
        htmltools::div(
          class = "study-progress-wrapper",
          htmltools::div(
            class = "study-progress-meta",
            htmltools::span(class = "study-progress-label", sprintf("Round %d Progress", r_num)),
            htmltools::span(class = "study-progress-stat", sprintf("%d of %d experts (%d%%)", rc$submitted, rc$total, pct))
          ),
          htmltools::div(
            class = "study-progress-track",
            htmltools::div(class = "study-progress-fill", style = sprintf("width: %d%%;", pct))
          )
        ),
        htmltools::div(
          class = "study-card-footer",
          htmltools::span(class = "study-slug-tag", sprintf("%d question%s · %s", nq, if (nq == 1) "" else "s", s$slug)),
          htmltools::tags$button(
            type = "button",
            class = "btn-primary btn-sm",
            onclick = sprintf(
              "Shiny.setInputValue('open_study', '%s', {priority: 'event'})",
              doc_id(s)
            ),
            if (is_expert_role(user)) "Take Survey →" else "Open Study →"
          )
        )
      )
    })
  }
  create_panel <- if (can_create_study(user)) {
    shiny::div(
      class = "panel create-panel",
      htmltools::tags$h3("Create survey"),
      shiny::textInput("new_title", "Title", placeholder = "HTA: Drug A vs Drug B"),
      shiny::textInput("new_qty", "Quantity of interest", placeholder = "5-year progression-free probability"),
      shiny::textAreaInput("new_desc", "Why this elicitation?", rows = 3),
      shiny::selectInput(
        "new_variable_type", "Variable type",
        choices = c("Proportion / probability" = "proportion", "Continuous" = "continuous", "Count" = "count"),
        selected = "proportion"
      ),
      shiny::textInput("new_unit", "Unit", value = "probability"),
      shiny::fluidRow(
        shiny::column(4, shiny::numericInput("new_lower", "Lower bound", value = 0, step = 0.01)),
        shiny::column(4, shiny::numericInput("new_upper", "Upper bound", value = 1, step = 0.01)),
        shiny::column(4, shiny::numericInput("new_precision", "Decimals", value = 2, min = 0, max = 6))
      ),
      shiny::checkboxGroupInput(
        "new_methods", "Elicitation tasks",
        choiceNames = c("Chips-N-Bins", "Low-High-Best (P10/P50/P90)"),
        choiceValues = c("chips_and_bins", "quantile"),
        selected = c("chips_and_bins", "quantile")
      ),
      shiny::div(
        class = "consent-box",
        style = "background: #f8fafc; border: 1px solid #cbd5e1; border-radius: 8px; padding: 14px; margin-top: 14px; margin-bottom: 16px;",
        htmltools::tags$h4(style = "margin: 0 0 4px 0; color: #0f172a; font-size: 0.95rem; font-weight: 600;", "Facilitator Consent: Data Integrity & Platform Access"),
        htmltools::tags$div(style = "font-size: 0.8rem; font-weight: 600; color: #475569; margin-bottom: 8px; text-transform: uppercase; letter-spacing: 0.03em;", "AMCHSS · SCTIMST Expert Elicitation Study"),
        htmltools::tags$p(style = "font-size: 0.85rem; color: #334155; margin-bottom: 8px;", "Before accessing the study platform, facilitators must acknowledge these core standards:"),
        htmltools::tags$ul(
          style = "font-size: 0.83rem; color: #334155; padding-left: 18px; margin-bottom: 12px; line-height: 1.5;",
          htmltools::tags$li(htmltools::tags$strong("Neutrality: "), "No coaching, steering, or influencing expert judgments."),
          htmltools::tags$li(htmltools::tags$strong("Confidentiality: "), "Strict protection of de-identified expert data across Delphi rounds."),
          htmltools::tags$li(htmltools::tags$strong("Methodology: "), "Adherence to the SHELF Linear Pool aggregation without selective trimming."),
          htmltools::tags$li(htmltools::tags$strong("Compliance & Auditing: "), "All actions and data access are immutably logged with timestamped IDs.")
        ),
        shiny::checkboxInput(
          "consent_data_integrity",
          "I agree. By checking this box, I consent to these terms and authorize AMCHSS-SCTIMST to log and audit my platform activity for research governance.",
          value = FALSE
        )
      ),
      shiny::actionButton("create_study", "Create Study", class = "btn-primary btn-block")
    )
  } else if (is_admin(user)) {
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Platform Administration"),
      htmltools::tags$p("As Platform Admin, you have full governance across all case studies: view status and deliberations, download audit packages and reports, and assign facilitators to any study.")
    )
  } else {
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Read only"),
      htmltools::tags$p("Completed consensus models and audit trails assigned to you.")
    )
  }
  staff_shell(user, shiny::tagList(
    htmltools::div(
      class = "page-head",
      htmltools::tags$h2("Surveys Dashboard"),
      if (can_seed_demo(user)) shiny::actionButton("seed_demo", "Seed HTA demo", class = "btn-secondary")
    ),
    kpi_banner,
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(class = "two-col",
      shiny::div(class = "panel", htmltools::tags$h3("Case Studies"), cards),
      create_panel
    )
  ))
}

staff_study_ui <- function(rv) {
  st <- rv$study_cache %||% find_study(rv$study_id)
  user <- rv$user
  if (is.null(st)) return(staff_shell(user, htmltools::p("Study not found.")))
  experts <- study_experts(st)
  study_slug_or_id <- utils::URLencode(as.character(st$slug %||% doc_id(st)), reserved = TRUE)
  study_qs <- sprintf("?study=%s", study_slug_or_id)
  url <- sprintf("%s/%s", sub("/+$", "", ee_survey_public_url()), study_qs)
  rows <- if (!length(experts)) {
    htmltools::p(class = "muted", "No experts invited yet.")
  } else {
    htmltools::tags$table(
      class = "data",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(sprintf("EXPERTS (%d)", length(experts))),
        htmltools::tags$th("EMAIL"),
        htmltools::tags$th("SURVEY LINK"),
        htmltools::tags$th("STATUS"),
        htmltools::tags$th("PROGRESS"),
        htmltools::tags$th("REMINDERS"),
        if (can_manage_study(user, st)) htmltools::tags$th("ACTIONS")
      )),
      htmltools::tags$tbody(lapply(experts, function(ex) {
        pct <- if (ex$totalQuestions > 0) min(100, round((ex$answeredCount / ex$totalQuestions) * 100)) else 0
        s_url <- ex$surveyUrl %||% ""
        s_qs  <- ex$queryString %||% s_url
        htmltools::tags$tr(
          htmltools::tags$td(htmltools::strong(ex$name)),
          htmltools::tags$td(htmltools::span(class = "muted", ex$email)),
          htmltools::tags$td(
            if (can_invite(user, st) && nzchar(s_qs)) {
              htmltools::div(
                style = "display: flex; align-items: center; gap: 6px;",
                htmltools::tags$button(
                  type = "button",
                  class = "btn-secondary btn-sm",
                  style = "padding: 3px 8px; font-size: 0.78rem; display: inline-flex; align-items: center; gap: 3px;",
                  onclick = sprintf("var base = (window.location.origin + window.location.pathname).replace(/\\/+$/, ''); eeCopyLink(base + '/%s', this);", s_qs),
                  "Copy URL ❐"
                ),
                htmltools::tags$a(
                  href = s_qs,
                  target = "_blank",
                  class = "btn-secondary btn-sm",
                  style = "padding: 3px 8px; font-size: 0.78rem; text-decoration: none;",
                  onclick = sprintf("var base = (window.location.origin + window.location.pathname).replace(/\\/+$/, ''); this.href = base + '/%s';", s_qs),
                  "Open ↗"
                )
              )
            } else {
              htmltools::span(class = "muted", "—")
            }
          ),
          htmltools::tags$td(status_pill(ex$status)),
          htmltools::tags$td(
            htmltools::div(
              style = "min-width: 110px;",
              htmltools::div(
                style = "display: flex; justify-content: space-between; font-size: 0.82rem; font-weight: 600; margin-bottom: 3px;",
                htmltools::span(sprintf("%d / %d", ex$answeredCount, ex$totalQuestions)),
                htmltools::span(class = "muted", sprintf("%d%%", pct))
              ),
              htmltools::div(
                style = "height: 6px; background: #e2e8f0; border-radius: 3px; overflow: hidden;",
                htmltools::div(style = sprintf("width: %d%%; height: 100%%; background: %s; transition: width 0.3s;", pct, if (pct >= 100) "#10b981" else "#6366f1"))
              )
            )
          ),
          htmltools::tags$td(htmltools::span(class = "muted", "0 / 3")),
          if (can_manage_study(user, st)) htmltools::tags$td(
            shiny::actionButton(
              paste0("remove_expert_", ex$personId),
              "Remove",
              class = "btn-secondary btn-sm",
              onclick = sprintf(
                "Shiny.setInputValue('remove_expert', '%s', {priority: 'event'})",
                ex$personId
              )
            )
          )
        )
      }))
    )
  }
  shelf_btn <- if (can_view_shelf(user, st)) {
    shiny::actionButton("goto_responses", "Responses >", class = "btn-primary", style = "background: #4f46e5; border-color: #4f46e5; font-weight: 600;")
  } else {
    NULL
  }
  download_btns <- if (can_export_audit(user, st)) {
    shiny::div(
      class = "btn-row",
      style = "display: inline-flex; gap: 8px; align-items: center;",
      shiny::actionButton("btn_open_export_modal", "📥 Export & Audit Center...", class = "btn-primary btn-sm", style = "background: #0d6efd; border-color: #0d6efd; color: white; font-weight: 600; padding: 6px 14px; border-radius: 6px;")
    )
  } else {
    NULL
  }

  facs <- study_facilitators(st)
  avail_facs <- list_available_facilitators()
  assigned_pids <- vapply(facs, function(f) as.character(f$personId %||% ""), character(1))
  unassigned <- Filter(function(p) !as.character(doc_id(p)) %in% assigned_pids, avail_facs)

  fac_choices <- if (length(unassigned) > 0) {
    c("— Select facilitator to add —" = "", stats::setNames(
      vapply(unassigned, doc_id, character(1)),
      vapply(unassigned, function(p) sprintf("%s (%s)", p$name %||% "Facilitator", p$email %||% ""), character(1))
    ))
  } else {
    c("All registered facilitators assigned" = "")
  }

  fac_table <- if (!length(facs)) {
    htmltools::p(class = "muted", "No facilitators assigned.")
  } else {
    htmltools::tags$table(
      class = "data",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Name"), htmltools::tags$th("Email"),
        htmltools::tags$th("Role"),
        if (can_assign_facilitator(user, st)) htmltools::tags$th("Actions")
      )),
      htmltools::tags$tbody(lapply(facs, function(fc) {
        htmltools::tags$tr(
          htmltools::tags$td(fc$name),
          htmltools::tags$td(fc$email),
          htmltools::tags$td(
            if (isTRUE(fc$isOwner)) {
              htmltools::span(class = "status-pill status-ready", "Primary Owner")
            } else {
              htmltools::span(class = "status-pill status-active", "Co-Facilitator")
            }
          ),
          if (can_assign_facilitator(user, st)) htmltools::tags$td(
            if (!isTRUE(fc$isOwner)) {
              shiny::actionButton(
                paste0("remove_fac_", fc$personId),
                "Remove",
                class = "btn-secondary btn-sm",
                onclick = sprintf(
                  "Shiny.setInputValue('remove_facilitator', '%s', {priority: 'event'})",
                  fc$personId
                )
              )
            } else {
              htmltools::span(class = "muted", "Owner")
            }
          )
        )
      }))
    )
  }

  assign_fac_ui <- if (can_assign_facilitator(user, st) && length(unassigned) > 0) {
    shiny::div(
      style = "margin-top: 14px; padding-top: 12px; border-top: 1px solid #e2e8f0; display: flex; gap: 10px; align-items: flex-end; flex-wrap: wrap;",
      shiny::div(style = "flex: 1; min-width: 220px;", shiny::selectInput("assign_facilitator_id", "Add facilitator to study", choices = fac_choices)),
      shiny::actionButton("add_facilitator_go", "Assign Facilitator", class = "btn-primary btn-sm", style = "margin-bottom: 15px;")
    )
  } else {
    NULL
  }

  facilitator_panel <- shiny::div(
    class = "panel",
    htmltools::tags$h3("Assigned Facilitators"),
    htmltools::tags$p(class = "muted", "Facilitators authorized to guide elicitation rounds, run SHELF, and engage experts."),
    fac_table,
    assign_fac_ui
  )

  qs <- rv$questions_cache %||% study_questions(doc_id(st))
  can_manage <- can_manage_study(user, st)

  q_rows <- if (!length(qs)) {
    htmltools::p(class = "muted", "No questions added to this case study yet.")
  } else {
    htmltools::tags$table(
      class = "data",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("#"),
        htmltools::tags$th("Question / Parameter"),
        htmltools::tags$th("Task Method"),
        htmltools::tags$th("Plausible Bounds [L, U]"),
        htmltools::tags$th("Unit"),
        if (can_manage) htmltools::tags$th("Actions")
      )),
      htmltools::tags$tbody(lapply(seq_along(qs), function(i) {
        q <- qs[[i]]
        qid <- doc_id(q)
        m_label <- if (identical(q$elicitationMethod, "chips_and_bins")) "Chips-N-Bins" else "Low-High-Best (P10/P50/P90)"
        bounds_txt <- sprintf("[%s, %s]", q$lowerBound %||% 0, q$upperBound %||% 1)
        htmltools::tags$tr(
          htmltools::tags$td(htmltools::strong(sprintf("Q%d", i))),
          htmltools::tags$td(
            htmltools::strong(q$title),
            if (nzchar(q$prompt %||% "") && !identical(q$prompt, q$title)) {
              htmltools::div(class = "muted", style = "font-size: 0.82rem; margin-top: 2px;", q$prompt)
            } else NULL
          ),
          htmltools::tags$td(
            htmltools::span(class = "status-pill status-ready", m_label)
          ),
          htmltools::tags$td(htmltools::tags$code(bounds_txt)),
          htmltools::tags$td(q$unit %||% "probability"),
          if (can_manage) htmltools::tags$td(
            if (length(qs) > 1) {
              shiny::actionButton(
                paste0("del_q_", qid),
                "Remove",
                class = "btn-secondary btn-sm",
                onclick = sprintf("Shiny.setInputValue('remove_question', '%s', {priority: 'event'})", qid)
              )
            } else {
              htmltools::span(class = "muted", style = "font-size: 0.8rem;", "Primary (Required)")
            }
          )
        )
      }))
    )
  }

  add_q_ui <- if (can_manage) {
    shiny::div(
      style = "margin-top: 14px; padding-top: 12px; border-top: 1px solid #e2e8f0; display: flex; justify-content: flex-end;",
      shiny::actionButton("open_add_question_modal", "+ Add Question to Series", class = "btn-primary btn-sm")
    )
  } else {
    NULL
  }

  questions_panel <- shiny::div(
    class = "panel",
    htmltools::div(
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
      htmltools::tags$h3(style = "margin: 0;", sprintf("Elicitation Questions Series (%d)", length(qs))),
      if (can_manage) shiny::actionButton("open_add_question_modal_top", "+ Add Question", class = "btn-secondary btn-sm") else NULL
    ),
    htmltools::tags$p(class = "muted", "Sequential parameters elicited from experts within this single case study."),
    q_rows,
    add_q_ui
  )

  invite_panel <- if (can_invite(user, st)) {
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Invite expert"),
      shiny::textInput("invite_email", "Email", width = "100%"),
      shiny::textInput("invite_name", "Name (optional)", width = "100%"),
      shiny::actionButton("invite_go", "Add expert", class = "btn-primary", style = "width: 100%;"),
      htmltools::hr(),
      if (can_advance_round(user, st)) shiny::actionButton(
        "advance_round",
        if (current_round(st) == 1L) "Initiate Delphi Round 2 →" else sprintf("Advance to Round %d →", current_round(st) + 1L),
        class = "btn-secondary btn-block"
      ),
      if (can_complete_study(user, st)) shiny::actionButton("complete_study", "Mark elicitation complete", class = "btn-secondary btn-block")
    )
  } else {
    shiny::div(class = "panel", htmltools::tags$h3("Access"), htmltools::tags$p(class = "muted", "Read-only."))
  }
  staff_shell(user, shiny::tagList(
    shiny::actionButton("goto_dash", "← Back to surveys", class = "btn-ghost"),
    htmltools::div(
      class = "page-head",
      htmltools::div(
        style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
        htmltools::tags$h2(style = "margin: 0;", st$title),
        status_pill(st$status),
        htmltools::span(class = "pill", sprintf("Round %s", current_round(st))),
        htmltools::tags$a(
          href = study_qs,
          target = "_blank",
          class = "btn-secondary btn-sm",
          style = "text-decoration: none; display: inline-flex; align-items: center; gap: 4px; font-weight: 600; padding: 4px 10px; border-radius: 6px; font-size: 0.82rem; background: #fff;",
          onclick = sprintf("var base = (window.location.origin + window.location.pathname).replace(/\\/+$/, ''); this.href = base + '/%s';", study_qs),
          "View Survey ↗"
        )
      ),
      shiny::tagList(
        download_btns,
        shelf_btn,
        if (can_manage_study(user, st)) shiny::actionButton("archive_study", "Archive survey", class = "btn-secondary")
      )
    ),
    if (can_manage_study(user, st)) shiny::div(
      class = "panel",
      htmltools::tags$h3("Edit survey"),
      shiny::textInput("edit_title", "Title", value = st$title %||% ""),
      shiny::textAreaInput("edit_description", "Description", value = st$description %||% "", rows = 3),
      shiny::actionButton("save_study_details", "Save survey details", class = "btn-primary")
    ),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(
      class = "panel",
      htmltools::tags$h3("Expert survey link"),
      htmltools::tags$p(class = "muted", "Share the personal invite link (token) from the table, or the study URL plus invited email."),
      shiny::div(
        style = "display: flex; gap: 8px; align-items: center; flex-wrap: wrap;",
        shiny::div(class = "url-box", id = "general-study-url-box", style = "flex: 1; min-width: 260px; margin: 0;", url),
        htmltools::tags$button(
          type = "button",
          class = "btn-secondary btn-sm",
          onclick = sprintf("var base = (window.location.origin + window.location.pathname).replace(/\\/+$/, ''); eeCopyLink(base + '/%s', this);", study_qs),
          "Copy Link"
        ),
        htmltools::tags$a(
          href = study_qs,
          target = "_blank",
          class = "btn-primary btn-sm",
          style = "text-decoration: none;",
          onclick = sprintf("var base = (window.location.origin + window.location.pathname).replace(/\\/+$/, ''); this.href = base + '/%s';", study_qs),
          "Open Survey ↗"
        )
      ),
      htmltools::tags$script(htmltools::HTML(sprintf(
        "(function(){ var el = document.getElementById('general-study-url-box'); if(el) { var base = (window.location.origin + window.location.pathname).replace(/\\/+$/, ''); el.innerText = base + '/%s'; } })();",
        study_qs
      )))
    ),
    facilitator_panel,
    questions_panel,
    shiny::div(
      class = "two-col",
      shiny::div(class = "panel", htmltools::tags$h3("Experts"), rows),
      invite_panel
    )
  ))
}

resp_nav_steps_ui <- function(rv) {
  st <- rv$study_cache %||% find_study(rv$study_id)
  if (is.null(st)) return(NULL)
  qs <- rv$questions_cache %||% study_questions(doc_id(st))
  total_q <- length(qs)
  curr_idx <- as.integer(rv$resp_q_idx %||% 1L)
  if (!is.finite(curr_idx) || curr_idx < 1L) curr_idx <- 1L
  if (curr_idx > total_q) curr_idx <- total_q

  nav_items <- list(
    htmltools::tags$li(
      class = "nav-step-item nav-step-meta",
      htmltools::tags$span(class = "step-num", "1"),
      htmltools::tags$span(class = "step-title", "Welcome / Study Overview")
    )
  )

  for (i in seq_along(qs)) {
    q_item <- qs[[i]]
    is_active <- (i == curr_idx)
    m_badge <- if (identical(q_item$elicitationMethod, "chips_and_bins")) "Chips-N-Bins" else "Low-High-Best"

    nav_items[[length(nav_items) + 1]] <- htmltools::tags$li(
      class = paste0("nav-step-item", if (is_active) " active" else ""),
      onclick = sprintf("Shiny.setInputValue('select_resp_q_idx', %d, {priority: 'event'})", i),
      htmltools::tags$span(class = "step-num", as.character(i + 1)),
      htmltools::div(
        class = "step-content",
        htmltools::div(class = "step-label", sprintf("%s: %s", m_badge, q_item$title)),
        htmltools::div(class = "step-sub", sprintf("[%s, %s] %s", q_item$lowerBound %||% 0, q_item$upperBound %||% 1, q_item$unit %||% ""))
      )
    )
  }

  nav_items[[length(nav_items) + 1]] <- htmltools::tags$li(
    class = "nav-step-item nav-step-meta",
    htmltools::tags$span(class = "step-num", as.character(total_q + 2)),
    htmltools::tags$span(class = "step-title", "Feedback & Submission")
  )

  htmltools::tags$ul(class = "sidebar-step-list", nav_items)
}

resp_header_nav_ui <- function(rv) {
  st <- rv$study_cache %||% find_study(rv$study_id)
  if (is.null(st)) return(NULL)
  qs <- rv$questions_cache %||% study_questions(doc_id(st))
  total_q <- length(qs)
  curr_idx <- as.integer(rv$resp_q_idx %||% 1L)
  if (!is.finite(curr_idx) || curr_idx < 1L) curr_idx <- 1L
  if (curr_idx > total_q) curr_idx <- total_q
  active_r <- as.integer(rv$resp_round %||% current_round(st))

  htmltools::div(
    style = "display: flex; align-items: center; gap: 12px;",
    if (current_round(st) > 1L) {
      htmltools::div(
        style = "display: inline-flex; align-items: center; gap: 4px; background: #f1f5f9; padding: 2px 4px; border-radius: 6px;",
        lapply(seq_len(current_round(st)), function(r) {
          is_sel <- (r == active_r)
          htmltools::tags$button(
            type = "button",
            class = paste0("btn-tab-toggle", if (is_sel) " active" else ""),
            style = sprintf("padding: 3px 10px; font-size: 0.8rem; font-weight: 600; cursor: pointer; border-radius: 4px; border: none; %s",
                            if (is_sel) "background: #4f46e5; color: #fff;" else "background: transparent; color: #64748b;"),
            onclick = sprintf("Shiny.setInputValue('select_resp_round', %d, {priority: 'event'})", r),
            sprintf("Round %d", r)
          )
        })
      )
    } else NULL,
    htmltools::div(
      class = "nav-step-counter",
      shiny::actionButton(
        "resp_q_prev", "<",
        class = paste0("btn-step-arrow", if (curr_idx <= 1) " disabled" else "")
      ),
      htmltools::span(class = "step-counter-text", sprintf("Question %d of %d", curr_idx, total_q)),
      shiny::actionButton(
        "resp_q_next", ">",
        class = paste0("btn-step-arrow", if (curr_idx >= total_q) " disabled" else "")
      )
    )
  )
}

resp_question_banner_ui <- function(rv) {
  st <- rv$study_cache %||% find_study(rv$study_id)
  if (is.null(st)) return(NULL)
  qs <- rv$questions_cache %||% study_questions(doc_id(st))
  total_q <- length(qs)
  curr_idx <- as.integer(rv$resp_q_idx %||% 1L)
  if (!is.finite(curr_idx) || curr_idx < 1L || curr_idx > total_q) return(NULL)
  curr_q <- qs[[curr_idx]]

  htmltools::div(
    class = "curr-question-banner",
    htmltools::div(
      style = "display: flex; justify-content: space-between; align-items: flex-start; gap: 12px;",
      htmltools::div(
        htmltools::tags$h3(style = "margin: 0 0 4px; font-size: 1.15rem; color: #0f172a;", curr_q$title),
        if (nzchar(curr_q$prompt %||% "") && !identical(curr_q$prompt, curr_q$title)) {
          htmltools::tags$p(style = "margin: 0; font-size: 0.88rem; color: #475569;", curr_q$prompt)
        } else NULL
      ),
      htmltools::div(
        class = "q-meta-badges",
        htmltools::span(class = "status-pill status-ready", if (identical(curr_q$elicitationMethod, "chips_and_bins")) "Chips-N-Bins" else "Low-High-Best"),
        htmltools::span(class = "status-pill status-active", sprintf("Range: [%s, %s] %s", curr_q$lowerBound %||% 0, curr_q$upperBound %||% 1, curr_q$unit %||% ""))
      )
    )
  )
}

resp_expert_weights_ui_content <- function(rv, input = NULL) {
  st <- rv$study_cache %||% find_study(rv$study_id)
  if (is.null(st)) return(NULL)
  user <- rv$user
  qs <- rv$questions_cache %||% study_questions(doc_id(st))
  total_q <- length(qs)
  curr_idx <- as.integer(rv$resp_q_idx %||% 1L)
  if (!is.finite(curr_idx) || curr_idx < 1L) curr_idx <- 1L
  if (curr_idx > total_q) curr_idx <- total_q
  curr_q <- if (total_q > 0) qs[[curr_idx]] else NULL
  curr_qid <- if (!is.null(curr_q)) doc_id(curr_q) else ""

  study_exs <- rv$experts_cache %||% study_experts(st)
  all_study_js <- rv$resp_judgments_cache %||% list()
  active_r <- as.integer(rv$resp_round %||% current_round(st))

  js <- Filter(function(j) {
    identical(as.character(j$questionId %||% ""), curr_qid) &&
      identical(as.integer(j$roundNumber %||% 1L), as.integer(active_r))
  }, all_study_js)
  if (!length(js) && active_r > 1L && is.null(rv$resp_round)) {
    js_r1 <- Filter(function(j) {
      identical(as.character(j$questionId %||% ""), curr_qid) &&
        identical(as.integer(j$roundNumber %||% 1L), 1L)
    }, all_study_js)
    if (length(js_r1) > 0) js <- js_r1
  }
  answered_expert_ids <- vapply(js, function(j) as.character(j$expertId %||% ""), character(1))

  expert_palette <- c("#0284c7", "#10b981", "#8b5cf6", "#f59e0b", "#ec4899", "#14b8a6", "#f97316", "#6366f1")

  if (!length(study_exs)) {
    return(htmltools::p(class = "muted", style = "font-size: 0.85rem; padding: 0.5rem;", "No experts in study yet."))
  }

  rows <- lapply(seq_along(study_exs), function(idx) {
    ex <- study_exs[[idx]]
    pid <- as.character(ex$personId)
    has_submitted <- pid %in% answered_expert_ids
    color_swatch <- expert_palette[((idx - 1) %% length(expert_palette)) + 1]
    input_id <- paste0("weight_expert_", pid)
    curr_val <- if (!is.null(input)) shiny::isolate(input[[input_id]]) %||% 1 else 1

    htmltools::div(
      class = "expert-weight-item",
      htmltools::div(
        class = "expert-meta-left",
        htmltools::span(
          class = "curve-dot",
          style = sprintf("background-color: %s;", if (has_submitted) color_swatch else "#cbd5e1;")
        ),
        htmltools::div(
          class = "expert-name-box",
          htmltools::span(class = "expert-name-text", ex$name %||% ex$email),
          if (!has_submitted) htmltools::span(class = "badge-no-data", "No Data")
        )
      ),
      htmltools::div(
        class = "expert-weight-right",
        if (can_run_shelf(user, st)) {
          shiny::numericInput(
            input_id,
            label = NULL,
            value = curr_val,
            min = 0,
            max = 100,
            step = 0.5,
            width = "65px"
          )
        } else {
          htmltools::span(class = "weight-static", as.character(curr_val))
        }
      )
    )
  })

  shiny::div(class = "expert-weights-list", rows)
}

resp_advance_round_btn_ui_content <- function(rv) {
  st <- rv$study_cache %||% find_study(rv$study_id)
  if (is.null(st) || !can_advance_round(rv$user, st)) return(NULL)
  cur_r <- current_round(st)
  shiny::actionButton(
    "advance_round",
    if (cur_r == 1L) "Initiate Delphi Round 2 →" else sprintf("Advance to Round %d →", cur_r + 1L),
    class = "btn-secondary btn-block"
  )
}

staff_responses_ui <- function(rv, input = NULL) {
  st <- rv$study_cache %||% find_study(rv$study_id)
  user <- rv$user
  if (is.null(st)) return(staff_shell(user, htmltools::p("Study not found.")))
  if (!can_view_shelf(user, st)) {
    return(staff_shell(user, shiny::tagList(
      shiny::actionButton("goto_dash", "← Surveys", class = "btn-ghost"),
      htmltools::tags$p("You cannot open SHELF results for this study while it is in active deliberation.")
    )))
  }

  preferred_distribution <- (st$protocolConfig %||% list())$preferredDistribution %||% "best"
  current_family <- rv$resp_family %||% preferred_distribution

  # Left Control Column: Fitted Distribution, Pooled Toggle, Expert & Weights List
  control_column <- shiny::div(
    class = "shelf-control-col",
    shiny::div(
      class = "control-section",
      htmltools::tags$label(class = "control-label", "POOLED"),
      htmltools::div(
        class = "pooled-toggle-row",
        htmltools::span(class = "curve-dot", style = "background-color: #2563eb;"),
        htmltools::span(style = "font-weight: 600; font-size: 0.88rem; flex: 1; color: #1e293b;", "Linear Pool"),
        shiny::checkboxInput("show_linear_pool", label = NULL, value = TRUE)
      )
    ),
    shiny::div(
      class = "control-section",
      htmltools::tags$label(class = "control-label", "FITTED DISTRIBUTION"),
      shiny::selectInput(
        "fit_family",
        label = NULL,
        choices = c(
          "Best fitting (SHELF)" = "best",
          "Beta" = "beta",
          "Normal" = "normal",
          "Student-t" = "student_t",
          "Gamma" = "gamma",
          "Log normal" = "log_normal",
          "Log Student-t" = "log_t"
        ),
        selected = current_family,
        width = "100%"
      )
    ),
    shiny::div(
      class = "control-section",
      htmltools::div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
        htmltools::tags$label(class = "control-label", style = "margin: 0;", "EXPERT & WEIGHT"),
        if (can_run_shelf(user, st)) {
          shiny::actionLink("recalc_weights", "Recalculate", class = "action-link-sm")
        } else NULL
      ),
      shiny::uiOutput("resp_expert_weights_ui")
    ),
    if (can_run_shelf(user, st)) {
      shiny::div(
        style = "margin-top: 1rem;",
        shiny::actionButton("run_shelf", "Update Fit & Consensus", class = "btn-primary btn-block")
      )
    } else NULL,
    if (can_advance_round(user, st)) {
      shiny::div(
        style = "margin-top: 0.75rem;",
        shiny::uiOutput("resp_advance_round_btn_ui")
      )
    } else NULL,
    if (can_complete_study(user, st)) {
      shiny::div(
        style = "margin-top: 0.5rem;",
        shiny::actionButton("complete_study", "Mark elicitation complete", class = "btn-secondary btn-block")
      )
    } else NULL
  )

  # Chart Header Controls: Fitted / Both mode, Layout switcher, Refresh, and Export dropdown
  chart_toolbar <- htmltools::div(
    class = "chart-toolbar",
    htmltools::div(
      class = "chart-toolbar-left",
      htmltools::div(
        class = "chart-view-mode-buttons",
        shiny::actionButton("btn_layout_split", "⚡ Auto-Fit Grid", class = "btn-tab-toggle active"),
        shiny::actionButton("btn_layout_chart", "📈 Chart Only", class = "btn-tab-toggle"),
        shiny::actionButton("btn_layout_table", "📋 Tables Only", class = "btn-tab-toggle")
      ),
      htmltools::div(
        class = "chart-view-mode-buttons",
        shiny::actionButton("btn_mode_fitted", "Fitted", class = "btn-tab-toggle"),
        shiny::actionButton("btn_mode_both", "Both", class = "btn-tab-toggle active")
      )
    ),
    htmltools::div(
      class = "chart-toolbar-right",
      shiny::actionButton("refresh_responses", "↺ Refresh", class = "btn-secondary btn-sm"),
      if (can_export_audit(user, st)) {
        shiny::div(
          class = "dropdown-export",
          style = "display: flex; gap: 8px; align-items: center;",
          shiny::actionButton("btn_open_export_modal_resp", "📥 Export & Audit Center...", class = "btn-primary btn-sm", style = "background: #0d6efd; border-color: #0d6efd; color: white; font-weight: 600; padding: 6px 14px; border-radius: 6px;"),
          shiny::uiOutput("resp_quick_download_ui", inline = TRUE)
        )
      } else NULL
    )
  )

  # Auto-Fit Result Cards (Plotly Chart + Parameters + Blinded Comments)
  plot_card <- shiny::div(
    class = "shelf-card shelf-card-plot",
    htmltools::div(
      class = "shelf-card-header",
      htmltools::tags$h4(style = "margin: 0; font-size: 0.92rem; font-weight: 600; color: #1e293b;", "Consensus Distribution (SHELF)")
    ),
    plotly::plotlyOutput("shelf_plot", height = "380px")
  )

  tables_card <- shiny::div(
    class = "shelf-card shelf-card-tables",
    shiny::tabsetPanel(
      id = "shelf_details_tabs",
      type = "pills",
      shiny::tabPanel(
        "Parameters & Weights",
        shiny::div(class = "table-responsive table-scroll-box", shiny::tableOutput("shelf_params"))
      ),
      shiny::tabPanel(
        "Raw Expert Data",
        shiny::div(class = "table-responsive table-scroll-box", shiny::tableOutput("raw_response_table"))
      ),
      shiny::tabPanel(
        "Rationales",
        shiny::div(class = "comments-scroll-box", shiny::uiOutput("shelf_comments_panel"))
      )
    )
  )

  result_ui <- shiny::tagList(
    chart_toolbar,
    shiny::uiOutput("shelf_conclusion_ui"),
    shiny::div(
      class = "shelf-results-grid",
      plot_card,
      tables_card
    )
  )

  # Main Content Workspace
  main_workspace <- shiny::div(
    class = "responses-main-col",
    # Top breadcrumbs and step pagination
    htmltools::div(
      class = "responses-header-nav",
      htmltools::div(
        class = "nav-breadcrumb",
        shiny::actionLink("goto_dash", "Surveys", class = "crumb-link"),
        htmltools::span(" / "),
        shiny::actionLink("goto_study", st$title %||% "Study", class = "crumb-link"),
        htmltools::span(" / "),
        htmltools::strong(style = "color: #0f172a;", "Responses & Consensus")
      ),
      shiny::uiOutput("resp_header_nav")
    ),
    shiny::uiOutput("resp_question_banner"),
    shiny::uiOutput("resp_notice"),
    # Dual-column chart grid: Side controls + Main Chart
    shiny::div(
      class = "shelf-dashboard-grid",
      control_column,
      shiny::div(class = "shelf-chart-col", result_ui)
    )
  )

  staff_shell(user, shiny::tagList(
    shiny::div(
      class = "modern-responses-layout",
      # Left Sidebar
      shiny::div(
        class = "responses-sidebar-nav",
        shiny::actionLink("goto_study", "← Back to Experts", class = "crumb-link",
                          style = "display: inline-flex; align-items: center; gap: 4px; margin-bottom: 12px; font-weight: 600; color: #4f46e5; text-decoration: none; font-size: 0.9rem;"),
        htmltools::div(class = "sidebar-header", "DELIBERATION QUESTIONS"),
        shiny::uiOutput("resp_nav_steps")
      ),
      # Main Area
      main_workspace
    )
  ))
}

input_safe_qid <- function(rv) {
  rv$shelf_result$questionId
}

staff_people_ui <- function(rv) {
  people <- list_people()
  tbl <- htmltools::tags$table(
    class = "data",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Name"), htmltools::tags$th("Email"),
      htmltools::tags$th("Type"), htmltools::tags$th("Affiliation")
    )),
    htmltools::tags$tbody(lapply(people, function(p) {
      htmltools::tags$tr(
        htmltools::tags$td(p$name),
        htmltools::tags$td(p$email),
        htmltools::tags$td(p$personType),
        htmltools::tags$td(p$affiliation %||% "")
      )
    }))
  )
  staff_shell(rv$user, shiny::tagList(
    htmltools::tags$h2("People directory"),
    notice(rv$msg, "ok"),
    notice(rv$err, "error"),
    shiny::div(
      class = "two-col",
      shiny::div(class = "panel", tbl),
      shiny::div(
        class = "panel",
        htmltools::tags$h3("Provision person"),
        shiny::textInput("person_name", "Name", width = "100%"),
        shiny::textInput("person_email", "Email", width = "100%"),
        shiny::textInput("person_affil", "Affiliation", width = "100%"),
        shiny::selectInput(
          "person_type", "Type",
          choices = c("Admin" = "admin", "Facilitator" = "facilitator", "Expert" = "expert"),
          width = "100%"
        ),
        shiny::actionButton("person_add", "Save", class = "btn-primary", style = "width: 100%;"),
        htmltools::tags$p(class = "muted", "Only administrators manage the organization-wide directory. Facilitators manage experts within their own surveys.")
      )
    )
  ))
}

export_center_modal <- function(study, questions, user) {
  sid <- doc_id(study)
  q_choices <- if (length(questions) > 0) {
    stats::setNames(
      vapply(questions, doc_id, character(1)),
      vapply(questions, function(q) sprintf("%s: %s", q$code %||% "Q", q$title %||% "Untitled"), character(1))
    )
  } else {
    c("No questions in this study" = "")
  }

  shiny::modalDialog(
    title = htmltools::div(
      style = "display: flex; align-items: center; justify-content: space-between; border-bottom: 1px solid #e2e8f0; padding-bottom: 12px;",
      htmltools::div(
        htmltools::tags$h3(style = "margin: 0; font-size: 1.15rem; font-weight: 700; color: #0f172a; display: flex; align-items: center; gap: 8px;",
          htmltools::span("📥"),
          htmltools::span("Study Outputs & Audit Dossier")
        ),
        htmltools::tags$p(style = "margin: 4px 0 0; font-size: 0.85rem; color: #64748b; font-weight: 400;",
          sprintf("%s · Round %d · %d Questions Asked", study$title %||% "Study", current_round(study), length(questions))
        )
      )
    ),
    size = "l",
    easyClose = TRUE,
    fade = TRUE,
    footer = shiny::tagList(
      shiny::modalButton("Close")
    ),
    shiny::div(
      class = "export-modal-body",
      style = "display: flex; flex-direction: column; gap: 1.25rem; max-height: 75vh; overflow-y: auto; padding-right: 4px;",

      # Box 1: Consolidated Study Dossier (All Questions)
      shiny::div(
        style = "background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%); border: 1px solid #cbd5e1; border-radius: 8px; padding: 1.25rem;",
        htmltools::tags$h4(style = "margin: 0 0 0.4rem; font-size: 0.98rem; font-weight: 700; color: #1e293b; display: flex; align-items: center; gap: 6px;",
          htmltools::span("📦"),
          htmltools::span("Consolidated Study Dossier (All Questions)")
        ),
        htmltools::tags$p(style = "margin: 0 0 1rem; font-size: 0.86rem; color: #475569; line-height: 1.45;",
          "Download a complete regulatory audit report or raw data bundle combining all questions, expert judgments, SHELF consensus distributions, and clinical rationales."
        ),
        shiny::div(
          style = "display: flex; gap: 10px; flex-wrap: wrap; align-items: center;",
          shiny::downloadButton("dl_study_pdf", "📄 Full Study Report (PDF)", class = "btn-primary btn-sm", style = "background: #2563eb; font-weight: 600;"),
          shiny::downloadButton("dl_study_csv", "📊 All Parameters (CSV)", class = "btn-secondary btn-sm"),
          shiny::downloadButton("dl_bundle", "📦 Complete Bundle (ZIP)", class = "btn-secondary btn-sm")
        )
      ),

      # Box 2: Individual Questions Asked by Facilitator
      shiny::div(
        style = "border: 1px solid #e2e8f0; border-radius: 8px; padding: 1.25rem; background: #ffffff;",
        htmltools::tags$h4(style = "margin: 0 0 0.5rem; font-size: 0.98rem; font-weight: 700; color: #1e293b; display: flex; align-items: center; gap: 6px;",
          htmltools::span("📋"),
          htmltools::span("Individual Question Outputs & Facilitator Prompts")
        ),
        htmltools::tags$p(style = "margin: 0 0 1rem; font-size: 0.86rem; color: #64748b;",
          "Select any specific clinical question to inspect the exact prompt, biological limits, response counts, and download question-specific audit files."
        ),
        if (length(questions) > 0) {
          shiny::tagList(
            shiny::selectInput(
              "modal_target_question_id",
              label = htmltools::tags$span(style = "font-size: 0.88rem; font-weight: 600; color: #334155;", "Select Question to Inspect & Download:"),
              choices = q_choices,
              width = "100%"
            ),
            shiny::uiOutput("modal_question_details_card")
          )
        } else {
          htmltools::tags$p(class = "muted", "No questions registered in this study yet.")
        }
      )
    )
  )
}
