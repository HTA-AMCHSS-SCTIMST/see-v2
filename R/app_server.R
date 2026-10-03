app_server <- function(input, output, session) {
  ping <- db_ping()
  if (isTRUE(ping$ok)) {
    ensure_indexes()
    ee_log("info", ping$message, where = "db")
  } else {
    ee_log("error", ping$message, where = "db")
  }
  ee_log(
    "info",
    sprintf("session started token=%s app_role=%s", session$token %||% "?", ee_app_role()),
    where = "session"
  )
  rv <- shiny::reactiveValues(
    user = NULL,
    page = "login",
    study_id = NULL,
    survey_study = NULL,
    survey_user = NULL,
    survey_page = 1L,
    msg = "",
    err = if (!isTRUE(ping$ok)) {
      paste0(
        "Database connection error: ", ping$message,
        ". Please verify SUPABASE_DB_URL in .Renviron and ensure your Supabase database is active."
      )
    } else "",
    seed_info = NULL,
    shelf_result = NULL,
    review_result = NULL,
    review_refresh = 0L,
    review_updated_at = NULL,
    bound_lo = NULL,
    bound_hi = NULL,
    token_tried = FALSE,
    last_activity = Sys.time(),
    oidc_tried = FALSE,
    resp_family = NULL,
    survey_questions = NULL,
    survey_round = 1L,
    survey_judgments = list(),
    survey_onboarded = FALSE,
    study_cache = NULL,
    questions_cache = NULL,
    experts_cache = NULL,
    resp_round = NULL,
    resp_judgments_cache = NULL,
    resp_judgments_study_id = NULL,
    resp_aggregations_cache = NULL,
    resp_aggregations_study_id = NULL,
    resp_comments_cache = NULL,
    resp_comments_study_id = NULL
  )

  get_cached_study <- function(study_id = rv$study_id) {
    if (is.null(study_id) || !nzchar(as.character(study_id))) return(NULL)
    if (!is.null(rv$study_cache) && identical(as.character(doc_id(rv$study_cache)), as.character(study_id))) {
      return(rv$study_cache)
    }
    st <- find_study(study_id)
    rv$study_cache <- st
    st
  }

  get_cached_questions <- function(study_id = rv$study_id) {
    if (is.null(study_id) || !nzchar(as.character(study_id))) return(list())
    if (!is.null(rv$questions_cache) && !is.null(rv$study_cache) && identical(as.character(doc_id(rv$study_cache)), as.character(study_id))) {
      return(rv$questions_cache)
    }
    qs <- study_questions(study_id)
    rv$questions_cache <- qs
    qs
  }

  get_cached_study_judgments <- function(study_id = rv$study_id) {
    if (is.null(study_id) || !nzchar(as.character(study_id))) return(list())
    if (!is.null(rv$resp_judgments_cache) && identical(as.character(rv$resp_judgments_study_id), as.character(study_id))) {
      return(rv$resp_judgments_cache)
    }
    js <- tryCatch(
      db_all("judgments", sprintf('{"studyId": %s, "isCurrent": true, "isConsensus": false}', json_escape(study_id))),
      error = function(e) list()
    )
    rv$resp_judgments_cache <- js
    rv$resp_judgments_study_id <- study_id
    js
  }

  get_cached_study_aggregations <- function(study_id = rv$study_id) {
    if (is.null(study_id) || !nzchar(as.character(study_id))) return(list())
    if (!is.null(rv$resp_aggregations_cache) && identical(as.character(rv$resp_aggregations_study_id), as.character(study_id))) {
      return(rv$resp_aggregations_cache)
    }
    aggs <- tryCatch(
      db_all("aggregations", sprintf('{"studyId": %s}', json_escape(study_id))),
      error = function(e) list()
    )
    rv$resp_aggregations_cache <- aggs
    rv$resp_aggregations_study_id <- study_id
    aggs
  }

  get_cached_study_comments <- function(study_id = rv$study_id) {
    if (is.null(study_id) || !nzchar(as.character(study_id))) return(list())
    if (!is.null(rv$resp_comments_cache) && identical(as.character(rv$resp_comments_study_id), as.character(study_id))) {
      return(rv$resp_comments_cache)
    }
    cmts <- tryCatch(
      db_all("peer_comments", sprintf('{"studyId": %s}', json_escape(study_id))),
      error = function(e) list()
    )
    rv$resp_comments_cache <- cmts
    rv$resp_comments_study_id <- study_id
    cmts
  }

  session_timeout <- ee_session_timeout_minutes() * 60
  touch_activity <- function() rv$last_activity <- Sys.time()

  session$onSessionEnded(function() {
    u <- shiny::isolate(rv$user)
    su <- shiny::isolate(rv$survey_user)
    ee_log(
      "info",
      sprintf("session ended user=%s", u$email %||% su$email %||% "anonymous"),
      where = "audit.session"
    )
  })

  shiny::observeEvent(input$ee_activity, {
    touch_activity()
  }, ignoreInit = TRUE)

  shiny::observe({
    shiny::invalidateLater(60000, session)
    if (difftime(Sys.time(), rv$last_activity, units = "secs") > session_timeout) {
      if (!is.null(rv$user) || !is.null(rv$survey_user)) {
        ee_log(
          "info",
          sprintf("session timeout user=%s after=%s minutes",
                  rv$user$email %||% rv$survey_user$email %||% "anonymous",
                  ee_session_timeout_minutes()),
          where = "audit.timeout"
        )
      }
      rv$user <- NULL
      rv$survey_user <- NULL
      rv$survey_study <- NULL
      rv$page <- "login"
      rv$err <- "Your session expired due to inactivity. Please sign in again."
      rv$msg <- ""
      rv$last_activity <- Sys.time()
    }
  })

  query_params <- shiny::reactive({
    res <- tryCatch(shiny::getQueryString(session), error = function(e) list())
    if (is.list(res) && length(res) > 0) return(res)
    qs <- session$clientData$url_search
    if (is.null(qs) || !nzchar(qs)) return(list())
    tryCatch(shiny::parseQueryString(qs), error = function(e) list())
  })

  query_study <- shiny::reactive({
    q <- query_params()
    trimws(as.character(q$study %||% q$id %||% ""))
  })

  query_token <- shiny::reactive({
    q <- query_params()
    trimws(as.character(q$t %||% q$token %||% ""))
  })

  is_survey_mode <- shiny::reactive({
    role <- ee_app_role()
    if (identical(role, "survey")) return(TRUE)
    if (identical(role, "staff")) return(FALSE)
    nzchar(query_study()) || nzchar(query_token())
  })

  survey_pid <- function() {
    u <- rv$survey_user
    u$personId %||% u$id
  }

  shiny::observe({
    if (!ee_auth_dev_mode()) {
      cu <- ee_connect_user(session)
      if (!is.null(cu) && is.null(rv$user) && !is_survey_mode()) {
      tryCatch({
        u <- connect_login(cu)
        rv$user <- user_as_list(u)
        rv$page <- "dash"
        touch_activity()
        ee_log("info", sprintf("Posit Connect login %s as %s", u$email, u$platformRole),
               where = "audit.login")
      }, error = function(e) {
        ee_log_error(e, where = "audit.login")
        rv$err <- conditionMessage(e)
      })
    }
    }
  })

  shiny::observe({
    if (isTRUE(rv$oidc_tried) || !ee_oidc_enabled() || is_survey_mode()) return()
    q <- query_params()
    if (!nzchar(q$code %||% "") && !nzchar(q$error %||% "")) return()
    rv$oidc_tried <- TRUE
    if (nzchar(q$error %||% "")) {
      rv$err <- paste("Sign-in was not completed:", q$error_description %||% q$error)
      return()
    }
    if (!ee_oidc_verify_state(q$state %||% "")) {
      rv$err <- "The sign-in session expired or was invalid. Please try again."
      return()
    }
    tryCatch({
      rv$user <- user_as_list(oidc_login(q$code))
      rv$page <- "dash"
      touch_activity()
      ee_log("info", sprintf("OIDC login %s", rv$user$email), where = "audit.login")
    }, error = function(e) ee_handle(rv, e, "oidc_login"))
  })

  shiny::observeEvent(input$oidc_login, {
    shiny::req(ee_oidc_enabled())
    tryCatch({
      session$sendCustomMessage("ee_oidc_redirect", ee_oidc_authorize_url(ee_oidc_state()))
    }, error = function(e) ee_handle(rv, e, "oidc_redirect"))
  })

  enter_survey <- function(res) {
    u <- user_as_list(res$user)
    if (is.null(u$personId) || !nzchar(as.character(u$personId))) {
      p <- find_person_by_email(u$email)
      u$personId <- doc_id(p)
    }
    rv$survey_user <- u
    rv$survey_study <- res$study
    pid <- as.character(u$personId %||% u$id %||% "")

    r_num <- expert_study_round(res$study, pid)
    qs <- study_questions(doc_id(res$study))
    rv$survey_questions <- qs
    rv$survey_round <- r_num
    pages <- survey_pages(res$study, qs, r_num)

    # Batch preload expert judgments into session cache (0 DB reads on question navigation)
    existing_js <- tryCatch(get_expert_judgments_for_study(doc_id(res$study), pid), error = function(e) list())
    j_map <- list()
    for (j in existing_js) {
      k <- sprintf("%s_%s", j$questionId, j$roundNumber %||% 1L)
      j_map[[k]] <- j
    }
    rv$survey_judgments <- j_map

    # Check onboarding state once
    onb <- tryCatch(load_onboarding(doc_id(res$study), pid), error = function(e) NULL)
    rv$survey_onboarded <- onboarding_complete(onb)

    if (expert_is_submitted(res$study, pid, r_num)) {
      done_idx <- which(vapply(pages, function(p) identical(p$kind, "done"), logical(1)))
      rv$survey_page <- if (length(done_idx)) done_idx[[1]] else length(pages)
    } else {
      rv$survey_page <- 1L
    }

    q <- primary_question(qs)
    if (!is.null(q)) {
      rv$bound_lo <- as.numeric(q$lowerBound %||% 0)
      rv$bound_hi <- as.numeric(q$upperBound %||% 1)
    }
    rv$review_result <- if (r_num >= 2L) {
      if (is.null(q)) {
        NULL
      } else {
        prev <- max(1L, r_num - 1L)
        aggregation_as_shelf_result(latest_aggregation(doc_id(res$study), doc_id(q), prev)) %||%
          tryCatch(run_shelf_for_question(res$study, q, prev, anonymize = TRUE), error = function(e) NULL)
      }
    } else {
      NULL
    }
    rv$review_updated_at <- if (!is.null(rv$review_result)) Sys.time() else NULL
    rv$review_refresh <- rv$review_refresh + 1L
  }

  shiny::observe({
    if (isTRUE(rv$token_tried) || !is.null(rv$survey_user) || !is_survey_mode()) return()
    tok <- trimws(query_token())
    if (!nzchar(tok)) return()
    rv$token_tried <- TRUE
    tryCatch(enter_survey(survey_entry_token(tok)), error = function(e) ee_handle(rv, e, "survey_token"))
  })

  shiny::observe({
    shiny::invalidateLater(30000, session)
    if (is.null(rv$survey_user) || !is_survey_mode() || is.null(rv$survey_study)) return()
    r_num <- rv$survey_round %||% 1L
    if (r_num < 2L) return()
    st <- rv$survey_study
    if (is.null(st)) return()
    qs <- rv$survey_questions %||% study_questions(doc_id(st))
    pages <- survey_pages(st, qs, r_num)
    idx <- max(1L, min(as.integer(rv$survey_page %||% 1L), length(pages)))
    pg <- pages[[idx]]
    if (!identical(pg$kind, "review") || is.null(pg$question)) return()
    q <- pg$question
    prev <- max(1L, r_num - 1L)
    latest <- aggregation_as_shelf_result(latest_aggregation(doc_id(st), doc_id(q), prev))
    if (is.null(latest)) return()
    old_id <- rv$review_result$aggregationId %||% ""
    new_id <- latest$aggregationId %||% ""
    if (!identical(as.character(old_id), as.character(new_id))) {
      rv$review_result <- latest
      rv$review_updated_at <- Sys.time()
    }
  })

  shiny::observeEvent(input$login_role, {
    role <- input$login_role
    shiny::updateTextInput(session, "login_email", value = paste0(gsub("_", "", role), "@sctimst.ac.in"))
    shiny::updateTextInput(session, "login_name", value = role_label(role))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$login_go, {
    rv$err <- ""
    tryCatch({
      u <- dev_login(input$login_email, input$login_name, input$login_role)
      rv$user <- user_as_list(u)
      rv$page <- "dash"
      touch_activity()
      ee_log("info", sprintf("dev login %s as %s", u$email, u$platformRole), where = "audit.login")
    }, error = function(e) ee_handle(rv, e, "login"))
  })

  shiny::observeEvent(input$logout, {
    ee_log("info", sprintf("staff logout user=%s", rv$user$email %||% "anonymous"), where = "audit.logout")
    rv$user <- NULL
    rv$page <- "login"
    rv$study_id <- NULL
    rv$msg <- ""
  })

  shiny::observeEvent(input$goto_dash, {
    rv$page <- "dash"
    rv$study_id <- NULL
    rv$shelf_result <- NULL
  })

  shiny::observeEvent(input$goto_people, {
    tryCatch({
      require_role(can_provision_people(rv$user))
      rv$page <- "people"
    }, error = function(e) ee_handle(rv, e, "goto_people"))
  })

  shiny::observeEvent(input$person_add, {
    shiny::req(rv$user)
    rv$err <- ""
    tryCatch({
      p <- add_person(input$person_name, input$person_email, input$person_type, input$person_affil %||% "", rv$user)
      rv$msg <- sprintf("Saved %s as %s.", p$email, p$personType)
    }, error = function(e) ee_handle(rv, e, "person_add"))
  })

  shiny::observeEvent(input$seed_demo, {
    shiny::req(rv$user)
    rv$err <- ""
    tryCatch({
      require_role(can_seed_demo(rv$user), "Only facilitators or administrators can seed the demo study.")
      rv$seed_info <- seed_demo(rv$user)
      rv$msg <- sprintf(
        "Demo ready with dummy SHELF judgments (%s). 1-Click Expert Survey: %s  ·  Experts: %s",
        rv$seed_info$dummyJudgments %||% 0L,
        rv$seed_info$surveyUrl,
        paste(rv$seed_info$expertEmails, collapse = ", ")
      )
      ee_log("info", "seeded HTA demo", where = "seed_demo")
    }, error = function(e) ee_handle(rv, e, "seed_demo"))
  })

  shiny::observeEvent(input$create_study, {
    shiny::req(rv$user)
    title <- trimws(input$new_title %||% "")
    if (!nzchar(title)) {
      rv$err <- "Please enter a study title."
      return()
    }
    rv$err <- ""
    tryCatch({
      require_role(can_create_study(rv$user), "Only facilitators can create case studies.")
      variable_type <- input$new_variable_type %||% "proportion"
      lower <- as.numeric(input$new_lower)
      upper <- as.numeric(input$new_upper)
      precision <- as.integer(input$new_precision)
      if (!is.finite(lower) || !is.finite(upper) || !(lower < upper)) {
        stop("Lower plausible bound must be less than the upper plausible bound.")
      }
      if (!is.finite(precision) || precision < 0 || precision > 6) {
        stop("Decimal places must be between 0 and 6.")
      }
      if (!nzchar(trimws(input$new_unit %||% ""))) stop("Enter a unit for the quantity.")

      shiny::showModal(shiny::modalDialog(
        title = "Facilitator Consent: Data Integrity & Platform Access",
        size = "m",
        easyClose = FALSE,
        htmltools::tags$div(
          style = "font-size: 0.85rem; font-weight: 600; color: #475569; margin-bottom: 12px; text-transform: uppercase; letter-spacing: 0.03em;",
          "AMCHSS · SCTIMST Expert Elicitation Study"
        ),
        htmltools::tags$p(
          style = "font-size: 0.9rem; color: #1e293b; margin-bottom: 12px; font-weight: 500;",
          "Before accessing the study platform, facilitators must acknowledge these core standards:"
        ),
        htmltools::tags$ul(
          style = "font-size: 0.88rem; color: #334155; padding-left: 20px; margin-bottom: 16px; line-height: 1.6;",
          htmltools::tags$li(htmltools::tags$strong("Neutrality: "), "No coaching, steering, or influencing expert judgments."),
          htmltools::tags$li(htmltools::tags$strong("Confidentiality: "), "Strict protection of de-identified expert data across Delphi rounds."),
          htmltools::tags$li(htmltools::tags$strong("Methodology: "), "Adherence to the SHELF Linear Pool aggregation without selective trimming."),
          htmltools::tags$li(htmltools::tags$strong("Compliance & Auditing: "), "All actions and data access are immutably logged with timestamped IDs.")
        ),
        shiny::checkboxInput(
          "modal_consent_data_integrity",
          "I agree. By checking this box, I consent to these terms and authorize AMCHSS-SCTIMST to log and audit my platform activity for research governance.",
          value = isTRUE(input$consent_data_integrity)
        ),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_create_study", "I Agree & Create Study", class = "btn-primary")
        )
      ))
    }, error = function(e) ee_handle(rv, e, "create_study_check"))
  })

  shiny::observeEvent(input$confirm_create_study, {
    shiny::req(rv$user, nzchar(trimws(input$new_title %||% "")))
    if (!isTRUE(input$modal_consent_data_integrity)) {
      shiny::showNotification("You must check the agreement box to consent before creating the study.", type = "error")
      return()
    }
    shiny::removeModal()
    rv$err <- ""
    methods <- input$new_methods
    if (is.null(methods) || !length(methods)) methods <- c("chips_and_bins", "quantile")
    tryCatch({
      require_role(can_create_study(rv$user), "Only facilitators can create case studies.")
      variable_type <- input$new_variable_type %||% "proportion"
      lower <- as.numeric(input$new_lower)
      upper <- as.numeric(input$new_upper)
      precision <- as.integer(input$new_precision)
      if (!identical(variable_type, "proportion")) {
        methods <- setdiff(methods, "chips_and_bins")
        if (!length(methods)) methods <- "quantile"
      }
      st <- create_study(
        rv$user,
        title = trimws(input$new_title),
        description = input$new_desc %||% "",
        quantity = input$new_qty %||% "",
        methods = methods,
        variable_type = variable_type,
        unit = trimws(input$new_unit),
        lower = lower,
        upper = upper,
        precision = precision,
        preferred_distribution = "best",
        consent = list(
          consented = TRUE,
          consentedAt = iso_now(),
          consentedBy = rv$user$id,
          consentedEmail = rv$user$email,
          institution = "AMCHSS · SCTIMST",
          version = "1.0",
          terms = "Neutrality, Confidentiality, Methodology, Compliance & Auditing"
        )
      )
      rv$study_id <- doc_id(st)
      rv$page <- "study"
      rv$msg <- "Survey created with AMCHSS · SCTIMST Data Integrity Consent recorded."
    }, error = function(e) ee_handle(rv, e, "confirm_create_study"))
  })

  shiny::observeEvent(input$open_study, {
    sid <- input$open_study
    rv$study_id <- sid
    if (is_expert_role(rv$user)) {
      tryCatch({
        enter_survey(survey_entry(sid, rv$user$email))
      }, error = function(e) ee_handle(rv, e, "open_study_survey"))
    } else {
      rv$page <- "study"
      rv$msg <- ""
      rv$shelf_result <- NULL
      st <- get_cached_study(sid)
      rv$questions_cache <- if (!is.null(st)) study_questions(doc_id(st)) else list()
      rv$experts_cache <- if (!is.null(st)) study_experts(st) else list()
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$goto_responses, {
    st <- get_cached_study(rv$study_id)
    if (!can_view_shelf(rv$user, st)) {
      rv$err <- "You cannot open SHELF fitting while this study is in active deliberation."
      return()
    }
    rv$page <- "responses"
    rv$err <- ""
    rv$shelf_result <- NULL
    rv$resp_family <- NULL
    if (is.null(rv$questions_cache) && !is.null(st)) rv$questions_cache <- study_questions(doc_id(st))
    if (is.null(rv$experts_cache) && !is.null(st)) rv$experts_cache <- study_experts(st)
  })

  shiny::observeEvent(input$invite_go, {
    shiny::req(rv$study_id, nzchar(trimws(input$invite_email %||% "")))
    st <- get_cached_study(rv$study_id)
    tryCatch({
      require_study_manager(rv$user, st, "invite experts to this study")
      p <- invite_expert(st, input$invite_email, input$invite_name, rv$user)
      tok <- issue_invite_token(st, p)
      rv$experts_cache <- study_experts(st)
      rv$msg <- sprintf(
        "Added %s (%s). Personal link: %s",
        p$name %||% "", p$email, expert_survey_url(st, p, tok)
      )
      shiny::updateTextInput(session, "invite_email", value = "")
      shiny::updateTextInput(session, "invite_name", value = "")
    }, error = function(e) ee_handle(rv, e, "invite_expert"))
  })

  shiny::observeEvent(input$remove_expert, {
    shiny::req(rv$study_id, nzchar(input$remove_expert %||% ""))
    tryCatch({
      st <- get_cached_study(rv$study_id)
      remove_study_expert(st, input$remove_expert, rv$user)
      rv$experts_cache <- study_experts(st)
      rv$msg <- "Expert removed from this survey."
      ee_log("info", sprintf("expert %s removed from study %s", input$remove_expert, rv$study_id),
             where = "audit.study_access")
    }, error = function(e) ee_handle(rv, e, "remove_expert"))
  })

  shiny::observeEvent(input$add_facilitator_go, {
    shiny::req(rv$study_id)
    person_id <- trimws(input$assign_facilitator_id %||% "")
    if (!nzchar(person_id)) {
      rv$err <- "Please select a facilitator to assign."
      return()
    }
    st <- get_cached_study(rv$study_id)
    tryCatch({
      assign_study_facilitator(st, person_id, rv$user)
      rv$msg <- "Facilitator assigned to study."
      rv$err <- ""
      ee_log("info", sprintf("assigned facilitator %s to study %s", person_id, rv$study_id),
             where = "audit.study_facilitator")
    }, error = function(e) ee_handle(rv, e, "assign_facilitator"))
  })

  shiny::observeEvent(input$remove_facilitator, {
    shiny::req(rv$study_id, nzchar(input$remove_facilitator %||% ""))
    st <- get_cached_study(rv$study_id)
    tryCatch({
      remove_study_facilitator(st, input$remove_facilitator, rv$user)
      rv$msg <- "Facilitator removed from study."
      rv$err <- ""
      ee_log("info", sprintf("facilitator %s removed from study %s", input$remove_facilitator, rv$study_id),
             where = "audit.study_facilitator")
    }, error = function(e) ee_handle(rv, e, "remove_facilitator"))
  })

  show_add_question_modal <- function() {
    shiny::showModal(shiny::modalDialog(
      title = "Add Question to Study Series",
      size = "m",
      easyClose = TRUE,
      shiny::textInput("modal_q_title", "Question / Parameter Title", placeholder = "e.g. 5-Year Overall Survival"),
      shiny::textAreaInput("modal_q_prompt", "Clinical Prompt / Context", rows = 2,
                           placeholder = "e.g. Provide P10, P50, and P90 estimates for 5-year survival probability..."),
      shiny::selectInput("modal_q_var_type", "Variable Type",
                         choices = c("Proportion / probability" = "proportion", "Continuous" = "continuous", "Count" = "count"),
                         selected = "proportion"),
      shiny::selectInput("modal_q_method", "Elicitation Task",
                         choices = c("Low-High-Best (P10/P50/P90)" = "quantile", "Chips-N-Bins" = "chips_and_bins"),
                         selected = "quantile"),
      shiny::textInput("modal_q_unit", "Unit", value = "probability"),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput("modal_q_lower", "Lower Bound (L)", value = 0, step = 0.01)),
        shiny::column(6, shiny::numericInput("modal_q_upper", "Upper Bound (U)", value = 1, step = 0.01))
      ),
      shiny::numericInput("modal_q_precision", "Decimal places", value = 2, min = 0, max = 6),
      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::actionButton("submit_new_question", "Add Question to Series", class = "btn-primary")
      )
    ))
  }

  shiny::observeEvent(input$open_add_question_modal, {
    show_add_question_modal()
  })

  shiny::observeEvent(input$open_add_question_modal_top, {
    show_add_question_modal()
  })

  shiny::observeEvent(input$submit_new_question, {
    shiny::req(rv$study_id)
    st <- get_cached_study(rv$study_id)
    tryCatch({
      add_study_question(
        study = st,
        title = input$modal_q_title,
        prompt = input$modal_q_prompt,
        variable_type = input$modal_q_var_type %||% "proportion",
        method = input$modal_q_method %||% "quantile",
        unit = input$modal_q_unit,
        lower = input$modal_q_lower,
        upper = input$modal_q_upper,
        precision = input$modal_q_precision,
        user = rv$user
      )
      rv$questions_cache <- study_questions(rv$study_id)
      shiny::removeModal()
      rv$msg <- "New question added to case study series."
      rv$err <- ""
      ee_log("info", sprintf("added question '%s' to study %s", input$modal_q_title, rv$study_id),
             where = "audit.study_question")
    }, error = function(e) ee_handle(rv, e, "submit_new_question"))
  })

  shiny::observeEvent(input$remove_question, {
    shiny::req(rv$study_id, nzchar(input$remove_question %||% ""))
    st <- get_cached_study(rv$study_id)
    tryCatch({
      remove_study_question(st, input$remove_question, rv$user)
      rv$questions_cache <- study_questions(rv$study_id)
      rv$msg <- "Question removed from case study."
      rv$err <- ""
      ee_log("info", sprintf("removed question %s from study %s", input$remove_question, rv$study_id),
             where = "audit.study_question")
    }, error = function(e) ee_handle(rv, e, "remove_question"))
  })

  shiny::observeEvent(input$save_study_details, {
    shiny::req(rv$study_id)
    tryCatch({
      st <- update_study_details(get_cached_study(rv$study_id), input$edit_title,
                                 input$edit_description, rv$user)
      rv$study_cache <- st
      rv$msg <- sprintf("Survey '%s' updated.", st$title)
      ee_log("info", sprintf("study %s updated", rv$study_id), where = "audit.study_edit")
    }, error = function(e) ee_handle(rv, e, "save_study_details"))
  })

  shiny::observeEvent(input$archive_study, {
    shiny::req(rv$study_id)
    tryCatch({
      archive_study(get_cached_study(rv$study_id), rv$user)
      rv$study_cache <- NULL
      rv$questions_cache <- NULL
      rv$experts_cache <- NULL
      rv$msg <- "Survey archived."
      rv$page <- "dash"
      ee_log("info", sprintf("study %s archived", rv$study_id), where = "audit.study_archive")
    }, error = function(e) ee_handle(rv, e, "archive_study"))
  })

  shiny::observeEvent(input$advance_round, {
    tryCatch({
      st <- get_cached_study(rv$study_id)
      require_study_manager(rv$user, st, "advance this study")
      st <- advance_round(st)
      rv$study_cache <- st
      rv$experts_cache <- study_experts(st)
      rv$msg <- sprintf("Now round %s. Experts who reopen the link will see blinded round-1 distributions.", current_round(st))
      ee_log("info", rv$msg, where = "advance_round")
    }, error = function(e) ee_handle(rv, e, "advance_round"))
  })

  shiny::observeEvent(input$complete_study, {
    tryCatch({
      st <- complete_study(get_cached_study(rv$study_id), rv$user)
      rv$study_cache <- st
      rv$msg <- "Study marked complete. Consensus is ready for review and reporting."
    }, error = function(e) ee_handle(rv, e, "complete_study"))
  })

  chips_mod <- mod_chips_server("chips", bounds = shiny::reactive({
    cur_q <- tryCatch({
      if (!is.null(rv$survey_study) && !is.null(rv$survey_page)) {
        qs <- rv$survey_questions %||% study_questions(doc_id(rv$survey_study))
        cur_r <- rv$survey_round %||% 1L
        pages <- survey_pages(rv$survey_study, qs, cur_r)
        pg <- pages[[rv$survey_page]]
        pg$question
      } else NULL
    }, error = function(e) NULL)

    if (!is.null(cur_q) && !is.null(cur_q$lowerBound) && !is.null(cur_q$upperBound)) {
      list(lo = as.numeric(cur_q$lowerBound), hi = as.numeric(cur_q$upperBound))
    } else {
      list(lo = rv$bound_lo %||% 0, hi = rv$bound_hi %||% 1)
    }
  }))

  shiny::observeEvent(input$survey_enter, {
    rv$err <- ""
    sid <- trimws(query_study())
    tok <- trimws(query_token())
    if (!nzchar(sid) && nzchar(tok)) {
      tok_doc <- find_invite_token(tok)
      if (!is.null(tok_doc)) sid <- tok_doc$studyId
    }
    if (!nzchar(sid) && !is.null(rv$survey_study)) {
      sid <- doc_id(rv$survey_study)
    }
    em <- trimws(input$survey_email %||% "")
    if (!nzchar(em)) {
      rv$err <- "Please enter your invited email address."
      return()
    }
    tryCatch(
      enter_survey(survey_entry(sid, em)),
      error = function(e) ee_handle(rv, e, "survey_entry")
    )
  })

  shiny::observeEvent(input$survey_next, {
    rv$survey_page <- rv$survey_page + 1L
  })
  shiny::observeEvent(input$survey_prev, {
    rv$survey_page <- max(1L, rv$survey_page - 1L)
  })

  shiny::observe({
    shiny::req(rv$survey_study, rv$survey_user, rv$survey_page)
    cur_r <- rv$survey_round %||% 1L
    if (cur_r < 2L) return()
    qs <- rv$survey_questions %||% study_questions(doc_id(rv$survey_study))
    pages <- survey_pages(rv$survey_study, qs, cur_r)
    idx <- max(1L, min(as.integer(rv$survey_page), length(pages)))
    pg <- pages[[idx]]
    if (identical(pg$kind, "review") && !is.null(pg$question)) {
      prev <- max(1L, (pg$round_num %||% cur_r) - 1L)
      res_agg <- aggregation_as_shelf_result(latest_aggregation(doc_id(rv$survey_study), doc_id(pg$question), prev))
      if (is.null(res_agg)) {
        res_agg <- tryCatch(run_shelf_for_question(rv$survey_study, pg$question, prev, anonymize = TRUE), error = function(e) NULL)
      }
      rv$review_result <- res_agg
      rv$review_updated_at <- if (!is.null(res_agg)) Sys.time() else NULL
    }
  })

  shiny::observeEvent(input$save_onboarding, {
    tryCatch({
      save_onboarding(
        rv$survey_study, survey_pid(),
        isTRUE(input$tou_accept)
      )
      rv$survey_onboarded <- TRUE
      rv$err <- ""
      rv$survey_page <- rv$survey_page + 1L
    }, error = function(e) ee_handle(rv, e, "save_onboarding"))
  })

  shiny::observeEvent(input$save_bounds, {
    tryCatch({
      if (!isTRUE(rv$survey_onboarded)) {
        require_onboarding(rv$survey_study, survey_pid())
        rv$survey_onboarded <- TRUE
      }
      cur_r <- rv$survey_round %||% 1L
      b <- save_bounds(rv$survey_study, survey_pid(), input$bound_lo, input$bound_hi, cur_r)
      rv$bound_lo <- as.numeric(b$lower)
      rv$bound_hi <- as.numeric(b$upper)
      rv$err <- ""
      rv$survey_page <- rv$survey_page + 1L
    }, error = function(e) ee_handle(rv, e, "save_bounds"))
  })

  shiny::observeEvent(input$save_comment, {
    tryCatch({
      qs <- rv$survey_questions %||% study_questions(doc_id(rv$survey_study))
      q <- primary_question(qs)
      shiny::req(q)
      cur_r <- rv$survey_round %||% 1L
      save_peer_comment(rv$survey_study, q, survey_pid(), input$peer_comment, cur_r)
      rv$msg <- "Comment posted."
      rv$err <- ""
      shiny::updateTextAreaInput(session, "peer_comment", value = "")
    }, error = function(e) ee_handle(rv, e, "save_comment"))
  })

  shiny::observeEvent(input$save_chips, {
    shiny::req(rv$survey_study, rv$survey_user)
    cur_r <- rv$survey_round %||% 1L
    qs <- rv$survey_questions %||% study_questions(doc_id(rv$survey_study))
    pages <- survey_pages(rv$survey_study, qs, cur_r)
    pg <- pages[[rv$survey_page]]
    shiny::req(identical(pg$kind, "chips"), !is.null(pg$question))
    val <- chips_mod$value()
    if (val$placed != val$totalChips) {
      rv$err <- sprintf("Allocate exactly %s chips (currently %s).", val$totalChips, val$placed)
      ee_log("warn", rv$err, where = "save_chips")
      return()
    }
    tryCatch({
      if (!isTRUE(rv$survey_onboarded)) {
        require_onboarding(rv$survey_study, survey_pid())
        rv$survey_onboarded <- TRUE
      }
      lo <- as.numeric(pg$question$lowerBound %||% rv$bound_lo %||% val$lowerBound)
      hi <- as.numeric(pg$question$upperBound %||% rv$bound_hi %||% val$upperBound)
      if (!is.finite(lo) || !is.finite(hi) || !(lo < hi)) stop("Set plausible bounds L < U first.")
      require_rationale_text(input$chips_rationale, isTRUE(pg$question$rationaleRequired %||% TRUE))
      payload <- chips_payload(val$bins, val$totalChips, lo, hi, input$chips_rationale %||% "")
      save_judgment(
        rv$survey_study, pg$question,
        survey_pid(),
        rv$survey_user$displayName,
        payload,
        pg$round_num %||% cur_r
      )
      key <- sprintf("%s_%s", doc_id(pg$question), pg$round_num %||% cur_r)
      rv$survey_judgments[[key]] <- list(
        payload = payload,
        rationale = input$chips_rationale %||% "",
        expertId = survey_pid(),
        questionId = doc_id(pg$question),
        roundNumber = pg$round_num %||% cur_r
      )
      rv$err <- ""
      rv$msg <- "Chips saved."
      shiny::updateTextAreaInput(session, "chips_rationale", value = "")
      rv$survey_page <- rv$survey_page + 1L
      ee_log("info", sprintf("chips judgment saved for question %s", doc_id(pg$question)), where = "save_chips")
    }, error = function(e) ee_handle(rv, e, "save_chips"))
  })

  shiny::observeEvent(input$save_quantile, {
    shiny::req(rv$survey_study, rv$survey_user)
    cur_r <- rv$survey_round %||% 1L
    qs <- rv$survey_questions %||% study_questions(doc_id(rv$survey_study))
    pages <- survey_pages(rv$survey_study, qs, cur_r)
    pg <- pages[[rv$survey_page]]
    shiny::req(identical(pg$kind, "quantile"), !is.null(pg$question))
    a <- as.numeric(input$p10)
    b <- as.numeric(input$p50)
    c <- as.numeric(input$p90)
    lo <- as.numeric(pg$question$lowerBound %||% rv$bound_lo %||% 0)
    hi <- as.numeric(pg$question$upperBound %||% rv$bound_hi %||% 1)
    tryCatch({
      if (!isTRUE(rv$survey_onboarded)) {
        require_onboarding(rv$survey_study, survey_pid())
        rv$survey_onboarded <- TRUE
      }
      require_rationale_text(input$q_rationale, isTRUE(pg$question$rationaleRequired %||% TRUE))
      mode <- input$q_mode %||% "percentile"
      if (identical(mode, "quartile")) {
        vals <- list(`0.25` = a, `0.5` = b, `0.75` = c)
        validate_strict_quantiles(vals, lo, hi)
        payload <- quartile_payload(a, b, c, input$q_rationale %||% "", lo, hi)
      } else {
        vals <- list(`0.1` = a, `0.5` = b, `0.9` = c)
        validate_strict_quantiles(vals, lo, hi)
        payload <- quantile_payload(a, b, c, input$q_rationale %||% "", lo, hi)
      }
      save_judgment(
        rv$survey_study, pg$question,
        survey_pid(),
        rv$survey_user$displayName,
        payload,
        pg$round_num %||% cur_r
      )
      key <- sprintf("%s_%s", doc_id(pg$question), pg$round_num %||% cur_r)
      rv$survey_judgments[[key]] <- list(
        payload = payload,
        rationale = input$q_rationale %||% "",
        expertId = survey_pid(),
        questionId = doc_id(pg$question),
        roundNumber = pg$round_num %||% cur_r
      )
      rv$err <- ""
      rv$msg <- "Percentiles saved."
      shiny::updateNumericInput(session, "p10", value = NA)
      shiny::updateNumericInput(session, "p50", value = NA)
      shiny::updateNumericInput(session, "p90", value = NA)
      shiny::updateTextAreaInput(session, "q_rationale", value = "")
      rv$survey_page <- rv$survey_page + 1L
      ee_log("info", sprintf("quantile judgment saved for question %s", doc_id(pg$question)), where = "save_quantile")
    }, error = function(e) ee_handle(rv, e, "save_quantile"))
  })

  shiny::observeEvent(input$survey_final_submit, {
    shiny::req(rv$survey_study, rv$survey_user)
    tryCatch({
      cur_r <- rv$survey_round %||% 1L
      submit_expert_survey(doc_id(rv$survey_study), survey_pid(), rv$survey_user, round_number = cur_r)
      qs <- rv$survey_questions %||% study_questions(doc_id(rv$survey_study))
      pages <- survey_pages(rv$survey_study, qs, cur_r)
      done_idx <- which(vapply(pages, function(p) identical(p$kind, "done"), logical(1)))
      if (length(done_idx)) {
        rv$survey_page <- done_idx[[1]]
      } else {
        rv$survey_page <- length(pages)
      }
      rv$msg <- if (cur_r >= 2L) "Your Round 2 deliberation responses have been successfully submitted." else "Your Round 1 responses have been successfully submitted."
      rv$err <- ""
      ee_log("info", sprintf("expert %s submitted survey round %d for study %s", rv$survey_user$email, cur_r, doc_id(rv$survey_study)),
             where = "survey_final_submit")
    }, error = function(e) ee_handle(rv, e, "survey_final_submit"))
  })

  shiny::observeEvent(input$goto_study, {
    rv$page <- "study"
    rv$msg <- ""
  })

  shiny::observeEvent(input$select_resp_q_idx, {
    idx <- as.integer(input$select_resp_q_idx)
    if (is.finite(idx) && idx >= 1L) {
      rv$resp_q_idx <- idx
      rv$shelf_result <- NULL
      rv$resp_family <- NULL
    }
  })

  shiny::observeEvent(input$select_resp_round, {
    r <- as.integer(input$select_resp_round)
    if (is.finite(r) && r >= 1L) {
      rv$resp_round <- r
      rv$shelf_result <- NULL
      rv$resp_family <- NULL
    }
  })

  shiny::observeEvent(input$resp_q_prev, {
    curr <- as.integer(rv$resp_q_idx %||% 1L)
    if (curr > 1L) {
      rv$resp_q_idx <- curr - 1L
      rv$shelf_result <- NULL
      rv$resp_family <- NULL
    }
  })

  shiny::observeEvent(input$resp_q_next, {
    qs <- get_cached_questions(rv$study_id)
    if (length(qs) > 0) {
      curr <- as.integer(rv$resp_q_idx %||% 1L)
      if (curr < length(qs)) {
        rv$resp_q_idx <- curr + 1L
        rv$shelf_result <- NULL
        rv$resp_family <- NULL
      }
    }
  })

  shiny::observeEvent(input$btn_layout_split, {
    rv$results_layout_mode <- "split"
  })

  shiny::observeEvent(input$btn_layout_chart, {
    rv$results_layout_mode <- "chart"
  })

  shiny::observeEvent(input$btn_layout_table, {
    rv$results_layout_mode <- "table"
  })

  shiny::observeEvent(input$btn_mode_fitted, {
    shiny::updateActionButton(session, "chart_view_mode", label = "fitted")
    rv$chart_view_mode <- "fitted"
  })

  shiny::observeEvent(input$btn_mode_both, {
    shiny::updateActionButton(session, "chart_view_mode", label = "both")
    rv$chart_view_mode <- "both"
  })

  shiny::observeEvent(input$refresh_responses, {
    rv$resp_judgments_cache <- NULL
    rv$resp_aggregations_cache <- NULL
    rv$resp_comments_cache <- NULL
    rv$experts_cache <- NULL
    rv$shelf_result <- NULL
    rv$msg <- "Refreshed expert judgments and responses."
  })

  current_resp_question <- function() {
    st <- get_cached_study(rv$study_id)
    if (is.null(st)) return(NULL)
    qs <- get_cached_questions(doc_id(st))
    if (!length(qs)) return(NULL)
    idx <- as.integer(rv$resp_q_idx %||% 1L)
    if (!is.finite(idx) || idx < 1L) idx <- 1L
    if (idx > length(qs)) idx <- length(qs)
    qs[[idx]]
  }

  shiny::observeEvent(list(input$run_shelf, input$recalc_weights), {
    shiny::req(rv$study_id)
    rv$err <- ""
    st <- get_cached_study(rv$study_id)
    if (is.null(st)) {
      rv$err <- "Study not found."
      return()
    }
    q <- current_resp_question()
    if (is.null(q)) {
      rv$err <- "No question found in study."
      return()
    }

    # Extract dynamic expert weights (respecting selected round and fallback)
    active_r <- as.integer(rv$resp_round %||% current_round(st))
    qid <- doc_id(q)
    all_study_js <- get_cached_study_judgments(doc_id(st))
    js <- Filter(function(j) {
      identical(as.character(j$questionId %||% ""), qid) &&
        identical(as.integer(j$roundNumber %||% 1L), as.integer(active_r))
    }, all_study_js)
    if (!length(js) && active_r > 1L && is.null(rv$resp_round)) {
      js_r1 <- Filter(function(j) {
        identical(as.character(j$questionId %||% ""), qid) &&
          identical(as.integer(j$roundNumber %||% 1L), 1L)
      }, all_study_js)
      if (length(js_r1) > 0) {
        active_r <- 1L
        js <- js_r1
      }
    }
    user_weights <- NULL
    if (length(js)) {
      w_vec <- vapply(js, function(j) {
        pid <- as.character(j$expertId %||% "")
        val <- input[[paste0("weight_expert_", pid)]]
        if (is.null(val) || !is.finite(as.numeric(val)) || as.numeric(val) < 0) 1.0 else as.numeric(val)
      }, numeric(1))
      if (any(w_vec > 0)) user_weights <- w_vec
    }

    tryCatch({
      require_role(can_run_shelf(rv$user, st), "Only facilitators can run SHELF.")
      chosen_fam <- input$fit_family %||% rv$resp_family %||% "best"
      rv$resp_family <- chosen_fam
      rv$shelf_result <- run_shelf_for_question(
        st, q,
        round_number = active_r,
        family = chosen_fam,
        weights = user_weights
      )
      rv$resp_aggregations_cache <- NULL
      rv$msg <- sprintf("SHELF fit updated with %s (%s experts, Round %d).", rv$shelf_result$fit$engine, rv$shelf_result$nExperts, active_r)
    }, error = function(e) ee_handle(rv, e, "run_shelf"))
  })

  shiny::observeEvent(input$fit_family, {
    shiny::req(input$fit_family)
    rv$resp_family <- input$fit_family
  }, ignoreInit = TRUE)

  shiny::observe({
    shiny::req(identical(rv$page, "responses"), rv$study_id)
    q <- current_resp_question()
    if (is.null(q)) return()
    qid <- doc_id(q)
    if (!is.null(rv$shelf_result) && identical(as.character(rv$shelf_result$questionId), as.character(qid))) return()
    st <- get_cached_study(rv$study_id)
    active_r <- as.integer(rv$resp_round %||% current_round(st))
    aggs <- get_cached_study_aggregations(doc_id(st))
    match_aggs <- Filter(function(a) {
      identical(as.character(a$questionId %||% ""), qid) &&
        identical(as.integer(a$roundNumber %||% 1L), as.integer(active_r))
    }, aggs)
    latest_agg <- if (length(match_aggs)) match_aggs[[length(match_aggs)]] else NULL
    if (is.null(latest_agg) && active_r > 1L && is.null(rv$resp_round)) {
      match_r1 <- Filter(function(a) {
        identical(as.character(a$questionId %||% ""), qid) &&
          identical(as.integer(a$roundNumber %||% 1L), 1L)
      }, aggs)
      if (length(match_r1)) latest_agg <- match_r1[[length(match_r1)]]
    }
    new_res <- aggregation_as_shelf_result(latest_agg)
    if (is.null(new_res) && is.null(rv$shelf_result)) return()
    rv$shelf_result <- new_res
    if (!is.null(new_res$fit$selectedFamily)) {
      rv$resp_family <- new_res$fit$selectedFamily
    }
  })

  output$login_err <- shiny::renderUI(notice(rv$err, "error"))

  output$root <- shiny::renderUI({
    if (!isTRUE(ping$ok) && !is_survey_mode() && identical(rv$page, "login")) {
      return(shiny::div(
        class = "login-page",
        shiny::div(
          class = "login-card",
          htmltools::tags$h1("Database not connected"),
          htmltools::tags$p(ping$message),
          htmltools::tags$p("Set SUPABASE_DB_URL in .Renviron. See README.md."),
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
        )
      ))
    }
    if (is_survey_mode() || !is.null(rv$survey_user)) return(survey_root_ui(input, rv, query_study(), ping, query_token()))
    if (is.null(rv$user) && ee_auth_dev_mode()) return(login_ui())
    if (is.null(rv$user)) {
      return(shiny::div(class = "login-page", shiny::div(class = "login-card",
        htmltools::tags$h1("Sign in required"),
        htmltools::tags$p(if (ee_oidc_enabled()) "Sign in with your organization account to continue." else
          "This staff app expects Posit Connect login (session user)."),
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
      )))
    }
    switch(
      rv$page,
      dash = staff_dash_ui(rv),
      study = staff_study_ui(rv),
      responses = staff_responses_ui(rv),
      people = staff_people_ui(rv),
      staff_dash_ui(rv)
    )
  })

  output$shelf_plot <- plotly::renderPlotly({
    shiny::req(rv$shelf_result)
    mode <- rv$chart_view_mode %||% "both"
    show_pool <- if (identical(mode, "fitted")) FALSE else if (!is.null(input$show_linear_pool)) isTRUE(input$show_linear_pool) else TRUE
    df <- shelf_plot_df(rv$shelf_result$fit, show_pool)
    shiny::req(df)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = pdf, colour = name, linetype = role, group = name)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = "Elicited Value", y = "Probability Density", colour = NULL, linetype = NULL) +
      ggplot2::theme(
        legend.position = "top",
        legend.text = ggplot2::element_text(size = 9),
        plot.margin = ggplot2::margin(t = 2, r = 10, b = 5, l = 10),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#f1f5f9"),
        axis.title.x = ggplot2::element_text(face = "bold", size = 11, color = "#334155", margin = ggplot2::margin(t = 6)),
        axis.title.y = ggplot2::element_text(face = "bold", size = 11, color = "#334155", margin = ggplot2::margin(r = 6)),
        axis.text = ggplot2::element_text(color = "#475569", size = 10)
      )
    ply <- plotly::ggplotly(p, tooltip = c("colour", "x", "y"))
    ply <- plotly::layout(
      ply,
      autosize = TRUE,
      margin = list(l = 50, r = 20, t = 35, b = 45),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.12,
        yanchor = "bottom",
        font = list(size = 10)
      ),
      xaxis = list(
        automargin = TRUE,
        showgrid = TRUE,
        gridcolor = "#f1f5f9",
        zeroline = TRUE,
        zerolinecolor = "#cbd5e1"
      ),
      yaxis = list(
        automargin = TRUE,
        showgrid = TRUE,
        gridcolor = "#f1f5f9",
        zeroline = TRUE,
        zerolinecolor = "#cbd5e1"
      )
    )
    plotly::config(ply, responsive = TRUE, displayModeBar = "hover", displaylogo = FALSE)
  })

  output$review_plot <- plotly::renderPlotly({
    shiny::req(rv$review_result)
    df <- shelf_plot_df(rv$review_result$fit, TRUE)
    shiny::req(df)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = pdf, colour = name, linetype = role, group = name)) +
      ggplot2::geom_line(linewidth = 1.1) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(x = "Elicited Value", y = "Probability Density", colour = NULL, linetype = NULL) +
      ggplot2::theme(
        legend.position = "top",
        legend.text = ggplot2::element_text(size = 9),
        plot.margin = ggplot2::margin(t = 2, r = 10, b = 5, l = 10),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_line(color = "#f1f5f9"),
        axis.title.x = ggplot2::element_text(face = "bold", size = 11, color = "#334155", margin = ggplot2::margin(t = 6)),
        axis.title.y = ggplot2::element_text(face = "bold", size = 11, color = "#334155", margin = ggplot2::margin(r = 6)),
        axis.text = ggplot2::element_text(color = "#475569", size = 10)
      )
    ply <- plotly::ggplotly(p, tooltip = c("colour", "x", "y"))
    ply <- plotly::layout(
      ply,
      autosize = TRUE,
      margin = list(l = 50, r = 20, t = 35, b = 45),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = 1.12,
        yanchor = "bottom",
        font = list(size = 10)
      ),
      xaxis = list(
        automargin = TRUE,
        showgrid = TRUE,
        gridcolor = "#f1f5f9",
        zeroline = TRUE,
        zerolinecolor = "#cbd5e1"
      ),
      yaxis = list(
        automargin = TRUE,
        showgrid = TRUE,
        gridcolor = "#f1f5f9",
        zeroline = TRUE,
        zerolinecolor = "#cbd5e1"
      )
    )
    plotly::config(ply, responsive = TRUE, displayModeBar = "hover", displaylogo = FALSE)
  })

  output$shelf_params <- shiny::renderTable({
    if (is.null(rv$shelf_result) || is.null(rv$shelf_result$params)) {
      return(data.frame(Status = "No consensus calculated yet. Click 'Update Fit & Consensus' to generate parameters.", check.names = FALSE))
    }
    params <- rv$shelf_result$params

    numeric_quantiles <- c("mean", "q_low", "q_mid", "q_high")
    for (col in numeric_quantiles) {
      if (col %in% names(params)) {
        params[[col]] <- formatC(as.numeric(params[[col]]), format = "f", digits = 2)
      }
    }
    if ("weight" %in% names(params)) {
      w_vals <- as.numeric(params[["weight"]])
      params[["weight"]] <- ifelse(is.finite(w_vals), formatC(w_vals, format = "f", digits = 2), "—")
    }
    params$sse <- NULL

    col_dict <- c(
      "series" = "Expert / result",
      "role" = "Role",
      "weight" = "Weight",
      "mean" = "Fitted mean",
      "q_low" = "Lower (P10)",
      "q_mid" = "Median (P50)",
      "q_high" = "Upper (P90)",
      "family" = "Family",
      "note" = "Best fit from SHELF"
    )
    for (orig in names(col_dict)) {
      if (orig %in% names(params)) names(params)[names(params) == orig] <- col_dict[[orig]]
    }
    params
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", rownames = FALSE)

  output$raw_response_table <- shiny::renderTable({
    shiny::req(rv$study_id)
    st <- get_cached_study(rv$study_id)
    shiny::req(st)
    q <- current_resp_question()
    shiny::req(q)
    qid <- doc_id(q)
    active_r <- as.integer(rv$resp_round %||% current_round(st))
    all_study_js <- get_cached_study_judgments(doc_id(st))
    js <- Filter(function(j) {
      identical(as.character(j$questionId %||% ""), qid) &&
        identical(as.integer(j$roundNumber %||% 1L), as.integer(active_r))
    }, all_study_js)
    if (!length(js) && active_r > 1L && is.null(rv$resp_round)) {
      js_r1 <- Filter(function(j) {
        identical(as.character(j$questionId %||% ""), qid) &&
          identical(as.integer(j$roundNumber %||% 1L), 1L)
      }, all_study_js)
      if (length(js_r1) > 0) js <- js_r1
    }
    if (!length(js)) return(data.frame(Status = "No responses received for this question yet.", check.names = FALSE))

    rows <- lapply(js, function(j) {
      expert_name <- as.character(j$expertName %||% j$expertId %||% "Expert")
      p <- j$payload %||% list()
      qmap <- quantiles_from_payload(p)

      q_low <- if (!is.null(qmap) && length(qmap) >= 1) formatC(as.numeric(qmap[[1]]), format = "f", digits = 2) else "—"
      q_mid <- if (!is.null(qmap) && length(qmap) >= 2) formatC(as.numeric(qmap[[2]]), format = "f", digits = 2) else "—"
      q_high <- if (!is.null(qmap) && length(qmap) >= 3) formatC(as.numeric(qmap[[3]]), format = "f", digits = 2) else "—"

      summary_str <- ""
      bins_list <- p$bins
      if (!is.null(bins_list) && length(bins_list)) {
        placed_bins <- Filter(function(b) isTRUE((as.integer(b$chips %||% 0L)) > 0L), bins_list)
        if (length(placed_bins)) {
          parts <- vapply(placed_bins, function(b) {
            b_from <- as.character(b$from %||% b$lower %||% "?")
            b_to   <- as.character(b$to %||% b$upper %||% "?")
            c_cnt  <- as.integer(b$chips %||% 0L)
            sprintf("[%s, %s]: %d chips", b_from, b_to, c_cnt)
          }, character(1))
          summary_str <- paste(parts, collapse = "; ")
        } else {
          summary_str <- "0 chips placed"
        }
      } else {
        summary_str <- as.character(j$rationale %||% p$rationale %||% "—")
      }
      if (!nzchar(trimws(summary_str))) summary_str <- "—"

      data.frame(
        "Expert" = expert_name,
        "Lower (P10/Q1)" = q_low,
        "Median (P50)" = q_mid,
        "Upper (P90/Q3)" = q_high,
        "Response Summary" = summary_str,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })

    do.call(rbind, rows)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "s", rownames = FALSE)

  output$resp_notice <- shiny::renderUI({
    shiny::tagList(
      notice(rv$msg, "ok"),
      notice(rv$err, "error")
    )
  })

  output$shelf_conclusion_ui <- shiny::renderUI({
    if (is.null(rv$shelf_result) || !nzchar(rv$shelf_result$conclusion %||% "")) {
      return(shiny::div(
        class = "empty-responses-box",
        style = "margin-bottom: 1rem;",
        htmltools::tags$h4("No Consensus Calculated Yet"),
        htmltools::tags$p(class = "muted", "Click 'Update Fit & Consensus' to compute individual SHELF fits and Linear Pool consensus for this question.")
      ))
    }
    htmltools::tags$div(
      class = "shelf-conclusion-banner",
      htmltools::tags$span(class = "badge-status", "OakleyJ/SHELF"),
      htmltools::tags$span(rv$shelf_result$conclusion)
    )
  })

  output$shelf_params_panel <- shiny::renderUI({
    shiny::req(rv$shelf_result$params)
    shiny::div(
      class = "parameter-panel",
      htmltools::tags$h4(style = "margin: 1.25rem 0 0.5rem; font-size: 1rem; color: #1e293b;", "Estimated Summary Parameters & Weights"),
      shiny::div(class = "table-responsive", shiny::tableOutput("shelf_params"))
    )
  })

  output$shelf_comments_panel <- shiny::renderUI({
    shiny::req(rv$study_id)
    q <- current_resp_question()
    shiny::req(q)
    st <- get_cached_study(rv$study_id)
    cmts <- get_cached_study_comments(doc_id(st))
    comments <- Filter(function(c) {
      identical(as.character(c$questionId %||% ""), doc_id(q)) &&
        identical(as.integer(c$roundNumber %||% 1L), as.integer(current_round(st)))
    }, cmts)
    if (!length(comments)) {
      return(htmltools::div(
        style = "padding: 2rem 1rem; text-align: center; color: #64748b; font-size: 0.9rem;",
        htmltools::tags$p("No expert rationales or comments recorded for this round yet.")
      ))
    }
    mapping <- blind_labels_for_ids(vapply(comments, function(c) as.character(c$authorPersonId %||% ""), character(1)))
    shiny::div(
      class = "comments-panel",
      htmltools::tags$h4(style = "margin: 1.25rem 0 0.5rem; font-size: 1rem; color: #1e293b;", "Blinded Expert Rationales"),
      htmltools::tags$ul(
        class = "comment-list",
        lapply(comments, function(cmt) {
          htmltools::tags$li(htmltools::tags$strong(blind_label(cmt$authorPersonId, mapping)), ": ", cmt$body)
        })
      )
    )
  })

  output$resp_nav_steps <- shiny::renderUI({
    shiny::req(identical(rv$page, "responses"), rv$study_id)
    resp_nav_steps_ui(rv)
  })

  output$resp_header_nav <- shiny::renderUI({
    shiny::req(identical(rv$page, "responses"), rv$study_id)
    resp_header_nav_ui(rv)
  })

  output$resp_question_banner <- shiny::renderUI({
    shiny::req(identical(rv$page, "responses"), rv$study_id)
    resp_question_banner_ui(rv)
  })

  output$resp_expert_weights_ui <- shiny::renderUI({
    shiny::req(identical(rv$page, "responses"), rv$study_id)
    resp_expert_weights_ui_content(rv, input)
  })

  output$resp_advance_round_btn_ui <- shiny::renderUI({
    shiny::req(identical(rv$page, "responses"), rv$study_id)
    resp_advance_round_btn_ui_content(rv)
  })

  output$survey_wizard_header <- shiny::renderUI({
    shiny::req(rv$survey_user, rv$survey_study)
    st <- rv$survey_study
    qs <- rv$survey_questions %||% study_questions(doc_id(st))
    r_num <- rv$survey_round %||% 1L
    pages <- survey_pages(st, qs, r_num)
    idx <- max(1L, min(as.integer(rv$survey_page %||% 1L), length(pages)))
    progress <- if (length(pages) <= 1) 0 else (idx - 1) / (length(pages) - 1)

    shiny::div(
      class = "survey-wizard-header",
      shiny::div(
        class = "wizard-steps",
        lapply(seq_along(pages), function(i) {
          is_active <- (i == idx)
          is_done <- (i < idx)
          htmltools::div(
            class = paste(
              "wizard-step-pill",
              if (is_active) "step-active",
              if (is_done) "step-done"
            ),
            htmltools::span(class = "step-num", if (is_done) "✓" else as.character(i)),
            htmltools::span(class = "step-title", pages[[i]]$title)
          )
        })
      ),
      shiny::div(
        class = "progress-track",
        shiny::div(class = "progress-bar", style = sprintf("width:%s%%", round(100 * progress)))
      )
    )
  })

  output$survey_sidebar_nav <- shiny::renderUI({
    shiny::req(rv$survey_user, rv$survey_study)
    st <- rv$survey_study
    qs <- rv$survey_questions %||% study_questions(doc_id(st))
    r_num <- rv$survey_round %||% 1L
    pages <- survey_pages(st, qs, r_num)
    idx <- max(1L, min(as.integer(rv$survey_page %||% 1L), length(pages)))

    shiny::div(
      class = "survey-nav",
      lapply(seq_along(pages), function(i) {
        htmltools::div(
          class = paste("nav-item", if (i == idx) "active", if (i < idx) "done"),
          htmltools::tags$strong(paste(i, pages[[i]]$title)),
          htmltools::tags$span(class = "muted", pages[[i]]$subtitle)
        )
      })
    )
  })

  output$survey_page_body <- shiny::renderUI({
    shiny::req(rv$survey_user, rv$survey_study)
    st <- rv$survey_study
    qs <- rv$survey_questions %||% study_questions(doc_id(st))
    r_num <- rv$survey_round %||% 1L
    pages <- survey_pages(st, qs, r_num)
    idx <- max(1L, min(as.integer(rv$survey_page %||% 1L), length(pages)))
    survey_page_body_content(input, rv, st, qs, r_num, pages, idx)
  })

  # Export & Audit Center Modal Handlers
  open_export_modal <- function() {
    st <- find_study(rv$study_id)
    if (is.null(st)) return()
    qs <- study_questions(doc_id(st))
    shiny::showModal(export_center_modal(st, qs, rv$user))
  }
  shiny::observeEvent(input$btn_open_export_modal, open_export_modal())
  shiny::observeEvent(input$btn_open_export_modal_resp, open_export_modal())

  # Quick single-question download button on the Responses toolbar
  output$resp_quick_download_ui <- shiny::renderUI({
    q <- current_resp_question()
    code <- if (!is.null(q)) q$code %||% "Q" else "Question"
    shiny::downloadButton("dl_pdf", sprintf("📄 Quick PDF (%s)", code), class = "btn-secondary btn-sm")
  })

  # Dynamic Question Details Card in the Export Modal
  output$modal_question_details_card <- shiny::renderUI({
    shiny::req(input$modal_target_question_id, rv$study_id)
    st <- find_study(rv$study_id)
    q <- db_one("questions", q_id(input$modal_target_question_id))
    shiny::req(st, q)

    sid <- doc_id(st)
    qid <- doc_id(q)
    cur_r <- current_round(st)

    aggs <- get_cached_study_aggregations(sid)
    match_aggs <- Filter(function(a) {
      identical(as.character(a$questionId %||% ""), qid) &&
        identical(as.integer(a$roundNumber %||% 1L), as.integer(cur_r))
    }, aggs)
    latest_agg <- if (length(match_aggs)) match_aggs[[length(match_aggs)]] else NULL
    js <- current_judgments(sid, qid, cur_r)

    method_label <- if (is_chips_question(q)) "Chips-N-Bins" else "Low-High-Best / Quantile"
    bounds_txt <- sprintf("[%s to %s] (%s)", q$lowerBound %||% 0, q$upperBound %||% 1, q$unit %||% "unit")

    consensus_txt <- if (!is.null(latest_agg) && nzchar(latest_agg$conclusion %||% "")) {
      latest_agg$conclusion
    } else if (length(js) > 0) {
      sprintf("%d judgments submitted for Round %d. (Consensus updated on Responses view).", length(js), cur_r)
    } else {
      "No judgments recorded yet for this question."
    }

    shiny::div(
      class = "modal-question-card",
      style = "background: #f8fafc; border: 2px solid #3b82f6; border-radius: 8px; padding: 1.25rem; margin-top: 0.75rem;",

      # Header Badges
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: flex-start; margin-bottom: 0.6rem; flex-wrap: wrap; gap: 6px;",
        shiny::div(
          htmltools::span(class = "badge-status", style = "background: #e0e7ff; color: #3730a3; font-weight: 700; margin-right: 6px;", q$code %||% "QUESTION"),
          htmltools::span(class = "badge-status", style = "background: #e2e8f0; color: #334155;", method_label)
        ),
        htmltools::span(
          class = if (length(js) > 0) "badge-status-completed" else "badge-status-pending",
          sprintf("%d Experts Submitted", length(js))
        )
      ),

      htmltools::tags$h4(style = "margin: 0 0 0.5rem; font-size: 1.05rem; font-weight: 700; color: #0f172a;", q$title %||% "Question"),

      # Facilitator Prompt Box
      shiny::div(
        style = "background: #ffffff; border-left: 4px solid #2563eb; padding: 10px 14px; border-radius: 4px; margin: 0.75rem 0; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
        htmltools::tags$strong(style = "display: block; font-size: 0.76rem; text-transform: uppercase; letter-spacing: 0.5px; color: #1d4ed8; margin-bottom: 3px;", "Facilitator Question Prompt:"),
        htmltools::tags$p(style = "margin: 0; font-size: 0.92rem; color: #1e293b; line-height: 1.5; font-style: italic;",
          sprintf("“%s”", q$prompt %||% q$description %||% "No specific prompt text provided.")
        )
      ),

      # Metadata
      shiny::div(
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 8px; font-size: 0.85rem; color: #475569; margin-bottom: 0.75rem; background: #ffffff; padding: 8px 12px; border-radius: 6px; border: 1px solid #e2e8f0;",
        shiny::div(htmltools::strong("Parameter Type: "), q$variableType %||% "continuous"),
        shiny::div(htmltools::strong("Plausible Range: "), bounds_txt)
      ),

      # Consensus Banner
      shiny::div(
        style = "background: #ecfdf5; border: 1px solid #a7f3d0; border-radius: 6px; padding: 8px 12px; margin-bottom: 1.1rem; font-size: 0.85rem; color: #065f46;",
        htmltools::tags$strong("Latest Consensus: "),
        htmltools::tags$span(consensus_txt)
      ),

      # Specific Download Buttons for this Question
      shiny::div(
        style = "display: flex; gap: 10px; align-items: center; flex-wrap: wrap; padding-top: 8px; border-top: 1px dashed #cbd5e1;",
        shiny::downloadButton("dl_modal_q_pdf", sprintf("📥 Download PDF for %s", q$code %||% "Question"), class = "btn-primary btn-sm", style = "font-weight: 600;"),
        shiny::downloadButton("dl_modal_q_csv", sprintf("📊 Export CSV for %s", q$code %||% "Question"), class = "btn-secondary btn-sm")
      )
    )
  })

  # Modal Question-Specific Downloads
  output$dl_modal_q_pdf <- shiny::downloadHandler(
    filename = function() {
      q <- db_one("questions", q_id(input$modal_target_question_id))
      code <- if (!is.null(q)) q$code %||% "question" else "question"
      sprintf("%s-audit-%s.pdf", code, Sys.Date())
    },
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- db_one("questions", q_id(input$modal_target_question_id))
      shiny::req(q)
      cur_r <- current_round(st)
      res <- tryCatch(run_shelf_for_question(st, q, round_number = cur_r, anonymize = FALSE), error = function(e) NULL)
      comments <- list_peer_comments(doc_id(st), doc_id(q), cur_r)
      write_audit_pdf(file, res, st, q, cur_r, comments)
    }
  )

  output$dl_modal_q_csv <- shiny::downloadHandler(
    filename = function() {
      q <- db_one("questions", q_id(input$modal_target_question_id))
      code <- if (!is.null(q)) q$code %||% "question" else "question"
      sprintf("%s-params-%s.csv", code, Sys.Date())
    },
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- db_one("questions", q_id(input$modal_target_question_id))
      shiny::req(q)
      cur_r <- current_round(st)
      res <- tryCatch(run_shelf_for_question(st, q, round_number = cur_r, anonymize = FALSE), error = function(e) NULL)
      write_audit_csv(file, res, st, q, cur_r)
    }
  )

  # Modal Consolidated Study Downloads
  output$dl_modal_study_pdf <- shiny::downloadHandler(
    filename = function() sprintf("study-%s-report-%s.pdf", rv$study_id %||% "export", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      qs <- study_questions(doc_id(st))
      q <- if (length(qs)) qs[[1]] else NULL
      comments <- if (!is.null(q)) list_peer_comments(doc_id(st), doc_id(q), current_round(st)) else list()
      write_audit_pdf(file, rv$shelf_result, st, q, current_round(st), comments)
    }
  )

  output$dl_modal_study_csv <- shiny::downloadHandler(
    filename = function() sprintf("study-%s-audit-%s.csv", rv$study_id %||% "export", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- current_resp_question()
      write_audit_csv(file, rv$shelf_result, st, q, current_round(st))
    }
  )

  output$dl_modal_bundle <- shiny::downloadHandler(
    filename = function() sprintf("study-audit-bundle-%s.zip", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- current_resp_question()
      if (is.null(q)) {
        qs <- study_questions(doc_id(st))
        q <- if (length(qs)) qs[[1]] else NULL
      }
      comments <- if (!is.null(q)) list_peer_comments(doc_id(st), doc_id(q), current_round(st)) else list()
      write_audit_bundle(file, rv$shelf_result, st, q, current_round(st), comments)
    }
  )

  # Standard Toolbar Downloads (Maintained & Updated with Active Question)
  output$dl_csv <- shiny::downloadHandler(
    filename = function() {
      q <- current_resp_question()
      code <- if (!is.null(q)) q$code %||% "params" else "params"
      sprintf("shelf-%s-%s.csv", code, Sys.Date())
    },
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- current_resp_question()
      write_audit_csv(file, rv$shelf_result, st, q, current_round(st))
    }
  )

  output$dl_pdf <- shiny::downloadHandler(
    filename = function() {
      q <- current_resp_question()
      code <- if (!is.null(q)) q$code %||% "audit" else "study"
      sprintf("%s-audit-%s.pdf", code, Sys.Date())
    },
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- current_resp_question()
      if (is.null(q)) {
        qs <- study_questions(doc_id(st))
        q <- if (length(qs)) qs[[1]] else NULL
      }
      comments <- if (!is.null(q)) list_peer_comments(doc_id(st), doc_id(q), current_round(st)) else list()
      write_audit_pdf(file, rv$shelf_result, st, q, current_round(st), comments)
    }
  )

  output$dl_study_csv <- shiny::downloadHandler(
    filename = function() sprintf("study-%s-audit-%s.csv", rv$study_id %||% "export", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- current_resp_question()
      write_audit_csv(file, rv$shelf_result, st, q, current_round(st))
    }
  )

  output$dl_study_pdf <- shiny::downloadHandler(
    filename = function() sprintf("study-%s-report-%s.pdf", rv$study_id %||% "export", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      qs <- study_questions(doc_id(st))
      q <- if (length(qs)) qs[[1]] else NULL
      comments <- if (!is.null(q)) list_peer_comments(doc_id(st), doc_id(q), current_round(st)) else list()
      write_audit_pdf(file, rv$shelf_result, st, q, current_round(st), comments)
    }
  )

  output$dl_bundle <- shiny::downloadHandler(
    filename = function() sprintf("study-audit-bundle-%s.zip", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      q <- current_resp_question()
      if (is.null(q)) {
        qs <- study_questions(doc_id(st))
        q <- if (length(qs)) qs[[1]] else NULL
      }
      comments <- if (!is.null(q)) list_peer_comments(doc_id(st), doc_id(q), current_round(st)) else list()
      write_audit_bundle(
        file, rv$shelf_result, st, q, current_round(st),
        comments
      )
    }
  )

  output$dl_quarto <- shiny::downloadHandler(
    filename = function() sprintf("shelf-dossier-%s.html", Sys.Date()),
    content = function(file) {
      st <- find_study(rv$study_id)
      require_role(can_export_audit(rv$user, st))
      shiny::req(rv$shelf_result)
      qs <- study_questions(doc_id(st))
      q <- Filter(function(x) identical(doc_id(x), input$resp_question), qs)
      q <- if (length(q)) q[[1]] else qs[[1]]
      render_quarto_dossier(file, st, q, current_round(st))
    }
  )
}
