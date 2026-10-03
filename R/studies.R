current_round <- function(study) {
  cfg <- study$protocolConfig %||% list()
  r <- suppressWarnings(as.integer(cfg$currentRound %||% 1))
  if (is.na(r) || r < 1) 1L else r
}

expert_study_access <- function(study_id, person_id) {
  if (is.null(study_id) || is.null(person_id)) return(NULL)
  sid <- as.character(study_id)
  pid <- as.character(person_id)
  if (!nzchar(sid) || !nzchar(pid)) return(NULL)
  db_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "accessRole": "expert"}',
    json_escape(sid), json_escape(pid)
  ))
}

expert_study_round <- function(study, person_id) {
  if (is.null(study) || is.null(person_id)) return(1L)
  acc <- expert_study_access(doc_id(study), person_id)
  if (is.null(acc)) return(1L)
  r <- suppressWarnings(as.integer(acc$currentRound %||% 1L))
  if (is.na(r) || r < 1L) 1L else r
}

expert_is_submitted <- function(study, person_id, round_number = NULL) {
  if (is.null(study) || is.null(person_id)) return(FALSE)
  acc <- expert_study_access(doc_id(study), person_id)
  if (is.null(acc)) return(FALSE)
  cur_r <- suppressWarnings(as.integer(acc$currentRound %||% 1L))
  if (is.na(cur_r) || cur_r < 1L) cur_r <- 1L
  sub_r <- suppressWarnings(as.integer(acc$submittedRound %||% 0L))
  if (is.na(sub_r)) sub_r <- 0L
  raw_status <- as.character(acc$status %||% "active")

  if (is.null(round_number)) {
    if (sub_r >= cur_r && cur_r > 0L) return(TRUE)
    if (identical(raw_status, "submitted") && sub_r >= cur_r) return(TRUE)
    if (identical(raw_status, "submitted") && cur_r <= 1L) return(TRUE)
    return(FALSE)
  } else {
    chk_r <- suppressWarnings(as.integer(round_number))
    if (is.na(chk_r) || chk_r < 1L) chk_r <- 1L
    return(sub_r >= chk_r)
  }
}

unique_slug <- function(base) {
  candidate <- base %||% "study"
  n <- 0L
  repeat {
    slug <- if (n == 0L) candidate else paste0(candidate, "-", n)
    if (is.null(db_one("studies", q_field("slug", slug)))) return(slug)
    n <- n + 1L
  }
}

create_study <- function(user, title, description = "", quantity = "",
                         methods = c("chips_and_bins", "quantile"),
                         variable_type = "proportion", unit = "probability",
                         lower = 0, upper = 1, precision = 2,
                         preferred_distribution = "best",
                         consent = NULL) {
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  precision <- as.integer(precision)
  if (!is.finite(lower) || !is.finite(upper) || !(lower < upper)) {
    stop("Lower plausible bound must be less than the upper plausible bound.")
  }
  if (!variable_type %in% c("proportion", "continuous", "count")) {
    stop("Unsupported variable type.")
  }
  if (!nzchar(trimws(unit %||% ""))) stop("A unit is required.")
  if (!is.finite(precision) || precision < 0 || precision > 6) {
    stop("Decimal places must be between 0 and 6.")
  }
  if (!identical(variable_type, "proportion")) methods <- setdiff(methods, "chips_and_bins")
  if (!length(methods)) methods <- "quantile"
  org <- ensure_org()
  now <- iso_now()
  slug <- unique_slug(slugify(title))
  protocol_config <- list(
    methods = as.list(methods),
    currentRound = 1L,
    rounds = 3L,
    anonymizeFeedback = TRUE,
    quantileLevels = list(0.1, 0.5, 0.9),
    variableType = variable_type,
    unit = unit,
    lowerBound = lower,
    upperBound = upper,
    precision = precision,
    preferredDistribution = preferred_distribution,
    surveyWelcome = list(
      conductedBy = "Achutha Menon Centre for Health Science Studies (AMCHSS), SCTIMST, Trivandrum",
      why = description %||% "To capture structured expert judgment for decision support.",
      reason = "SHELF expert elicitation for HTA / clinical decision support.",
      instructions = "Complete chips-and-bins and/or P10/P50/P90 tasks, then submit.",
      quantityOfInterest = quantity,
      variableType = variable_type,
      unit = unit,
      lowerBound = lower,
      upperBound = upper,
      precision = precision,
      preferredDistribution = preferred_distribution,
      contactEmail = user$email
    ),
    caseStudyMeta = list(
      quantityOfInterest = quantity,
      explanation = description
    )
  )
  consent_record <- if (!is.null(consent) && is.list(consent)) {
    consent
  } else {
    list(
      consented = TRUE,
      consentedAt = now,
      consentedBy = user$id,
      institution = "AMCHSS · SCTIMST",
      version = "1.0"
    )
  }
  study <- db_insert("studies", list(
    orgId = doc_id(org),
    slug = slug,
    title = title,
    description = description,
    protocolType = "shelf",
    status = "draft",
    ownerId = user$id,
    surveyAccessMode = "invited_only",
    protocolConfig = protocol_config,
    dataIntegrityConsent = consent_record,
    createdAt = now,
    updatedAt = now
  ))
  sid <- doc_id(study)
  person_id <- user$personId
  if (is.null(person_id) || !nzchar(as.character(person_id))) {
    person <- find_person_by_email(user$email)
    if (is.null(person)) {
      person <- db_insert("people", list(
        orgId = doc_id(org),
        personType = "facilitator",
        name = user$displayName,
        email = user$email,
        expertise = list(),
        tags = list("facilitator"),
        inviteStatus = "active",
        userId = user$id,
        isActive = TRUE,
        createdAt = now,
        updatedAt = now
      ))
    }
    person_id <- doc_id(person)
    db_update("users", q_id(user$id), list(personId = person_id))
  }
  db_insert("study_access", list(
    studyId = sid,
    orgId = doc_id(org),
    personId = person_id,
    userId = user$id,
    accessRole = "facilitator",
    status = "active",
    notes = "Study owner / facilitator",
    createdAt = now,
    updatedAt = now
  ))
  sort_order <- 0L
  if ("chips_and_bins" %in% methods) {
    db_insert("questions", list(
      studyId = sid,
      code = "Q1_CHIPS",
      title = paste("Chips-N-Bins —", quantity %||% title),
      prompt = paste(
        "Allocate all chips across bins for:",
        quantity %||% "the uncertain quantity"
      ),
      variableType = variable_type,
      elicitationMethod = "chips_and_bins",
      unit = unit,
      lowerBound = lower,
      upperBound = upper,
      judgmentSchema = list(type = "chips_and_bins", binCount = 10L, totalChips = 20L),
      rationaleRequired = TRUE,
      sortOrder = sort_order,
      isActive = TRUE,
      createdAt = now,
      updatedAt = now,
      createdBy = user$id
    ))
    sort_order <- sort_order + 1L
  }
  if ("quantile" %in% methods) {
    db_insert("questions", list(
      studyId = sid,
      code = if (sort_order > 0) "Q2_QUANTILES" else "Q1_QUANTILES",
      title = paste("Low-High-Best —", quantity %||% title),
      prompt = paste("Provide P10, P50, and P90 for:", quantity %||% "the uncertain quantity"),
      variableType = variable_type,
      elicitationMethod = "quantile",
      unit = unit,
      lowerBound = lower,
      upperBound = upper,
      judgmentSchema = list(type = "quantile", levels = list(0.1, 0.5, 0.9), requireMonotonic = TRUE),
      rationaleRequired = TRUE,
      sortOrder = sort_order,
      isActive = TRUE,
      createdAt = now,
      updatedAt = now,
      createdBy = user$id
    ))
  }
  find_study(sid)
}

list_studies_for_user <- function(user) {
  if (is.null(user)) return(list())
  if (is_admin(user)) {
    return(db_all("studies", "{}", sort = '{"updatedAt": -1}'))
  }
  access <- db_all("study_access", sprintf(
    '{"$or": [{"userId": %s}, {"personId": %s}]}',
    json_escape(user$id %||% ""),
    json_escape(user$personId %||% "")
  ))
  access <- Filter(function(a) {
    s_val <- as.character(a$status %||% "active")
    !s_val %in% c("removed", "revoked", "inactive")
  }, access)
  ids <- unique(vapply(access, function(a) as.character(a$studyId %||% ""), character(1)))
  ids <- ids[nzchar(ids)]
  owned <- if (is_facilitator(user)) db_all("studies", q_field("ownerId", user$id)) else list()
  extra <- lapply(ids, function(id) db_one("studies", q_id(id)))
  all_s <- c(owned, Filter(Negate(is.null), extra))
  seen <- character()
  out <- list()
  for (s in all_s) {
    id <- doc_id(s)
    if (id %in% seen) next
    seen <- c(seen, id)
    out[[length(out) + 1]] <- s
  }
  if (is_viewer(user)) {
    out <- Filter(study_is_completed, out)
  }
  out
}

study_questions <- function(study_id) {
  qs <- db_all("questions", sprintf('{"studyId": %s, "isActive": true}', json_escape(study_id)))
  ords <- vapply(qs, function(q) as.integer(q$sortOrder %||% 0), integer(1))
  qs[order(ords)]
}

add_study_question <- function(study, title, prompt = "", variable_type = "proportion",
                               method = "quantile", unit = "probability",
                               lower = 0, upper = 1, precision = 2, user) {
  require_study_manager(user, study, "add questions to this study")
  title <- trimws(title %||% "")
  if (!nzchar(title)) stop("Enter a question title.")
  lower <- as.numeric(lower)
  upper <- as.numeric(upper)
  precision <- as.integer(precision)
  if (!is.finite(lower) || !is.finite(upper) || !(lower < upper)) {
    stop("Lower plausible bound must be less than the upper plausible bound.")
  }
  if (!is.finite(precision) || precision < 0 || precision > 6) {
    stop("Decimal places must be between 0 and 6.")
  }
  if (!variable_type %in% c("proportion", "continuous", "count")) {
    variable_type <- "proportion"
  }
  if (!identical(variable_type, "proportion") && identical(method, "chips_and_bins")) {
    method <- "quantile"
  }
  if (!method %in% c("chips_and_bins", "quantile")) {
    method <- "quantile"
  }
  unit <- trimws(unit %||% "")
  if (!nzchar(unit)) unit <- if (identical(variable_type, "proportion")) "probability" else "units"
  prompt <- trimws(prompt %||% "")
  if (!nzchar(prompt)) prompt <- sprintf("Provide assessments for: %s", title)

  sid <- doc_id(study)
  existing <- study_questions(sid)
  sort_order <- length(existing)
  m_code <- if (identical(method, "chips_and_bins")) "CHIPS" else "QUANTILES"
  code <- sprintf("Q%d_%s", sort_order + 1L, m_code)
  now <- iso_now()

  schema <- if (identical(method, "chips_and_bins")) {
    list(type = "chips_and_bins", binCount = 10L, totalChips = 20L)
  } else {
    list(type = "quantile", levels = list(0.1, 0.5, 0.9), requireMonotonic = TRUE)
  }

  q_res <- db_insert("questions", list(
    studyId = sid,
    code = code,
    title = title,
    prompt = prompt,
    variableType = variable_type,
    elicitationMethod = method,
    unit = unit,
    lowerBound = lower,
    upperBound = upper,
    precision = precision,
    judgmentSchema = schema,
    rationaleRequired = TRUE,
    sortOrder = sort_order,
    isActive = TRUE,
    createdAt = now,
    updatedAt = now,
    createdBy = user$id
  ))
  db_update("studies", q_id(sid), list(updatedAt = now))
  q_res
}

remove_study_question <- function(study, question_id, user) {
  require_study_manager(user, study, "remove questions from this study")
  sid <- doc_id(study)
  q <- db_one("questions", sprintf('{"_id": %s, "studyId": %s, "isActive": true}', json_escape(question_id), json_escape(sid)))
  if (is.null(q)) stop("Question not found in this study.")
  existing <- study_questions(sid)
  if (length(existing) <= 1) {
    stop("A case study must have at least one active question.")
  }
  now <- iso_now()
  db_update("questions", q_id(doc_id(q)), list(
    isActive = FALSE,
    updatedAt = now
  ))
  db_update("studies", q_id(sid), list(updatedAt = now))
  invisible(TRUE)
}

invite_expert <- function(study, email, name = NULL, user) {
  email <- tolower(trimws(email))
  org <- ensure_org()
  now <- iso_now()
  person <- find_person_by_email(email)
  if (is.null(person)) {
    person <- db_insert("people", list(
      orgId = doc_id(org),
      personType = "expert",
      name = name %||% email,
      email = email,
      expertise = list(),
      tags = list(),
      inviteStatus = "invited",
      isActive = TRUE,
      createdAt = now,
      updatedAt = now
    ))
  }
  exists <- db_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "accessRole": "expert", "status": "active"}',
    json_escape(doc_id(study)),
    json_escape(doc_id(person))
  ))
  if (is.null(exists)) {
    db_insert("study_access", list(
      studyId = doc_id(study),
      orgId = doc_id(org),
      personId = doc_id(person),
      accessRole = "expert",
      status = "active",
      currentRound = 1L,
      submittedRound = 0L,
      remindersSent = 0,
      remindersTotal = 3,
      notes = "Invited by facilitator",
      grantedBy = user$id,
      createdAt = now,
      updatedAt = now
    ))
  }
  if (identical(study$status %||% "", "draft")) {
    db_update("studies", q_id(doc_id(study)), list(status = "recruiting", updatedAt = now))
    study <- find_study(doc_id(study))
  }
  issue_invite_token(study, person)
  person
}

study_experts <- function(study) {
  sid <- doc_id(study)
  qs <- study_questions(sid)
  nq <- length(qs)
  access <- db_all("study_access", sprintf(
    '{"studyId": %s, "accessRole": "expert"}',
    json_escape(sid)
  ))
  access <- Filter(function(a) {
    s_val <- as.character(a$status %||% "active")
    !s_val %in% c("removed", "revoked", "inactive")
  }, access)
  if (!length(access)) return(list())

  # Batch fetch all people, judgments, and tokens in 3 queries instead of 20+ sequential queries
  all_people <- db_all("people", "{}")
  people_map <- setNames(all_people, vapply(all_people, doc_id, character(1)))

  all_js <- db_all("judgments", sprintf(
    '{"studyId": %s, "isCurrent": true, "isConsensus": false}',
    json_escape(sid)
  ))

  all_toks <- db_all("invite_tokens", sprintf(
    '{"studyId": %s, "revoked": false}',
    json_escape(sid)
  ))
  tok_map <- list()
  for (tk in all_toks) {
    pid_tk <- as.character(tk$personId %||% "")
    if (nzchar(pid_tk) && is.null(tok_map[[pid_tk]])) tok_map[[pid_tk]] <- tk
  }

  q_ids <- vapply(qs, doc_id, character(1))

  lapply(access, function(a) {
    pid <- as.character(a$personId)
    person <- people_map[[pid]]
    cur_r <- suppressWarnings(as.integer(a$currentRound %||% 1L))
    if (is.na(cur_r) || cur_r < 1L) cur_r <- 1L
    sub_r <- suppressWarnings(as.integer(a$submittedRound %||% 0L))
    if (is.na(sub_r)) sub_r <- 0L
    raw_status <- as.character(a$status %||% "active")

    answered <- 0L
    if (!is.null(person) && nq > 0) {
      answered_qids <- unique(vapply(
        Filter(function(j) {
          identical(as.character(j$expertId %||% ""), pid) &&
            identical(as.integer(j$roundNumber %||% 1L), as.integer(cur_r)) &&
            as.character(j$questionId %||% "") %in% q_ids
        }, all_js),
        function(j) as.character(j$questionId),
        character(1)
      ))
      answered <- length(answered_qids)
    }

    is_sub <- (sub_r >= cur_r && cur_r > 0L) || identical(raw_status, "submitted")
    status <- if (is_sub) {
      if (cur_r >= 2L) "Round 2 Submitted" else "Round 1 Submitted"
    } else if (answered > 0L) {
      if (cur_r >= 2L) "Round 2 In Progress" else "ongoing"
    } else {
      if (cur_r >= 2L) "Round 2 Pending" else "not_started"
    }

    tok <- tok_map[[pid]]
    if (is.null(tok) && !is.null(person)) {
      tok <- issue_invite_token(study, person)
    }

    list(
      personId = a$personId,
      name = if (!is.null(person)) (person$name %||% person$email %||% "Expert") else "Expert",
      email = if (!is.null(person)) (person$email %||% "") else "",
      currentRound = cur_r,
      submittedRound = sub_r,
      answeredCount = answered,
      totalQuestions = nq,
      status = status,
      surveyUrl = if (!is.null(person)) expert_survey_url(study, person, tok) else "",
      queryString = if (!is.null(person)) survey_query_string(study, person, tok) else "",
      updatedAt = a$updatedAt
    )
  })
}

save_judgment <- function(study, question, expert_person_id, expert_name, payload, round_number = 1L) {
  now <- iso_now()
  sid <- doc_id(study)
  qid <- doc_id(question)
  db_col("judgments")$update(
    sprintf(
      '{"studyId": %s, "questionId": %s, "expertId": %s, "roundNumber": %s, "isConsensus": false}',
      json_escape(sid), json_escape(qid), json_escape(expert_person_id), as.integer(round_number)
    ),
    '{"$set": {"isCurrent": false}}',
    multiple = TRUE
  )
  db_insert("judgments", list(
    studyId = sid,
    questionId = qid,
    expertId = expert_person_id,
    expertName = expert_name,
    roundNumber = as.integer(round_number),
    isConsensus = FALSE,
    version = 1L,
    isCurrent = TRUE,
    payload = payload,
    rationale = payload$rationale %||% NULL,
    elicitedAt = now,
    createdAt = now
  ))
}

current_judgments <- function(study_id, question_id, round_number = 1L) {
  db_all("judgments", sprintf(
    '{"studyId": %s, "questionId": %s, "roundNumber": %s, "isCurrent": true, "isConsensus": false}',
    json_escape(study_id), json_escape(question_id), as.integer(round_number)
  ))
}

get_expert_judgment <- function(study_id, question_id, expert_id, round_number = 1L) {
  docs <- db_all("judgments", sprintf(
    '{"studyId": %s, "questionId": %s, "expertId": %s, "roundNumber": %s, "isCurrent": true, "isConsensus": false}',
    json_escape(study_id), json_escape(question_id), json_escape(expert_id), as.integer(round_number)
  ))
  if (length(docs)) docs[[1]] else NULL
}

get_expert_judgments_for_study <- function(study_id, expert_id) {
  db_all("judgments", sprintf(
    '{"studyId": %s, "expertId": %s, "isCurrent": true, "isConsensus": false}',
    json_escape(study_id), json_escape(expert_id)
  ))
}


submit_expert_survey <- function(study_id, person_id, user = NULL, round_number = NULL) {
  now <- iso_now()
  access <- db_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "accessRole": "expert"}',
    json_escape(study_id), json_escape(person_id)
  ))
  if (!is.null(access)) {
    cur_r <- suppressWarnings(as.integer(access$currentRound %||% 1L))
    rnd <- if (!is.null(round_number)) suppressWarnings(as.integer(round_number)) else cur_r
    if (is.na(rnd) || rnd < 1L) rnd <- 1L
    db_update("study_access", q_id(doc_id(access)), list(
      status = "submitted",
      currentRound = rnd,
      submittedRound = rnd,
      submittedAt = now,
      updatedAt = now
    ))
  }
  invisible(TRUE)
}

advance_round <- function(study) {
  r <- current_round(study) + 1L
  cfg <- study$protocolConfig %||% list()
  cfg$currentRound <- r
  now <- iso_now()
  db_update("studies", q_id(doc_id(study)), list(
    protocolConfig = cfg,
    status = "workshop",
    updatedAt = now
  ))
  # Advance all participating experts who completed Round 1 (or prior rounds) to the new round
  sid <- doc_id(study)
  access_list <- db_all("study_access", sprintf(
    '{"studyId": %s, "accessRole": "expert"}',
    json_escape(sid)
  ))
  for (acc in access_list) {
    s_val <- as.character(acc$status %||% "active")
    if (s_val %in% c("removed", "revoked", "inactive")) next
    sub_r <- suppressWarnings(as.integer(acc$submittedRound %||% 0L))
    if (is.na(sub_r)) sub_r <- 0L
    if (identical(s_val, "submitted") || sub_r >= (r - 1L)) {
      db_update("study_access", q_id(doc_id(acc)), list(
        currentRound = r,
        status = "active",
        updatedAt = now
      ))
    }
  }
  find_study(doc_id(study))
}

run_shelf_for_question <- function(study, question, round_number = 1L, family = "best",
                                   anonymize = NULL, weights = NULL) {
  js <- current_judgments(doc_id(study), doc_id(question), round_number)
  experts <- list()
  ids <- character()
  for (j in js) {
    qmap <- quantiles_from_payload(j$payload)
    if (is.null(qmap)) next
    eid <- as.character(j$expertId %||% j$expertName %||% "expert")
    ids <- c(ids, eid)
    experts[[length(experts) + 1]] <- list(
      id = eid,
      name = j$expertName %||% eid,
      rationale = j$rationale %||% j$payload$rationale %||% "",
      quantiles = qmap
    )
  }
  if (!length(experts)) stop("No current judgments with quantiles for this question.")
  lo <- as.numeric(question$lowerBound %||% 0)
  hi <- as.numeric(question$upperBound %||% 1)
  bds <- lapply(js, function(j) {
    list(lo = j$payload$lowerBound, hi = j$payload$upperBound)
  })
  if (length(bds)) {
    los <- suppressWarnings(as.numeric(vapply(bds, function(b) b$lo %||% NA_real_, numeric(1))))
    his <- suppressWarnings(as.numeric(vapply(bds, function(b) b$hi %||% NA_real_, numeric(1))))
    if (any(is.finite(los))) lo <- min(c(lo, los[is.finite(los)]))
    if (any(is.finite(his))) hi <- max(c(hi, his[is.finite(his)]))
  }
  probs <- payload_probs(js[[1]]$payload)
  do_anon <- if (is.null(anonymize)) isTRUE((study$protocolConfig %||% list())$anonymizeFeedback) else isTRUE(anonymize)
  mapping <- blind_labels_for_ids(ids)
  if (do_anon) {
    for (i in seq_along(experts)) {
      experts[[i]]$realName <- experts[[i]]$name
      experts[[i]]$name <- blind_label(experts[[i]]$id, mapping)
    }
  }
  fit <- run_shelf_fit(experts, lo = lo, hi = hi, probs = probs, preferred = family, weights = weights)
  fit$anonymized <- do_anon
  fit$labelMap <- as.list(mapping)
  fit$questionId <- doc_id(question)
  fit$roundNumber <- as.integer(round_number)
  conclusion <- build_conclusion(fit, question$title %||% question$code, length(experts))
  params <- shelf_params_table(fit)
  agg <- db_insert("aggregations", list(
    studyId = doc_id(study),
    questionId = doc_id(question),
    roundNumber = as.integer(round_number),
    engine = fit$engine,
    shelfVersion = fit$shelfVersion,
    nExperts = length(experts),
    result = fit,
    params = params,
    conclusion = conclusion,
    createdAt = iso_now()
  ))
  list(
    fit = fit, conclusion = conclusion, aggregationId = doc_id(agg),
    nExperts = length(experts), params = params, studyId = doc_id(study),
    questionId = doc_id(question), roundNumber = as.integer(round_number)
  )
}

latest_aggregation <- function(study_id, question_id, round_number = NULL) {
  q <- sprintf('{"studyId": %s, "questionId": %s}', json_escape(study_id), json_escape(question_id))
  if (!is.null(round_number)) {
    q <- sprintf(
      '{"studyId": %s, "questionId": %s, "roundNumber": %s}',
      json_escape(study_id), json_escape(question_id), as.integer(round_number)
    )
  }
  docs <- db_all("aggregations", q, sort = '{"createdAt": -1}')
  if (!length(docs)) return(NULL)
  docs[[1]]
}

aggregation_as_shelf_result <- function(agg) {
  if (is.null(agg)) return(NULL)
  params <- agg$params
  if (!is.null(params) && !is.data.frame(params)) {
    params <- tryCatch(as.data.frame(params, stringsAsFactors = FALSE), error = function(e) NULL)
  }
  if (is.null(params) && !is.null(agg$result)) {
    params <- tryCatch(shelf_params_table(agg$result), error = function(e) NULL)
  }
  list(
    fit = agg$result,
    conclusion = agg$conclusion,
    aggregationId = doc_id(agg),
    nExperts = agg$nExperts %||% length(agg$result$experts),
    params = params,
    studyId = agg$studyId,
    questionId = agg$questionId,
    roundNumber = agg$roundNumber
  )
}

survey_url <- function(study) {
  pub <- ee_survey_public_url()
  slug_or_id <- utils::URLencode(as.character(study$slug %||% doc_id(study)), reserved = TRUE)
  if (!nzchar(pub)) return(sprintf("?study=%s", slug_or_id))
  sprintf("%s/?study=%s", sub("/+$", "", pub), slug_or_id)
}

study_response_count <- function(study_id, round_number = 1L) {
  access <- db_all("study_access", sprintf(
    '{"studyId": %s, "accessRole": "expert"}',
    json_escape(study_id)
  ))
  valid_access <- Filter(function(a) {
    s_val <- as.character(a$status %||% "active")
    !s_val %in% c("removed", "revoked", "inactive")
  }, access)
  ne <- length(valid_access)
  js <- db_all("judgments", sprintf(
    '{"studyId": %s, "roundNumber": %s, "isCurrent": true, "isConsensus": false}',
    json_escape(study_id), as.integer(round_number)
  ))
  expert_ids <- unique(vapply(js, function(j) as.character(j$expertId %||% ""), character(1)))
  expert_ids <- expert_ids[nzchar(expert_ids)]
  list(submitted = length(expert_ids), total = ne)
}

list_available_facilitators <- function() {
  db_all("people", '{"personType": "facilitator", "isActive": true}', sort = '{"name": 1}')
}

study_facilitators <- function(study) {
  sid <- doc_id(study)
  access <- db_all("study_access", sprintf(
    '{"studyId": %s, "accessRole": "facilitator", "status": "active"}',
    json_escape(sid)
  ))
  owner_id <- as.character(study$ownerId %||% "")
  res <- lapply(access, function(a) {
    p <- db_one("people", q_id(a$personId))
    is_owner <- identical(as.character(a$userId %||% ""), owner_id) ||
                (!is.null(p) && identical(as.character(p$userId %||% ""), owner_id))
    list(
      accessId = doc_id(a),
      personId = a$personId,
      name = if (!is.null(p)) p$name %||% "Facilitator" else "Facilitator",
      email = if (!is.null(p)) p$email %||% "" else "",
      affiliation = if (!is.null(p)) p$affiliation %||% "" else "",
      isOwner = is_owner,
      grantedAt = a$createdAt %||% ""
    )
  })
  has_owner <- any(vapply(res, function(x) isTRUE(x$isOwner), logical(1)))
  if (!has_owner && nzchar(owner_id)) {
    owner_user <- db_one("users", q_id(owner_id))
    owner_p <- if (!is.null(owner_user$personId)) db_one("people", q_id(owner_user$personId)) else NULL
    if (is.null(owner_p) && !is.null(owner_user$email)) owner_p <- find_person_by_email(owner_user$email)
    if (!is.null(owner_p)) {
      res <- c(list(list(
        accessId = paste0("owner_", owner_id),
        personId = doc_id(owner_p),
        name = owner_p$name %||% owner_user$displayName %||% "Primary Facilitator",
        email = owner_p$email %||% owner_user$email %||% "",
        affiliation = owner_p$affiliation %||% "",
        isOwner = TRUE,
        grantedAt = study$createdAt %||% ""
      )), res)
    }
  }
  res
}

assign_study_facilitator <- function(study, person_id, user) {
  require_role(can_assign_facilitator(user, study), "You do not have permission to assign facilitators to this study.")
  person <- db_one("people", q_id(person_id))
  if (is.null(person)) stop("Facilitator record not found.")
  if (!identical(as.character(person$personType %||% ""), "facilitator")) {
    stop("Person is not registered as a facilitator.")
  }
  sid <- doc_id(study)
  org <- ensure_org()
  now <- iso_now()
  existing <- db_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "accessRole": "facilitator"}',
    json_escape(sid), json_escape(person_id)
  ))
  if (!is.null(existing)) {
    db_update("study_access", q_id(doc_id(existing)), list(
      status = "active",
      userId = person$userId %||% existing$userId,
      updatedAt = now,
      grantedBy = user$id
    ))
  } else {
    db_insert("study_access", list(
      studyId = sid,
      orgId = doc_id(org),
      personId = person_id,
      userId = person$userId %||% "",
      accessRole = "facilitator",
      status = "active",
      notes = "Assigned facilitator",
      grantedBy = user$id,
      createdAt = now,
      updatedAt = now
    ))
  }
  invisible(TRUE)
}

remove_study_facilitator <- function(study, person_id, user) {
  require_role(can_assign_facilitator(user, study), "You do not have permission to remove facilitators from this study.")
  person <- db_one("people", q_id(person_id))
  owner_id <- as.character(study$ownerId %||% "")
  if (!is.null(person) && identical(as.character(person$userId %||% ""), owner_id)) {
    stop("Cannot remove the primary owner of the case study.")
  }
  access <- db_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "accessRole": "facilitator", "status": "active"}',
    json_escape(doc_id(study)), json_escape(person_id)
  ))
  if (is.null(access)) stop("This facilitator is not assigned to this study.")
  if (identical(as.character(access$userId %||% ""), owner_id)) {
    stop("Cannot remove the primary owner of the case study.")
  }
  db_update("study_access", q_id(doc_id(access)), list(
    status = "removed",
    updatedAt = iso_now(),
    removedBy = user$id
  ))
  invisible(TRUE)
}
