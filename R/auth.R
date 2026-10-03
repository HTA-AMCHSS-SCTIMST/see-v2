ROLES <- c("admin", "facilitator", "expert")

role_label <- function(role) {
  switch(
    role %||% "",
    admin = "Admin",
    facilitator = "Facilitator",
    expert = "Expert",
    super_admin = "Admin",
    "User"
  )
}

staff_roles <- function() c("admin", "super_admin", "facilitator")

is_staff_role <- function(role) role %in% staff_roles()

.ee_auth_cache <- new.env(parent = emptyenv())

ensure_org <- function() {
  if (!is.null(.ee_auth_cache$org)) return(.ee_auth_cache$org)
  org <- db_one("organizations", '{"slug": "sctimst"}')
  if (!is.null(org)) {
    .ee_auth_cache$org <- org
    return(org)
  }
  res <- db_insert("organizations", list(
    name = "Sree Chitra Tirunal Institute for Medical Sciences & Technology",
    slug = "sctimst",
    settings = list(allowExpertSelfSignup = FALSE, defaultProtocol = "shelf"),
    createdAt = iso_now(),
    updatedAt = iso_now()
  ))
  .ee_auth_cache$org <- res
  res
}

find_user_by_email <- function(email) {
  db_one("users", q_field("email", tolower(trimws(email))))
}

find_person_by_email <- function(email) {
  email <- tolower(trimws(email %||% ""))
  if (!nzchar(email)) return(NULL)
  p <- db_one("people", sprintf('{"email": %s}', json_escape(email)))
  if (is.null(p) || identical(p$isActive, FALSE)) NULL else p
}

link_person_to_user <- function(user_id, email) {
  person <- find_person_by_email(email)
  if (is.null(person)) return(NULL)
  now <- iso_now()
  db_update("people", q_id(doc_id(person)), list(
    userId = user_id,
    inviteStatus = "accepted",
    updatedAt = now
  ))
  db_col("study_access")$update(
    sprintf('{"personId": %s, "status": "active"}', json_escape(doc_id(person))),
    sprintf('{"$set": {"userId": %s, "updatedAt": %s}}', json_escape(user_id), json_escape(now)),
    multiple = TRUE
  )
  db_update("users", q_id(user_id), list(personId = doc_id(person), updatedAt = now))
  doc_id(person)
}

dev_login <- function(email, display_name, platform_role) {
  stopifnot(ee_auth_dev_mode())
  v_res <- validate_email(email)
  if (!v_res$ok) {
    stop(v_res$message, call. = FALSE)
  }
  email <- v_res$clean
  role <- if (platform_role %in% ROLES) platform_role else "expert"
  org <- ensure_org()
  now <- iso_now()
  existing <- find_user_by_email(email)
  if (!is.null(existing)) {
    db_update("users", q_id(doc_id(existing)), list(
      displayName = display_name,
      platformRole = role,
      lastLoginAt = now,
      updatedAt = now,
      isActive = TRUE
    ))
    user_id <- doc_id(existing)
  } else {
    doc <- db_insert("users", list(
      email = email,
      displayName = display_name,
      photoURL = NULL,
      authProvider = "dev",
      platformRole = role,
      orgId = doc_id(org),
      personId = NULL,
      isActive = TRUE,
      lastLoginAt = now,
      createdAt = now,
      updatedAt = now
    ))
    user_id <- doc_id(doc)
  }
  link_person_to_user(user_id, email)
  find_user_by_email(email)
}

connect_login <- function(username) {
  email <- tolower(trimws(username))
  if (!grepl("@", email)) email <- paste0(email, "@sctimst.ac.in")
  existing <- find_user_by_email(email)
  if (!is.null(existing)) return(existing)
  org <- ensure_org()
  person <- find_person_by_email(email)
  role <- if (!is.null(person)) person$personType %||% "expert" else "expert"
  now <- iso_now()
  doc <- db_insert("users", list(
    email = email,
    displayName = person$name %||% username,
    authProvider = "posit-connect",
    platformRole = role,
    orgId = doc_id(org),
    personId = if (!is.null(person)) doc_id(person) else NULL,
    isActive = TRUE,
    lastLoginAt = now,
    createdAt = now,
    updatedAt = now
  ))
  link_person_to_user(doc_id(doc), email)
  find_user_by_email(email)
}

oidc_login <- function(code) {
  discovery <- ee_oidc_discovery()
  token <- httr2::request(discovery$token_endpoint) |>
    httr2::req_body_form(
      grant_type = "authorization_code",
      code = code,
      client_id = ee_env("OIDC_CLIENT_ID"),
      client_secret = ee_env("OIDC_CLIENT_SECRET"),
      redirect_uri = ee_oidc_callback_url()
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  access_token <- token$access_token %||% ""
  if (!nzchar(access_token)) stop("OIDC provider did not return an access token.", call. = FALSE)
  identity <- httr2::request(discovery$userinfo_endpoint) |>
    httr2::req_headers(Authorization = paste("Bearer", access_token)) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  email <- tolower(trimws(identity$email %||% ""))
  if (!nzchar(email) || !isTRUE(identity$email_verified %||% FALSE)) {
    stop("The OIDC provider did not verify an email address.", call. = FALSE)
  }
  existing <- find_user_by_email(email)
  person <- find_person_by_email(email)
  if (is.null(existing) && is.null(person)) {
    stop("This account is not authorized for the platform.", call. = FALSE)
  }
  if (is.null(existing)) {
    now <- iso_now()
    existing <- db_insert("users", list(
      email = email,
      displayName = identity$name %||% identity$preferred_username %||% email,
      authProvider = "oidc",
      platformRole = person$personType %||% "expert",
      orgId = person$orgId,
      personId = doc_id(person),
      isActive = TRUE,
      lastLoginAt = now,
      createdAt = now,
      updatedAt = now
    ))
  } else if (!isTRUE(existing$isActive %||% FALSE)) {
    stop("This account is inactive.", call. = FALSE)
  }
  db_update("users", q_id(doc_id(existing)), list(
    displayName = identity$name %||% existing$displayName %||% email,
    lastLoginAt = iso_now(),
    updatedAt = iso_now(),
    authProvider = "oidc",
    isActive = TRUE
  ))
  link_person_to_user(doc_id(existing), email)
  find_user_by_email(email)
}

survey_entry <- function(study_id_or_slug, email) {
  email <- tolower(trimws(email %||% ""))
  if (!nzchar(email)) stop("Please provide an email address.")
  study <- find_study(study_id_or_slug)
  if (is.null(study)) stop("Case study not found.")
  if (identical(study$status %||% "", "archived")) {
    stop("This survey has been archived and no longer accepts responses.")
  }
  person <- find_person_by_email(email)
  if (is.null(person)) {
    stop("This email is not invited. Ask the facilitator to add your email.")
  }
  sid <- doc_id(study)
  pid <- doc_id(person)
  all_acc <- db_all("study_access", sprintf(
    '{"studyId": %s, "personId": %s}',
    json_escape(sid),
    json_escape(pid)
  ))
  valid_acc <- Filter(function(a) {
    s <- as.character(a$status %||% "active")
    !s %in% c("removed", "revoked", "inactive")
  }, all_acc)
  if (!length(valid_acc)) {
    user_exist <- find_user_by_email(email)
    if (!is.null(user_exist)) {
      u_acc <- db_all("study_access", sprintf(
        '{"studyId": %s, "userId": %s}',
        json_escape(sid),
        json_escape(doc_id(user_exist))
      ))
      valid_acc <- Filter(function(a) {
        s <- as.character(a$status %||% "active")
        !s %in% c("removed", "revoked", "inactive")
      }, u_acc)
    }
  }
  if (!length(valid_acc)) {
    tok_check <- db_one("invite_tokens", sprintf(
      '{"studyId": %s, "personId": %s, "revoked": false}',
      json_escape(sid),
      json_escape(pid)
    ))
    if (!is.null(tok_check)) {
      now_iso <- iso_now()
      db_insert("study_access", list(
        studyId = sid,
        orgId = person$orgId,
        personId = pid,
        accessRole = "expert",
        status = "active",
        remindersSent = 0,
        remindersTotal = 3,
        notes = "Auto-healed from active invite token",
        createdAt = now_iso,
        updatedAt = now_iso
      ))
      valid_acc <- list(list(studyId = sid, personId = pid, status = "active"))
    }
  }
  if (!length(valid_acc)) {
    stop("This email is not invited to this case study.")
  }
  access <- valid_acc[[1]]
  now <- iso_now()
  existing <- find_user_by_email(email)
  if (!is.null(existing)) {
    db_update("users", q_id(doc_id(existing)), list(
      displayName = person$name %||% existing$displayName %||% email,
      personId = pid,
      lastLoginAt = now,
      updatedAt = now,
      isActive = TRUE
    ))
    user <- find_user_by_email(email)
  } else {
    user <- db_insert("users", list(
      email = email,
      displayName = person$name %||% email,
      authProvider = "survey-invite",
      platformRole = "expert",
      orgId = person$orgId,
      personId = pid,
      isActive = TRUE,
      lastLoginAt = now,
      createdAt = now,
      updatedAt = now
    ))
  }
  link_person_to_user(doc_id(user), email)
  list(user = find_user_by_email(email), study = study)
}

user_as_list <- function(u) {
  if (is.null(u)) return(NULL)
  list(
    id = doc_id(u),
    email = u$email,
    displayName = u$displayName %||% u$email,
    platformRole = u$platformRole %||% "expert",
    orgId = u$orgId,
    personId = u$personId,
    isActive = isTRUE(u$isActive)
  )
}
