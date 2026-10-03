new_invite_secret <- function() {
  paste(sample(c(0:9, letters, LETTERS), 32, replace = TRUE), collapse = "")
}

token_expires_iso <- function(days = 90) {
  format(as.POSIXct(Sys.time(), tz = "UTC") + days * 86400, "%Y-%m-%dT%H:%M:%SZ")
}

issue_invite_token <- function(study, person, days = 90) {
  now <- iso_now()
  existing <- db_one("invite_tokens", sprintf(
    '{"studyId": %s, "personId": %s, "revoked": false}',
    json_escape(doc_id(study)),
    json_escape(doc_id(person))
  ))
  if (!is.null(existing) && nzchar(existing$token %||% "")) return(existing)
  db_insert("invite_tokens", list(
    token = new_invite_secret(),
    studyId = doc_id(study),
    personId = doc_id(person),
    email = person$email,
    revoked = FALSE,
    expiresAt = token_expires_iso(days),
    createdAt = now
  ))
}

find_invite_token <- function(secret) {
  secret <- trimws(as.character(secret %||% ""))
  if (!nzchar(secret)) return(NULL)
  db_one("invite_tokens", sprintf('{"token": %s, "revoked": false}', json_escape(secret)))
}

survey_query_string <- function(study, person = NULL, token_doc = NULL) {
  slug_or_id <- utils::URLencode(as.character(study$slug %||% doc_id(study)), reserved = TRUE)
  base <- sprintf("?study=%s", slug_or_id)
  tok <- token_doc
  if (is.null(tok) && !is.null(person)) tok <- issue_invite_token(study, person)
  if (!is.null(tok) && !is.null(tok$token) && nzchar(as.character(tok$token))) {
    base <- paste0(base, "&t=", utils::URLencode(as.character(tok$token), reserved = TRUE))
  }
  base
}

expert_survey_url <- function(study, person = NULL, token_doc = NULL) {
  pub <- ee_survey_public_url()
  qs <- survey_query_string(study, person, token_doc)
  if (!nzchar(pub)) return(qs)
  sprintf("%s/%s", sub("/+$", "", pub), qs)
}

survey_entry_token <- function(secret) {
  secret <- trimws(as.character(secret %||% ""))
  tok <- find_invite_token(secret)
  if (is.null(tok)) stop("This invite link is invalid or has been revoked.")
  study <- find_study(tok$studyId)
  if (is.null(study)) stop("Case study not found.")
  person <- db_one("people", q_id(tok$personId))
  if (is.null(person) && nzchar(tok$email %||% "")) {
    person <- find_person_by_email(tok$email)
  }
  if (is.null(person) || identical(person$isActive, FALSE)) {
    stop("This expert account is not invited or has been deactivated.")
  }
  survey_entry(doc_id(study), person$email)
}
