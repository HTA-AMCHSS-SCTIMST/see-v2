onboarding_key_query <- function(study_id, person_id) {
  sprintf(
    '{"studyId": %s, "personId": %s}',
    json_escape(study_id),
    json_escape(person_id)
  )
}

load_onboarding <- function(study_id, person_id) {
  db_one("onboarding", onboarding_key_query(study_id, person_id))
}

onboarding_complete <- function(doc) {
  isTRUE(doc$touAccepted)
}

save_onboarding <- function(study, person_id, tou, coi_financial = "None", coi_academic = "None", attribution = "anonymous") {
  if (!isTRUE(tou)) stop("Please check the box to agree to participant consent before continuing.")
  attr <- if (identical(attribution, "attributed")) "attributed" else "anonymous"
  fin <- trimws(coi_financial %||% "None")
  aca <- trimws(coi_academic %||% "None")
  now <- iso_now()
  sid <- doc_id(study)
  existing <- load_onboarding(sid, person_id)
  fields <- list(
    studyId = sid,
    personId = person_id,
    coiFinancial = fin,
    coiAcademic = aca,
    coiDeclared = TRUE,
    touAccepted = TRUE,
    attribution = attr,
    calibrationDone = TRUE,
    updatedAt = now
  )
  if (is.null(existing)) {
    fields$createdAt <- now
    db_insert("onboarding", fields)
  } else {
    db_update("onboarding", q_id(doc_id(existing)), fields)
  }
  load_onboarding(sid, person_id)
}

save_calibration <- function(study, person_id, practice_value) {
  v <- as.numeric(practice_value)
  if (!is.finite(v) || v < 0 || v > 1) {
    stop("Enter a probability between 0 and 1 for the practice task.")
  }
  now <- iso_now()
  sid <- doc_id(study)
  existing <- load_onboarding(sid, person_id)
  if (is.null(existing) || !isTRUE(existing$touAccepted)) {
    stop("Complete conflict-of-interest and terms of use first.")
  }
  db_update("onboarding", q_id(doc_id(existing)), list(
    calibrationDone = TRUE,
    calibrationValue = v,
    updatedAt = now
  ))
  load_onboarding(sid, person_id)
}

load_bounds <- function(study_id, person_id, round_number) {
  db_one("elicitation_bounds", sprintf(
    '{"studyId": %s, "personId": %s, "roundNumber": %s}',
    json_escape(study_id),
    json_escape(person_id),
    as.integer(round_number)
  ))
}

save_bounds <- function(study, person_id, lo, hi, round_number = 1L) {
  lo <- as.numeric(lo)
  hi <- as.numeric(hi)
  if (!is.finite(lo) || !is.finite(hi) || !(lo < hi)) {
    stop("Upper plausible limit must be greater than the lower limit.")
  }
  now <- iso_now()
  sid <- doc_id(study)
  existing <- load_bounds(sid, person_id, round_number)
  fields <- list(
    studyId = sid,
    personId = person_id,
    roundNumber = as.integer(round_number),
    lower = lo,
    upper = hi,
    updatedAt = now
  )
  if (is.null(existing)) {
    fields$createdAt <- now
    db_insert("elicitation_bounds", fields)
  } else {
    db_update("elicitation_bounds", q_id(doc_id(existing)), fields)
  }
  load_bounds(sid, person_id, round_number)
}

require_onboarding <- function(study, person_id) {
  doc <- load_onboarding(doc_id(study), person_id)
  if (!onboarding_complete(doc)) {
    stop("Please accept participant consent before continuing.")
  }
  invisible(doc)
}
