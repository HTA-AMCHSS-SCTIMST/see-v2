user_role <- function(user) {
  if (is.null(user)) return("")
  as.character(user$platformRole %||% "")
}

is_admin <- function(user) user_role(user) %in% c("admin", "super_admin")
is_facilitator <- function(user) identical(user_role(user), "facilitator")
is_viewer <- function(user) FALSE
is_expert_role <- function(user) identical(user_role(user), "expert")

study_status <- function(study) as.character(study$status %||% "draft")

study_is_active_deliberation <- function(study) {
  study_status(study) %in% c("draft", "recruiting", "eliciting", "workshop")
}

study_is_completed <- function(study) identical(study_status(study), "completed")

can_provision_people <- function(user) is_admin(user)
can_create_study <- function(user) is_facilitator(user) || is_admin(user)
can_seed_demo <- function(user) is_facilitator(user) || is_admin(user)
can_invite <- function(user, study = NULL) {
  is_facilitator(user) && (is.null(study) || can_manage_study(user, study))
}
can_advance_round <- function(user, study = NULL) {
  is_facilitator(user) && (is.null(study) || can_manage_study(user, study))
}
can_complete_study <- function(user, study = NULL) {
  is_facilitator(user) && (is.null(study) || can_manage_study(user, study))
}

can_manage_study <- function(user, study) {
  if (is.null(user) || is.null(study)) return(FALSE)
  if (is_admin(user)) return(TRUE)
  if (!is_facilitator(user)) return(FALSE)
  if (identical(as.character(study$ownerId %||% ""), as.character(user$id %||% ""))) return(TRUE)
  sid <- doc_id(study)
  fac_acc <- db_one("study_access", sprintf(
    '{"studyId": %s, "accessRole": "facilitator", "status": "active", "$or": [{"userId": %s}, {"personId": %s}]}',
    json_escape(sid),
    json_escape(as.character(user$id %||% "")),
    json_escape(as.character(user$personId %||% ""))
  ))
  !is.null(fac_acc)
}

can_assign_facilitator <- function(user, study = NULL) {
  is_admin(user) || (is_facilitator(user) && (is.null(study) || can_manage_study(user, study)))
}

require_study_manager <- function(user, study, action = "manage this study") {
  require_role(can_manage_study(user, study), paste("You cannot", action, "because it is not assigned to you."))
}

remove_study_expert <- function(study, person_id, user) {
  require_study_manager(user, study, "remove experts from this study")
  access <- db_one("study_access", sprintf(
    '{"studyId": %s, "personId": %s, "accessRole": "expert", "status": "active"}',
    json_escape(doc_id(study)), json_escape(person_id)
  ))
  if (is.null(access)) stop("This expert is not assigned to the study.")
  db_update("study_access", q_id(doc_id(access)), list(
    status = "removed", updatedAt = iso_now(), removedBy = user$id
  ))
  invisible(TRUE)
}

archive_study <- function(study, user) {
  require_study_manager(user, study, "archive this study")
  db_update("studies", q_id(doc_id(study)), list(
    status = "archived", updatedAt = iso_now(), archivedAt = iso_now(), archivedBy = user$id
  ))
  find_study(doc_id(study))
}

update_study_details <- function(study, title, description, user) {
  require_study_manager(user, study, "edit this study")
  title <- trimws(title %||% "")
  if (!nzchar(title)) stop("Enter a study title.")
  db_update("studies", q_id(doc_id(study)), list(
    title = title, description = description %||% "", updatedAt = iso_now()
  ))
  find_study(doc_id(study))
}

can_run_shelf <- function(user, study = NULL) {
  if (is.null(user)) return(FALSE)
  is_facilitator(user) || is_admin(user)
}

can_view_shelf <- function(user, study) {
  if (is.null(user) || is.null(study)) return(FALSE)
  if (is_facilitator(user) || is_admin(user)) return(TRUE)
  FALSE
}

can_export_audit <- function(user, study) can_view_shelf(user, study)

require_role <- function(ok, msg = "You do not have permission for this action.") {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
  invisible(TRUE)
}

add_person <- function(name, email, person_type, affiliation = "", user) {
  require_role(can_provision_people(user))
  v_res <- validate_email(email)
  if (!v_res$ok) {
    stop(v_res$message, call. = FALSE)
  }
  email <- v_res$clean
  person_type <- match.arg(person_type, c("admin", "facilitator", "expert"))
  org <- ensure_org()
  now <- iso_now()
  existing <- find_person_by_email(email)
  if (!is.null(existing)) {
    db_update("people", q_id(doc_id(existing)), list(
      name = name %||% existing$name,
      personType = person_type,
      affiliation = affiliation %||% existing$affiliation %||% "",
      isActive = TRUE,
      updatedAt = now
    ))
    return(find_person_by_email(email))
  }
  db_insert("people", list(
    orgId = doc_id(org),
    personType = person_type,
    name = name %||% email,
    email = email,
    affiliation = affiliation,
    expertise = list(),
    tags = list(person_type),
    inviteStatus = if (person_type == "facilitator") "active" else "invited",
    isActive = TRUE,
    createdAt = now,
    updatedAt = now,
    createdBy = user$id
  ))
}

complete_study <- function(study, user) {
  require_study_manager(user, study, "complete this study")
  db_update("studies", q_id(doc_id(study)), list(
    status = "completed",
    updatedAt = iso_now()
  ))
  find_study(doc_id(study))
}
