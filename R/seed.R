seed_demo <- function(user) {
  org <- ensure_org()
  now <- iso_now()
  people_specs <- list(
    list(personType = "facilitator", name = "Dr. Ananya Menon", email = "ananya.menon@sctimst.ac.in",
         affiliation = "AMCHSS, SCTIMST"),
    list(personType = "expert", name = "Dr. Priya Nair", email = "priya.nair@hospital.org",
         affiliation = "Regional Cancer Centre (RCC), Trivandrum"),
    list(personType = "expert", name = "Dr. Rahul Kurian", email = "rahul.kurian@hospital.org",
         affiliation = "Govt. Medical College, Kottayam"),
    list(personType = "expert", name = "Dr. Lakshmi Pillai", email = "lakshmi.pillai@hospital.org",
         affiliation = "Amrita Institute of Medical Sciences, Kochi"),
    list(personType = "expert", name = "Dr. Arun Varma", email = "arun.varma@hospital.org",
         affiliation = "Malabar Cancer Centre, Thalassery")
  )
  person_ids <- list()
  created <- 0L

  # Auto-migrate any legacy non-Malayali seed demo records to Malayali names
  legacy_map <- list(
    "rahul.das@hospital.org" = list(email = "rahul.kurian@hospital.org", name = "Dr. Rahul Kurian", affil = "Govt. Medical College, Kottayam"),
    "kavita.rao@hospital.org" = list(email = "lakshmi.pillai@hospital.org", name = "Dr. Lakshmi Pillai", affil = "Amrita Institute of Medical Sciences, Kochi"),
    "arjun.patel@hospital.org" = list(email = "arun.varma@hospital.org", name = "Dr. Arun Varma", affil = "Malabar Cancer Centre, Thalassery"),
    "sam.rivera@sctimst.ac.in" = list(email = "sneha.george@sctimst.ac.in", name = "Sneha George", affil = "AMCHSS, SCTIMST")
  )
  for (old_email in names(legacy_map)) {
    old_p <- find_person_by_email(old_email)
    if (!is.null(old_p)) {
      t_info <- legacy_map[[old_email]]
      p_id <- doc_id(old_p)
      old_name <- old_p$name %||% ""
      db_update("people", q_id(p_id), list(
        name = t_info$name,
        email = t_info$email,
        affiliation = t_info$affil,
        updatedAt = now
      ))
      # Update legacy judgments
      tryCatch({
        js <- db_all("judgments", sprintf('{"expertId": %s}', json_escape(p_id)))
        for (j in js) {
          db_update("judgments", q_id(doc_id(j)), list(expertName = t_info$name, updatedAt = now))
        }
      }, error = function(e) NULL)
    }
  }

  for (spec in people_specs) {
    existing <- find_person_by_email(spec$email)
    if (!is.null(existing)) {
      person_ids[[spec$email]] <- doc_id(existing)
      next
    }
    p <- db_insert("people", c(spec, list(
      orgId = doc_id(org),
      expertise = list(),
      tags = list(),
      inviteStatus = "invited",
      isActive = TRUE,
      createdAt = now,
      updatedAt = now
    )))
    person_ids[[spec$email]] <- doc_id(p)
    created <- created + 1L
  }

  study <- db_one("studies", '{"slug": "hta-drug-a-vs-b"}')
  if (is.null(study)) {
    study <- db_insert("studies", list(
      orgId = doc_id(org),
      slug = "hta-drug-a-vs-b",
      title = "HTA: Drug A vs Drug B — 5-year durability",
      description = "SHELF elicitation for 5-year treatment durability where 12-month trial data exist but long-term outcomes are uncertain.",
      protocolType = "shelf",
      status = "recruiting",
      ownerId = user$id,
      surveyAccessMode = "invited_only",
      protocolConfig = list(
        currentRound = 1L,
        rounds = 2L,
        anonymizeFeedback = TRUE,
        methods = list("chips_and_bins", "quantile"),
        surveyWelcome = list(
          conductedBy = "Achutha Menon Centre for Health Science Studies (AMCHSS), SCTIMST, Trivandrum",
          why = "12-month trial data for Drug A vs Drug B exist, but 5-year durability is unknown and needed for HTA.",
          reason = "SHELF expert elicitation turns specialist judgment into structured probabilities for decision modelling.",
          instructions = "Task 1 uses chips-and-bins. Task 2 asks for P10, P50, and P90. Place all chips, then submit.",
          quantityOfInterest = "Probability of remaining progression-free at 5 years on Drug A",
          contactEmail = user$email
        )
      ),
      createdAt = now,
      updatedAt = now
    ))
  }
  sid <- doc_id(study)

  if (db_count("questions", sprintf('{"studyId": %s, "code": "Q1_CHIPS_DURABILITY"}', json_escape(sid))) == 0) {
    db_insert("questions", list(
      studyId = sid,
      code = "Q1_CHIPS_DURABILITY",
      title = "Task 1 — Chips-N-Bins: 5-year durability (Drug A)",
      prompt = "Allocate all chips across probability bins for the chance that a patient starting Drug A remains progression-free at 5 years.",
      variableType = "proportion",
      elicitationMethod = "chips_and_bins",
      unit = "probability",
      lowerBound = 0,
      upperBound = 1,
      judgmentSchema = list(type = "chips_and_bins", binCount = 10L, totalChips = 20L),
      rationaleRequired = TRUE,
      sortOrder = 0L,
      isActive = TRUE,
      createdAt = now,
      updatedAt = now,
      createdBy = user$id
    ))
  }
  if (db_count("questions", sprintf('{"studyId": %s, "code": "Q1_DURABILITY_5Y"}', json_escape(sid))) == 0) {
    db_insert("questions", list(
      studyId = sid,
      code = "Q1_DURABILITY_5Y",
      title = "Task 2 — Low-High-Best: 5-year durability (Drug A)",
      prompt = "What is your belief about the probability that a patient initiating Drug A remains progression-free at 5 years?",
      variableType = "proportion",
      elicitationMethod = "quantile",
      unit = "probability",
      lowerBound = 0,
      upperBound = 1,
      judgmentSchema = list(type = "quantile", levels = list(0.1, 0.5, 0.9), requireMonotonic = TRUE),
      rationaleRequired = TRUE,
      sortOrder = 1L,
      isActive = TRUE,
      createdAt = now,
      updatedAt = now,
      createdBy = user$id
    ))
  }

  access_map <- list(
    list(email = "ananya.menon@sctimst.ac.in", role = "facilitator"),
    list(email = "priya.nair@hospital.org", role = "expert"),
    list(email = "rahul.kurian@hospital.org", role = "expert"),
    list(email = "lakshmi.pillai@hospital.org", role = "expert"),
    list(email = "arun.varma@hospital.org", role = "expert")
  )
  granted <- 0L
  for (row in access_map) {
    pid <- person_ids[[row$email]]
    exists <- db_one("study_access", sprintf(
      '{"studyId": %s, "personId": %s, "accessRole": %s, "status": "active"}',
      json_escape(sid), json_escape(pid), json_escape(row$role)
    ))
    if (!is.null(exists)) next
    db_insert("study_access", list(
      studyId = sid,
      orgId = doc_id(org),
      personId = pid,
      accessRole = row$role,
      status = "active",
      notes = "Seeded demo",
      createdAt = now,
      updatedAt = now
    ))
    granted <- granted + 1L
  }

  for (row in access_map) {
    if (!identical(row$role, "expert")) next
    pid <- person_ids[[row$email]]
    person <- db_one("people", q_id(pid))
    if (!is.null(person)) issue_invite_token(find_study(sid), person)
  }

  dummy <- seed_dummy_shelf_judgments(find_study(sid), person_ids)

  first_pid <- person_ids[["priya.nair@hospital.org"]]
  first_person <- if (!is.null(first_pid)) db_one("people", q_id(first_pid)) else NULL
  demo_url <- if (!is.null(first_person)) expert_survey_url(find_study(sid), first_person) else survey_url(find_study(sid))

  list(
    studyId = sid,
    slug = "hta-drug-a-vs-b",
    peopleCreated = created,
    accessGranted = granted,
    dummyJudgments = dummy$n,
    surveyUrl = demo_url,
    expertEmails = c(
      "priya.nair@hospital.org",
      "rahul.kurian@hospital.org",
      "lakshmi.pillai@hospital.org",
      "arun.varma@hospital.org"
    )
  )
}

# Varied dummy SHELF judgements (round 1) so facilitators can click Run SHELF.
seed_dummy_shelf_judgments <- function(study, person_ids = NULL) {
  if (is.null(study)) return(list(n = 0L))
  sid <- doc_id(study)
  qs <- study_questions(sid)
  q_chips <- Filter(is_chips_question, qs)
  q_quant <- Filter(function(q) !is_chips_question(q), qs)
  q_chips <- if (length(q_chips)) q_chips[[1]] else NULL
  q_quant <- if (length(q_quant)) q_quant[[1]] else NULL

  dummy <- list(
    list(
      email = "priya.nair@hospital.org",
      name = "Dr. Priya Nair",
      chips = c(0L, 0L, 1L, 2L, 4L, 5L, 4L, 3L, 1L, 0L),
      p10 = 0.42, p50 = 0.58, p90 = 0.74,
      rationale = "Dummy: optimistic oncology view; 12-month PFS suggests a heavier right tail."
    ),
    list(
      email = "rahul.kurian@hospital.org",
      name = "Dr. Rahul Kurian",
      chips = c(1L, 3L, 5L, 5L, 4L, 2L, 0L, 0L, 0L, 0L),
      p10 = 0.22, p50 = 0.38, p90 = 0.55,
      rationale = "Dummy: conservative HTA view; waning after trial follow-up."
    ),
    list(
      email = "lakshmi.pillai@hospital.org",
      name = "Dr. Lakshmi Pillai",
      chips = c(0L, 1L, 2L, 4L, 5L, 4L, 2L, 2L, 0L, 0L),
      p10 = 0.32, p50 = 0.48, p90 = 0.66,
      rationale = "Dummy: central estimate between trial extrapolation and real-world dropout."
    ),
    list(
      email = "arun.varma@hospital.org",
      name = "Dr. Arun Varma",
      chips = c(0L, 0L, 2L, 3L, 3L, 4L, 4L, 3L, 1L, 0L),
      p10 = 0.38, p50 = 0.52, p90 = 0.70,
      rationale = "Dummy: moderate-optimistic; similar class agents held ~50% at 5 years."
    )
  )

  n <- 0L
  for (ex in dummy) {
    pid <- if (!is.null(person_ids)) person_ids[[ex$email]] else NULL
    if (is.null(pid)) {
      person <- find_person_by_email(ex$email)
      pid <- doc_id(person)
    }
    if (is.null(pid) || !nzchar(as.character(pid))) next
    # Ensure participant consent / onboarding is marked complete for this expert
    tryCatch({
      save_onboarding(study, pid, tou = TRUE, coi_financial = "None", coi_academic = "None", attribution = "anonymous")
    }, error = function(e) NULL)
    if (!is.null(q_chips)) {
      bins <- build_bins(10L, 0, 1, as.list(ex$chips))
      save_judgment(
        study, q_chips, pid, ex$name,
        chips_payload(bins, 20L, 0, 1, ex$rationale),
        round_number = 1L
      )
      n <- n + 1L
    }
    if (!is.null(q_quant)) {
      save_judgment(
        study, q_quant, pid, ex$name,
        quantile_payload(ex$p10, ex$p50, ex$p90, ex$rationale, lo = 0, hi = 1),
        round_number = 1L
      )
      n <- n + 1L
    }
    # Mark survey as submitted for Round 1 so status shows "Round 1 Submitted" and advancing to Round 2 succeeds
    tryCatch({
      submit_expert_survey(sid, pid, round_number = 1L)
    }, error = function(e) NULL)
  }
  if (exists("ee_log", mode = "function")) {
    ee_log("info", sprintf("dummy SHELF judgments written: %s", n), where = "seed_dummy")
  }
  list(n = n)
}

list_people <- function() {
  db_all("people", '{"isActive": true}', sort = '{"name": 1}')
}
