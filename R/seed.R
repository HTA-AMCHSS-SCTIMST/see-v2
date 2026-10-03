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

  study <- db_one("studies", '{"slug": "infection-rate-demo"}')
  if (is.null(study)) {
    # Check for legacy slug migration if present
    study <- db_one("studies", '{"slug": "hta-drug-a-vs-b"}')
    if (!is.null(study)) {
      db_update("studies", q_id(doc_id(study)), list(
        slug = "infection-rate-demo",
        title = "Surgical Site Infection Rate with New Antiseptic Protocol",
        description = "A new skin antiseptic protocol is being introduced for elective abdominal surgeries. While small pilot trials show promise, the true 30-day surgical site infection rate across public hospitals remains uncertain. We are eliciting expert clinical judgment to inform the health economic evaluation.",
        updatedAt = now
      ))
      study <- db_one("studies", '{"slug": "infection-rate-demo"}')
    }
  }

  if (is.null(study)) {
    study <- db_insert("studies", list(
      orgId = doc_id(org),
      slug = "infection-rate-demo",
      title = "Surgical Site Infection Rate with New Antiseptic Protocol",
      description = "A new skin antiseptic protocol is being introduced for elective abdominal surgeries. While small pilot trials show promise, the true 30-day surgical site infection rate across public hospitals remains uncertain. We are eliciting expert clinical judgment to inform the health economic evaluation.",
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
          why = "Evaluating 30-day surgical site infection (SSI) rates in elective surgeries under a new skin antiseptic protocol.",
          reason = "SHELF expert elicitation synthesizes specialist clinical judgment into structured probability distributions for health technology assessment.",
          instructions = "Task 1 demonstrates the visual Chips-and-Bins method. Task 2 demonstrates the 3-point Quantiles method (P10 Low, P50 Likely, P90 High).",
          quantityOfInterest = "30-day surgical site infection rate (%) in elective abdominal surgery",
          contactEmail = user$email
        )
      ),
      createdAt = now,
      updatedAt = now
    ))
  } else {
    # Refresh metadata to ensure demo stays crystal clear
    db_update("studies", q_id(doc_id(study)), list(
      title = "Surgical Site Infection Rate with New Antiseptic Protocol",
      description = "A new skin antiseptic protocol is being introduced for elective abdominal surgeries. While small pilot trials show promise, the true 30-day surgical site infection rate across public hospitals remains uncertain. We are eliciting expert clinical judgment to inform the health economic evaluation.",
      protocolConfig = list(
        currentRound = 1L,
        rounds = 2L,
        anonymizeFeedback = TRUE,
        methods = list("chips_and_bins", "quantile"),
        surveyWelcome = list(
          conductedBy = "Achutha Menon Centre for Health Science Studies (AMCHSS), SCTIMST, Trivandrum",
          why = "Evaluating 30-day surgical site infection (SSI) rates in elective surgeries under a new skin antiseptic protocol.",
          reason = "SHELF expert elicitation synthesizes specialist clinical judgment into structured probability distributions for health technology assessment.",
          instructions = "Task 1 demonstrates the visual Chips-and-Bins method. Task 2 demonstrates the 3-point Quantiles method (P10 Low, P50 Likely, P90 High).",
          quantityOfInterest = "30-day surgical site infection rate (%) in elective abdominal surgery",
          contactEmail = user$email
        )
      ),
      updatedAt = now
    ))
  }
  sid <- doc_id(study)

  # Task 1: Chips & Bins (Visual Histogram)
  q1 <- db_one("questions", sprintf('{"studyId": %s, "code": "DEMO_Q1_CHIPS"}', json_escape(sid)))
  if (is.null(q1)) {
    # Check for legacy question code
    q_leg1 <- db_one("questions", sprintf('{"studyId": %s, "code": "Q1_CHIPS_DURABILITY"}', json_escape(sid)))
    if (!is.null(q_leg1)) {
      db_update("questions", q_id(doc_id(q_leg1)), list(
        code = "DEMO_Q1_CHIPS",
        title = "Task 1 — Visual Belief Histogram (Chips & Bins)",
        prompt = "Distribute all 20 chips across the bins (0% to 20%) to represent your belief about the 30-day surgical site infection rate under the new antiseptic protocol. Placing more chips in a bin indicates higher likelihood.",
        variableType = "proportion",
        unit = "percentage / proportion",
        lowerBound = 0,
        upperBound = 0.20,
        judgmentSchema = list(type = "chips_and_bins", binCount = 10L, totalChips = 20L),
        updatedAt = now
      ))
    } else {
      db_insert("questions", list(
        studyId = sid,
        code = "DEMO_Q1_CHIPS",
        title = "Task 1 — Visual Belief Histogram (Chips & Bins)",
        prompt = "Distribute all 20 chips across the bins (0% to 20%) to represent your belief about the 30-day surgical site infection rate under the new antiseptic protocol. Placing more chips in a bin indicates higher likelihood.",
        variableType = "proportion",
        elicitationMethod = "chips_and_bins",
        unit = "percentage / proportion",
        lowerBound = 0,
        upperBound = 0.20,
        judgmentSchema = list(type = "chips_and_bins", binCount = 10L, totalChips = 20L),
        rationaleRequired = TRUE,
        sortOrder = 0L,
        isActive = TRUE,
        createdAt = now,
        updatedAt = now,
        createdBy = user$id
      ))
    }
  }

  # Task 2: 3-Point Quantiles (P10 Low, P50 Likely, P90 High)
  q2 <- db_one("questions", sprintf('{"studyId": %s, "code": "DEMO_Q2_QUANTILES"}', json_escape(sid)))
  if (is.null(q2)) {
    q_leg2 <- db_one("questions", sprintf('{"studyId": %s, "code": "Q1_DURABILITY_5Y"}', json_escape(sid)))
    if (!is.null(q_leg2)) {
      db_update("questions", q_id(doc_id(q_leg2)), list(
        code = "DEMO_Q2_QUANTILES",
        title = "Task 2 — 3-Point Estimate (P10 Low, P50 Likely, P90 High)",
        prompt = "State your 3-point judgment for the 30-day surgical site infection rate: (1) Conservative Lower P10 (10% chance true rate is lower), (2) Most Likely Median P50, and (3) Worst-case Upper P90 (10% chance true rate is higher).",
        variableType = "proportion",
        unit = "percentage / proportion",
        lowerBound = 0,
        upperBound = 0.25,
        judgmentSchema = list(type = "quantile", levels = list(0.1, 0.5, 0.9), requireMonotonic = TRUE),
        updatedAt = now
      ))
    } else {
      db_insert("questions", list(
        studyId = sid,
        code = "DEMO_Q2_QUANTILES",
        title = "Task 2 — 3-Point Estimate (P10 Low, P50 Likely, P90 High)",
        prompt = "State your 3-point judgment for the 30-day surgical site infection rate: (1) Conservative Lower P10 (10% chance true rate is lower), (2) Most Likely Median P50, and (3) Worst-case Upper P90 (10% chance true rate is higher).",
        variableType = "proportion",
        elicitationMethod = "quantile",
        unit = "percentage / proportion",
        lowerBound = 0,
        upperBound = 0.25,
        judgmentSchema = list(type = "quantile", levels = list(0.1, 0.5, 0.9), requireMonotonic = TRUE),
        rationaleRequired = TRUE,
        sortOrder = 1L,
        isActive = TRUE,
        createdAt = now,
        updatedAt = now,
        createdBy = user$id
      ))
    }
  }

  access_map <- list(
    list(email = "ananya.menon@sctimst.ac.in", role = "facilitator"),
    list(email = "priya.nair@hospital.org", role = "expert"),
    list(email = "rahul.kurian@hospital.org", role = "expert"),
    list(email = "lakshmi.pillai@hospital.org", role = "expert"),
    list(email = "arun.varma@hospital.org", role = "expert")
  )
  if (nzchar(user$email %||% "") && !identical(user$email, "ananya.menon@sctimst.ac.in")) {
    access_map <- c(access_map, list(list(email = user$email, role = "facilitator")))
    if (is.null(person_ids[[user$email]])) {
      person_ids[[user$email]] <- user$personId %||% user$id
    }
  }

  granted <- 0L
  for (row in access_map) {
    pid <- person_ids[[row$email]]
    if (is.null(pid) || !nzchar(as.character(pid))) next
    existing_accs <- db_all("study_access", sprintf(
      '{"studyId": %s, "personId": %s, "accessRole": %s}',
      json_escape(sid), json_escape(pid), json_escape(row$role)
    ))
    if (length(existing_accs) > 1) {
      # Prune redundant duplicate access records if any exist from legacy seeds
      for (extra_acc in existing_accs[-1]) {
        tryCatch(db_delete("study_access", q_id(doc_id(extra_acc))), error = function(e) NULL)
      }
    }
    if (length(existing_accs) > 0) next
    db_insert("study_access", list(
      studyId = sid,
      orgId = doc_id(org),
      personId = pid,
      userId = if (identical(row$email, user$email)) user$id else NULL,
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
  demo_qs <- if (!is.null(first_person)) survey_query_string(find_study(sid), first_person) else sprintf("?study=%s", sid)

  list(
    studyId = sid,
    slug = "infection-rate-demo",
    title = "Surgical Site Infection Rate with New Antiseptic Protocol",
    peopleCreated = created,
    accessGranted = granted,
    dummyJudgments = dummy$n,
    surveyUrl = demo_url,
    queryString = demo_qs,
    expertEmails = c(
      "priya.nair@hospital.org",
      "rahul.kurian@hospital.org",
      "lakshmi.pillai@hospital.org",
      "arun.varma@hospital.org"
    )
  )
}

# Varied dummy SHELF judgements with clear contrasting clinical personas
seed_dummy_shelf_judgments <- function(study, person_ids = NULL) {
  if (is.null(study)) return(list(n = 0L))
  sid <- doc_id(study)
  qs <- study_questions(sid)
  q_chips <- Filter(is_chips_question, qs)
  q_quant <- Filter(function(q) !is_chips_question(q), qs)
  q_chips <- if (length(q_chips)) q_chips[[1]] else NULL
  q_quant <- if (length(q_quant)) q_quant[[1]] else NULL

  # 10 bins over 0 to 0.20:
  # [0-0.02], [0.02-0.04], [0.04-0.06], [0.06-0.08], [0.08-0.10],
  # [0.10-0.12], [0.12-0.14], [0.14-0.16], [0.16-0.18], [0.18-0.20]
  dummy <- list(
    list(
      email = "priya.nair@hospital.org",
      name = "Dr. Priya Nair",
      chips = c(3L, 9L, 6L, 2L, 0L, 0L, 0L, 0L, 0L, 0L),
      p10 = 0.02, p50 = 0.04, p90 = 0.06,
      rationale = "Optimistic View: Evidence from specialized cancer centers shows rigorous antiseptic pre-treatment drops infection rates to around 3-4%."
    ),
    list(
      email = "rahul.kurian@hospital.org",
      name = "Dr. Rahul Kurian",
      chips = c(0L, 0L, 1L, 3L, 6L, 5L, 3L, 2L, 0L, 0L),
      p10 = 0.07, p50 = 0.10, p90 = 0.14,
      rationale = "Cautious/Realist View: In high-volume public tertiary hospitals with diabetic patient loads, baseline contamination and wound strain typically keep rates near 10%."
    ),
    list(
      email = "lakshmi.pillai@hospital.org",
      name = "Dr. Lakshmi Pillai",
      chips = c(0L, 2L, 6L, 7L, 4L, 1L, 0L, 0L, 0L, 0L),
      p10 = 0.04, p50 = 0.07, p90 = 0.10,
      rationale = "Moderate View: Anticipating standard wound care protocols with moderate staff compliance, 6-7% represents a realistic median."
    ),
    list(
      email = "arun.varma@hospital.org",
      name = "Dr. Arun Varma",
      chips = c(1L, 2L, 4L, 4L, 3L, 3L, 2L, 1L, 0L, 0L),
      p10 = 0.03, p50 = 0.08, p90 = 0.15,
      rationale = "Wide Uncertainty: Limited local validation of the new antiseptic regimen; multi-center variability justifies wide uncertainty intervals."
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
      lo_c <- as.numeric(q_chips$lowerBound %||% 0)
      hi_c <- as.numeric(q_chips$upperBound %||% 0.20)
      bins <- build_bins(10L, lo_c, hi_c, as.list(ex$chips))
      save_judgment(
        study, q_chips, pid, ex$name,
        chips_payload(bins, 20L, lo_c, hi_c, ex$rationale),
        round_number = 1L
      )
      n <- n + 1L
    }
    if (!is.null(q_quant)) {
      lo_q <- as.numeric(q_quant$lowerBound %||% 0)
      hi_q <- as.numeric(q_quant$upperBound %||% 0.25)
      save_judgment(
        study, q_quant, pid, ex$name,
        quantile_payload(ex$p10, ex$p50, ex$p90, ex$rationale, lo = lo_q, hi = hi_q),
        round_number = 1L
      )
      n <- n + 1L
    }
    # Mark survey as submitted for Round 1 for experts 2, 3, 4 so SHELF consensus pool works immediately.
    # Keep Dr. Priya Nair unsubmitted so anyone testing the demo can experience the live interactive survey wizard!
    if (!identical(ex$email, "priya.nair@hospital.org")) {
      tryCatch({
        submit_expert_survey(sid, pid, round_number = 1L)
      }, error = function(e) NULL)
    } else {
      # For Dr. Priya Nair, ensure her access status remains 'active' (not locked/submitted)
      tryCatch({
        acc <- db_one("study_access", sprintf(
          '{"studyId": %s, "personId": %s, "accessRole": "expert"}',
          json_escape(sid), json_escape(pid)
        ))
        if (!is.null(acc)) {
          db_update("study_access", q_id(doc_id(acc)), list(
            status = "active",
            submittedRound = 0L,
            updatedAt = iso_now()
          ))
        }
      }, error = function(e) NULL)
    }
  }
  if (exists("ee_log", mode = "function")) {
    ee_log("info", sprintf("dummy SHELF judgments written: %s", n), where = "seed_dummy")
  }
  list(n = n)
}

list_people <- function() {
  db_all("people", '{"isActive": true}', sort = '{"name": 1}')
}
