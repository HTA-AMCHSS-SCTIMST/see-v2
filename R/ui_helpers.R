ee_header <- function(user = NULL, extra = NULL) {
  htmltools::tags$header(
    class = "site-header",
    htmltools::div(
      class = "header-inner",
      htmltools::tags$img(src = "srlogo.jpg", alt = "SCTIMST", class = "brand-logo"),
      htmltools::div(
        class = "brand-text",
        htmltools::tags$h1("STRUCTURED EXPERT ELICITATION-AMCHSS"),
        htmltools::tags$p(
          class = "institute",
          "Sree Chitra Tirunal Institute for Medical Sciences & Technology, Trivandrum"
        ),
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
      ),
      if (!is.null(user)) {
        initial <- toupper(substr(user$displayName %||% user$email %||% "U", 1, 1))
        htmltools::div(
          class = "header-actions",
          htmltools::div(
            class = "user-chip",
            htmltools::span(class = "avatar-circle", initial),
            htmltools::div(
              class = "user-details",
              htmltools::tags$strong(user$displayName %||% user$email),
              htmltools::tags$span(class = "user-role-badge", role_label(user$platformRole))
            )
          ),
          extra
        )
      }
    )
  )
}

status_pill <- function(status) {
  st_str <- as.character(status %||% "")
  css_cls <- switch(
    st_str,
    "Round 1 Submitted" = "submitted",
    "Round 2 Submitted" = "submitted",
    "Round 2 In Progress" = "ongoing",
    "Round 2 Pending" = "draft",
    gsub("[^a-zA-Z0-9_-]", "-", tolower(st_str))
  )
  lab <- switch(
    st_str,
    draft = "Draft",
    recruiting = "Published",
    eliciting = "Published",
    workshop = "Deliberation",
    submitted = "Submitted",
    ongoing = "Ongoing",
    not_started = "Not started",
    completed = "Completed",
    ready = "Ready for SHELF",
    active = "Active",
    st_str
  )
  htmltools::span(
    class = paste("pill", css_cls),
    htmltools::span(class = "status-dot"),
    lab
  )
}

notice <- function(msg, kind = "info") {
  if (!nzchar(msg %||% "")) return(NULL)
  htmltools::div(class = paste("notice", kind), msg)
}
