login_ui <- function() {
  shiny::div(
    class = "login-page",
    shiny::div(
      class = "login-card",
      htmltools::tags$img(src = "srlogo.jpg", alt = "SCTIMST"),
      htmltools::tags$h1("STRUCTURED EXPERT ELICITATION-AMCHSS"),
      htmltools::tags$p(class = "muted",
        "Sree Chitra Tirunal Institute for Medical Sciences & Technology, Trivandrum"),
      htmltools::tags$p(
        class = "team",
        htmltools::tags$a(
          href = "https://hta-amchss-sctimst.github.io/RRC/",
          target = "_blank",
          rel = "noopener noreferrer",
          aria_label = "Achutha Menon Centre for Health Science Studies website",
          "Achutha Menon Centre for Health Science Studies (AMCHSS)"
        )
      ),
      
      shiny::radioButtons(
        "login_role",
        "Role",
        choiceNames = c("Admin", "Facilitator", "Expert"),
        choiceValues = c("admin", "facilitator", "expert"),
        selected = "facilitator",
        inline = TRUE
      ),
      shiny::textInput("login_email", "Email", value = "facilitator@sctimst.ac.in", width = "100%"),
      shiny::textInput("login_name", "Display name", value = "Facilitator", width = "100%"),
      shiny::actionButton("login_go", "Sign in", class = "btn-primary"),
      if (ee_oidc_enabled()) shiny::actionButton("oidc_login", "Sign in with organization account", class = "btn-secondary"),
      shiny::uiOutput("login_err")
    )
  )
}
