# App configuration from environment (local .Renviron or Posit Connect vars)

ee_env <- function(name, default = "") {
  val <- Sys.getenv(name, unset = NA_character_)
  if (is.na(val) || !nzchar(val)) default else val
}

ee_on_posit_connect <- function() {
  nzchar(ee_env("CONNECT_SERVER")) ||
    nzchar(ee_env("CONNECT_CONTENT_GUID")) ||
    identical(tolower(ee_env("POSIT_CONNECT", "false")), "true")
}

ee_on_shinyapps <- function() {
  identical(ee_env("R_CONFIG_ACTIVE"), "shinyapps") ||
    identical(Sys.info()[["user"]], "shiny") ||
    nzchar(ee_env("SHINYAPPS_IO"))
}

ee_is_cloud_runtime <- function() {
  ee_on_posit_connect() || ee_on_shinyapps()
}

ee_auth_dev_mode <- function() {
  if (ee_on_posit_connect()) return(FALSE)
  if (ee_oidc_enabled()) return(FALSE)
  tolower(ee_env("AUTH_DEV_MODE", "true")) %in% c("1", "true", "yes")
}

ee_oidc_enabled <- function() {
  all(nzchar(c(
    ee_env("OIDC_ISSUER"),
    ee_env("OIDC_CLIENT_ID"),
    ee_env("OIDC_CLIENT_SECRET"),
    ee_env("OIDC_REDIRECT_URI"),
    ee_env("OIDC_STATE_SECRET")
  )))
}

ee_oidc_issuer <- function() sub("/+$", "", ee_env("OIDC_ISSUER"))

ee_oidc_discovery <- function() {
  if (!ee_oidc_enabled()) stop("OIDC is not configured.", call. = FALSE)
  httr2::request(paste0(ee_oidc_issuer(), "/.well-known/openid-configuration")) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

ee_oidc_callback_url <- function() ee_env("OIDC_REDIRECT_URI")

ee_oidc_state <- function() {
  payload <- paste(as.integer(Sys.time()), openssl::base64_encode(openssl::rand_bytes(32)), sep = ".")
  signature <- openssl::sha256(charToRaw(payload), key = charToRaw(ee_env("OIDC_STATE_SECRET")))
  paste(payload, openssl::base64_encode(signature), sep = ".")
}

ee_oidc_verify_state <- function(state) {
  parts <- strsplit(state %||% "", ".", fixed = TRUE)[[1]]
  if (length(parts) != 3L || !grepl("^[0-9]+$", parts[[1]])) return(FALSE)
  age <- as.numeric(Sys.time()) - as.numeric(parts[[1]])
  if (!is.finite(age) || age < 0 || age > 600) return(FALSE)
  payload <- paste(parts[[1]], parts[[2]], sep = ".")
  expected <- openssl::base64_encode(
    openssl::sha256(charToRaw(payload), key = charToRaw(ee_env("OIDC_STATE_SECRET")))
  )
  identical(parts[[3]], expected)
}

ee_oidc_authorize_url <- function(state) {
  discovery <- ee_oidc_discovery()
  params <- c(
    response_type = "code",
    client_id = ee_env("OIDC_CLIENT_ID"),
    redirect_uri = ee_oidc_callback_url(),
    scope = ee_env("OIDC_SCOPE", "openid profile email"),
    state = state
  )
  paste0(
    discovery$authorization_endpoint,
    "?",
    paste(
      vapply(names(params), function(name) {
        paste(utils::URLencode(name, reserved = TRUE),
              utils::URLencode(params[[name]], reserved = TRUE), sep = "=")
      }, character(1)),
      collapse = "&"
    )
  )
}

ee_session_timeout_minutes <- function() {
  value <- suppressWarnings(as.numeric(ee_env("SESSION_TIMEOUT_MINUTES", "30")))
  if (!is.finite(value) || value <= 0) 30 else value
}

ee_app_role <- function() {
  role <- tolower(ee_env("APP_ROLE", "both"))
  if (!role %in% c("staff", "survey", "both")) "both" else role
}

ee_supabase_url <- function() {
  url <- ee_env("SUPABASE_DB_URL", "")
  if (!nzchar(url)) url <- ee_env("DATABASE_URL", "")
  if (!nzchar(url)) url <- ee_env("POSTGRES_URL", "")
  url
}

ee_db_type <- function() {
  "supabase"
}

ee_parse_pg_url <- function(url) {
  clean <- sub("^postgres(ql)?://", "", url)
  if (grepl("?", clean, fixed = TRUE)) {
    clean <- strsplit(clean, "?", fixed = TRUE)[[1]][1]
  }
  user <- "postgres"
  password <- ""
  host <- "localhost"
  port <- 5432L
  dbname <- "postgres"
  if (grepl("@", clean, fixed = TRUE)) {
    parts <- strsplit(clean, "@", fixed = TRUE)[[1]]
    auth_part <- parts[1]
    clean <- parts[2]
    if (grepl(":", auth_part, fixed = TRUE)) {
      aparts <- strsplit(auth_part, ":", fixed = TRUE)[[1]]
      user <- aparts[1]
      password <- aparts[2]
    } else {
      user <- auth_part
    }
  }
  if (grepl("/", clean, fixed = TRUE)) {
    parts <- strsplit(clean, "/", fixed = TRUE)[[1]]
    clean <- parts[1]
    dbname <- parts[2]
  }
  if (grepl(":", clean, fixed = TRUE)) {
    parts <- strsplit(clean, ":", fixed = TRUE)[[1]]
    host <- parts[1]
    port <- as.integer(parts[2])
  } else {
    host <- clean
  }
  # For stateful Shiny applications, Supabase port 6543 (transaction pooler) drops idle connections,
  # causing 30s TCP timeouts. Port 5432 (session pooler) keeps connections persistently alive.
  if (grepl("pooler\\.supabase\\.com", host, ignore.case = TRUE) && identical(port, 6543L)) {
    override_port <- suppressWarnings(as.integer(ee_env("SUPABASE_PORT", "5432")))
    if (is.finite(override_port) && override_port > 0) port <- override_port
  }
  list(user = user, password = utils::URLdecode(password), host = host, port = port, dbname = dbname)
}

ee_supabase_config <- function() {
  url <- ee_supabase_url()
  if (nzchar(url)) {
    ee_parse_pg_url(url)
  } else {
    list(
      host = ee_env("SUPABASE_HOST", "localhost"),
      port = as.integer(ee_env("SUPABASE_PORT", "5432")),
      dbname = ee_env("SUPABASE_DB", "postgres"),
      user = ee_env("SUPABASE_USER", "postgres"),
      password = ee_env("SUPABASE_PASSWORD", "")
    )
  }
}

ee_survey_public_url <- function() {
  env_url <- ee_env("SURVEY_PUBLIC_URL", "")
  if (nzchar(env_url)) {
    return(sub("/+$", "", env_url))
  }
  sess <- tryCatch(shiny::getDefaultReactiveDomain(), error = function(e) NULL)
  if (!is.null(sess) && !is.null(sess$clientData)) {
    cdata <- sess$clientData
    host <- shiny::isolate(tryCatch(as.character(cdata$url_hostname %||% ""), error = function(e) ""))
    if (length(host) > 0 && nzchar(host[[1]])) {
      proto <- shiny::isolate(tryCatch(as.character(cdata$url_protocol %||% "http:"), error = function(e) "http:"))
      port <- shiny::isolate(tryCatch(as.character(cdata$url_port %||% ""), error = function(e) ""))
      pathname <- shiny::isolate(tryCatch(as.character(cdata$url_pathname %||% "/"), error = function(e) "/"))
      port_val <- if (length(port) > 0) port[[1]] else ""
      proto_val <- if (length(proto) > 0) proto[[1]] else "http:"
      path_val <- if (length(pathname) > 0) pathname[[1]] else "/"
      port_str <- if (nzchar(port_val) && !port_val %in% c("80", "443", "")) paste0(":", port_val) else ""
      out <- paste0(proto_val, "//", host[[1]], port_str, path_val)
      return(sub("/+$", "", out))
    }
  }
  "http://127.0.0.1:3938"
}

ee_connect_user <- function(session) {
  u <- session$user
  if (is.null(u) || !nzchar(u)) NULL else u
}
