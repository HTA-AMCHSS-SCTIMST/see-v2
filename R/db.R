# Database Access Layer for Supabase (PostgreSQL with JSONB)
# Uses DBI and RPostgres for high-performance JSONB storage and GIN indexing.

.ee_db <- new.env(parent = emptyenv())

db_conn <- function() {
  conn <- .ee_db$conn

  if (is.null(conn) || !DBI::dbIsValid(conn)) {
    cfg <- ee_supabase_config()
    port <- if (is.finite(cfg$port) && cfg$port > 0) cfg$port else 5432L
    if (grepl("pooler\\.supabase\\.com", cfg$host, ignore.case = TRUE) && port == 6543L) {
      port <- 5432L
    }
    .ee_db$conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = cfg$host,
      port = port,
      dbname = cfg$dbname,
      user = cfg$user,
      password = cfg$password,
      sslmode = "require",
      connect_timeout = 5,
      keepalives = 1,
      keepalives_idle = 5,
      keepalives_interval = 2,
      keepalives_count = 3
    )
  }
  .ee_db$conn
}

db_reset_cache <- function() {
  if (!is.null(.ee_db$conn) && DBI::dbIsValid(.ee_db$conn)) {
    try(DBI::dbDisconnect(.ee_db$conn), silent = TRUE)
  }
  rm(list = ls(.ee_db), envir = .ee_db)
}

db_tables <- c(
  "organizations", "users", "people", "studies", "questions", "judgments",
  "study_access", "invite_tokens", "onboarding", "elicitation_bounds",
  "peer_comments", "aggregations"
)

ensure_db_schema <- function() {
  if (isTRUE(.ee_db$schema_ready)) return(invisible(TRUE))
  conn <- db_conn()
  exists <- tryCatch({
    res <- DBI::dbGetQuery(conn, "SELECT to_regclass('public.studies') IS NOT NULL as ok;")
    isTRUE(res$ok[[1]])
  }, error = function(e) FALSE)
  if (exists) {
    .ee_db$schema_ready <- TRUE
    return(invisible(TRUE))
  }
  for (tbl in db_tables) {
    try({
      DBI::dbExecute(conn, sprintf(
        "CREATE TABLE IF NOT EXISTS %s (id TEXT PRIMARY KEY, data JSONB NOT NULL, created_at TIMESTAMPTZ DEFAULT NOW(), updated_at TIMESTAMPTZ DEFAULT NOW());",
        tbl
      ))
      DBI::dbExecute(conn, sprintf(
        "CREATE INDEX IF NOT EXISTS idx_%s_data ON %s USING GIN (data);",
        tbl, tbl
      ))
      DBI::dbExecute(conn, sprintf(
        "ALTER TABLE %s ENABLE ROW LEVEL SECURITY;",
        tbl
      ))
    }, silent = TRUE)
  }
  .ee_db$schema_ready <- TRUE
  invisible(TRUE)
}

db_ping <- function() {
  if (!is.null(.ee_db$ping_result) && isTRUE(.ee_db$ping_result$ok)) {
    return(.ee_db$ping_result)
  }
  tryCatch({
    conn <- db_conn()
    res <- DBI::dbGetQuery(conn, "SELECT 1 as ok;")
    ensure_db_schema()
    cfg <- ee_supabase_config()
    out <- list(ok = TRUE, message = sprintf("Connected to Supabase PostgreSQL (%s)", cfg$host))
    .ee_db$ping_result <- out
    out
  }, error = function(e) {
    if (exists("ee_log_error", mode = "function")) ee_log_error(e, "db_ping")
    list(ok = FALSE, message = conditionMessage(e))
  })
}

db_normalize_query <- function(query) {
  if (is.null(query)) return(list(sql = "1=1", params = list()))
  q_str <- if (is.list(query)) {
    jsonlite::toJSON(query, auto_unbox = TRUE, null = "null")
  } else {
    as.character(query)
  }
  q_str <- trimws(q_str)
  if (!nzchar(q_str) || q_str == "{}" || q_str == "[]") {
    return(list(sql = "1=1", params = list()))
  }

  parsed <- tryCatch(jsonlite::fromJSON(q_str, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(parsed) && is.list(parsed) && length(parsed) == 1L && !is.null(parsed[["_id"]])) {
    return(list(sql = "id = $1", params = list(as.character(parsed[["_id"]]))))
  }

  # Support MongoDB $or operator in JSON queries for PostgreSQL JSONB
  if (!is.null(parsed) && is.list(parsed) && !is.null(parsed[["$or"]]) && is.list(parsed[["$or"]])) {
    or_list <- parsed[["$or"]]
    base_parsed <- parsed[setdiff(names(parsed), "$or")]
    params <- list()
    clauses <- character()
    param_idx <- 1L

    if (length(base_parsed) > 0) {
      base_json <- jsonlite::toJSON(base_parsed, auto_unbox = TRUE, null = "null")
      clauses <- c(clauses, sprintf("data @> $%d::jsonb", param_idx))
      params[[param_idx]] <- as.character(base_json)
      param_idx <- param_idx + 1L
    }

    or_clauses <- character()
    for (elem in or_list) {
      elem_json <- jsonlite::toJSON(elem, auto_unbox = TRUE, null = "null")
      or_clauses <- c(or_clauses, sprintf("data @> $%d::jsonb", param_idx))
      params[[param_idx]] <- as.character(elem_json)
      param_idx <- param_idx + 1L
    }

    if (length(or_clauses) > 0) {
      clauses <- c(clauses, paste0("(", paste(or_clauses, collapse = " OR "), ")"))
    }

    sql <- if (length(clauses)) paste(clauses, collapse = " AND ") else "1=1"
    return(list(sql = sql, params = params))
  }

  list(sql = "data @> $1::jsonb", params = list(q_str))
}

db_order_by <- function(sort) {
  if (is.null(sort)) return("")
  s_str <- trimws(as.character(sort))
  if (!nzchar(s_str) || s_str == "{}") return("")
  parsed <- tryCatch(jsonlite::fromJSON(s_str), error = function(e) NULL)
  if (is.null(parsed) || !length(parsed)) return("")
  clauses <- character()
  for (nm in names(parsed)) {
    dir <- if (is.numeric(parsed[[nm]]) && parsed[[nm]] < 0) "DESC" else "ASC"
    clauses <- c(clauses, sprintf("data->>'%s' %s", nm, dir))
  }
  if (length(clauses)) paste("ORDER BY", paste(clauses, collapse = ", ")) else ""
}

db_with_retry <- function(fn, max_tries = 2L) {
  for (try_i in seq_len(max_tries)) {
    res <- tryCatch({
      conn <- db_conn()
      fn(conn)
    }, error = function(e) {
      msg <- conditionMessage(e)
      is_conn_err <- grepl("closed the connection|No route to host|connection to server|broken pipe|SSL SYSCALL|terminating connection|could not receive data|timeout", msg, ignore.case = TRUE)
      if (is_conn_err && try_i < max_tries) {
        try(DBI::dbDisconnect(.ee_db$conn), silent = TRUE)
        .ee_db$conn <- NULL
        Sys.sleep(0.3)
        return("RETRY_FLAG")
      }
      stop(e)
    })
    if (!identical(res, "RETRY_FLAG")) return(res)
  }
}

db_all <- function(name, query = "{}", sort = NULL) {
  db_with_retry(function(conn) {
    norm <- db_normalize_query(query)
    order_clause <- db_order_by(sort)
    sql <- sprintf("SELECT data FROM %s WHERE %s %s;", name, norm$sql, order_clause)
    res <- if (length(norm$params)) {
      DBI::dbGetQuery(conn, sql, params = norm$params)
    } else {
      DBI::dbGetQuery(conn, sql)
    }
    if (!nrow(res)) return(list())
    lapply(res$data, function(txt) {
      if (is.list(txt)) txt else jsonlite::fromJSON(txt, simplifyVector = FALSE)
    })
  })
}

db_one <- function(name, query) {
  db_with_retry(function(conn) {
    norm <- db_normalize_query(query)
    sql <- sprintf("SELECT data FROM %s WHERE %s LIMIT 1;", name, norm$sql)
    res <- if (length(norm$params)) {
      DBI::dbGetQuery(conn, sql, params = norm$params)
    } else {
      DBI::dbGetQuery(conn, sql)
    }
    if (!nrow(res)) return(NULL)
    txt <- res$data[[1]]
    if (is.list(txt)) txt else jsonlite::fromJSON(txt, simplifyVector = FALSE)
  })
}

db_insert <- function(name, doc) {
  if (identical(name, "studies")) .ee_db$studies_cache <- NULL
  db_with_retry(function(conn) {
    if (is.null(doc[["_id"]])) doc[["_id"]] <- new_id()
    id_val <- as.character(doc[["_id"]])
    payload <- jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", POSIXt = "ISO8601")
    sql <- sprintf(
      "INSERT INTO %s (id, data, updated_at) VALUES ($1, $2::jsonb, NOW()) ON CONFLICT (id) DO UPDATE SET data = EXCLUDED.data, updated_at = NOW();",
      name
    )
    DBI::dbExecute(conn, sql, params = list(id_val, as.character(payload)))
    doc
  })
}

db_update <- function(name, query, set_fields, multiple = FALSE) {
  if (identical(name, "studies")) .ee_db$studies_cache <- NULL
  db_with_retry(function(conn) {
    norm <- db_normalize_query(query)
    set_json <- if (is.character(set_fields) && (grepl("^\\{", trimws(set_fields)) || grepl("^\\[", trimws(set_fields)))) {
      set_fields
    } else {
      jsonlite::toJSON(set_fields, auto_unbox = TRUE, null = "null", POSIXt = "ISO8601")
    }

    if (length(norm$params)) {
      where_sql <- norm$sql
      for (k in rev(seq_along(norm$params))) {
        where_sql <- gsub(sprintf("\\$%d\\b", k), sprintf("$%d", k + 1L), where_sql)
      }
      sql <- sprintf(
        "UPDATE %s SET data = data || $1::jsonb, updated_at = NOW() WHERE %s;",
        name, where_sql
      )
      params <- c(list(as.character(set_json)), norm$params)
      DBI::dbExecute(conn, sql, params = params)
    } else {
      sql <- sprintf(
        "UPDATE %s SET data = data || $1::jsonb, updated_at = NOW();",
        name
      )
      DBI::dbExecute(conn, sql, params = list(as.character(set_json)))
    }
    invisible(TRUE)
  })
}

db_count <- function(name, query = "{}") {
  db_with_retry(function(conn) {
    norm <- db_normalize_query(query)
    sql <- sprintf("SELECT COUNT(*) as n FROM %s WHERE %s;", name, norm$sql)
    res <- if (length(norm$params)) {
      DBI::dbGetQuery(conn, sql, params = norm$params)
    } else {
      DBI::dbGetQuery(conn, sql)
    }
    if (is.null(res) || !nrow(res) || is.null(res$n) || !length(res$n)) return(0L)
    as.integer(res$n[[1]] %||% 0L)
  })
}

# Collection/Table interface for callers
db_col <- function(name) {
  list(
    update = function(query, update_doc, multiple = FALSE) {
      parsed_up <- tryCatch(jsonlite::fromJSON(update_doc), error = function(e) list())
      set_fields <- if (!is.null(parsed_up[["$set"]])) parsed_up[["$set"]] else parsed_up
      db_update(name, query, set_fields, multiple = multiple)
    },
    count = function(query = "{}") db_count(name, query),
    insert = function(payload) {
      doc <- if (is.character(payload)) jsonlite::fromJSON(payload) else payload
      db_insert(name, doc)
    },
    iterate = function(query = "{}", fields = "{}", sort = NULL) {
      docs <- db_all(name, query, sort)
      idx <- 0L
      list(
        one = function() {
          idx <<- idx + 1L
          if (idx <= length(docs)) docs[[idx]] else NULL
        }
      )
    },
    index = function(...) invisible(TRUE)
  )
}

ensure_indexes <- function() {
  ensure_db_schema()
}

q_id <- function(id) sprintf('{"_id": %s}', json_escape(id))
q_field <- function(field, value) {
  sprintf("{%s: %s}", jsonlite::toJSON(field, auto_unbox = TRUE), json_escape(value))
}

find_study <- function(id_or_slug) {
  if (is.null(id_or_slug)) return(NULL)
  if (is.list(id_or_slug) && !is.null(doc_id(id_or_slug))) return(id_or_slug)
  key <- trimws(as.character(id_or_slug)[[1]] %||% "")
  if (!nzchar(key)) return(NULL)
  if (!is.null(.ee_db$studies_cache[[key]])) return(.ee_db$studies_cache[[key]])
  st <- db_one("studies", q_id(key)) %||%
    db_one("studies", q_field("slug", key))
  if (!is.null(st)) {
    if (is.null(.ee_db$studies_cache)) .ee_db$studies_cache <- list()
    .ee_db$studies_cache[[key]] <- st
    sid <- doc_id(st)
    if (!is.null(sid) && nzchar(sid)) .ee_db$studies_cache[[sid]] <- st
    slug <- as.character(st$slug %||% "")
    if (nzchar(slug)) .ee_db$studies_cache[[slug]] <- st
  }
  st
}
