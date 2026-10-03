mod_chips_ui <- function(id, bin_count = 10L) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "chips-header-bar",
      style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 12px; padding: 10px 14px; background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px;",
      shiny::div(
        style = "display: flex; align-items: center; gap: 12px; flex: 1;",
        htmltools::strong(style = "font-size: 0.95rem; color: #1e293b;", "Chips:"),
        shiny::uiOutput(ns("chips_progress_ui"), style = "flex: 1; max-width: 480px;")
      ),
      shiny::div(
        style = "display: flex; align-items: center; gap: 12px;",
        shiny::uiOutput(ns("chips_counter_badge")),
        shiny::actionLink(ns("reset_chips"), "Reset", style = "font-size: 0.88rem; color: #4f46e5; font-weight: 600; text-decoration: underline;")
      )
    ),
    shiny::uiOutput(ns("chips_interactive_grid")),
    htmltools::tags$details(
      style = "margin: 14px 0; border: 1px dashed #cbd5e1; border-radius: 6px; padding: 6px 12px; background: #fff;",
      htmltools::tags$summary(style = "cursor: pointer; color: #4f46e5; font-size: 0.88rem; font-weight: 600; outline: none; text-align: center;", "⌃ Change range"),
      shiny::div(
        style = "padding-top: 10px;",
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(ns("lo"), "Lowest plausible limit", value = 0, step = 0.01)),
          shiny::column(6, shiny::numericInput(ns("hi"), "Upper plausible limit", value = 1, step = 0.01))
        )
      )
    ),
    shiny::div(
      class = "chip-summary-card",
      style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px; padding: 16px; margin-top: 16px; text-align: left;",
      htmltools::tags$h4(style = "margin: 0 0 10px 0; font-size: 0.95rem; font-weight: 700; color: #0f172a;", "Summary"),
      shiny::uiOutput(ns("summary_bullets")),
      htmltools::tags$p(style = "margin: 12px 0 0 0; font-size: 0.82rem; color: #64748b;", "Please review the summary and make sure the distribution of chips is as you intended.")
    )
  )
}

mod_chips_server <- function(id, bin_count = 10L, total_chips = 20L, lo0 = 0, hi0 = 1, bounds = NULL, unit_label = "Percentage (%)") {
  shiny::moduleServer(id, function(input, output, session) {
    chips <- shiny::reactiveVal(rep(0L, bin_count))

    shiny::observe({
      if (is.null(bounds)) return()
      b <- bounds()
      if (is.null(b)) return()
      if (is.finite(b$lo %||% NA_real_) && is.finite(b$hi %||% NA_real_) && b$lo < b$hi) {
        shiny::updateNumericInput(session, "lo", value = b$lo)
        shiny::updateNumericInput(session, "hi", value = b$hi)
      }
    })

    shiny::observeEvent(input$reset_chips, {
      chips(rep(0L, bin_count))
    })

    lapply(seq_len(bin_count), function(i) {
      shiny::observeEvent(input[[paste0("add_", i)]], {
        v <- chips()
        if (sum(v) < total_chips) {
          v[i] <- v[i] + 1L
          chips(v)
        }
      })
      shiny::observeEvent(input[[paste0("rm_", i)]], {
        v <- chips()
        if (v[i] > 0) {
          v[i] <- v[i] - 1L
          chips(v)
        }
      })
    })

    bins <- shiny::reactive({
      lo <- as.numeric(input$lo %||% lo0)
      hi <- as.numeric(input$hi %||% hi0)
      if (!is.finite(lo) || !is.finite(hi) || !(lo < hi)) {
        lo <- lo0
        hi <- hi0
      }
      build_bins(bin_count, lo, hi, as.list(chips()))
    })

    output$chips_progress_ui <- shiny::renderUI({
      placed <- sum(chips())
      pct <- min(100, round((placed / total_chips) * 100))
      htmltools::div(
        style = "height: 12px; width: 100%; background: #e2e8f0; border-radius: 6px; overflow: hidden;",
        htmltools::div(
          style = sprintf("width: %d%%; height: 100%%; background: linear-gradient(90deg, #6366f1, #4f46e5); transition: width 0.2s ease-in-out;", pct)
        )
      )
    })

    output$chips_counter_badge <- shiny::renderUI({
      placed <- sum(chips())
      htmltools::span(
        style = "font-weight: 700; font-size: 0.95rem; color: #4f46e5;",
        sprintf("%d / %d", placed, total_chips)
      )
    })

    output$chips_interactive_grid <- shiny::renderUI({
      b_list <- bins()
      c_vec <- chips()
      max_slots <- 12L

      cols_html <- lapply(seq_len(bin_count), function(i) {
        b <- b_list[[i]]
        cnt <- c_vec[[i]]
        ns <- session$ns

        # 12 vertical slot blocks from top to bottom
        slot_divs <- lapply(seq(max_slots, 1L, by = -1L), function(slot_idx) {
          is_filled <- slot_idx <= cnt
          htmltools::div(
            style = sprintf(
              "height: 18px; border: 1px solid #cbd5e1; margin-bottom: 2px; border-radius: 3px; background: %s; transition: background 0.15s;",
              if (is_filled) "#6366f1" else "#ffffff"
            )
          )
        })

        htmltools::div(
          style = "flex: 1; min-width: 0; display: flex; flex-direction: column; align-items: stretch; margin: 0 1px;",
          # Button to add chip
          shiny::actionButton(
            ns(paste0("add_", i)), "+",
            style = "padding: 1px 0; font-size: 0.8rem; font-weight: 700; margin-bottom: 4px; height: 24px; border: 1px solid #cbd5e1; background: #f8fafc;"
          ),
          # Stack area
          htmltools::div(
            style = "background: #f1f5f9; padding: 4px 2px; border-radius: 4px; border: 1px solid #e2e8f0; display: flex; flex-direction: column;",
            slot_divs
          ),
          # Button to remove chip
          shiny::actionButton(
            ns(paste0("rm_", i)), "−",
            style = "padding: 1px 0; font-size: 0.8rem; font-weight: 700; margin-top: 4px; height: 24px; border: 1px solid #cbd5e1; background: #f8fafc;"
          ),
          # Boundary label
          htmltools::div(
            style = "font-size: 0.72rem; color: #64748b; text-align: center; margin-top: 4px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
            sprintf("%.1f", b$from)
          )
        )
      })

      # Add final upper boundary label on the very right
      final_label <- sprintf("%.1f", b_list[[length(b_list)]]$to)

      htmltools::div(
        class = "chips-grid-wrapper",
        style = "background: #fff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 14px 10px;",
        htmltools::div(
          style = "display: flex; align-items: flex-end; justify-content: space-between;",
          cols_html
        ),
        htmltools::div(
          style = "display: flex; justify-content: flex-end; margin-top: 2px; padding-right: 4px;",
          htmltools::span(style = "font-size: 0.72rem; color: #64748b;", final_label)
        ),
        htmltools::div(
          style = "text-align: center; margin-top: 8px; font-size: 0.85rem; font-weight: 600; color: #475569;",
          unit_label
        )
      )
    })

    output$summary_bullets <- shiny::renderUI({
      lines <- chips_summary_text(bins(), unit_label)
      htmltools::tags$ul(
        style = "margin: 0; padding-left: 20px; line-height: 1.6; font-size: 0.9rem; color: #334155;",
        lapply(lines, function(item) htmltools::tags$li(item))
      )
    })

    list(
      value = shiny::reactive({
        list(
          bins = bins(),
          totalChips = total_chips,
          lowerBound = as.numeric(input$lo %||% lo0),
          upperBound = as.numeric(input$hi %||% hi0),
          placed = sum(chips())
        )
      })
    )
  })
}
