#' protocols UI Function
#'
#' @description A shiny Module that surfaces protocol collections written as
#' R Markdown files bundled with the application.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_protocols_ui <- function(id) {
  ns <- NS(id)

  bslib::page_fillable(
    class = "protocols-page",
    title = "Protocols",
    bslib::layout_columns(
      col_widths = c(4, 8),
      ### List of protocols ----
      bslib::card(
        class = "protocols-filter-card",
        bslib::card_header(
          h3("Protocol library"),
          p(class = "text-muted", "Curated SOPs contributed by the MISTRAL network")
        ),
        uiOutput(ns("filter_inputs")),
        div(class = "protocols-list", uiOutput(ns("protocol_cards")))
      ),
      ### Protocol detail ----
      bslib::card(
        class = "protocols-detail-card",
        bslib::card_header(uiOutput(ns("protocol_title"))),
        div(class = "protocol-meta-bar", uiOutput(ns("protocol_meta"))),
        div(class = "protocol-body", uiOutput(ns("protocol_body")))
      )
    )
  )
}

#' protocols Server Functions
#'
#' @noRd
mod_protocols_server <- function(id, parent) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    protocols <- load_protocols_catalogue()

    if (nrow(protocols) == 0) {
      output$filter_inputs <- renderUI({ "" })
      output$protocol_cards <- renderUI({
        bslib::callout(
          title = "No protocols found",
          "Add .Rmd files to inst/app/protocols to populate this section."
        )
      })
      output$protocol_title <- renderUI({ h3("Pending content") })
      output$protocol_meta <- renderUI({ NULL })
      output$protocol_body <- renderUI({ NULL })
      return(invisible())
    }

    selected_protocol <- reactiveVal(protocols$id[[1]])

    output$filter_inputs <- renderUI({
      categories <- sort(unique(protocols$category))

      shiny::selectInput(
        inputId = ns("category"),
        label = "Filter by focus area",
        choices = c("All protocols" = "all", categories),
        selected = "all"
      )
    })

    filtered_protocols <- reactive({
      req(input$category)
      if (identical(input$category, "all")) {
        protocols
      } else {
        dplyr::filter(protocols, .data$category == input$category)
      }
    })

    observe({
      current <- selected_protocol()
      pool <- filtered_protocols()
      if (!is.null(current) && current %in% pool$id) {
        return()
      }
      if (nrow(pool) > 0) {
        selected_protocol(pool$id[[1]])
      }
    })

    lapply(protocols$id, function(id) {
      observeEvent(input[[paste0("open_", id)]], {
        selected_protocol(id)
      }, ignoreInit = TRUE)
    })

    output$protocol_cards <- renderUI({
      items <- filtered_protocols()
      req(nrow(items) > 0)

      card_list <- lapply(seq_len(nrow(items)), function(idx) {
        protocol <- items[idx, ]
        bslib::card(
          class = "protocol-card",
          bslib::card_body(
            div(class = "protocol-title", protocol$title),
            p(class = "protocol-summary", protocol$summary %||% ""),
            div(
              class = "protocol-tags",
              span(class = "protocol-badge", protocol$category),
              if (!is.na(protocol$duration)) span(class = "protocol-pill", protocol$duration)
            ),
            shiny::actionLink(
              inputId = ns(paste0("open_", protocol$id)),
              label = "Read protocol",
              class = "protocol-read-link"
            )
          )
        )
      })

      do.call(tagList, card_list)
    })

    selection <- reactive({
      req(selected_protocol())
      dplyr::filter(protocols, .data$id == selected_protocol()) |> dplyr::slice(1)
    })

    output$protocol_title <- renderUI({
      req(selection())
      h3(selection()$title)
    })

    output$protocol_meta <- renderUI({
      req(selection())
      tags$div(
        span(class = "protocol-badge", selection()$category),
        if (!is.na(selection()$duration)) span(class = "protocol-pill", selection()$duration),
        if (!is.na(selection()$owner)) span(class = "protocol-owner", selection()$owner)
      )
    })

    output$protocol_body <- renderUI({
      req(selection())
      shiny::includeMarkdown(selection()$md_path)
    })
  })
}

# Internal: load protocol catalogue from inst/app/protocols ----
load_protocols_catalogue <- function() {
  protocol_dir <- app_sys("app/protocols")
  files <- list.files(protocol_dir, pattern = "\\.Rmd$", full.names = TRUE)

  if (length(files) == 0) {
    return(tibble::tibble())
  }

  tibble::tibble(path = files) |>
    dplyr::mutate(
      front_matter = purrr::map(.data$path, rmarkdown::yaml_front_matter),
      id = purrr::map_chr(.data$path, protocol_id_from_path),
      title = purrr::map2_chr(
        .data$front_matter,
        .data$path,
        ~ .x$title %||% tools::file_path_sans_ext(basename(.y))
      ),
      category = purrr::map_chr(.data$front_matter, ~ .x$category %||% "General"),
      summary = purrr::map_chr(.data$front_matter, ~ .x$summary %||% ""),
      duration = purrr::map_chr(.data$front_matter, ~ .x$duration %||% NA_character_),
      owner = purrr::map_chr(.data$front_matter, ~ .x$owner %||% NA_character_),
      md_path = purrr::map2_chr(.data$path, .data$id, render_protocol_markdown)
    ) |>
    dplyr::select("id", "title", "summary", "category", "duration", "owner", "md_path")
}

protocol_id_from_path <- function(path) {
  slug <- tools::file_path_sans_ext(basename(path))
  slug <- stringr::str_replace_all(slug, "[^A-Za-z0-9]+", "-")
  stringr::str_to_lower(slug)
}

render_protocol_markdown <- function(path, id_prefix) {
  output_file <- file.path(tempdir(), paste0(id_prefix, ".md"))
  rmarkdown::render(
    input = path,
    output_format = rmarkdown::md_document(variant = "gfm"),
    output_file = output_file,
    quiet = TRUE,
    encoding = "UTF-8"
  )
  output_file
}
