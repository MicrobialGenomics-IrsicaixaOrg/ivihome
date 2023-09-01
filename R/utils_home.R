#' Format a Number in a Compact Representation
#'
#' This function takes a numeric value and formats it in a compact representation
#' using SI prefixes (e.g., k, M, m, u) to denote magnitudes.
#'
#' @param x A numeric value to be formatted.
#' @param digits Number of significant digits to use for formatting.
#'
#' @return A character vector with the compact representation of the input number.
#' @noRd
#' @examples
#' si_number(1500) # Returns "1.50k"
#' si_number(2500000) # Returns "2.50M"
si_number <- function(x, digits = 3) {
  compress <- function(x, n) { signif(x * 10^(-n), digits) }
  dplyr::case_when(
    x >= 1e6   ~ paste0(compress(x, 6), "M"),
    x >= 1000  ~ paste0(compress(x, 3), "k"),
    x >= 1     ~ as.character(compress(x, 0)),
    x >= 0.001 ~ paste0(compress(x, -3), "m"),
    x >= 1e-6  ~ paste0(compress(x, -6), "u")
  )
}


#' stat_box function
#'
#' Create a styled stat box with title, value, and icon.
#'
#' @param title The title of the stat box.
#' @param value The value displayed in the stat box.
#' @param icon The icon to be displayed.
#' @param icon_size The size of the icon.
#' @param opacity The opacity of the icon.
#'
#' @return A tag containing the styled stat box.
#'
#' @export
stat_box <- function(title, value, icon, icon_size = "4em", opacity = 0.8) {
  fixedRow(
    class = "stats-box",
    column(
      width = 3,
      align = "center",
      shiny::icon(
        name = icon,
        class = "fa-thin",
        style = glue::glue("font-size: {icon_size}; opacity: {opacity}; color: var(--purple-dark);")
      )
    ),
    column(
      width = 3,
      align = "center",
      h2(class = "stat_value", value, style = "color: var(--purple-dark);"),
      h4(class = "stat_title", title)
    )
  )
}

#' Create a Sunburst Plot
#'
#' This function generates a sunburst plot using the Plotly library, based on
#' the provided data frame and variables.
#'
#' @param df A data frame containing the data to be visualized.
#' @param vars A character vector specifying the variables to use for generating
#'   the sunburst plot.
#' @param max_depth The maximum depth of the sunburst plot. Defaults to Inf (no
#'   depth limit).
#' @param opacity The opacity of the plot elements. Should be a value between 0
#'   and 1.
#' @param branchvalues The value to determine how the branches are computed.
#'   Should be either "remainder" or "total".
#' @param source_id a character string of length 1. Match the value of this
#'   string with the source argument in event_data() to retrieve the event data
#'   corresponding to a specific plot (shiny apps can have multiple plots).
#'
#' @return A Plotly sunburst plot.
#' @export
sunburst_plt <- function(df,
                         vars,
                         max_depth = Inf,
                         opacity = 1,
                         branchvalues = "total",
                         source_id = "A") {

  if (!branchvalues %in% c("remainder", "total")) {
    rlang::abort("branchvalues must be remainder or total!")
  }

  vars %>%
    purrr::imap(~ {
      vars[1:.y]
    }) %>%
    purrr::map_dfr( ~ {
      df %>%
        tidyr::unite(col = "ids", dplyr::all_of(.x), sep = "-") %>%
        dplyr::select(ids) %>%
        dplyr::count(ids, name = "count") %>%
        dplyr::mutate(
          labels = stringr::str_remove_all(ids, ".*-"),
          parents = stringr::str_remove_all(ids, stringr::str_c("-", labels, "|", labels))
        )
    }) %>%
    plotly::plot_ly(
      ids = ~ ids,
      customdata = ~ ids,
      labels = ~ labels,
      parents = ~ parents,
      values = ~ count,
      type = 'sunburst',
      maxdepth = max_depth,
      opacity = opacity,
      branchvalues = branchvalues,
      source = source_id
    )
}

#' Create a customized button.
#'
#' This function creates a customized button using the `actionButton()` function
#' from the Shiny package. The button's appearance is customized through CSS
#' variables.
#'
#' @param id The ID for the button input.
#' @param lab The label for the button.
#' @param ns The namespace for Shiny.
#' @return A Shiny UI element representing the customized button.
wp_button <- function(id, lab, ns) {
  div(
    class = "wp-button",
    actionButton(
      inputId = ns(id),
      width = "200px",
      lab,
      style = "
        --bs-btn-bg: var(--withe);
        --bs-btn-color: var(--purple-dark);
        --bs-btn-border-color: var(--purple-dark);
        --bs-btn-hover-bg: var(--purple-light);
        --bs-btn-hover-color: var(--purple-dark);
        --bs-btn-hover-border-color: var(--purple-dark);
        --bs-btn-focus-shadow-rgb: 245,166,169;
        --bs-btn-active-color: var(--purple-dark);
        --bs-btn-active-bg: var(--purple-light);
        --bs-btn-active-border-color: var(--purple-dark);
        --bs-btn-active-shadow: inset 0 3px 5px rgba(0,0,0,0.125);
      "
    )
  )
}

#' Create a bar plot showing the number of donors per experiment type.
#'
#' This function takes a data frame and an identifier for the plot's data source
#' and generates a bar plot using Plotly. The plot displays the number of donors
#' for each experiment type.
#'
#' @param df A data frame containing experiment information.
#' @param source_id Identifier for the plot's data source.
#'
#' @return A Plotly bar plot visualizing the number of donors per experiment type.
#' @export
donors_by_exp_plt <- function(df, source_id) {
  df <-
    dplyr::group_by(df, experiment_type_id) %>%
    dplyr::summarise(n_donors = unique(patient_id) %>% length())

  tibble::tibble(
    experiment_type_id = c(
      "microbiome_WMGS",
      "microbiome_16S",
      "transcriptome_bulk",
      "transcriptome_sc",
      "proteome",
      "metaproteome",
      "resistome",
      "epigenome_DNAme450K",
      "epigenome_DNAmeEPIC",
      "metabolome"
    )
  ) %>%
    dplyr::left_join(df, by = "experiment_type_id") %>%
    dplyr::mutate(Donors = dplyr::if_else(is.na(n_donors), 1, n_donors)) %>%
    plotly::plot_ly(
      y = ~ experiment_type_id,
      x = ~ Donors,
      color = ~ experiment_type_id,
      colors = "Set3",
      type = "bar",
      source = source_id
    ) %>%
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = "Number of Donors"),
      yaxis = list(title = "")
    ) %>% plotly::config(displaylogo = FALSE)
}
