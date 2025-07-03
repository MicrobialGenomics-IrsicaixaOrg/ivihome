#' Extract Levels of a Categorical Variable from a Data Frame
#'
#' This function takes a data frame and the name of a categorical variable, and
#' returns the levels of that variable.
#'
#' @param df A data frame containing the categorical variable.
#' @param var_name A character vector specifying the name of the categorical
#'   variable. It must be a column name in the data frame.
#'
#' @return A character vector containing the levels of the specified categorical
#'   variable.
#'
#' @details This function checks if the \code{var_name} exists as a column in
#'   the provided data frame (\code{df}). It then extracts the values from the
#'   specified column and converts them to a factor. If the variable is not of
#'   character or factor class, an error is raised. Finally, the function
#'   returns the levels of the extracted factor.
#'
#' @seealso \code{\link{levels}}, \code{\link{factor}}
#' @noRd
#' @autoglobal
#' @examples
#' data <- data.frame(Gender = c("Male", "Female", "Male", "Female"))
#' extract_var_levels(data, "Gender")
extract_var_levels <- function(df, var_name) {
 if (!var_name %in% names(df)) {
  rlang::abort("var_name must be one column of the input data.frame!")
 }

 var <- dplyr::pull(df, .env$var_name)
 if (is.numeric(var)) {
  rlang::abort("var_name must be character or factor class")
 }

 as.factor(var) %>% levels()
}

#' Transform Units in a Data Frame Column
#'
#' This function transforms the values in a specified column of a data frame to a human-readable format
#' with appropriate units such as B (Bytes), KB (Kilobytes), MB (Megabytes), GB (Gigabytes), and TB (Terabytes).
#'
#' @param df A data frame.
#' @param col A column in the data frame containing the values to be transformed.
#'
#' @return A data frame with the specified column transformed into a human-readable format with units.
#' @noRd
#' @autoglobal
#'
#' @examples
#' df <- data.frame(size = c(1500, 500000, 12000000, 5000000000))
#' transformed_df <- transform_units(df, "size")
transform_units <- function(df, col) {
  df %>%
    dplyr::mutate(!!dplyr::sym(col) := dplyr::case_when(
      floor(log10(!!dplyr::sym(col))) >= 12 ~ stringr::str_c(round(!!dplyr::sym(col) / 1e12, 2), " TB"),
      dplyr::between(floor(log10(!!dplyr::sym(col))), 9, 11) ~ stringr::str_c(round(!!dplyr::sym(col) / 1e9, 2), " GB"),
      dplyr::between(floor(log10(!!dplyr::sym(col))), 6, 8) ~ stringr::str_c(round(!!dplyr::sym(col) / 1e6, 2), " MB"),
      dplyr::between(floor(log10(!!dplyr::sym(col))), 3, 5) ~ stringr::str_c(round(!!dplyr::sym(col) / 1e3, 2), " KB"),
      TRUE ~ stringr::str_c(!!dplyr::sym(col), " B")
    ))
}

#' Create a Slider Input for Categorical Data
#'
#' This function generates a slider input for categorical data stored in a data
#' frame column.
#'
#' @param df A data frame containing the categorical data.
#' @param col The name of the column in the data frame containing the
#'   categorical data.
#' @param ns The namespace for the Shiny app.
#' @param title The title to be displayed above the slider input. If NULL, the
#'   column name will be used.
#'
#' @return A Shiny UI element containing a slider input for selecting
#'   categories.
#' @noRd
#' @autoglobal
#'
#' @examples
#' # Assuming "data" is a data frame containing a column named "Category"
#' ns <- NS("my_app")
#' ui <- fluidPage(
#'   chr_sliderinput(df = data, col = "Category", ns = ns)
#' )
chr_sliderinput <- function(df, col, ns, title = NULL) {
  var <- df %>% dplyr::pull(col) %>% unique()

  if (!(is.character(var) | is.factor(var))) {
    rlang::abort("col must be character or factor")
  }

  if (is.null(title)) {
    title <- stringr::str_replace_all(col, "_", " ") %>% stringr::str_to_title()
  }

  lvls <- as.factor(var) %>% levels()

  tagList(
    h6(title, align = "center"),
    shiny::hr(),
    checkboxGroupInput(
      inputId = ns(col),
      label = NULL,
      choiceNames = stringr::str_replace_all(lvls, "_", " ") %>% stringr::str_to_title(),
      choiceValues = lvls,
      selected = lvls
    )
  )
}

#' Create a Slider Range Input for Numeric Data
#'
#' This function generates a slider range input for numeric data stored in a
#' data frame column.
#'
#' @param df A data frame containing the numeric data.
#' @param col The name of the column in the data frame containing the numeric
#'   data.
#' @param ns The namespace for the Shiny app.
#' @param title The title to be displayed above the slider range input. If NULL,
#'   the column name will be used.
#'
#' @return A Shiny UI element containing a slider range input for selecting
#'   numeric range.
#' @noRd
#' @autoglobal
#'
#' @examples
#' # Assuming "data" is a data frame containing a column named "Age"
#' ns <- NS("my_app")
#' ui <- fluidPage(
#'   num_sliderrange(df = data, col = "Age", ns = ns)
#' )
num_sliderrange <- function(df, col, ns, title = NULL) {
  var <- df %>% dplyr::pull(col)

  if (!is.numeric(var)) {
    rlang::abort("col must be numeric")
  }

  if (is.null(title)) {
    title <- stringr::str_replace_all(col, "_", " ") %>% stringr::str_to_title()
  }

  lvls <- as.factor(var) %>% levels()

  tagList(
    h6(title, align = "center"),
    shiny::hr(),
    shiny::br(),
    # shinyWidgets::setSliderColor(color = "#999999", sliderId = 1),
    sliderInput(
      inputId = ns(col),
      label = NULL,
      min = min(var),
      max = max(var),
      value = c(min(var), max(var)),
      step = 1
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
#'
#' @return A Plotly sunburst plot.
#' @export
#' @autoglobal
sunburst_plt <- function(df,
                         vars,
                         max_depth = Inf,
                         opacity = 1,
                         branchvalues = "total") {

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
      labels = ~ labels,
      parents = ~ parents,
      values = ~ count,
      type = 'sunburst',
      maxdepth = max_depth,
      opacity = opacity,
      branchvalues = 'total'
    )
}



