#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @autoglobal
app_server <- function(input, output, session) {
 mod_exploration_server("exploration_1")
}
