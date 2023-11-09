#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_home_server("home_1", session)
  mod_explor_server("explor_1", session)
  mod_project_adv4_server("project_adv4_1", session)
}
