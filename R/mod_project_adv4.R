#' project_adv4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_project_adv4_ui <- function(id){
  ns <- NS(id)
  tagList(


  )
}

#' project_adv4 Server Functions
#'
#' @noRd
mod_project_adv4_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_project_adv4_ui("project_adv4_1")

## To be copied in the server
# mod_project_adv4_server("project_adv4_1")
