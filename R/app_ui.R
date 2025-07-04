#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @autoglobal
app_ui <- function(request) {
  link_github <- tags$a(
    shiny::icon("github"),
    href = "https://github.com/MicrobialGenomics-IrsicaixaOrg/ivihome",
    target = "_blank"
  )

  link_twitter <- tags$a(
    shiny::icon("twitter"),
    href = "https://twitter.com/mistralhiv",
    target = "_blank"
  )

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    bslib::page_navbar(
      fillable = TRUE,
      title = "IVIHome",
      theme = bslib::bs_theme(
        bootswatch = "minty",
        base_font = bslib::font_google("Arvo"),
        danger = "#FFFFFF",
        primary = "#999999"
      ),
      bslib::nav_panel(
        title = "Home",
        mod_home_ui("home_1"),
        icon = icon("house")
      ),
      bslib::nav_panel(title = "Exploration", mod_exploration_ui("exploration_1")),
      bslib::nav_spacer(),
      bslib::nav_item(link_twitter),
      bslib::nav_item(link_github)
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
#' @autoglobal
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(path = app_sys("app/www"),
                     app_title = "ivihome"),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}
