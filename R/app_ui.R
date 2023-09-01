#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  link_github <- tags$a(
    shiny::icon("github"),
    href = "https://github.com/MicrobialGenomics-IrsicaixaOrg/mistralDP",
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
      fillable = FALSE,
      id = "main_page",
      title = strong("ivihome"),
      theme = bslib::bs_theme(
        bootswatch = "minty",
        base_font = bslib::font_google("Arvo")
      ),
      bslib::nav_panel(
        value = "home_tab",
        title = "Home",
        icon = bsicons::bs_icon("house-fill", size = "1em"),
        mod_home_ui("home_1")
      ),
      bslib::nav_panel(
        value = "explore_tab",
        title = "Explore",
        icon = bsicons::bs_icon("bar-chart-line-fill", size = "1em"),
        mod_explore_ui("explore_1")
      ),
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "mistralDP"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs()
  )
}
