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
      title = tags$a(
        href = "http://ivihome.eu",
        tags$img(
          src = "www/mistral_dp_1.png",
          title = "Example Image Link",
          width = "200",
          height = "50",
          style = "padding: 0 20px;"
        ), ""
      ),
      theme = bslib::bs_theme(
        version = 5,
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
        mod_explor_ui("explor_1")
      ),
      bslib::nav_panel(
        value = "protocols_tab",
        title = "Protocols",
        icon = bsicons::bs_icon("file-earmark-text-fill", size = "1em")
      ),
      bslib::nav_panel(
        value = "pipelines_tab",
        title = "Pipelines",
        icon = icon("diagram-project", size = "1em"),
      ),
      bslib::nav_panel_hidden(
        value = "tab_ADZ4",
        mod_project_adv4_ui("project_adv4_1")
      ),

      # bslib::nav_panel(
      #   value = "project_tab",
      #   title = "Project",
      #   icon = bsicons::bs_icon("folder-fill", size = "1em"),
      #   mod_project_adv4_ui("project_adv4_1")
      # ),
      bslib::nav_panel_hidden(
        value = "tab_ADZ2",
        "ADZ2 tab"
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
