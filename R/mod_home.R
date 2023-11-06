#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    ## First Row - TITLE ----
    div(
      class = "header-box",
      h1(
        class = "header-title-1",
        "To decipher the role of the microbiome on HIV pathogenesis and cure"
      ),
    ),

    div(
      class = "sub-header-box",
      h2(class = "header-title-2", "MISTRAL's generated, multi-omic, open data")
    ),

    ## Second Row - BOX-STATS ----
    fluidRow(
      class = "stats-box-container",
      uiOutput(outputId = ns("stat_boxs"))
    ),

    ## Third Row - SEARCHER ----
    fluidRow(
      class = "proj-searcher-container",
      uiOutput(outputId = ns("proj_searcher"))
    ),

    ## Fourth Row - SUMMARY PLOTS ----
    fluidRow(
      class = "wp-plots",
      column(
        width = 2,
        align = "center",
        class = "button-column",
        fluidRow(uiOutput(outputId = ns("wp_a"))),
        fluidRow(uiOutput(outputId = ns("wp_1"))),
        fluidRow(uiOutput(outputId = ns("wp_2"))),
        fluidRow(uiOutput(outputId = ns("wp_3"))),
        fluidRow(uiOutput(outputId = ns("wp_4"))),
        fluidRow(uiOutput(outputId = ns("wp_5"))),
        fluidRow(uiOutput(outputId = ns("wp_6"))),
        fluidRow(uiOutput(outputId = ns("wp_7")))
      ),
      column(
        width = 7,
        align = "center",
        class = "plots-column",
        fluidRow(
          style = "justify-content: center;",
          uiOutput(outputId = ns("title_text")),
          column(
            width = 5,
            align = "center",
            plotly::plotlyOutput(outputId = ns("donor_info"), height = "325px"),
            plotly::plotlyOutput(outputId = ns("file_info"), height = "325px")
          ),
          column(
            width = 7,
            align = "center",
            style = "display: flex; align-items: center; justify-content: center;",
            plotly::plotlyOutput(outputId = ns("nfiles_info"), width = "175%")
          )
        )
      )
    ),


    ## Fifth Row - Data Portal Description
    fluidRow(
      class = "dp-description",
      column(width = 5, div(
        class = "dp-content",
        align = "center",
        p(class = "dp-title", "WHAT IS THE MISTRAL DATA PORTAL?"),
        p(
          class = "dp-text",
          "The MISTRAL Data Portal is an open-access database that stores
          and provides OMIC's data related to HIV/AIDS, specifically HIV-1
          MISTRAL. Anyone can find data, or access MISTRAL tools and applications.
          "
        )
      )),
      column(
        width = 5,
        align = "center",
        tags$img(class = "dp-image", src = "www/mistral_flow_1.png")
      )
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Stat boxed ----
    output$stat_boxs <- renderUI({
      req(df)
      n_samples <- df$data$aliquot_id %>% unique() %>% length() %>% si_number()
      n_donors <- df$data$patient_id %>% unique() %>% length() %>% si_number()
      n_projects <- df$data$project_id %>% unique() %>% length() %>% si_number()
      n_files <- df$data$file_id %>% unique() %>% length() %>% si_number()
      n_labs <- "10"

      fluidRow(
        class = "home-stats",
        column(2, stat_box("SAMPLES", n_samples, "vial")),
        column(2, stat_box("DONORS", n_donors, "user-group")),
        column(2, stat_box("PROJECTS", n_projects, "list-check")),
        column(2, stat_box("FILES", n_files, "database")),
        column(2, stat_box("LABS", n_labs, "microscope"))
      )
    })

    ## Project Searcher ----
    output$proj_searcher <- renderUI({
      fluidRow(div(
        class = "home-searcher",
        h6(class = "title-searcher", "FIND PROJECTS"),
        shinyWidgets::searchInput(
          inputId = "search",
          label = NULL,
          placeholder = "Filter project by attribute e.g. project title",
          width = "700px"
        )
      ))
    })

    ## Work Package buttons ----
    output$wp_a <- renderUI({ wp_button("wp_a", "All", ns) })
    output$wp_1 <- renderUI({ wp_button("wp_1", "Work Package 1", ns) })
    output$wp_2 <- renderUI({ wp_button("wp_2", "Work Package 2", ns) })
    output$wp_3 <- renderUI({ wp_button("wp_3", "Work Package 3", ns) })
    output$wp_4 <- renderUI({ wp_button("wp_4", "Work Package 4", ns) })
    output$wp_5 <- renderUI({ wp_button("wp_5", "Work Package 5", ns) })
    output$wp_6 <- renderUI({ wp_button("wp_6", "Work Package 6", ns) })
    output$wp_7 <- renderUI({ wp_button("wp_7", "Work Package 7", ns) })


    ## Filter full_data ----
    df <- reactiveValues(data = full_data, title = "Complete Dataset of MISTRAL Project Work Packages")
    observeEvent(input$wp_a, {
      df$data <- full_data
      df$title <- "Complete Dataset of MISTRAL Project Work Packages"
    })
    observeEvent(input$wp_1, {
      df$data <- dplyr::filter(full_data, stringr::str_detect(project_name, "wp1"))
      df$title <- "WP1: Microbiome correlates of HIV-1 protection and control"
    })
    observeEvent(input$wp_2, {
      df$data <- dplyr::filter(full_data, stringr::str_detect(project_name, "wp2"))
      df$title <- "WP2: Modulation of HIV-1 immunotherapy (HTI vaccine)"
    })
    observeEvent(input$wp_3, {
      df$data <- dplyr::filter(full_data, stringr::str_detect(project_name, "wp3"))
      df$title <- "WP3: HTI vaccine randomized clinical trial"
    })
    observeEvent(input$wp_4, {
      df$data <- dplyr::filter(full_data, stringr::str_detect(project_name, "wp4"))
      df$title <- "WP4: Gut microbiome correlates of serious AIDS/non-AIDS events"
    })
    observeEvent(input$wp_5, {
      df$data <- dplyr::filter(full_data, stringr::str_detect(project_name, "wp5"))
      df$title <- "WP5: Impact of antiretroviral therapy in the gut bacterial resistome"
    })
    observeEvent(input$wp_6, {
      df$data <- dplyr::filter(full_data, stringr::str_detect(project_name, "wp6"))
      df$title <- "WP6: Randomized clinical trials of interventions to modulate the gut microbiome in HIV-1 infected adults"
    })
    observeEvent(input$wp_7, {
      df$data <- dplyr::filter(full_data, stringr::str_detect(project_name, "wp7"))
      df$title <- "WP7: Data integration and systems biology"
    })


    ## Reactive plots titles ----
    output$title_text <- renderUI({
      req(df)
      p(class = "header-text", df$title)
    })

    ## Summary Plots ----
    output$file_info <- plotly::renderPlotly({
      req(df)
      sunburst_plt(
        df$data,
        vars = c("sub_class", "type", "software_name", "content", "extension"),
        max_depth = 4,
        branchvalues = "remainder",
        source_id = "sunburst_file_info"
      ) %>% plotly::config(displaylogo = FALSE)
    })


    output$donor_info <- plotly::renderPlotly({
      req(df)
      sunburst_plt(
        dplyr::bind_rows(df$data,  df$data %>% dplyr::mutate(project_id = "asdf")),
        vars = c("genus_specie", "disease", "gender", "sample_source"),
        max_depth = 3,
        branchvalues = "remainder",
        source_id = "sunburst_donor_info"
      ) %>%
        plotly::config(displaylogo = FALSE)
    })

    output$nfiles_info <- plotly::renderPlotly({
      req(df)
      donors_by_exp_plt(df$data, "barplot_nfiles_info")
    })

    ## Reactivity Plots ----
    donor_info_data <- reactive({
      currentEventData <-
        unlist(plotly::event_data(
          event = "plotly_sunburstclick",
          source = "sunburst_donor_info",
          priority = "event"
        ))
    })

    file_info_data <- reactive({
      currentEventData <-
        unlist(plotly::event_data(
          event = "plotly_sunburstclick",
          source = "sunburst_file_info",
          priority = "event"
        ))
    })


    # output$clickDataOut <- renderText({
    #   paste("Click data:", paste(
    #     names(clickData()),
    #     unlist(clickData()),
    #     sep = ": ",
    #     collapse = " | "
    #   ))
    # })

    # shinyjs::onevent("hover","toggleAdvanced", shinyjs::toggle(id = "advanced", anim = TRUE))
  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
