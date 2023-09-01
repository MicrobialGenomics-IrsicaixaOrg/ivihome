#' explore UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explore_ui <- function(id){
  ns <- NS(id)
  fluidPage(
    title = strong("Explore Data"),
    sidebar = NULL,
    fluidRow(h2("Explore Data", class = "explore-title")),
    fluidRow(column(
      width = 5,
      class = "explore-picker-containter",
      uiOutput(outputId = ns("all_picker")),
      uiOutput(outputId = ns("donor_picker")),
      uiOutput(outputId = ns("tissue_picker")),
      uiOutput(outputId = ns("method_picker")),
      uiOutput(outputId = ns("file_picker"))
    )),
    fluidRow(uiOutput(outputId = ns("filter_stats"))),
    # uiOutput(ns("pie_projects"), height = "100px"),
    bslib::navset_tab(
      bslib::nav_panel(
        class = "explore-nav-tab",
        title = textOutput(ns("projects_tab_title")),
        value = 1,
        rep_br(1),
        DT::dataTableOutput(ns("projects_formattable"))
      ),
      bslib::nav_panel(
        class = "explore-nav-tab",
        title = textOutput(ns("samples_tab_title")),
        value = 2,
        rep_br(1),
        DT::dataTableOutput(ns("samples_formattable"), fill = FALSE)
      ),
      bslib::nav_panel(
        class = "explore-nav-tab",
        title = textOutput(ns("files_tab_title")),
        value = 3,
        rep_br(1),
        DT::dataTableOutput(ns("files_formattable"), fill = FALSE)
      )
    )
  )
}

#' explore Server Functions
#'
#' @noRd
mod_explore_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Tabs titles ----
    output$projects_tab_title <- renderText({
      txt <- "Projects"
      if (exists("projects_df")) {
        txt <- glue::glue("Projects ({nrow(projects_df)})")
      }
      txt
    })

    output$samples_tab_title <- renderText({
      txt <- "Samples"
      if (exists("samples_df")) {
        txt <- glue::glue("Samples ({nrow(samples_df)})")
      }
      txt
    })

    output$files_tab_title <- renderText({
      txt <- "Files"
      if (exists("files_df")) {
        txt <- glue::glue("Files ({nrow(files_df)})")
      }
      txt
    })

    ## Filter Pickers ----

    ### Search all Filters ----
    output$all_picker <- renderUI({
      shinyWidgets::dropdownButton(
        circle = FALSE,
        status = "button-picker",
        label = "Search all filters",
        width = "600px",
        fluidRow(column(
          width = 4
        ))
      )
    })

    ### Donor ----

    pik_genus <- chr_sliderinput(full_data, "genus_specie", ns)
    pik_gender <- chr_sliderinput(full_data, "gender", ns)
    pik_disease <- chr_sliderinput(full_data, "disease", ns)
    pik_age <- num_sliderrange(full_data, "age", ns, "Age Range")

    output$donor_picker <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = ns("don"),
        circle = FALSE,
        # status = "danger",
        label = "Donor",
        width = "600px",
        status = "button-picker",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            fluidRow(
              column(4, pik_genus),
              column(4, pik_gender),
              column(4, pik_disease)
            ),
            shiny::br(),
            fluidRow(column(12, pik_age, align = "center"))
          )
        )
      )
    })

    ### Tissue Type ----

    pik_source <- chr_sliderinput(full_data, "sample_source", ns)
    pik_preservation_media <- chr_sliderinput(full_data, "preservation_media", ns)

    output$tissue_picker <- renderUI({
      shinyWidgets::dropdownButton(
        inputId = ns("tiss"),
        circle = FALSE,
        status = "button-picker",
        label = "Tissue Type",
        width = "600px",
        fluidRow(
          column(
            width = 10,
            offset = 1,
            fluidRow(
              column(6, pik_source),
              column(6, pik_preservation_media)
            )
          )
        )
      )
    })

    ### Method ----

    pik_software <- chr_sliderinput(full_data, "software_name", ns, "Software")
    # pik_experiment_type_id <- chr_sliderinput(full_data, "experiment_type_id", ns, "")
    # pik_class <- chr_sliderinput(full_data, "class", ns, "")
    # pik_ <- chr_sliderinput(full_data, "", ns, "")
    # pik_ <- chr_sliderinput(full_data, "", ns, "")
    # pik_experiment_type_id <-

    output$method_picker <- renderUI({
      shinyWidgets::dropdownButton(
        circle = FALSE,
        status = "button-picker",
        label = "Method",
        width = "600px",
        fluidRow(column(
          width = 4
        ))
      )
    })
    ### File ----
    output$file_picker <- renderUI({
      shinyWidgets::dropdownButton(
        circle = FALSE,
        status = "button-picker",
        label = "File",
        width = "600px",
        fluidRow(column(
          width = 4
        ))
      )
    })

    ## Filter table ----

    filtered_data <- reactive({
      req(input$age)
      full_data %>%
        dplyr::filter(dplyr::between(age, input$age[[1]], input$age[[2]])) %>%
        dplyr::filter(genus_specie %in% input$genus_specie) %>%
        dplyr::filter(gender %in% input$gender) %>%
        dplyr::filter(disease %in% input$disease) %>%
        dplyr::filter(sample_source %in% input$sample_source) %>%
        dplyr::filter(preservation_media %in% input$preservation_media)
    })

    ## Filter stats text ----
    output$filter_stats <- renderUI({
      req(filtered_data())
      n_samples <- filtered_data() %>% dplyr::distinct(aliquot_id) %>% nrow()
      n_donors <- filtered_data() %>% dplyr::distinct(patient_id) %>% nrow()
      n_files <-
        filtered_data() %>%
        dplyr::distinct(file_id) %>%
        dplyr::summarise(file_id = dplyr::n()) %>%
        dplyr::pull(file_id) %>%
        si_number()

      f_size <-
        filtered_data() %>%
        dplyr::summarise(size = sum(size)) %>%
        transform_units(col = "size")

      shiny::tags$p(
        class = "explore-stat-containter",
        HTML(glue::glue("<span class='explore-stat-val'>{n_samples}</span> <span class='explore-stat-lab'>Samples</span>")),
        HTML(glue::glue("<span class='explore-stat-val'>{n_donors }</span> <span class='explore-stat-lab'>Donors</span>")),
        HTML(glue::glue("<span class='explore-stat-val'>{n_files}</span> <span class='explore-stat-lab'>Files</span>")),
        HTML(glue::glue("<span class='explore-stat-val'>{f_size}</span> <span class='explore-stat-lab'>File Size</span>"))
      )
    })


    ## Main Panel -- Pie charts ----
    output$pie_projects <- pie_plt(filtered_data(), "project_id", title = "Project")
    output$pie_genus <- pie_plt(filtered_data(), "genus_specie", title = "Specie")
    output$pie_projects <- pie_plt(filtered_data(), "gender")
    output$pie_projects <- pie_plt(filtered_data(), "sample_source", title = "Primary Site")







    # pie_plt(df, "project_id", title = "Project")
    # pie_plt(df, "genus_specie", title = "Specie")
    # pie_plt(df, "gender")
    # pie_plt(df, "sample_source", title = "Primary Site")
    # pie_plt(df, "age")
    # pie_plt(df, "disease")
    # pie_plt(df, "class", title = "Data Type")
    # pie_plt(df, "sub_class")

    ## Main Panel -- Print Tables ----


    ## Projects
    output$projects_formattable <- DT::renderDataTable({
      req(filtered_data())
      filtered_data() %>%
        dplyr::select(
          project_id,
          project_name,
          genus_specie,
          sample_source,
          experiment_type_id,
          batch_id,
          donor_count = aliquot_id,
          gender,
          disease
        ) %>%
        dplyr::mutate(donor_count = length(unique(donor_count))) %>%
        dplyr::group_by(project_id) %>%
        dplyr::summarise(dplyr::across(
          dplyr::everything(),
          ~ unique(.x) %>% stringr::str_c(collapse = ", ")
        )) %>%
        custom_dt()
    })

    ### Samples ----
    output$samples_formattable <- DT::renderDataTable({
      req(filtered_data())
      filtered_data() %>%
        dplyr::select(
          sample_id,
          aliquot_id,
          project_name,
          genus_specie,
          sample_source,
          preservation_media,
          gender,
          birth_date,
          age,
          disease,
          experiment_type_id,
          batch_id
        ) %>%
        dplyr::group_by(aliquot_id) %>%
        dplyr::summarise(dplyr::across(
          dplyr::everything(),
          ~ unique(.x) %>% stringr::str_c(collapse = ", ")
        )) %>%
        custom_dt()
    })

    ### Files
    output$files_formattable <- DT::renderDataTable({
      req(filtered_data())
      filtered_data() %>%
        dplyr::select(
          file_id,
          extension,
          content,
          size,
          experiment_type_id,
          class,
          seq_instrument,
          library_source,
          library_selection,
          library_strategy,
          project_name,
          sample_id,
          aliquot_id,
          genus_specie,
          sample_source,
          preservation_media,
          gender,
          birth_date,
          disease
        ) %>%
        transform_units("size") %>%
        custom_dt()
    })
  })
}

## To be copied in the UI
# mod_explore_ui("explore_1")

## To be copied in the server
# mod_explore_server("explore_1")
