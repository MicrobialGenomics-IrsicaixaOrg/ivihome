#' explor UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_explor_ui <- function(id) {
  ns <- NS(id)
  bslib::page_sidebar(
    ## Sidebar ----
    sidebar = bslib::sidebar(
      width = "25%",
      bg = "#fff",
      fg = "#aa2b8b",
      open = "desktop",
      bslib::navset_card_underline(
        bslib::nav_panel("Donor", uiOutput(outputId = ns("donor_picker"))),
        bslib::nav_panel("Tissue", uiOutput(outputId = ns("tissue_picker"))),
        bslib::nav_panel("Method", uiOutput(outputId = ns("method_picker"))),
        bslib::nav_panel("File", uiOutput(outputId = ns("file_picker")))
      )
    ),

    ## Main -----

    ### Stats-boxes ----
    bslib::accordion(
      bslib::accordion_panel(
        title = "",
        icon = bsicons::bs_icon("handbag"),
        uiOutput(outputId = ns("stat_boxs"))
      )
    ),
    # verbatimTextOutput(outputId = ns("click_info")),  # Añade esto

    ### Plots ----
    bslib::accordion(
      bslib::accordion_panel(
        title = "",
        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),
          height = "200px",
          highcharter::highchartOutput(ns("pie_projects")),
          highcharter::highchartOutput(ns("pie_genus")),
          highcharter::highchartOutput(ns("pie_gender")),
          highcharter::highchartOutput(ns("pie_source"))
        )
      )
    ),


    ### Tables ----
    bslib::navset_card_underline(
      bslib::nav_panel(
        class = "explore-nav-tab",
        title = textOutput(ns("projects_tab_title")),
        value = 1,
        DT::dataTableOutput(ns("projects_formattable"))
      ),
      bslib::nav_panel(
        class = "explore-nav-tab",
        title = textOutput(ns("samples_tab_title")),
        value = 2,
        DT::dataTableOutput(ns("samples_formattable"), fill = FALSE)
      ),
      bslib::nav_panel(
        class = "explore-nav-tab",
        title = textOutput(ns("files_tab_title")),
        value = 3,
        DT::dataTableOutput(ns("files_formattable"), fill = FALSE)
      )
    )
  )
}

#' explor Server Functions
#'
#' @noRd
mod_explor_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Sidebar ----

    ### Donor ----
    pik_genus <- chr_sliderinput_2(full_data, "genus_specie", ns)
    pik_gender <- chr_sliderinput_2(full_data, "gender", ns)
    pik_disease <- chr_sliderinput_2(full_data, "disease", ns)
    pik_age <- num_sliderrange_2(full_data, "age", ns, "Age Range")

    output$donor_picker <- renderUI({
      bslib::accordion(
        open = TRUE,
        inputID = ns("don"),
        bslib::accordion_panel(title = "Genus", pik_genus),
        bslib::accordion_panel(title = "Gender", pik_gender),
        bslib::accordion_panel(title = "Disease", pik_disease),
        bslib::accordion_panel(title = "Age", pik_age)

      )
    })

    ### Tissue Type ----
    output$tissue_picker <- renderUI({
      bslib::accordion(
        open = TRUE,
        inputID = ns("tiss"),
        bslib::accordion_panel(title = "Sample Source"),
        bslib::accordion_panel(title = "Preservation Media")
      )
    })

    ### Method ----
    output$method_picker <- renderUI({
      bslib::accordion(
        open = TRUE,
        inputID = ns("meth"),
        bslib::accordion_panel(title = "Protocol Class"),
        bslib::accordion_panel(title = "Protocol Subclass"),
        bslib::accordion_panel(title = "Protocol Type"),
        bslib::accordion_panel(title = "Platform"),
        bslib::accordion_panel(title = "Instrument"),
        bslib::accordion_panel(title = "Sample Source"),
      )
    })

    ### File ----
    output$file_picker <- renderUI({
      bslib::accordion(
        open = TRUE,
        inputID = ns("file"),
        bslib::accordion_panel(title = "Content Type"),
        bslib::accordion_panel(title = "Content"),
        bslib::accordion_panel(title = "Software"),
        bslib::accordion_panel(title = "File Format"),
        bslib::accordion_panel(title = "Compressed"),
      )
    })

    ## Main Panel ----

    ### Stat boxs ----
    output$stat_boxs <- renderUI({
      n_samples <-
        full_data %>%
        dplyr::distinct(aliquot_id) %>%
        nrow() %>%
        si_number()

      n_donors <-
        full_data %>%
        dplyr::distinct(patient_id) %>%
        nrow() %>%
        si_number()

      n_files <-
        full_data %>%
        dplyr::distinct(file_id) %>%
        dplyr::summarise(file_id = dplyr::n()) %>%
        dplyr::pull(file_id) %>%
        si_number()

      f_size <-
        full_data %>%
        dplyr::summarise(size = sum(size)) %>%
        transform_units(col = "size")

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        # height = "125px",
        bslib::value_box(
          title = "SAMPLES",
          value = 10,
          showcase = shiny::icon("vial"),
          # showcase_layout = "top right"
        ),
        bslib::value_box(
          title = "DONORS",
          value = n_donors,
          showcase = shiny::icon("user-group"),
          # showcase_layout = "top right"
        ),
        bslib::value_box(
          title = "FILES",
          value = n_files,
          showcase = shiny::icon("list-check"),
          # showcase_layout = "top right"
        ),
        bslib::value_box(
          title = "SIZE",
          value = f_size,
          showcase = shiny::icon("database"),
          # showcase_layout = "top right"
        )
      )
    })

    ## Plots ----
    output$pie_projects <- pie_plt_2(
      full_data,
      group_var = "project_id",
      title = "Project",
      ns = ns,
      click_id = "project_click"
    )

    output$pie_genus <- pie_plt_2(
      full_data,
      group_var = "genus_specie",
      title = "Specie",
      ns = ns,
      click_id = "genus_click"
    )

    output$pie_gender <- pie_plt_2(
      full_data,
      count_var = "gender",
      title = "Gender",
      ns = ns,
      click_id = "gender_click"
    )

    output$pie_source <- pie_plt_2(
      full_data,
      group_var = "sample_source",
      title = "Primary Site",
      ns = ns,
      click_id = "source_click"
    )


    output$click_info <- renderPrint({
      print(c(input$project_click, input$genus_click, input$gender_click, input$source_click))
    })

    ## Main Panel -- Print Tables ----

    ## Tabs titles ----
    output$projects_tab_title <- renderText({
      txt <- "Projects"
      # if (exists("projects_df")) {
      #   txt <- glue::glue("Projects ({nrow(projects_df)})")
      # }
      txt
    })

    output$samples_tab_title <- renderText({
      txt <- "Samples"
      # if (exists("samples_df")) {
      #   txt <- glue::glue("Samples ({nrow(samples_df)})")
      # }
      txt
    })

    output$files_tab_title <- renderText({
      txt <- "Files"
      # if (exists("files_df")) {
      #   txt <- glue::glue("Files ({nrow(files_df)})")
      # }
      txt
    })

    ## Projects
    output$projects_formattable <- DT::renderDataTable({
      # req(filtered_data())
      full_data %>%
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
      # req(filtered_data())
      full_data %>%
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
      # req(filtered_data())
      full_data %>%
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
# mod_explor_ui("explor_1")

## To be copied in the server
# mod_explor_server("explor_1")
