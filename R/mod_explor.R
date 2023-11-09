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
      class = "explo-sidebar",
      bslib::navset_card_underline(
        bslib::nav_panel("Donor", uiOutput(outputId = ns("donor_picker"))),
        bslib::nav_panel("Tissue", uiOutput(outputId = ns("tissue_picker"))),
        bslib::nav_panel("Method", uiOutput(outputId = ns("method_picker"))),
        bslib::nav_panel("File", uiOutput(outputId = ns("file_picker")))
      )
    ),

    ## Main ----
    div(
      actionButton(
        inputId = ns("reset_input"),
        "Reset Filters",
        width = "15%",
        align = "right",
        style = "background-color: #aa2b8b; color: #fff; border-color: #aa2b8b;"
      ),
      align = "right"
    ),
    ### Stats-boxes ----
    bslib::accordion(
      bslib::accordion_panel(
        title = "STATS",
        icon = shiny::icon("chart-line"),
        uiOutput(outputId = ns("stat_boxs"))
      )
    ),

    ### Plots ----
    bslib::accordion(
      bslib::accordion_panel(
        title = "GRAPHICS",
        icon = shiny::icon("chart-pie"),
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
    div(
      class = "tbl-container",
      bslib::navset_card_underline(
        id = "explor-tbl-card",
        bslib::nav_panel(
          title = textOutput(ns("projects_tab_title")),
          value = 1,
          DT::dataTableOutput(ns("projects_formattable"))
        ),
        bslib::nav_panel(
          title = textOutput(ns("samples_tab_title")),
          value = 2,
          DT::dataTableOutput(ns("samples_formattable"), fill = FALSE)
        ),
        bslib::nav_panel(
          title = textOutput(ns("files_tab_title")),
          value = 3,
          DT::dataTableOutput(ns("files_formattable"), fill = FALSE)
        )
      )
    )
  )
}

#' explor Server Functions
#'
#' @noRd
mod_explor_server <- function(id, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## Sidebar ----

    ### Project ----
    project_ids_ <- dplyr::pull(full_data, project_id) %>% unique()
    r_values <- reactiveValues(project_ids = project_ids_)

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
    pik_source <- chr_sliderinput_2(full_data, "sample_source", ns)
    pik_media <- chr_sliderinput_2(full_data, "preservation_media", ns)

    output$tissue_picker <- renderUI({
      bslib::accordion(
        open = TRUE,
        inputID = ns("tiss"),
        bslib::accordion_panel(title = "Sample Source", pik_source),
        bslib::accordion_panel(title = "Preservation Media", pik_media)
      )
    })

    ### Method ----
    pik_class <- chr_sliderinput_2(full_data, "class", ns)
    pik_subclass <- chr_sliderinput_2(full_data, "sub_class", ns)
    pik_platform <- chr_sliderinput_2(full_data, "seq_platform", ns)
    pik_instrument <- chr_sliderinput_2(full_data, "seq_instrument", ns)

    output$method_picker <- renderUI({
      bslib::accordion(
        open = TRUE,
        inputID = ns("meth"),
        bslib::accordion_panel(title = "Protocol Class", pik_class),
        bslib::accordion_panel(title = "Protocol Subclass", pik_subclass),
        bslib::accordion_panel(title = "Platform", pik_platform),
        bslib::accordion_panel(title = "Instrument", pik_instrument)
      )
    })

    ### File ----
    pik_content_type <- chr_sliderinput_2(full_data, "type", ns)
    pik_content <- chr_sliderinput_2(full_data, "content", ns)
    pik_software <- chr_sliderinput_2(full_data, "software_name", ns)
    pik_file_format <- chr_sliderinput_2(full_data, "extension", ns)
    pik_compressed <- chr_sliderinput_2(full_data, "compressed", ns)

    output$file_picker <- renderUI({
      bslib::accordion(
        open = TRUE,
        inputID = ns("file"),
        bslib::accordion_panel(title = "Content Type", pik_content_type),
        bslib::accordion_panel(title = "Content", pik_content),
        bslib::accordion_panel(title = "Software", pik_software),
        bslib::accordion_panel(title = "File Format", pik_file_format),
        bslib::accordion_panel(title = "Compressed", pik_compressed)
      )
    })

    ## Main Panel ----

    ### Stat boxs ----
    output$stat_boxs <- renderUI({
      ## Values
      n_samples <-
        dplyr::distinct(filtered_data(), aliquot_id) %>%
        nrow() %>%
        si_number()

      n_donors <-
        dplyr::distinct(filtered_data(), patient_id) %>%
        nrow() %>%
        si_number()

      n_files <-
        dplyr::distinct(filtered_data(), file_id) %>%
        dplyr::summarise(file_id = dplyr::n()) %>%
        dplyr::pull(file_id) %>%
        si_number()

      f_size <-
        dplyr::summarise(filtered_data(), size = sum(size)) %>%
        transform_units(col = "size")

      ## Boxes
      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box(
          title = "SAMPLES",
          value = n_samples,
          showcase = shiny::icon("vial"),
          class = "explo-stats-card"
        ),
        bslib::value_box(
          title = "DONORS",
          value = n_donors,
          showcase = shiny::icon("user-group"),
          class = "explo-stats-card"
        ),
        bslib::value_box(
          title = "FILES",
          value = n_files,
          showcase = shiny::icon("list-check"),
          class = "explo-stats-card"
        ),
        bslib::value_box(
          title = "SIZE",
          value = f_size,
          showcase = shiny::icon("database"),
          class = "explo-stats-card"
        )
      )
    })

    ### Pies Plots ----
    output$pie_projects <- highcharter::renderHighchart({
      req(filtered_data())
      pie_plt_2(
        filtered_data(),
        group_var = "project_id",
        title = "Project",
        ns = ns,
        click_id = "project_click"
      )
    })

    output$pie_genus <- highcharter::renderHighchart({
      req(filtered_data())
      pie_plt_2(
        filtered_data(),
        group_var = "genus_specie",
        title = "Specie",
        ns = ns,
        click_id = "genus_click"
      )
    })

    output$pie_gender <- highcharter::renderHighchart({
      req(filtered_data())
      pie_plt_2(
        filtered_data(),
        count_var = "gender",
        title = "Gender",
        ns = ns,
        click_id = "gender_click"
      )
    })

    output$pie_source <- highcharter::renderHighchart({
      req(filtered_data())
      pie_plt_2(
        filtered_data(),
        group_var = "sample_source",
        title = "Sample Source",
        ns = ns,
        click_id = "source_click"
      )
    })

    ### Tabs titles ----
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

    ### Table Projects ----
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
        acctionbutton_col("project_id", ns, "sel_proj") %>%
        custom_dt()
    })

    ### Table Projects Reactivity ----
    observeEvent(input$sel_proj, {
      bslib::nav_select(
        id = "main_page",
        selected = stringr::str_replace_all(input$sel_proj, ".*button_", "tab_"),
        session = parent
      )
    })

    ### Table Samples ----
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

    ### Table Files ----
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

    ## Data Reactivity ----

    ### Sidebar inputs ----
    filtered_data <- reactive({
      req(input$age)

      f_df <- full_data %>%
        dplyr::filter(dplyr::between(age, input$age[[1]], input$age[[2]])) %>%
        dplyr::filter(genus_specie %in% input$genus_specie) %>%
        dplyr::filter(gender %in% input$gender) %>%
        dplyr::filter(disease %in% input$disease) %>%
        dplyr::filter(project_id %in% r_values$project_ids)

      if (!is.null(input$sample_source)) {
        f_df <- f_df %>%
          dplyr::filter(sample_source %in% input$sample_source) %>%
          dplyr::filter(preservation_media %in% input$preservation_media)
      }

      if (!is.null(input$class)) {
        f_df <- f_df %>%
          dplyr::filter(class %in% input$class) %>%
          dplyr::filter(sub_class %in% input$sub_class) %>%
          dplyr::filter(seq_platform %in% input$seq_platform) %>%
          dplyr::filter(seq_instrument %in% input$seq_instrument)
      }

      if (!is.null(input$type)) {
        f_df <- f_df %>%
          dplyr::filter(type %in% input$type) %>%
          dplyr::filter(content %in% input$content) %>%
          dplyr::filter(software_name %in% input$software_name) %>%
          dplyr::filter(extension %in% input$extension) %>%
          dplyr::filter(as.factor(compressed) %in% input$compressed)
      }

      f_df
    })

    ### Pie-chart filter ----
    observe({
      updateCheckboxGroupInput(session, "genus_specie", selected = input$genus_click)
      updateCheckboxGroupInput(session, "gender", selected = input$gender_click)
      updateCheckboxGroupInput(session, "sample_source", selected = input$source_click)
    })

    observeEvent(input$project_click, {
      r_values$project_ids <- input$project_click
    })

    ### Reset inputs button ----
    observeEvent(input$reset_input, {
      r_values$project_ids <- project_ids_
      shinyjs::reset("donor_picker")
      shinyjs::reset("tissue_picker")
      shinyjs::reset("method_picker")
      shinyjs::reset("file_picker")
    })
  })
}

## To be copied in the UI
# mod_explor_ui("explor_1")

## To be copied in the server
# mod_explor_server("explor_1")
