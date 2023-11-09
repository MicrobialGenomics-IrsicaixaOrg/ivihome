#' project_adv4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_project_adv4_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    id = "project-description",
    theme = bslib::bs_theme(
      version = 5,
      bootswatch = "minty",
      base_font = bslib::font_google("Arvo"),
      font_scale = 0.6,
      primary = "#aa2b8b"
    ),
    uiOutput(ns("project_title")),
    uiOutput(ns("project_subtitle")),
    div(
      class = "content-project-container",
      uiOutput(ns("project_description")),
      uiOutput(ns("contact_info")),
      uiOutput(ns("publications")),
      uiOutput(ns("contributors")),
      uiOutput(ns("collaborating_institutions")),
      uiOutput(ns("data_curators")),
      uiOutput(ns("project_downloads")),
      uiOutput(ns("project_details")),
      uiOutput(ns("file_counts")),
      uiOutput(ns("accessions")),
      uiOutput(ns("release_policy"))
    )
  )
}

#' project_adv4 Server Functions
#'
#' @noRd
mod_project_adv4_server <- function(id, parent){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ## DATA PREPARATION ----

    ### PROJECT DOWNLOADS TABLE ----
    df_project_downloads <- data.frame(
      "file_type" = c("Metadata", "Matrices", "Project Files via AWS"),
      "link" = c("link_1", "link_2", "link_3")
    ) %>%
      gt::gt() %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = file_type)
      ) %>%
      gt::tab_options(column_labels.hidden = TRUE) %>%
      gt::opt_table_lines("none")

    ### PROJECT DETAILS TABLE ----
    df_project_details <-
      tibble::tibble() %>%
      gt::gt() %>%
      gt::rows_add(
        var_name = c(
          "Project Label",
          "Species",
          "Sample Source",
          "Disease",
          "Experiment Types",
          "File Format",
          "Donor Count",
          "Sample Count"
        ),
        var_value = c(
          "ADZ4",
          "Homo Sapiens",
          "feces",
          "hiv-1, normal",
          "Microbiome_WMGS, Proteome",
          "fastq.gz, h5",
          150,
          345
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = var_name)
      ) %>%
      gt::tab_options(column_labels.hidden = TRUE) %>%
      gt::opt_table_lines("none")

    ### FILE COUNTS TABLE ----
    df_file_counts <-
      data.frame(
        type = c("fastq", "h5", "Total"),
        count = c("345", "345", "690")
      ) %>%
      gt::gt() %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = type)
      ) %>%
      gt::tab_options(column_labels.hidden = TRUE) %>%
      gt::opt_table_lines("none")


    ### ACCESSIONS TABLE ----
    df_accessions <-
      data.frame(
        type = "GEO Series Accessions",
        value = "GSE9134591"
      ) %>%
      gt::gt() %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold"),
        locations = gt::cells_body(columns = type)
      ) %>%
      gt::tab_options(column_labels.hidden = TRUE) %>%
      gt::opt_table_lines("none")


    project_data <- list(
      project_title = "Explore Project",
      project_subtitle = "Gut microbiome correlates of serious AIDS/non-AIDS events",
      project_description = "Although the function of the mammalian pancreas hinges on complex interactions of distinct cell types, gene expression profiles have primarily been described with bulk mixtures. Here we implemented a droplet-based, single-cell RNA-seq method to determine the transcriptomes of over 12,000 individual pancreatic cells from four human donors and two mouse strains. Cells could be divided into 15 clusters that matched previously characterized cell types: all endocrine cell types, including rare epsilon-cells; exocrine cell types; vascular cells; Schwann cells; quiescent and activated stellate cells; and four types of immune cells. We detected subpopulations of ductal cells with distinct expression profiles and validated their existence with immuno-histochemistry stains. Moreover, among human beta- cells, we detected heterogeneity in the regulation of genes relating to functional maturation and levels of ER stress. Finally, we deconvolved bulk gene expression samples using the single-cell data to detect disease-associated differential expression. Our dataset provides a resource for the discovery of novel cell type-specific transcription factors, signaling receptors, and medically relevant genes.",
      contact_info = "Contact Info",
      publications = list("Publication 1", "Publication 2"),
      contributors = list("Contributor 1", "Contributor 2"),
      collaborating_institutions = list("Institution 1", "Institution 2"),
      data_curators = list("Data Curator 1", "Data Curator 2"),
      project_downloads = df_project_downloads,
      project_details = df_project_details,
      file_counts = df_file_counts,
      accessions = df_accessions,
      release_policy = "Release Policy"
    )


    ## RENDERING ----

    ### PROJECT TITLE ----
    output$project_title <- renderUI({
      tags$h1(project_data$project_title)
    })

    ### PROJECT SUBTITLE ----
    output$project_subtitle <- renderUI({
      tags$h2(project_data$project_subtitle)
    })

    ### PROJECT DESCRIPTION ----
    output$project_description <- renderUI({
      bslib::card(
        bslib::card_header("Project Description"),
        bslib::card_body(project_data$project_description),
        height = "100%",
        fill = TRUE
      )
    })

    ### CONTACT INFO ----
    output$contact_info <- renderUI({
      bslib::card(
        bslib::card_header("Contact Info"),
        bslib::card_body(project_data$contact_info),
        height = "100%",
        fill = TRUE
      )
    })

    ### PUBLICATIONS ----
    output$publications <- renderUI({
      bslib::card(
        bslib::card_header("Publications"),
        bslib::card_body(project_data$publications),
        height = "100%",
        fill = TRUE
      )
    })

    ### CONTRIBUTORS ----
    output$contributors <- renderUI({
      bslib::card(
        bslib::card_header("Contributors"),
        bslib::card_body(project_data$contributors),
        height = "100%",
        fill = TRUE
      )
    })

    ### COLLABORATING INSTITUTIONS ----
    output$collaborating_institutions <- renderUI({
      bslib::card(
        bslib::card_header("Collaborating Institutions"),
        bslib::card_body(project_data$collaborating_institutions),
        height = "100%",
        fill = TRUE
      )
    })

    ### DATA CURATORS ----
    output$data_curators <- renderUI({
      bslib::card(
        bslib::card_header("Data Curators"),
        bslib::card_body(project_data$data_curators),
        height = "100%",
        fill = TRUE
      )
    })

    ### PROJECT DOWNLOADS ----
    output$project_downloads <- renderUI({
      bslib::card(
        bslib::card_header("Project Downloads & Portals"),
        bslib::card_body(
          downloadLink(
            ns("down_meta"),
            label = "Metadata",
            icon = icon("download")
          ),
          downloadLink(
            ns("down_fastqs"),
            label = "Raw Data",
            icon = icon("download")
          ),
          actionLink(
            ns("biom_shiny"),
            label = "BIOM explorer",
            icon = icon("arrow-up-right-from-square")
          )
        ),
        height = "100%",
        fill = TRUE
      )
    })


    output$down_meta <- downloadHandler(
      filename = function() {
        "mtcars.csv"
      },
      content = function(file) {
        write.csv(mtcars, file)
      }
    )


    output$down_fastqs <- downloadHandler(
      filename = function() {
        "mtcars.csv"
      },
      content = function(file) {
        write.csv(mtcars, file)
      }
    )

    observeEvent(input$biom_shiny, {
      browseURL("https://tooldemos.biodata.pt/shiny/apps/Biomeshiny/")
    })

    ### PROJECT DETAILS ----
    output$project_details <- renderUI({
      bslib::card(
        bslib::card_header("Project Details"),
        bslib::card_body(gt::render_gt({ project_data$project_details })),
        height = "100%",
        fill = TRUE
      )
    })

    ### FILE COUNTS ----
    output$file_counts <- renderUI({
      bslib::card(
        bslib::card_header("File Counts"),
        bslib::card_body(gt::render_gt({ project_data$file_counts })),
        height = "100%",
        fill = TRUE
      )
    })

    ### ACCESSIONS ----
    output$accessions <- renderUI({
      bslib::card(
        bslib::card_header("Accessions"),
        bslib::card_body(gt::render_gt({ project_data$accessions })),
        height = "100%",
        fill = TRUE
      )
    })

    ### RELEASE POLICY ----
    output$release_policy <- renderUI({
      bslib::card(
        bslib::card_header("Release Policy"),
        bslib::card_body(project_data$release_policy),
        height = "100%",
        fill = TRUE
      )
    })
  })
}

## To be copied in the UI
# mod_project_adv4_ui("project_adv4_1")

## To be copied in the server
# mod_project_adv4_server("project_adv4_1")
