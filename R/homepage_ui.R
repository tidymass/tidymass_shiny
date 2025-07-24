#' Homepage UI Module
#'
#' Creates the homepage interface for tidymassShiny
#'
#' @param id module id
#'
#' @return UI tagList
#' @export
homepage_ui <- function(id) {
  ns <- NS(id)

  tagList(
    useShinyjs(),
    div(class = "homepage-header",
        div(class = "container",
            img(src = "www/homepage.ico.png", height = "100px", style = "margin-bottom: 20px;"),
            h1("TidyMass Shiny (Online service)", style = "font-weight: 700;"),
            p("An object-oriented, reproducible analysis framework for LC-MS data",
              style = "font-size: 1.2em; max-width: 800px; margin: 0 auto;"),
            br(),
            actionButton(ns("start_btn"), "Get Started",
                         class = "btn-action", icon = icon("play")),
            actionButton(ns("docs_btn"), "Documentation",
                         class = "btn-action", icon = icon("book"))
        )
    ),

    div(class = "container",
        fluidRow(
          column(8, offset = 2,
                 h3("Streamlined LC-MS Data Analysis", align = "center"),
                 p("TidyMass provides a comprehensive, reproducible workflow for metabolomics and lipidomics data processing,
                   from raw data to biological insights.", align = "center"),
                 br()
          )
        ),

        # Key features section
        h2("Key Features", align = "center"),
        fluidRow(
          column(4,
                 div(class = "feature-card card",
                     div(class = "card-body",
                         h4(icon("object-group"), "Object-Oriented Framework"),
                         p("Structured data representation ensuring reproducibility and traceability throughout the analysis workflow.")
                     )
                 )
          ),
          column(4,
                 div(class = "feature-card card",
                     div(class = "card-body",
                         h4(icon("bezier-curve"), "Comprehensive Workflow"),
                         p("From raw data processing to statistical analysis and biological interpretation in one integrated environment.")
                     )
                 )
          ),
          column(4,
                 div(class = "feature-card card",
                     div(class = "card-body",
                         h4(icon("code-branch"), "Reproducible Research"),
                         p("Complete analysis provenance tracking with version control for all processing steps and parameters.")
                     )
                 )
          )
        ),
        br(), br(),

        # Concept and Workflow
        h2("Concept & Workflow", align = "center"),
        fluidRow(
          column(6,
                 div(class = "img-container",
                     h4("TidyMass Conceptual Framework"),
                     img(src = "www/Fig1.Concept.webp", style = "max-height: 400px;"),
                     p("Comprehensive metabolomics data processing framework", style = "font-style: italic;")
                 )
          ),
          column(6,
                 div(class = "img-container",
                     h4("Analysis Workflow"),
                     img(src = "www/Fig3.Workflow.webp", style = "max-height: 400px;"),
                     p("End-to-end LC-MS data processing workflow", style = "font-style: italic;")
                 )
          )
        ),
        br(), br(),

        # TidyMass2 New Features - 新增部分
        h2("TidyMass2 New Features", align = "center"),
        fluidRow(
          column(10, offset = 1,
                 div(class = "img-container",
                     img(src = "www/Fig2.TidyMass2New.png",
                         style = "max-height: 1000px; border: 1px solid #e0e0e0;"),
                     p("Advanced features in TidyMass2: Metabolite origin inference and metabolic feature-based functional module analysis",
                       style = "font-style: italic; text-align: center; margin-top: 15px;")
                 ),
                 div(style = "padding: 20px; background-color: #f9f9f9; border-radius: 8px; margin-top: 20px;",
                     h4("Key Innovations in TidyMass2:", style = "color: #2c3e50;"),
                     tags$ul(
                       tags$li(tags$strong("Cross-Platform Identifier Conversion:"),
                               "Comprehensive chemical identifier conversion system operating across multiple metabolite ID systems."),

                       tags$li(tags$strong("Web-Based Analysis Interface:"),
                               "TidyMassShiny package providing a user-friendly web interface for all TidyMass2 functions."),

                       tags$li(tags$strong("Metabolite Origin Inference:"),
                               "Integration of 11 databases into MetOriginDB for precise metabolite source categorization across 7 origin categories."),

                       tags$li(tags$strong("Origin-Annotation Integration:"),
                               "Seamless connection between metabolite source information and MS2 spectral libraries for enhanced biological interpretation."),

                       tags$li(tags$strong("Feature-Based Functional Module Analysis:"),
                               "Novel network approach identifying biologically relevant metabolic modules without relying solely on MS2 annotation."),

                       tags$li(tags$strong("Comprehensive Metabolic Network:"),
                               "Human metabolic network with 9,630 metabolites and 30,196 connections for functional module detection.")
                     )
                 ),
          )
        ),
        br(), br(),
        # Citation
        h2("Citation", align = "center"),
        div(class = "citation-box",
            p("If you use TidyMass in your publications, please cite:"),
            p("Shen, X., Yan, H., Wang, C. et al. TidyMass an object-oriented reproducible analysis framework for LC–MS data.
              Nat Commun 13, 4365 (2022).", style = "font-weight: bold;"),
            p("Wang, X., Liu, Y., Jiang, C. et al. TidyMass2: Advancing LC-MS Untargeted Metabolomics Through Metabolite Origin
              Inference and Metabolic Feature-based Functional Module Analysis.")
        ),
        br(),

        # Resources
        h2("Resources", align = "center"),
        fluidRow(
          column(3,
                 a(class = "resource-link", href = "https://www.tidymass.org/", target = "_blank",
                   div(icon("globe"), "Official Website")
                 )),
          column(3,
                 a(class = "resource-link", href = "https://www.tidymass.org/tidymassshiny-tutorial/", target = "_blank",
                   div(icon("book-open"), "User Manual")
                   )
                 ),
          column(3,
                 a(class = "resource-link", href = "https://github.com/tidymass", target = "_blank",
                   div(icon("github"), "GitHub Repository")
                   )
                 ),
          column(3,
                 a(class = "resource-link", href = "https://www.shen-lab.org/", target = "_blank",
                   div(icon("university"), "Shen Lab")
                   )
                 ),
          br(), br(),
          # Footer
          hr(),
          p(style = "text-align: center; color: #6c757d;","TidyMass Shiny | © ", format(Sys.Date(), "%Y"), " Shen Lab")
          )
        )
  )
  }
