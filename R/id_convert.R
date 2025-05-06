#' Metabolite ID Conversion UI
#'
#' @param id Module ID for Shiny.
#' @import shiny
#' @importFrom bsicons bs_icon
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjqui jqui_resizable
#' @importFrom shinyWidgets materialSwitch
#' @importFrom DT dataTableOutput
#' @noRd
id_convert_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Metabolite ID Conversion',
    icon = bs_icon("arrow-left-right"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "Conversion Input",
          icon = bs_icon("pencil"),
          textInput(
            ns("query"),
            label = tooltip(
              trigger = list("Metabolite ID", bs_icon("info-circle")),
              "Enter the metabolite ID to convert (e.g., C00001)"
            ),
            value = "C00001"
          )
        ),
        accordion_panel(
          title = "Conversion Parameters",
          icon = bs_icon("gear"),
          selectInput(
            ns("server"),
            label = tooltip(
              trigger = list("Conversion Server", bs_icon("info-circle")),
              "Select the service for ID conversion"
            ),
            choices = c("cts.fiehnlab", "chemspider", "openai"),
            selected = "cts.fiehnlab"
          ),
          uiOutput(ns("api_key_inputs")),
          selectInput(
            ns("from_type"),
            label = tooltip(
              trigger = list("Source ID Type", bs_icon("info-circle")),
              "Select the type of input metabolite ID"
            ),
            choices = NULL  # Will be updated dynamically
          ),
          selectInput(
            ns("to_type"),
            label = tooltip(
              trigger = list("Target ID Type", bs_icon("info-circle")),
              "Select the desired output ID type"
            ),
            choices = NULL  # Will be updated dynamically
          ),
          numericInput(
            ns("top"),
            label = tooltip(
              trigger = list("Top Matches", bs_icon("info-circle")),
              "Number of top matches to return"
            ),
            value = 1,
            min = 1,
            max = 10
          ),
          actionButton(
            ns("convert_ids"),
            "Convert ID",
            icon = icon("exchange"),
            class = "btn-success"
          )
        )
      ),
      page_fluid(
        nav_panel(
          title = "Result",
          icon = bs_icon("table"),
          textInput(
            ns("output_name"),
            label = tooltip(
              trigger = list("Output Name", bs_icon("info-circle")),
              "Name for the converted ID result"
            ),
            value = "converted_id_v1"
          ),
          dataTableOutput(outputId = ns("conversion_summary"))
        )
      )
    )
  )
}

#' Metabolite ID Conversion Server
#'
#' @param id Module ID for Shiny.
#' @import shiny
#' @importFrom shinyjs toggle runjs useShinyjs
#' @import shinyalert
#' @import dplyr
#' @noRd
id_convert_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    convert_values <- reactiveValues(
      result = NULL,
      log = NULL
    )

    # Define the source lists for each server
    chemspider_source <- c("ChemSpider", "InChIKey", "InChI", "SMILES")
    cts_fiehnlab_chemical_sources <- c(
      "AAA Chemistry", "ABBLIS Chemicals", "Abbott Labs", "ABI Chem", "AbMole Bioscience", "Acesobio", "Achemica", "Acorn PharmaTech",
      "Active Biopharma", "Adooq BioScience", "AK Scientific", "AKos Consulting & Solutions", "Alagar Yadav", "ALDRICH", "Alinda Chemical",
      "Alsachim", "Amadis Chemical", "Amatye", "Ambinter", "Ambit Biosciences", "AmicBase - Antimicrobial Activities", "Angene Chemical",
      "Angene International", "Anitha", "Annamalai Universtiy", "Annker Organics", "Anward", "Apeiron Synthesis", "ApexBio Technology",
      "Apexmol", "Ark Pharm", "Aromsyn catalogue", "Aronis", "ASINEX", "Aurora Fine Chemicals LLC", "Aurum Pharmatech LLC",
      "Avanti Polar Lipids", "Beijing Advanced Technology Co", "Bertin Pharma", "Bharathiar University", "Bharathidasan University",
      "Bhaskar Lab", "BIDD", "BIND", "BindingDB", "BioChemPartner", "BioCyc", "Biological Magnetic Resonance Data Bank (BMRB)",
      "Bioprocess Technology Lab", "Biosynth", "Broad Institute", "BroadPharm", "Burnham Center for Chemical Genomics", "Calbiochem",
      "Calicut University", "Cambridge Crystallographic Data Centre", "CAPOT", "CAS", "Cayman Chemical", "CC_PMLSC", "Center for Chemical Genomics",
      "ChEBI", "ChemBank", "Chembase.cn", "ChEMBL", "ChemBlock", "Chembo", "ChemBridge", "ChemDB", "ChemExper Chemical Directory",
      "ChemFrog", "Chemical Biology Department", "Chemical Name", "chemicalize.org by ChemAxon", "ChemIDplus", "ChemMol", "ChemScene",
      "ChemSpider", "ChemSynthesis", "ChemTik", "Chiralblock Biosciences", "Circadian Research", "CLRI (CSIR)", "CMLD-BU",
      "Columbia University Molecular Screening Center", "Comparative Toxicogenomics Database", "Creasyn Finechem", "Department of Bioinformatics",
      "Department of Biotechnology", "Department of Environmental Biotechnology", "Department of Microbiology", "Department of Pharmacy",
      "Department of Zoology", "DiscoveryGate", "DrugBank", "DTP/NCI", "EDASA Scientific Compounds June 2013", "EMD Biosciences",
      "Emory University Molecular Libraries Screening Center", "Enamine", "Ennopharm", "EPA DSSTox", "Excenen Pharmatech",
      "Exchemistry", "Faculty of Marine Sciences", "FINETECH", "Finley and King Labs", "FLUKA", "ForeChem", "Fragmenta",
      "Georganics", "GlaxoSmithKline (GSK)", "GLIDA", "GNF / Scripps Winzeler lab", "Golm Metabolome Database (GMD)", "GPCR-Ligand Database",
      "Hangzhou APIChem Technology", "Hangzhou Trylead Chemical Technology", "Harvard Medical School", "HDH Pharma", "Human Metabolome Database",
      "HUMGENEX", "IBCH RAS", "IBM", "ICCB-Longwood/NSRB Screening Facility", "IIT Guwahati", "Immunology Lab", "Inc.", "Inc. (AKSCI)",
      "InChIKey", "InFarmatik", "Inhibitor 2", "Insect Molecular Biology Lab", "Ion Channels and Transporters", "IS Chemical Technology",
      "Isoprenoids", "iThemba Pharmaceuticals", "IUPHAR-DB", "Jamson Pharmachem Technology", "Japan Chemical Substance Dictionary (Nikkaji)",
      "Johns Hopkins Ion Channel Center", "Karpagam University", "Kay Laboratory", "KEGG", "Kingston Chemistry", "KUMGM", "LeadScope",
      "Leiden University Medical Center", "LipidMAPS", "LLC", "LMSD", "LMU", "Ltd", "M.Jeyam and G.Shalini. Biochematics Division",
      "Marine Medicinal Plant Biotechnology Laboratory", "Max Planck Institute of Molecular Physiology", "Max Planck Institute of Molecular Plant Physiology",
      "MedChemexpress MCE", "MIC Scientific", "MICAD", "Milwaukee Institute for Drug Discovery", "MLSMR", "MMDB", "Molecular Libraries Program",
      "MOLI", "MolPort", "MP Biomedicals", "MTDP", "Nanjing Pharmaceutical Factory", "Nantong Baihua Bio-Pharmaceutical Co.", "National Cancer Institute (NCI)",
      "Nature Chemical Biology", "Nature Chemistry", "Nature Communications", "NCGC", "NIAID", "NIH Clinical Collection", "NINDS Approved Drug Screening Program",
      "NIST", "NIST Chemistry WebBook", "Nitric Oxide Research", "NMMLSC", "NMRShiftDB", "NovoSeek", "Oakwood Products", "ORST SMALL MOLECULE SCREENING CENTER",
      "P.Ravikumar", "P3 BioSystems", "PANACHE", "Paul Baures", "PCMD", "PDSP", "PENN-ABS", "PennChem-GAM", "PFC", "Phytomatics Laboratory",
      "priyadharshini sabarathinam angayarkanni murugesh palaniswamy", "Prous Science Drugs of the Future", "PubChem CID", "Pubchem SID",
      "Quorum sensing and Peptidomimetics Laboratory", "R.Sathishkumar", "R&D Chemicals", "Rangan Lab", "RSChem", "S.GURUDEEBAN",
      "SASTRA University", "SCRIPDB", "Selleck Chemicals", "Selleckbio", "SGCOxCompounds", "SGCStoCompounds", "Shanghai Institute of Organic Chemistry",
      "Shanghai Sinofluoro Scientific Company", "SIGMA", "Sigma-Aldrich", "SLING Consortium", "SMID", "SMILES", "Southern Research Institute",
      "Southern Research Specialized Biocontainment Screening Center", "Specialized Chemistry Center", "Specs", "Sri Venkateswara University",
      "SRMLSC", "Structural Genomics Consortium", "SureChem", "SYNCHEM OHG", "Syntechem", "T.RAMANATHAN & K.SATYAVANI", "TCI (Tokyo Chemical Industry)",
      "ten Dijke Lab", "Tetrahedron Scientific Inc", "The Scripps Research Institute Molecular Screening Center", "Therapeutic Targets Database",
      "Thomson Pharma", "TimTec", "Total TOSLab Building-Blocks", "Tox21", "True PharmaChem", "Tyger Scientific", "UCLA Molecular Screening Shared Resource",
      "UM-BBD", "UniCarbKB", "University of California at San Diego (UCSD)", "University of Kansas", "University of Michigan", "University of Pittsburgh Molecular Library Screening Center",
      "UPCMLD", "Vanderbilt Screening Center for GPCRs", "Vanderbilt Specialized Chemistry Center", "Vanderbilt University Medical Center", "VIT University",
      "Vitas-M Laboratory", "Watec Laboratories", "Watson International Ltd", "Web of Science", "xPharm", "Zancheng Functional Chemicals", "zealing chemical",
      "ZINC"
    )
    openai_chemical_sources <- c(
      "BioCyc", "CAS", "ChEBI", "ChEMBL", "Chemical Name", "DrugBank", "Human Metabolome Database", "InChIKey", "KEGG", "LipidMAPS", "LMSD", "NIST",
      "PubChem CID", "Pubchem SID", "SMILES"
    )

    # Conditionally render API key inputs based on server selection
    output$api_key_inputs <- renderUI({
      req(input$server)
      if (input$server == "chemspider") {
        textInput(
          ns("chemspider_apikey"),
          label = tooltip(
            trigger = list("ChemSpider API Key", bs_icon("info-circle")),
            "Enter your ChemSpider API key (see RSC Developer Portal)"
          ),
          value = ""
        )
      } else if (input$server == "openai") {
        textInput(
          ns("openai_apikey"),
          label = tooltip(
            trigger = list("OpenAI API Key", bs_icon("info-circle")),
            "Enter your OpenAI API key"
          ),
          value = ""
        )
      } else {
        NULL
      }
    })

    # Update from_type and to_type based on selected server
    observeEvent(input$server, {
      if (input$server == "chemspider") {
        updateSelectInput(session, "from_type", choices = chemspider_source)
        updateSelectInput(session, "to_type", choices = chemspider_source)
      } else if (input$server == "cts.fiehnlab") {
        updateSelectInput(session, "from_type", choices = cts_fiehnlab_chemical_sources)
        updateSelectInput(session, "to_type", choices = cts_fiehnlab_chemical_sources)
      } else if (input$server == "openai") {
        updateSelectInput(session, "from_type", choices = openai_chemical_sources)
        updateSelectInput(session, "to_type", choices = openai_chemical_sources)
      }
    })

    observeEvent(input$convert_ids, {
      req(input$query)
      output_name <- as.character(input$output_name)

      # Input validation for output_name
      if (is.null(output_name) || output_name == "" || !grepl("^[a-zA-Z0-9_]+$", output_name)) {
        shinyalert(
          title = "Invalid Output Name",
          text = "Please provide a valid output name (alphanumeric and underscores only).",
          type = "error",
          timer = 5000
        )
        return()
      }

      # Validate API keys if required
      if (input$server == "chemspider" && (is.null(input$chemspider_apikey) || input$chemspider_apikey == "")) {
        shinyalert(
          title = "Missing ChemSpider API Key",
          text = "Please provide a valid ChemSpider API key.",
          type = "error",
          timer = 5000
        )
        return()
      }
      if (input$server == "openai" && (is.null(input$openai_apikey) || input$openai_apikey == "")) {
        shinyalert(
          title = "Missing OpenAI API Key",
          text = "Please provide a valid OpenAI API key.",
          type = "error",
          timer = 5000
        )
        return()
      }

      # Show modal progress dialog
      showModal(modalDialog(
        title = tags$div(
          tags$i(class = "fa fa-spinner fa-spin"),
          "Converting Metabolite ID"
        ),
        "This may take a moment depending on the server response.",
        easyClose = FALSE,
        footer = NULL
      ))

      tryCatch({
        # Convert the single ID
        convert_values$result <- convert_metabolite_id(
          query = input$query,
          from = input$from_type,
          to = input$to_type,
          top = as.integer(input$top),
          server = input$server,
          chemspider_apikey = input$chemspider_apikey %||% "",
          openai_apikey = input$openai_apikey %||% ""
        )

        # Render results in dataTableOutput
        output$conversion_summary <- renderDataTable_formated(
          condition1 = convert_values$result,
          filename.a = paste0(output_name, "_converted"),
          tbl = convert_values$result
        )

        # Assign to global environment for debugging
        assign(output_name, convert_values$result, envir = .GlobalEnv)

        removeModal()


        # Check if the server is "openai" and add a warning message
        if (input$server == "openai") {
          shinyalert(
            title = "Success",
            text = "Metabolite ID converted successfully! However, conversions based on large language models may not be accurate. Please verify the results.",
            type = "success",
            timer = 5000
          )
        } else {
          shinyalert(
            title = "Success",
            text = "Metabolite ID converted successfully!",
            type = "success",
            timer = 5000
          )
        }

      }, error = function(e) {
        removeModal()
        shinyalert(
          title = "Error",
          text = paste("Conversion failed:", e$message),
          type = "error",
          timer = 5000
        )
      })
    })
  })
}
