# Time Trails Shiny App
# Dependencies: tidyverse, data.table, shiny, and related visualization packages

# Use ragg for high-quality, fast PNG graphics in Shiny
options(shiny.useragg = TRUE)

# Example: Enable persistent on-disk cache for shared use across sessions
# shinyOptions(cache = cachem::cache_disk("./myapp-cache"))
# shinyOptions(cache = cachem::cache_disk("./app_cache/cache/"))

# Package setup: install any missing dependencies
list.of.packages <- c(
  "tidyverse", "data.table", "shiny", "RColorBrewer", "ggforce",
  "ggbreak", "magick", "av", "plotly", "ragg", "ggrastr", "cachem"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# Load required libraries quietly
suppressPackageStartupMessages({
  library(shiny)
  library(ggplot2)
  library(data.table)
  library(RColorBrewer)
  library(ggforce)
  library(magick)
  library(av)
  library(plotly)
  library(ggrastr)
  library(cachem)
})

source("Needed_Functions.R")

# Set display order for variable selector
scramble_key <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,21,16,17,18,19,20)

# Discrete slider step sizes for each variable
step_size_list <- c(
  0.1, 0.2, 0.005, 0.001, 0.001, 0.005, 0.005, 0.01, 0.005, 0.001, 0.05,
  0.01, 1, 0.001, 0.01, 1, 0.01, 0.01, 0.001, 0.5, 1
)

# Labels for all variable options
graph_choices_ref <- c(
  paste0("C2N Precivity ","plasma %p-tau217 (%)"),
  paste0("C2N Precivity ","plasma p-tau217 (pg/mL)"),
  paste0("C2N Precivity ","plasma A","\U03B2","42/A","\U03B2","40"),
  paste0("Fujirebio Lumipulse ","plasma p-tau217 (pg/mL)"),
  paste0("Fujirebio Lumipulse ","plasma A","\U03B2","42/A","\U03B2","40"),
  paste0("ALZpath Quanterix ","plasma p-tau217 (pg/mL)"),
  paste0("Janssen LucentAD Quanterix ","plasma p-tau217 (pg/mL)"),
  paste0("Roche NeuroToolKit ","plasma p-tau181 (pg/mL)"),
  paste0("Roche NeuroToolKit ","plasma A","\U03B2","42/A","\U03B2","40"),
  paste0("Roche NeuroToolKit ","plasma GFAP (ng/mL)"),
  paste0("Roche NeuroToolKit ","plasma NfL (pg/mL)"),
  paste0("Quanterix Neurology 4-Plex ","plasma p-tau181 (pg/ml)"),
  paste0("Quanterix Neurology 4-Plex ","plasma GFAP (pg/mL)"),
  paste0("Quanterix Neurology 4-Plex ","plasma A","\U03B2","42/A","\U03B2","40"),
  paste0("Quanterix Neurology 4-Plex ","plasma NfL (pg/mL)"),
  "Amyloid PET Centiloid",
  "Tau PET Mesial-Temporal (Early)",
  "Tau PET Temporo-Parietal (Late)",
  "Cortical Thickness (Meta ROI)",
  "Clinical Dementia Rating Sum of Boxes",
  paste0("Roche Elecsys ","CSF A","\U03B2","42/p-tau181")
)

# Visualization ranges for each variable
graph_limits_list <- list(
  c(0,25), c(0,20), c(0,0.15), c(0,1.8), c(0,0.3), c(0,3), c(0,0.35), c(0,4.7), c(0,0.23), c(0,0.46), c(0,16),
  c(0,80), c(0,565), c(0,0.125), c(0,80), c(-40,190), c(0.85,2), c(0.9,2.5), c(1.8,3.25), c(-0.5,10), c(5,185)
)

# Default group thresholds for variables
group_threshold_defaults <- c(
  "1.5, 2.5, 4.1, 8",
  "3, 7, 13, 16",
  "0.04, 0.06, 0.0924, 0.1",
  "0.06, 0.1, 0.158, 0.5",
  "0.04, 0.06, 0.0869, 1.5",
  "0.2,0.3,0.444,0.5",
  "0.04,0.05,0.0615,0.085",
  "0.65,1.0,1.14,1.3",
  "0.11,0.12,0.126,0.14",
  "0.0661, 0.0916, 0.113, 0.1230",
  "2.650, 3.270, 4.29, 7",
  "12.1, 17.8, 20.5, 30",
  "89, 128, 205, 250",
  "0.045, 0.053, 0.0582, 0.070",
  "13.27, 17.57, 21.7, 28",
  "-5.00, 7.00, 20, 39",
  "1.09, 1.15, 1.328, 1.4",
  "1.05, 1.12, 1.269, 1.31",
  "2.5, 2.572, 2.932, 3",
  "0.5, 1, 2, 4",
  "10,25,60,91"
)

# Defaults for threshold line markers
value_thresholds <- c(
  4.06, 2.34, 0.0924, 0.158, 0.0869, 0.444, 0.0615, 1.14, 0.126, 0.113, 4.29,
  20.5, 205, 0.0582, 21.7, 20, 1.328, 1.269, 2.572, 0.5, 25
)

# Suffixes for variable ID construction
graph_choices_suffixes <- c(
  "C2N_plasma_ptau217_ratio", "C2N_plasma_ptau217", "C2N_plasma_Abeta42_Abeta40",
  "Fuji_plasma_ptau217", "Fuji_plasma_Ab42_Ab40",
  "AlzPath_plasma_ptau217", "Janssen_plasma_ptau217", "Roche_plasma_ptau181", "Roche_plasma_Ab42_Ab40",
  "Roche_plasma_GFAP", "Roche_plasma_NfL",
  "QX_plasma_ptau181", "QX_plasma_GFAP", "QX_plasma_Ab42_Ab40", "QX_plasma_NfL",
  "CENTILOIDS","MesialTemporal","TemporoParietal","metaROI","CDSOB","AB42_PTAU181"
)

# Variable directionality by risk
high_risk_high <- graph_choices_ref[c(1,2,4,6,7,8,10,11,12,13,15,16,17,18,20)]
high_risk_low <- graph_choices_ref[c(3,5,9,14,19,21)]

# --- UI Definition ---

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      :root {
        --primary-red: #BA0C2F;
        --highlight-red: #dc2626;
        --light-red: #fecaca;
        --dark-red: #8b0000;
        --accent-orange: #f97316;
        --success-green: black;
        --neutral-gray: #6b7280;
        --light-gray: #f9fafb;
      }
      
      body {
        font-family: 'Inter', Arial, sans-serif;
        background: linear-gradient(135deg, #fef2f2 0%, #f9fafb 100%);
        color: #1f2937;
        margin: 0;
        padding: 0;
      }
      
      .custom-header {
        background: linear-gradient(135deg, var(--primary-red) 0%, var(--dark-red) 100%);
        padding: 25px 0;
        box-shadow: 0 6px 20px rgba(186, 12, 47, 0.3);
        border-bottom: 4px solid var(--dark-red);
        margin-bottom: 20px;
      }
      
      .header-content {
        max-width: 1400px;
        margin: 0 auto;
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 0 30px;
      }
      
      .title-section {
        display: flex;
        flex-direction: column;
      }
      
      .app-title {
        color: white;
        font-size: 28px;
        font-weight: 700;
        margin: 0;
        text-shadow: 2px 2px 4px rgba(0, 0, 0, 0.4);
        line-height: 1.2;
      }
      
      .subtitle {
        color: rgba(255, 255, 255, 0.95);
        font-size: 16px;
        font-weight: 500;
        margin: 8px 0 0 0;
        text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.3);
      }
      
      .header-nav {
        display: flex;
        gap: 20px;
        align-items: center;
      }
      
      .nav-button {
        background: rgba(255, 255, 255, 0.25);
        color: white;
        border: 2px solid rgba(255, 255, 255, 0.6);
        padding: 12px 24px;
        border-radius: 12px;
        text-decoration: none;
        font-weight: 600;
        font-size: 16px;
        transition: all 0.3s ease;
        cursor: pointer;
        backdrop-filter: blur(10px);
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
        text-shadow: 1px 1px 2px rgba(0, 0, 0, 0.3);
      }
      
      .nav-button:hover {
        background: rgba(255, 255, 255, 0.4);
        border-color: rgba(255, 255, 255, 0.8);
        transform: translateY(-2px);
        box-shadow: 0 6px 12px rgba(0, 0, 0, 0.25);
      }
      
      .nav-button.active {
        background: white;
        color: var(--primary-red);
        border-color: white;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        text-shadow: none;
        font-weight: 700;
      }
      
      .content-section {
        display: none;
      }
      
      .content-section.active {
        display: block;
      }
      
      .well {
        background: white;
        border-radius: 12px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
        border: 1px solid #e5e7eb;
        padding: 25px;
      }
      
      .form-control {
        border: 2px solid #e5e7eb;
        border-radius: 8px;
        padding: 12px 16px;
        font-size: 14px;
        transition: all 0.3s ease;
        background: #fafafa;
      }
      
      .form-control:focus {
        border-color: var(--primary-red);
        box-shadow: 0 0 0 3px rgba(186, 12, 47, 0.1);
        background: white;
        outline: none;
      }
      
      select.form-control {
        height: auto;
      }
      
      .control-label {
        font-weight: 600;
        color: #374151;
        margin-bottom: 8px;
      }
      
      .btn-default {
        background: linear-gradient(90deg, var(--primary-red) 0%, var(--dark-red) 100%);
        color: white;
        border: none;
        padding: 10px 20px;
        font-weight: 600;
        border-radius: 8px;
        transition: all 0.3s ease;
        box-shadow: 0 2px 4px rgba(186, 12, 47, 0.3);
      }
      
      .btn-default:hover {
        background: linear-gradient(90deg, var(--dark-red) 0%, #660000 100%);
        transform: translateY(-1px);
        box-shadow: 0 4px 8px rgba(186, 12, 47, 0.4);
        color: white;
      }
      
      .btn-default:active, .btn-default:focus {
        background: linear-gradient(90deg, var(--dark-red) 0%, #660000 100%);
        color: white;
        outline: none;
      }
      
      hr {
        border-top: 2px solid var(--light-red);
        margin: 20px 0;
      }
      
      h6 {
        color: var(--neutral-gray);
        font-size: 13px;
        font-style: italic;
      }
      
      .shiny-input-container {
        margin-bottom: 20px;
      }
      
      .irs-bar {
        background: linear-gradient(to bottom, var(--primary-red), var(--dark-red));
        border-top: 1px solid var(--primary-red);
        border-bottom: 1px solid var(--dark-red);
      }
      
      .irs-bar-edge {
        background: linear-gradient(to bottom, var(--primary-red), var(--dark-red));
        border: 1px solid var(--primary-red);
      }
      
      .irs-single, .irs-from, .irs-to {
        background: var(--primary-red);
      }
      
      .irs-slider {
        background: white;
        border: 3px solid var(--primary-red);
      }
      
      .footer {
        background: linear-gradient(135deg, var(--primary-red) 0%, var(--dark-red) 100%);
        color: white;
        padding: 40px 20px;
        text-align: center;
        margin-top: 50px;
        box-shadow: 0 -4px 20px rgba(186, 12, 47, 0.3);
      }
      
      .footer-logo {
        margin-bottom: 25px;
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 40px;
        padding: 20px;
        background: rgba(255, 255, 255, 0.1);
        border-radius: 12px;
        backdrop-filter: blur(10px);
        max-width: 600px;
        margin: 0 auto 25px auto;
      }
      
      .footer-logo a {
        display: block;
        transition: all 0.3s ease;
      }
      
      .footer-logo a:hover {
        transform: scale(1.1);
      }
      
      .footer-logo img {
        height: 70px;
        filter: brightness(0) invert(1) drop-shadow(2px 2px 4px rgba(0, 0, 0, 0.3));
        transition: all 0.3s ease;
      }
      
      .shiny-notification {
        position: fixed;
        top: 15%;
        left: 5%;
        width: 500px;
        height: 190px;
        font-size: 30px;
      }
      
      .recalculating {
        opacity: 1.0;
      }
      
      .references-section {
        background: white;
        border-radius: 12px;
        padding: 40px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.05);
        border: 1px solid #e5e7eb;
        margin: 20px auto;
        max-width: 1200px;
      }
      
      .slider-animate-container {
        text-align: center;
        width: 100%;
        margin-top: 15px !important;
        padding-top: 10px;
      }
      
      .slider-animate-button {
        display: block;
        margin: 0 auto;
      }
    "))
  ),
  
  # JavaScript for navigation
  tags$script(HTML("
    function showSection(sectionName) {
      document.querySelectorAll('.content-section').forEach(function(section) {
        section.classList.remove('active');
      });
      document.getElementById(sectionName + '-section').classList.add('active');
      document.querySelectorAll('.nav-button').forEach(function(button) {
        button.classList.remove('active');
      });
      event.target.classList.add('active');
    }
  ")),
  
  # ENHANCED CUSTOM HEADER WITH NAV BUTTONS
  div(class = "custom-header",
      div(class = "header-content",
          div(class = "title-section",
              h1("Alzheimer's Disease Biomarker Time Trail Viewer", class = "app-title"),
              p("Longitudinal biomarker simultaneous trajectories", class = "subtitle")
          ),
          div(class = "header-nav",
              tags$a("Analysis", class = "nav-button active", href = "#", onclick = "showSection('analysis')"),
              tags$a("Help/References", class = "nav-button", href = "#", onclick = "showSection('references')")
          )
      )
  ),
  
  # ANALYSIS SECTION
  div(id = "analysis-section", class = "content-section active",
      sidebarLayout(
        sidebarPanel(
          selectInput("y_var", "Y-axis variable (longitudinal value):",
                      choices = graph_choices_ref[scramble_key], selected = graph_choices_ref[1]),
          selectInput("x_var", "X-axis variable (longitudinal value):",
                      choices = graph_choices_ref[scramble_key], selected = graph_choices_ref[16]),
          tags$hr(),
          sliderInput("Time", "Years", min = 0, max = 10, step = 0.2, value = 1,
                      animate = animationOptions(
                        interval = 600,
                        loop = FALSE,
                        playButton = HTML('<button style="width:100%; height:40px; font-size:20px; background-color: #BA0C2F; color: white; border: none; border-radius: 5px; cursor: pointer; margin-top: 15px;">▶</button>'),
                        pauseButton = HTML('<button style="width:100%; height:40px; font-size:20px; background-color: #BA0C2F; color: white; border: none; border-radius: 5px; cursor: pointer; margin-top: 15px;">⏸</button>')
                      )
          ),
          tags$hr(),
          selectInput("group_xy_check", "Variable used to color groups:",
                      choices = c("Longitudinal","Baseline"), selected = "Baseline"),
          conditionalPanel("input.group_xy_check =='Longitudinal'",
                           selectInput("other_type", "Base group colors off which value?",
                                       graph_choices_ref[scramble_key], selected = graph_choices_ref[1])
          ),
          conditionalPanel("input.group_xy_check =='Baseline'",
                           selectInput("otherb_type", "Base group colors off which value?",
                                       graph_choices_ref[scramble_key], selected = graph_choices_ref[1])
          ),
          h6("Points with no data for grouping variable will be displayed in grey"),
          numericInput("th2", label = "X-axis threshold line", value = 0.158, step = 0.0005),
          numericInput("th1", label = "Y-axis threshold line", value = 0.158, step = 0.0005),
          tags$hr(),
          uiOutput("tab")
        ),
        mainPanel(
          plotOutput(outputId = "plot1", width = 1000, height = 750),
          tags$hr(),
          tags$hr(),
          div(style="display: inline-block; width: 300px;",
              sliderInput("cutoff1","Group cut-off 1",min=0,max=20, step = 0.2,value=0,width='100%')),
          div(style="display: inline-block; width: 300px;",
              sliderInput("cutoff2","Group cut-off 2",min=0,max=20, step = 0.2,value=0,width='100%')),
          div(style="display: inline-block; width: 300px;",
              sliderInput("cutoff3","Group cut-off 3",min=0,max=20, step = 0.2,value=0,width='100%')),
          div(style="display: inline-block; width: 300px;",
              sliderInput("cutoff4","Group cut-off 4",min=0,max=20, step = 0.2,value=0,width='100%')),
          actionButton("reset_thresholds", "Reset Thresholds"),
          tags$hr()
        )
      )
  ),
  
  # REFERENCES SECTION
  div(id = "references-section", class = "content-section",
      div(class = "container-fluid",
          fluidRow(
            column(12,
                   div(class = "references-section",
                       h2("Help", style = "color: var(--primary-red); margin-bottom: 30px;"),
                       
                       # Add the help figure
                       tags$img(src = "help_figure.png", 
                                alt = "Help Figure",
                                style = "max-width: 100%; height: auto; margin-bottom: 30px;"),
                       
                       h2("References", style = "color: var(--primary-red); margin-bottom: 30px;"),
                       div(class = "reference-container",
                           p(class = "hangingindent",
                             strong("1. "), "Schindler SE, Petersen KK, Saef B, Tosun D, Shaw LM, Zetterberg H, Dage JL, Ferber K, Triana-Baltzer G, Du-Cuny L, Li Y, Coomaraswamy J, Baratta M, Mordashova Y, Saad ZS, Raunig DL, Ashton NJ, Meyers EA, Rubel CE, Rosenbaugh EG, Bannon AW, Potter WZ; Alzheimer's Disease Neuroimaging Initiative (ADNI) Foundation for the National Institutes of Health (FNIH) Biomarkers Consortium Plasma Aβ and Phosphorylated Tau as Predictors of Amyloid and Tau Positivity in Alzheimer's Disease Project Team. Head-to-head comparison of leading blood tests for Alzheimer's disease pathology. Alzheimers Dement. 2024 Nov;20(11):8074-8096. doi: 10.1002/alz.14315. Epub 2024 Oct 12. Erratum in: Alzheimers Dement. 2025 Feb;21(2):e14494. doi: 10.1002/alz.14494. PMID: 39394841; PMCID: PMC11567821. ",
                             tags$a("https://pubmed.ncbi.nlm.nih.gov/39394841/", href = "https://pubmed.ncbi.nlm.nih.gov/39394841/", target = "_blank", class = "podcast-link"))
                       )
                   )
            )
          )
      )
  ),
  
  # ENHANCED FOOTER
  div(class = "footer",
      div(class = "footer-logo",
          tags$a(href = "https://medicine.washu.edu/", target = "_blank",
                 img(src = "washu_logo.png", height = "70px", alt = "Washington University")),
          tags$a(href = "https://knightadrc.wustl.edu/", target = "_blank",
                 img(src = "kadrc_logo.png", height = "70px", alt = "Knight ADRC"))
      )
  )
)

# --- Server Logic ---

server <- function(input, output, session) {
  # Set in-memory cache size: 200 MB per session
  shinyOptions(cache = cachem::cache_mem(max_size = 200e6))
  
  # Load core ADNI dataset
  load("ADNI_Raindrop_Data_Test_v3.rdata")
  
  # Variable name mappings for efficient lookup
  var_suffix_by_label <- setNames(graph_choices_suffixes, graph_choices_ref)
  est_col_by_label <- setNames(paste0("EST_", graph_choices_suffixes), graph_choices_ref)
  bl_est_col_by_label <- setNames(paste0("BL_EST_", graph_choices_suffixes), graph_choices_ref)
  var_step_by_label <- setNames(step_size_list, graph_choices_ref)
  
  # Compute min/max for sliders per variable (from EST_ columns)
  range_map <- setNames(vector("list", length(graph_choices_ref)), graph_choices_ref)
  for (lbl in graph_choices_ref) {
    var_step <- var_step_by_label[[lbl]]
    dec_count <- decimalplaces(var_step)
    v <- est_col_by_label[[lbl]]
    v_min <- suppressWarnings(floor(min(data_10th_demo[, v], na.rm = TRUE)))
    v_max <- suppressWarnings(signif(max(data_10th_demo[, v], na.rm = TRUE), dec_count))
    if (!is.finite(v_min)) v_min <- 0
    if (!is.finite(v_max)) v_max <- 1
    range_map[[lbl]] <- c(min = v_min, max = as.numeric(paste(v_max)))
  }
  
  output$secondSelection <- renderUI({
    selectInput("other", "Grouping Variable:", choices = graph_choices_ref[scramble_key])
  })
  
  # Return y and x variable column names for plotting
  variableids_for_plot <- reactive({
    graph_choice_y <- var_suffix_by_label[[input$y_var]]
    graph_choice_x <- var_suffix_by_label[[input$x_var]]
    c(paste0("EST_", graph_choice_x), paste0("EST_", graph_choice_y))
  }) %>% bindCache(input$y_var, input$x_var)
  
  # Track all parameter changes affecting either thresholds or main inputs
  param_change <- reactive({
    list(
      input$cutoff1,input$cutoff2,input$cutoff3,input$cutoff4,
      input$y_var,input$x_var,input$group_xy_check,
      input$other_type,input$otherb_type,input$th1,input$th2
    )
  })
  
  # Track only which cutoffs or group field selection changed
  cutoff_change <- reactive({
    list(input$group_xy_check, input$other_type, input$otherb_type, input$reset_thresholds)
  })
  
  c1_change <- reactive({ list(input$cutoff1) })
  c2_change <- reactive({ list(input$cutoff2) })
  c3_change <- reactive({ list(input$cutoff3) })
  
  # Enforce monotonicity in group cutoffs
  observeEvent(c1_change(), {
    min2 <- input$cutoff1
    updateSliderInput(session, "cutoff2", min = min2)
  })
  
  observeEvent(c2_change(), {
    min3 <- input$cutoff2
    updateSliderInput(session, "cutoff3", min = min3)
  })
  
  observeEvent(c3_change(), {
    min4 <- input$cutoff3
    updateSliderInput(session, "cutoff4", min = min4)
  })
  
  # Auto-reset all group cutoffs on major changes
  observeEvent(cutoff_change(), {
    var <- if (input$group_xy_check == "Longitudinal") input$other_type else input$otherb_type
    group_thresholds <- extract_comma_sep(group_threshold_defaults[which(graph_choices_ref == var)])
    var_step <- var_step_by_label[[var]]
    r <- range_map[[var]]
    updateSliderInput(session, "cutoff1", value = group_thresholds[1], min = r[["min"]], max = r[["max"]], step = var_step)
    updateSliderInput(session, "cutoff2", value = group_thresholds[2], min = r[["min"]], max = r[["max"]], step = var_step)
    updateSliderInput(session, "cutoff3", value = group_thresholds[3], min = r[["min"]], max = r[["max"]], step = var_step)
    updateSliderInput(session, "cutoff4", value = group_thresholds[4], min = r[["min"]], max = r[["max"]], step = var_step)
  })
  
  group_cutoffs <- reactive({ paste(input$cutoff1,input$cutoff2,input$cutoff3,input$cutoff4,sep=",") })
  
  # Variable grouping logic (cached & event-driven)
  data_cat <- reactive({
    group_breaks <- group_cutoffs()
    group_thresholds <- extract_comma_sep(group_breaks)
    if (input$group_xy_check == "Longitudinal") {
      graph_choice <- var_suffix_by_label[[input$other_type]]
      splitting_variable <- paste0("EST_", graph_choice)
      if (input$other_type %in% high_risk_high) {
        create_groups_EST(data_10th_demo, splitting_variable, group_thresholds)
      } else if (input$other_type %in% high_risk_low) {
        create_groups_ratio_EST(data_10th_demo, splitting_variable, group_thresholds)
      }
    } else if (input$group_xy_check == "Baseline") {
      graph_choice <- var_suffix_by_label[[input$otherb_type]]
      splitting_variable <- paste0("BL_EST_", graph_choice)
      if (input$otherb_type %in% high_risk_high) {
        create_groups_EST(data_10th_demo, splitting_variable, group_thresholds)
      } else {
        create_groups_ratio_EST(data_10th_demo, splitting_variable, group_thresholds)
      }
    }
  }) %>%
    bindCache(
      input$group_xy_check, input$other_type, input$otherb_type,
      input$cutoff1, input$cutoff2, input$cutoff3, input$cutoff4
    ) %>%
    bindEvent(param_change())
  
  update_group_long <- reactive({ list(input$group_xy_check, input$other_type, input$otherb_type) })
  
  # Reset group thresholds to variable default on selection change
  observeEvent(input$other_type, {
    if (input$group_xy_check=="Longitudinal") {
      updated_threshold_set <- group_threshold_defaults[which(graph_choices_ref==input$other_type)]
      updateTextInput(session, 'group_breaks', value = updated_threshold_set)
    }
  })
  
  observeEvent(input$otherb_type, {
    if (input$group_xy_check=="Baseline") {
      updated_threshold_set <- group_threshold_defaults[which(graph_choices_ref==input$otherb_type)]
      updateTextInput(session, 'group_breaks', value = updated_threshold_set)
    }
  })
  
  # Main plot: uses renderCachedPlot (caches by input and group parameters)
  output$plot1 <- renderCachedPlot({
    req(data_cat())
    in.csv.graphdata <- data_cat()
    variables_for_plot <- variableids_for_plot()
    
    # Remove rows with missing key data for Fujirebio variables
    if (input$x_var==graph_choices_ref[4] | input$y_var==graph_choices_ref[4] |
        input$x_var==graph_choices_ref[5] | input$y_var==graph_choices_ref[5]) {
      in.csv.graphdata <- in.csv.graphdata[which(!is.na(in.csv.graphdata[, variables_for_plot[1]])), ]
      in.csv.graphdata <- in.csv.graphdata[which(!is.na(in.csv.graphdata[, variables_for_plot[2]])), ]
    }
    
    create_timepath_plot(
      in.csv.graphdata,
      (input$Time - 1),
      input$Time,
      variables_for_plot,
      input$x_var, input$y_var,
      graph_limits_list[[which(graph_choices_ref==input$x_var)]],
      graph_limits_list[[which(graph_choices_ref==input$y_var)]],
      input$th1, input$th2
    )
  },
  cache = "session",
  cacheKeyExpr = {
    list(
      x = input$x_var, y = input$y_var,
      mode = input$group_xy_check,
      ot = input$other_type, otb = input$otherb_type,
      c1 = input$cutoff1, c2 = input$cutoff2, c3 = input$cutoff3, c4 = input$cutoff4,
      th1 = input$th1, th2 = input$th2,
      t = input$Time
    )
  },
  res = 96
  )
  
  thlines <- reactive({list(input$th1,
                            input$th2,
                            input$group_xy_check)})
  
  # Generate MP4 creation URL using current parameters
  observeEvent(param_change(), {
    leadingurl <- "https://amyloid.shinyapps.io/ADNI_Movie_Out/"
    x_variable_id <- var_suffix_by_label[[input$x_var]]
    y_variable_id <- var_suffix_by_label[[input$y_var]]
    oty_variable_id <- var_suffix_by_label[[input$other_type]]
    otyb_variable_id <- var_suffix_by_label[[input$otherb_type]]
    group_breaks <- group_cutoffs()
    group_thresholds <- extract_comma_sep(group_breaks)
    
    out_args_opt <- paste(
      paste0(leadingurl,"?","graphtype=","Timetrails"),
      paste0("xvar=",x_variable_id),
      paste0("yvar=", y_variable_id),
      paste0("gxyc=",input$group_xy_check),
      paste0("oty=",oty_variable_id),
      paste0("otyb=",otyb_variable_id),
      paste0("co1=",group_thresholds[1]),
      paste0("co2=",group_thresholds[2]),
      paste0("co3=",group_thresholds[3]),
      paste0("co4=",group_thresholds[4]),
      paste0("th1=",input$th1),
      paste0("th2=",input$th2),
      sep="&"
    )
    
    output$tab <- renderUI({ 
      tags$a(
        "Generate MP4 in New Tab", 
        href = out_args_opt, 
        target = "_blank",
        class = "btn btn-default",
        style = "display: inline-block; margin-top: 10px;"
      )
    })
  })
  
  
  # Auto-sync threshold lines to currently selected X/Y variables
  observeEvent(input$x_var, {
    x <- input$x_var
    updated_threshold_th2 <- value_thresholds[which(graph_choices_ref==x)]
    updateNumericInput(session, 'th2', value = updated_threshold_th2)
  })
  
  observeEvent(input$y_var, {
    y <- input$y_var
    updated_threshold_th1 <- value_thresholds[which(graph_choices_ref==y)]
    updateNumericInput(session, 'th1', value = updated_threshold_th1)
  })
}

shinyApp(ui, server)
