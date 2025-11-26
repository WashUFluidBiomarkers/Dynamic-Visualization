#raindrops actual
# tidyverse in particular is incredibly important for graphing and filtering.
list.of.packages <- c("tidyverse", "data.table","shiny","RColorBrewer","ggforce","ggbreak","av","polished","shinyjs")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
library(colourpicker)
library(shiny)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(ggforce)
library(av)
library(shinyjs)
library(polished)
source("Needed_Functions.R")

##Replace APS with Tipping Point Probability ## Not
##X default - Centiloid
##Y default - % pt217
##Color default - CDR_SB
#(optional legend)
scramble_key <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,21,16,17,18,19,20)

step_size_list <- c(0.1, #C2N Plasma Ptau217 ratio
                    0.2, #C2N Plasma Ptau217 conc 
                    0.005, #C2N Plasma Ab4240
                    0.001, #Fujirebio Ptau217
                    0.001, #Fujirebio Ab4240
                    0.005, #Alzpath Ptau217
                    0.005, #Janssen Ptau217
                    0.01, #Roche Ptau181
                    0.005, #Roche Ab4240
                    0.001, #Roche GFAP
                    0.05, #Roche NfL
                    0.01, #Quanterix Ptau 181
                    1, #Quanterix GFAP
                    0.005, #Quanterix Ab4240
                    0.01, #Quanterix NfL
                    1,#Centiloid
                    0.01,#Tau PET Early
                    0.01,#Tau PET Late
                    0.001,#Cortical Thickness Meta ROI
                    0.5, #CDR Sum of Boxes
                    1)    #CSF ab42/ptau181

graph_choices_ref <- c(paste0("C2N PrecivityAD2 ","plasma %p-tau217 (%)"),
                       paste0("C2N PrecivityAD2 ","plasma p-tau217 (pg/mL)"),
                       paste0("C2N PrecivityAD2 ","plasma A","\U03B2","42/A","\U03B2","40"),
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
                       paste0("Roche Elecsys ","CSF A","\U03B2","42/p-tau181"))



graph_limits_list <- list(c(0,25), #C2N Plasma Ptau217 ratio
                          c(0,20), #C2N Plasma Ptau217 conc  
                          c(0,0.15), #C2N Plasma Ab4240
                          c(0,1.8), #Fujirebio Ptau217
                          c(0,0.3), #Fujirebio Ab4240
                          c(0,3), #Alzpath Ptau217
                          c(0,0.35), #Janssen Ptau217
                          c(0,4.7), #Roche Ptau181
                          c(0,0.23), #Roche Ab4240
                          c(0,0.46), #Roche GFAP
                          c(0,16), #Roche NfL
                          c(0,80), #Quanterix Ptau 181
                          c(0,565), #Quanterix GFAP
                          c(0,0.125), #Quanterix Ab4240
                          c(0,80), #Quanterix NfL
                          c(-40,190),#Centiloid
                          c(0.85,2),#Tau PET Early
                          c(0.9,2.5),#Tau PET Late
                          c(1.8,3.25),#Cortical Thickness Meta ROI
                          c(-0.5,10),#CDR Sum of Boxes
                          c(5,185)   #CSF ab42/ptau181
)




group_threshold_defaults <- c("1.5, 2.5, 4.1, 8",#C2N Plasma Ptau217 ratio
                              "3, 7, 13, 16", #C2N Plasma Ptau217 conc 
                              "0.04, 0.06, 0.0924, 0.1", #C2N Plasma Ab4240
                              "0.06, 0.1, 0.158, 0.5",  #Fujirebio Ptau217
                              "0.04, 0.06, 0.0869, 1.5",  #Fujirebio Ab4240
                              "0.2,0.3,0.444,0.5", #Alzpath Ptau217
                              "0.04,0.05,0.0615,0.085", #Janssen Ptau217
                              "0.65,1.0,1.14,1.3", #Roche Ptau181
                              "0.11,0.12,0.126,0.14", #Roche Ab4240
                              "0.0661, 0.0916, 0.113, 0.1230", #Roche GFAP
                              "2.650, 3.270, 4.29, 7", #Roche NfL
                              "12.1, 17.8, 20.5, 30", #Quanterix Ptau 181
                              "89, 128, 205, 250", #Quanterix GFAP
                              "0.045, 0.053, 0.0582, 0.070", #Quanterix Ab4240
                              "13.27, 17.57, 21.7, 28", #Quanterix NfL
                              "-5.00, 7.00, 20, 39",#Centiloid
                              "1.09, 1.15, 1.328, 1.4",#Tau PET Early
                              "1.05, 1.12, 1.269, 1.31",#Tau PET Late
                              "2.5, 2.572, 2.932, 3",#Cortical Thickness Meta ROI
                              "0.5, 1, 2, 4",#CDR Sum of Boxes
                              "10,25,60,91")    #CSF ab42/ptau181
rank_thresholds <- c(
  236, #C2N Plasma Ptau217 ratio
  254, #C2N Plasma Ptau217 conc 
  186, #C2N Plasma Ab4240
  241, #Fujirebio Ptau217
  160, #Fujirebio Ab4240
  250, #Alzpath Ptau217
  252, #Janssen Ptau217
  237, #Roche Ptau181
  209, #Roche Ab4240
  250, #Roche GFAP
  269, #Roche NfL
  203, #Quanterix Ptau 181
  260, #Quanterix GFAP
  130, #Quanterix Ab4240
  217, #Quanterix NfL
  233, #Centiloid
  103, #Tau PET Early
  114, #Tau PET Late
  26, #Cortical Thickness Meta ROI
  266, #CDR Sum of Boxes
  53 #CSF ab42/ptau181
)

# Default threshold lines for each variable
value_thresholds <- c(
  4.06, #C2N Plasma Ptau217 ratio
  2.34, #C2N Plasma Ptau217 conc
  0.0924, #C2N Plasma Ab4240
  0.158, #Fujirebio Ptau217
  0.0869, #Fujirebio Ab4240
  0.444, #Alzpath Ptau217
  0.0615, #Janssen Ptau217
  1.14, #Roche Ptau181
  0.126, #Roche Ab4240
  0.113, #Roche GFAP
  4.29, #Roche NfL
  20.5, #Quanterix Ptau 181
  205, #Quanterix GFAP
  0.0582, #Quanterix Ab4240
  21.7, #Quanterix NfL
  20, #Centiloid
  1.328, #Tau PET Early
  1.269, #Tau PET Late
  2.572, #Cortical Thickness Meta ROI
  1, #CDR Sum of Boxes
  25 #CSF ab42/ptau181
)
graph_choices_suffixes <- c("C2N_plasma_ptau217_ratio","C2N_plasma_ptau217", "C2N_plasma_Abeta42_Abeta40",    
                            "Fuji_plasma_ptau217","Fuji_plasma_Ab42_Ab40",   
                            "AlzPath_plasma_ptau217","Janssen_plasma_ptau217","Roche_plasma_ptau181","Roche_plasma_Ab42_Ab40",
                            "Roche_plasma_GFAP",  "Roche_plasma_NfL",       
                            "QX_plasma_ptau181","QX_plasma_GFAP","QX_plasma_Ab42_Ab40","QX_plasma_NfL",
                            "CENTILOIDS","MesialTemporal","TemporoParietal","metaROI","CDSOB","AB42_PTAU181")


high_risk_high <- graph_choices_ref[c(1,2,4,6,7,8,10,11,12,13,15,16,17,18,20)]
high_risk_low <- graph_choices_ref[c(3,5,9,14,19,21)]


ui <- shinyUI(fluidPage(
  useShinyjs(),
  tags$head(tags$style(".shiny-notification {
    position: fixed;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    width: 600px;
    height: 200px;
    font-size: 24px;
    text-align: center;
    opacity: 0.95;
    z-index: 1000;
  }")),
  
  # Hidden inputs - these exist for the code but aren't displayed
  shinyjs::hidden(
    selectInput("graphtype_anim", "Type:", choices=c("Raindrop","Timetrails"), selected="Timetrails"),
    selectInput("y_var", "Y:", choices=graph_choices_ref[scramble_key], selected=graph_choices_ref[1]),
    selectInput("x_var", "X:", choices=graph_choices_ref[scramble_key], selected=graph_choices_ref[16]),
    selectInput("group_xy_check", "Group:", choices=c("Longitudinal","Baseline"), selected="Baseline"),
    selectInput("other_type", "Other:", choices=graph_choices_ref[scramble_key], selected=graph_choices_ref[1]),
    selectInput("otherb_type", "OtherB:", choices=graph_choices_ref[scramble_key], selected=graph_choices_ref[1]),
    checkboxInput("origin_plot", "Origin", value=FALSE),
    sliderInput("cutoff1", "C1", min=0, max=20, step=0.2, value=0),
    sliderInput("cutoff2", "C2", min=0, max=20, step=0.2, value=0),
    sliderInput("cutoff3", "C3", min=0, max=20, step=0.2, value=0),
    sliderInput("cutoff4", "C4", min=0, max=20, step=0.2, value=0),
    numericInput("th1", "Th1", value=0.0673, step=0.0005),
    numericInput("th2_rank", "Th2 Rank", value=90),
    numericInput("th2_trail", "Th2 Trail", value=0.158, step=0.0005),
    actionButton("reset_thresholds", "Reset"),
    downloadButton("downloadData", "Download")
  ),
  
  # Empty white screen
  tags$div(style = "background-color: white; width: 100%; height: 100vh;")
))


server <- function(input, output, session) {
  load("ADNI_Raindrop_Data_Test_v3.rdata")
  
  # Auto-trigger download when app loads with URL parameters
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    # Only auto-trigger if coming from another app with URL parameters
    if(length(query) > 0){
      delay(1000, click("downloadData"))  # Wait 1 second then trigger download
    }
  })
  
  # Parse URL parameters and update hidden inputs
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if(!is.null(query[['graphtype']])){
      graph_variable_id <- query[['graphtype']]
      updateSelectInput(session, "graphtype_anim", selected = graph_variable_id)
    }
    
    if(!is.null(query[['xvar']])){
      x_variable_id <- graph_choices_ref[which(graph_choices_suffixes==query[['xvar']])]
      updateSelectInput(session, "x_var", selected = x_variable_id)
    }
    
    if(!is.null(query[['yvar']])){
      y_variable_id <- graph_choices_ref[which(graph_choices_suffixes==query[['yvar']])]
      updateSelectInput(session, "y_var", selected = y_variable_id)
    }
    
    if(!is.null(query[['gxyc']])){
      gxyc <- query[['gxyc']]
      updateSelectInput(session, "group_xy_check", selected = gxyc)
    }   
    
    if(!is.null(query[['oty']])){
      oty_variable_id <- graph_choices_ref[which(graph_choices_suffixes==query[['oty']])]
      updateSelectInput(session, "other_type", selected = oty_variable_id)
    }
    
    if(!is.null(query[['otyb']])){
      otyb_variable_id <- graph_choices_ref[which(graph_choices_suffixes==query[['otyb']])]
      updateSelectInput(session, "otherb_type", selected = otyb_variable_id)
    }
    
    if(!is.null(query[['co1']])){
      co1 <- as.numeric(query[['co1']])
      updateSliderInput(session, "cutoff1", value = co1)
    }
    
    if(!is.null(query[['co2']])){
      co2 <- as.numeric(query[['co2']])
      updateSliderInput(session, "cutoff2", value = co2)
    }
    
    if(!is.null(query[['co3']])){
      co3 <- as.numeric(query[['co3']])
      updateSliderInput(session, "cutoff3", value = co3)
    }
    
    if(!is.null(query[['co4']])){
      co4 <- as.numeric(query[['co4']])
      updateSliderInput(session, "cutoff4", value = co4)
    }
    
    if(!is.null(query[['th1']])){
      th1 <- as.numeric(query[['th1']])
      updateNumericInput(session,'th1', value=th1)
    }
    
    if(!is.null(query[['th2']])){
      th2 <- as.numeric(query[['th2']])
      updateNumericInput(session,'th2_rank', value=th2)
      updateNumericInput(session,'th2_trail', value=th2)
    }
    
    if(!is.null(query[['ogn']])){
      ogn <- query[['ogn']]
      ogn <- ogn =="TRUE"
      updateCheckboxInput(session,'origin_plot',value = ogn)
    }
  })
  
  output$secondSelection <- renderUI({
    selectInput("other", "Grouping Variable:", choices = graph_choices_ref[scramble_key])
  })
  
  # Reactive expressions
  variableids_for_plot <- reactive({
    if(input$graphtype_anim=="Raindrop"){
      graph_choice_y <- graph_choices_suffixes[which(graph_choices_ref==input$y_var)]
      graph_choice_BL <- graph_choices_suffixes[which(graph_choices_ref==input$x_var)]
      graph_vars_list <- c(paste0("N_",graph_choice_BL),
                           paste0("BL_EST_",graph_choice_BL),
                           paste0("EST_",graph_choice_y))
    }else{
      graph_choice_y <- graph_choices_suffixes[which(graph_choices_ref==input$y_var)]
      graph_choice_x <- graph_choices_suffixes[which(graph_choices_ref==input$x_var)]
      graph_vars_list <- c(paste0("EST_",graph_choice_x),
                           paste0("EST_",graph_choice_y))
    }
    graph_vars_list
  })
  
  param_change <- reactive({
    list(input$graphtype_anim,input$cutoff1,input$cutoff2,input$cutoff3,input$cutoff4,
         input$y_var, input$x_var, input$group_xy_check,
         input$other_type, input$otherb_type)
  })
  
  update_origin_point <- reactive({input$origin_plot})
  
  cutoff_change <- reactive({
    list(input$group_xy_check, input$other_type, input$otherb_type, input$reset_thresholds)
  })
  
  c1_change <- reactive({list(input$cutoff1)})
  c2_change <- reactive({list(input$cutoff2)})
  c3_change <- reactive({list(input$cutoff3)})
  
  observeEvent(c1_change(),{
    min2 <- input$cutoff1
    updateSliderInput(session, "cutoff2", min = min2)
  })
  
  observeEvent(c2_change(),{
    min3 <- input$cutoff2
    updateSliderInput(session, "cutoff3", min = min3)
  })
  
  observeEvent(c3_change(),{
    min4 <- input$cutoff3
    updateSliderInput(session, "cutoff4", min = min4)
  })
  
  observeEvent(cutoff_change(),{
    req(input$group_xy_check)
    
    if(input$group_xy_check=="Longitudinal"){
      req(input$other_type)
      var <- input$other_type
    } else {
      req(input$otherb_type)
      var <- input$otherb_type 
    }
    
    group_thresholds <- extract_comma_sep(group_threshold_defaults[which(graph_choices_ref==var)])
    group_choice <- graph_choices_suffixes[which(graph_choices_ref==var)]
    group_var <- paste0("EST_",group_choice)
    
    var_step <- step_size_list[which(graph_choices_ref==var)]
    dec_count <- decimalplaces(var_step)
    
    max_value_slider <- paste(signif(max(data_10th_demo[,group_var],na.rm = T),dec_count))
    
    updateSliderInput(session, "cutoff1",value=group_thresholds[1],
                      min = floor(min(data_10th_demo[,group_var],na.rm = T)), 
                      max = max_value_slider, 
                      step = var_step)
    updateSliderInput(session, "cutoff2",value=group_thresholds[2],
                      min = floor(min(data_10th_demo[,group_var],na.rm = T)), 
                      max = max_value_slider, 
                      step = var_step)
    updateSliderInput(session, "cutoff3",value=group_thresholds[3],
                      min = floor(min(data_10th_demo[,group_var],na.rm = T)), 
                      max = max_value_slider, 
                      step = var_step)
    updateSliderInput(session, "cutoff4",value=group_thresholds[4],
                      min = floor(min(data_10th_demo[,group_var],na.rm = T)), 
                      max = max_value_slider, 
                      step = var_step)
    
    query <- parseQueryString(session$clientData$url_search)
    if(!is.null(query[['co1']])){
      co1 <- as.numeric(query[['co1']])
      updateSliderInput(session, "cutoff1", value = co1)
    }
    
    if(!is.null(query[['co2']])){
      co2 <- as.numeric(query[['co2']])
      updateSliderInput(session, "cutoff2", value = co2)
    }
    
    if(!is.null(query[['co3']])){
      co3 <- as.numeric(query[['co3']])
      updateSliderInput(session, "cutoff3", value = co3)
    }
    
    if(!is.null(query[['co4']])){
      co4 <- as.numeric(query[['co4']])
      updateSliderInput(session, "cutoff4", value = co4)
    }
  })
  
  group_cutoffs <- reactive({
    paste(input$cutoff1,input$cutoff2,input$cutoff3,input$cutoff4,sep=",")
  })
  
  data_cat <- eventReactive(param_change(),{
    variables_for_plot <- variableids_for_plot()
    group_breaks <- group_cutoffs()
    group_thresholds <- extract_comma_sep(group_breaks)
    
    if(input$group_xy_check=="Longitudinal"){
      graph_choice <- graph_choices_suffixes[which(graph_choices_ref==input$other_type)]
      splitting_variable <- paste0("EST_",graph_choice)
      
      if(input$other_type %in% high_risk_high){
        create_groups_EST(data_10th_demo,splitting_variable,group_thresholds)
      } else if (input$other_type %in% high_risk_low) {
        create_groups_ratio_EST(data_10th_demo,splitting_variable,group_thresholds)
      }
    } else if(input$group_xy_check=="Baseline"){
      graph_choice <- graph_choices_suffixes[which(graph_choices_ref==input$otherb_type)]
      splitting_variable <- paste0("BL_EST_",graph_choice)
      
      if(input$otherb_type %in% high_risk_high){
        create_groups_EST(data_10th_demo,splitting_variable,group_thresholds)
      } else {
        create_groups_ratio_EST(data_10th_demo,splitting_variable,group_thresholds)
      }
    }
  })
  
  # Download Handler - at top level, not inside observe()
  output$downloadData <- downloadHandler(
    filename = function() {
      xvar_filename <- graph_choices_suffixes[which(graph_choices_ref==input$x_var)]
      yvar_filename <- graph_choices_suffixes[which(graph_choices_ref==input$y_var)]
      
      if(input$graphtype_anim=="Raindrop"){
        paste0("out_raindrop_",yvar_filename,"_vs_",xvar_filename,".mp4")
      } else {
        paste0("out_timetrails_",yvar_filename,"_vs_",xvar_filename,".mp4")
      }
    },
    content = function(file) {
      source("Needed_Functions.R")
      
      in.csv.graphdata <- data_cat()
      variables_for_plot <- variableids_for_plot()
      in.csv.graphdata <- in.csv.graphdata[which(complete.cases(in.csv.graphdata[,variables_for_plot])),]
      
      if(input$graphtype_anim=="Raindrop"){
        in.csv.graphdata[,variables_for_plot[1]] <- as.numeric(factor(in.csv.graphdata[,variables_for_plot[1]]))
        limits_for_plot <- graph_limits_list[[which(graph_choices_ref==input$y_var)]]
        
        withProgress(message = 'Video being created', 
                     detail = 'Please wait approximately 1 minute. Video will download automatically when complete.', 
                     value = 0, {
                       av_capture_graphics(create_raindrop_plot_movie(data = in.csv.graphdata,
                                                                      variablelist = variables_for_plot,
                                                                      ylimits=limits_for_plot,
                                                                      graph.xlab = input$x_var,
                                                                      graph.ylab = input$y_var,
                                                                      th1=input$th1,
                                                                      th2=input$th2_rank,
                                                                      originpoints=FALSE),
                                           output = file,
                                           res=100,
                                           width=1250,
                                           height=750, 
                                           framerate = 5)
                     }
        )
        
        showNotification(
          "Video completed and downloading!",
          duration = 3,
          type = "message"
        )
        
      } else if(input$graphtype_anim=="Timetrails"){
        xlimits_for_plot <- graph_limits_list[[which(graph_choices_ref==input$x_var)]]
        limits_for_plot <- graph_limits_list[[which(graph_choices_ref==input$y_var)]]
        
        withProgress(message = 'Video being created', 
                     detail = 'Please wait approximately 1 minute. Video will download automatically when complete.', 
                     value = 0, {
                       av_capture_graphics(create_timepath_plot_video(data = in.csv.graphdata,
                                                                      variablelist = variables_for_plot,
                                                                      xlimits=xlimits_for_plot,
                                                                      ylimits=limits_for_plot,
                                                                      graph.xlab = input$x_var,
                                                                      graph.ylab = input$y_var,
                                                                      th1=input$th1,
                                                                      th2=input$th2_trail),
                                           output = file,
                                           res=100,
                                           width=1250,
                                           height=750, 
                                           framerate = 5)
                     }
        )
        
        showNotification(
          "Video completed and downloading!",
          duration = 3,
          type = "message"
        )
      }
    },
    contentType = "video/mp4"
  )
  
  outputOptions(output, "downloadData", suspendWhenHidden = FALSE)
  
  update_group_long <- reactive({
    list(input$group_xy_check, input$other_type, input$otherb_type)
  })
  
  # Update threshold values when x_var changes
  observeEvent(input$x_var,{
    x = input$x_var
    query <- parseQueryString(session$clientData$url_search)
    updated_threshold_th2 <- rank_thresholds[which(graph_choices_ref==x)]
    
    updateNumericInput(session,'th2_rank', value=updated_threshold_th2)
    
    if(!is.null(query[['th2']])){
      th2 <- as.numeric(query[['th2']])
      updateNumericInput(session,'th2_rank', value=th2)
    }
    
    updated_threshold_th2 <- value_thresholds[which(graph_choices_ref==x)]
    updateNumericInput(session,'th2_trail', value=updated_threshold_th2)
    
    if(!is.null(query[['th2']])){
      th2 <- as.numeric(query[['th2']])
      updateNumericInput(session,'th2_trail', value=th2)
    }
  })
  
  # Update threshold values when y_var changes
  observeEvent(input$y_var,{
    y = input$y_var
    query <- parseQueryString(session$clientData$url_search)
    
    updated_threshold_th1 <- value_thresholds[which(graph_choices_ref==y)]
    updateNumericInput(session,'th1', value=updated_threshold_th1)
    
    if(!is.null(query[['th1']])){
      th1 <- as.numeric(query[['th1']])
      updateNumericInput(session,'th1', value=th1)
    }
  })
}

shinyApp(ui, server)

