#Useful Functions
#Ben Saef
#This will be added to whenever needed
library(tidyverse)
library(ggplot2)
library(data.table)
library(khroma)
library(cli)
####Raindrop Plot - Testing ####
##Replace APS with Tipping Point Probability ## Not
##
scramble_key <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

graph_choices_ref <- c(paste0("C2N Precivity AD2 ","plasma %p-tau217 (%)"),
                       paste0("C2N Precivity AD2 ","plasma p-tau217 (pg/mL)"),
                       paste0("C2N Precivity AD2 ","plasma A","\U03B2","42/A","\U03B2","40"),
                       paste0("Fujirebio Lumipulse ","plasma p-tau217 (pg/mL)"),
                       paste0("Fujirebio Lumipulse ","plasma A","\U03B2","42/A","\U03B2","40"),
                       paste0("ALZpath Quanterix ","plasma p-tau217 (pg/mL)"),
                       paste0("Janssen LucentAD Quanterix ","plasma p-tau217 (pg/mL)"),
                       paste0("Roche NeuroToolKit ","plasma p-tau 181 (pg/mL)"),
                       paste0("Roche NeuroToolKit ","plasma A","\U03B2","42/A","\U03B2","40"),
                       paste0("Roche NeuroToolKit ","plasma GFAP (ng/mL)"),
                       paste0("Roche NeuroToolKit ","plasma NfL (pg/mL)"),
                       paste0("Quanterix Neurology 4-Plex ","plasma p-tau 181 (pg/ml)"),
                       paste0("Quanterix Neurology 4-Plex ","plasma GFAP (pg/mL)"),
                       paste0("Quanterix Neurology 4-Plex ","plasma A","\U03B2","42/A","\U03B2","40"),
                       paste0("Quanterix Neurology 4-Plex ","plasma NfL (pg/mL)"),
                       "Amyloid PET Centiloid",
                       "Tau PET Mesial-Temporal (Early)",
                       "Tau PET Temporo-Parietal (Late)",
                       "Cortical Thickness (Meta ROI)",
                       "Clinical Dementia Rating Sum of Boxes")



graph_limits_list <- list(c(0,30.7), #C2N Plasma Ptau217 ratio
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
                          c(0,10)#CDR Sum of Boxes
)


graph_choices_suffixes <- c("C2N_plasma_ptau217_ratio","C2N_plasma_ptau217", "C2N_plasma_Abeta42_Abeta40",    
                            "Fuji_plasma_ptau217","Fuji_plasma_Ab42_Ab40",   
                            "AlzPath_plasma_ptau217","Janssen_plasma_ptau217","Roche_plasma_ptau181","Roche_plasma_Ab42_Ab40",
                            "Roche_plasma_GFAP",  "Roche_plasma_NfL",       
                            "QX_plasma_ptau181","QX_plasma_GFAP","QX_plasma_Ab42_Ab40","QX_plasma_NfL",
                            "CENTILOIDS","MesialTemporal","TemporoParietal","metaROI","CDSOB")

create_groups_ratio_EST <- function(dataset,Estimated_val_var,thresholds){
  dataset[,"groupvar"] <- "NODATA"
  dataset[which(dataset[,Estimated_val_var] < thresholds[1]),"groupvar"] <- "A"
  dataset[which(dataset[,Estimated_val_var] > thresholds[1] & dataset[,Estimated_val_var] < thresholds[2] ),
          "groupvar"]  <- "B"
  dataset[which(dataset[,Estimated_val_var] > thresholds[2] & dataset[,Estimated_val_var] < thresholds[3] ),
          "groupvar"]  <- "C"
  dataset[which(dataset[,Estimated_val_var] > thresholds[3] & dataset[,Estimated_val_var] < thresholds[4] ),
          "groupvar"]  <- "D"
  dataset[which(dataset[,Estimated_val_var] > thresholds[4]),
          "groupvar"]  <- "E"
  return(dataset)
}


create_groups_EST <- function(dataset,Estimated_val_var,thresholds){
  dataset[,"groupvar"] <- "NODATA"
  dataset[which(dataset[,Estimated_val_var] < thresholds[1]),"groupvar"] <- "E"
  dataset[which(dataset[,Estimated_val_var] > thresholds[1] & dataset[,Estimated_val_var] < thresholds[2] ),
          "groupvar"]  <- "D"
  dataset[which(dataset[,Estimated_val_var] > thresholds[2] & dataset[,Estimated_val_var] < thresholds[3] ),
          "groupvar"]  <- "C"
  dataset[which(dataset[,Estimated_val_var] > thresholds[3] & dataset[,Estimated_val_var] < thresholds[4] ),
          "groupvar"]  <- "B"
  dataset[which(dataset[,Estimated_val_var] > thresholds[4]),
          "groupvar"]  <- "A"
  return(dataset)
}


extract_comma_sep <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  split_n <- as.numeric(split)
  return(split_n)
}

decimalplaces <- function(x) { ## Retrieved from https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

create_raindrop_plot_pro <- function(data,
                                     time1,time2,
                                     variablelist,ylimits,
                                     graph.xlab,graph.ylab,
                                     th1=0.0673,th2=90){
  require(RColorBrewer)
  require(tidyverse)
  require(ggbreak)
  require(ggforce)
  graph_choices_ref <- c(paste0("C2N Precivity AD2 ","plasma %p-tau217 (%)"),
                         paste0("C2N Precivity AD2 ","plasma p-tau217 (pg/mL)"),
                         paste0("C2N Precivity AD2 ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Fujirebio Lumipulse ","plasma p-tau217 (pg/mL)"),
                         paste0("Fujirebio Lumipulse ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("ALZpath Quanterix ","plasma p-tau217 (pg/mL)"),
                         paste0("Janssen LucentAD Quanterix ","plasma p-tau217 (pg/mL)"),
                         paste0("Roche NeuroToolKit ","plasma p-tau 181 (pg/mL)"),
                         paste0("Roche NeuroToolKit ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Roche NeuroToolKit ","plasma GFAP (ng/mL)"),
                         paste0("Roche NeuroToolKit ","plasma NfL (pg/mL)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma p-tau 181 (pg/ml)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma GFAP (pg/mL)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Quanterix Neurology 4-Plex ","plasma NfL (pg/mL)"),
                         "Amyloid PET Centiloid",
                         "Tau PET Mesial-Temporal (Early)",
                         "Tau PET Temporo-Parietal (Late)",
                         "Cortical Thickness (Meta ROI)",
                         "Clinical Dementia Rating Sum of Boxes")
  
  
  graph_choices_suffixes <- c("C2N_plasma_ptau217_ratio","C2N_plasma_ptau217", "C2N_plasma_Abeta42_Abeta40",    
                              "Fuji_plasma_ptau217","Fuji_plasma_Ab42_Ab40",   
                              "AlzPath_plasma_ptau217","Janssen_plasma_ptau217","Roche_plasma_ptau181","Roche_plasma_Ab42_Ab40",
                              "Roche_plasma_GFAP",  "Roche_plasma_NfL",       
                              "QX_plasma_ptau181","QX_plasma_GFAP","QX_plasma_Ab42_Ab40","QX_plasma_NfL",
                              "CENTILOIDS","MesialTemporal","TemporoParietal","metaROI","CDSOB")
  
  graph_vars_list <- variablelist
  
  mycols <- c("A"="#dc143c", "B"="#ffa500", "C"="#3cb371", "D"="#4169e1", "E"="#4b0082","NODATA"="#D3D3D3")
  th2_lab <- round(data[which(data[,graph_vars_list[1]]==th2),][1,graph_vars_list[2]],digits=4)
  interval_xrank <- round(max(data[,graph_vars_list[1]],na.rm = T)/5)
  keeps<-c(1,
           interval_xrank*1,
           interval_xrank*2,
           interval_xrank*3,
           interval_xrank*4,
           max(data[,graph_vars_list[1]]))
  breaklabels <- c()
  for(i in 1:length(keeps)){
    current_break <- keeps[i]
    breaklabels[i] <- round(data[which(data[,graph_vars_list[1]]==current_break),][1,graph_vars_list[2]],digits=4)
  }
  keeps[7] <- th2
  breaklabels[7] <- paste0("\n",th2_lab)
  data.for.graph <- subset(data, time >= time1 & time < time2)
  ymax <- ylimits[2]
  ymin <- ylimits[1]
  data.for.graph.latest <- data.for.graph %>% group_by(RID) %>% filter(time==max(time) & time >= time2-0.2)
  data.for.graph.leadup <- data.for.graph %>% group_by(RID) %>% filter(time!=max(time) & time < time2-0.2)
  
  
  graph.data.baseline <- subset(data, time == 0)
  N_subs <- nrow(unique(data.for.graph.latest[,"RID"]))
  Title_for_plot <- paste0("Years since baseline: ",format(round(time2,digits = 2),nsmall=1),"\n Number of individuals:", N_subs)
  raindrops_plot  <- ggplot(data=data.for.graph.latest)+
    geom_point(aes_string(x=graph_vars_list[1],y=graph_vars_list[3],color="groupvar"),pch=19,size=3)+
    geom_point(data=data.for.graph.leadup,aes_string(x=graph_vars_list[1],y=graph_vars_list[3],color="groupvar"),pch=19,size=1.5)+
    geom_line(aes_string(x=graph_vars_list[1],y=graph_vars_list[3],group = "RID",color="groupvar"),
              arrow = arrow(length=unit(0.20,"cm"), 
                            ends="last", type = "closed"))+
    scale_fill_manual(values = mycols)+scale_color_manual(values = mycols)+
    geom_point(data=graph.data.baseline,aes_string(x=graph_vars_list[1],y=graph_vars_list[3]),pch=21,size=2)+
    ylim(ymin,ymax)+
    coord_fixed(ratio=0.5)+
    ylab(graph.ylab)+
    xlab(paste0(graph.xlab," in rank order of baseline value"))+
    geom_hline(yintercept=th1, linetype="dashed", 
               color = "black", size=1)+
    geom_vline(xintercept=th2, linetype="dashed", 
               color = "black", size=1)+
    theme_classic()+
    theme(aspect.ratio = 0.5,legend.position = "none",
          axis.title.x = element_text(face="bold",size=14),
          axis.title.y = element_text(face="bold",size=14),
          axis.text.x = element_text(size=13),
          axis.text.y = element_text(size=13),
          plot.title = element_text(size=14,face="bold",hjust=0.75))+
          ggtitle(Title_for_plot)

  

  
if(graph.xlab == graph_choices_ref[1] | graph.xlab == graph_choices_ref[3] | graph.xlab == graph_choices_ref[8] | graph.xlab == graph_choices_ref[10]){
raindrops_final <- raindrops_plot+scale_x_reverse(breaks=keeps,labels=breaklabels)
  
} else {
raindrops_final <- raindrops_plot+scale_x_continuous(breaks=keeps,labels=breaklabels)  
  }
raindrops_final
  #
  #
  
  
}








create_timepath_plot <- function(data,
                                     time1,time2,
                                     variablelist,
                                     graph.xlab,graph.ylab,
                                     xlimits,ylimits,
                                     th1=0.0673,th2=90){
  require(RColorBrewer)
  require(tidyverse)
  require(ggbreak)
  require(ggforce)
  graph_choices_ref <- c(paste0("C2N Precivity AD2 ","plasma %p-tau217 (%)"),
                         paste0("C2N Precivity AD2 ","plasma p-tau217 (pg/mL)"),
                         paste0("C2N Precivity AD2 ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Fujirebio Lumipulse ","plasma p-tau217 (pg/mL)"),
                         paste0("Fujirebio Lumipulse ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("ALZpath Quanterix ","plasma p-tau217 (pg/mL)"),
                         paste0("Janssen LucentAD Quanterix ","plasma p-tau217 (pg/mL)"),
                         paste0("Roche NeuroToolKit ","plasma p-tau 181 (pg/mL)"),
                         paste0("Roche NeuroToolKit ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Roche NeuroToolKit ","plasma GFAP (ng/mL)"),
                         paste0("Roche NeuroToolKit ","plasma NfL (pg/mL)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma p-tau 181 (pg/ml)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma GFAP (pg/mL)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Quanterix Neurology 4-Plex ","plasma NfL (pg/mL)"),
                         "Amyloid PET Centiloid",
                         "Tau PET Mesial-Temporal (Early)",
                         "Tau PET Temporo-Parietal (Late)",
                         "Cortical Thickness (Meta ROI)",
                         "Clinical Dementia Rating Sum of Boxes")
  
  
  graph_choices_suffixes <- c("C2N_plasma_ptau217_ratio","C2N_plasma_ptau217", "C2N_plasma_Abeta42_Abeta40",    
                              "Fuji_plasma_ptau217","Fuji_plasma_Ab42_Ab40",   
                              "AlzPath_plasma_ptau217","Janssen_plasma_ptau217","Roche_plasma_ptau181","Roche_plasma_Ab42_Ab40",
                              "Roche_plasma_GFAP",  "Roche_plasma_NfL",       
                              "QX_plasma_ptau181","QX_plasma_GFAP","QX_plasma_Ab42_Ab40","QX_plasma_NfL",
                              "CENTILOIDS","MesialTemporal","TemporoParietal","metaROI","CDSOB","AB42_PTAU181")
  
  graph_vars_list <- variablelist
  
  mycols <- c("A"="#dc143c", "B"="#ffa500", "C"="#3cb371", "D"="#4169e1", "E"="#4b0082","NODATA"="#D3D3D3")
  data.1 <- data
  data.for.graph <- subset(data.1, time >= time1 & time <= time2)
  ymax <- ylimits[2]
  xmax <- xlimits[2]
  xmin <- xlimits[1]
  ymin <- ylimits[1]
  
  #2)	The arrow is the point estimate and the line is the trail for 1 year afterwards.  
  #Can you make the arrow “disappear” when the time is past the point?  Let me know if you’re not sure what I mean.
  
  ##Troubleshooting
  #A. *Can't Work* If statement changing plot based on status - this doesn't work because it's on a by RID basis, not the plot
  #as a whole
  
  #B. Split data for graph leadup line into 2 datasets, it will go into the arrow dataset until max(time) > time2
  # After which the data goes into a second dataset that will go into a second geom_path statement but with no arrow statement -
  # Altering the plot code, but theoretically it should be minimal differences. need to be very careful
  
  ####Addendum to approach B - add filter statement to exclude RIDs that have max(time) < time2
  
  #C. Attempt to add another groupvar/color variable code for "transparant" once max(time) >  time2 - less altering the plot code
  #but more work on the front end
  
  #D. Check to see if we can separate out the arrow statement somehow? - questionable, not sure if possible while maintaining proper
  #directionality - kind of a last resort if the other approaches fail
  
  data.for.graph.latest <- data.for.graph %>% group_by(RID) %>% filter(time==max(time) & time >= time2-0.2)
  endtime <- max(unique(data.for.graph.latest$time))
  data.for.graph.leadup <- data.for.graph %>% group_by(RID) %>% filter(time!=max(time) & time < endtime)
  #Don't like that this is very dependent on interval
  data.for.graph.leadup.line <- data.for.graph %>% 
    group_by(RID) %>% 
    arrange(RID,time,by_group=TRUE) %>% 
    filter(max(time)-min(time) >= 0.95 | max(time) < 1.2) %>%
    slice(tail(row_number(), 2)) 

  N_subs <- nrow(unique(data.for.graph.latest[,"RID"]))
  Title_for_plot <- paste0("Years since baseline: ",format(round(time2,digits = 2),nsmall=1),"\n Number of individuals:", N_subs)

    omnidir_plot  <- ggplot(data=data.for.graph.latest)+
    geom_path(data=data.for.graph.leadup.line,aes_string(x=graph_vars_list[1],y=graph_vars_list[2],group = "RID",color="groupvar"),
              arrow = arrow(length=unit(0.20,"cm"), 
                            ends="last", type = "closed"))+
    geom_path(data=data.for.graph.leadup,aes_string(x=graph_vars_list[1],y=graph_vars_list[2],group = "RID",color="groupvar"))+
    scale_fill_manual(values = mycols)+scale_color_manual(values = mycols)+
    coord_fixed(ratio=0.5)+
    ylab(graph.ylab)+
    xlab(paste0(graph.xlab))+
    geom_hline(yintercept=th1, linetype="dashed", 
               color = "black", size=1)+
    geom_vline(xintercept=th2, linetype="dashed", 
               color = "black", size=1)+
    theme_classic()+
    theme(aspect.ratio = 0.5,legend.position = "none",
          axis.title.x = element_text(face="bold",size=14),
          axis.title.y = element_text(face="bold",size=14),
          axis.text.x = element_text(size=13),
          axis.text.y = element_text(size=13),
          plot.title = element_text(size=14,face="bold",hjust=1))+
    ggtitle(Title_for_plot)
  
  if(graph.ylab == graph_choices_ref[3] | graph.ylab == graph_choices_ref[5] | graph.ylab == graph_choices_ref[9] | graph.ylab == graph_choices_ref[14]){
    omnidir_final <- omnidir_plot+scale_y_reverse(limits=c(ymax,ymin), expand = c(0, 0))
  }else {
    omnidir_final <- omnidir_plot+scale_y_continuous(limits=c(ymin,ymax), expand = c(0, 0))  
  }
  
  if(graph.xlab == graph_choices_ref[3] | graph.xlab == graph_choices_ref[5] | graph.xlab == graph_choices_ref[9] | graph.xlab == graph_choices_ref[14]){
    omnidir_final <- omnidir_final+scale_x_reverse(limits=c(xmax,xmin), expand = c(0, 0))
  }else {
    omnidir_final <- omnidir_final+scale_x_continuous(limits=c(xmin,xmax), expand = c(0, 0))  
  }
  omnidir_final
  #
  #
  
  
}

create_timepath_plot_video <- function(data,
                                     variablelist,
                                     graph.xlab,graph.ylab,
                                     xlimits,ylimits,
                                     th1,th2){
  require(av)
  graph_choices_ref <- c(paste0("C2N Precivity AD2 ","plasma %p-tau217 (%)"),
                         paste0("C2N Precivity AD2 ","plasma p-tau217 (pg/mL)"),
                         paste0("C2N Precivity AD2 ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Fujirebio Lumipulse ","plasma p-tau217 (pg/mL)"),
                         paste0("Fujirebio Lumipulse ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("ALZpath Quanterix ","plasma p-tau217 (pg/mL)"),
                         paste0("Janssen LucentAD Quanterix ","plasma p-tau217 (pg/mL)"),
                         paste0("Roche NeuroToolKit ","plasma p-tau 181 (pg/mL)"),
                         paste0("Roche NeuroToolKit ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Roche NeuroToolKit ","plasma GFAP (ng/mL)"),
                         paste0("Roche NeuroToolKit ","plasma NfL (pg/mL)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma p-tau 181 (pg/ml)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma GFAP (pg/mL)"),
                         paste0("Quanterix Neurology 4-Plex ","plasma A","\U03B2","42/A","\U03B2","40"),
                         paste0("Quanterix Neurology 4-Plex ","plasma NfL (pg/mL)"),
                         "Amyloid PET Centiloid",
                         "Tau PET Mesial-Temporal (Early)",
                         "Tau PET Temporo-Parietal (Late)",
                         "Cortical Thickness (Meta ROI)",
                         "Clinical Dementia Rating Sum of Boxes")
  
  graph_vars_list <- variablelist

  
  ymax <- ylimits[2]
  xmax <- xlimits[2]
  xmin <- xlimits[1]
  ymin <- ylimits[1]
  
  for(time_set in 1:120){
    
    time2 <- (time_set-1)*0.1
    time1 <- time2-1
    
    require(RColorBrewer)
    require(tidyverse)
    require(ggbreak)
    require(ggforce)

    
    
    graph_choices_suffixes <- c("L_Ab42","L_Ab40","L_Ab4240",
                                "APS_C2N","CENT",
                                "L_tau","L_ptau",
                                "Abeta42_plasma_C2N","Abeta40_plasma_C2N","Abeta4240_plasma_C2N")
    
    mycols <- c("A"="#dc143c", "B"="#ffa500", "C"="#3cb371", "D"="#4169e1", "E"="#4b0082","NODATA"="#D3D3D3")
    data.1 <- data
    data.for.graph <- subset(data.1, time >= time1 & time <= time2)

    
    #Needs rewrite
    data.for.graph.latest <- data.for.graph %>% group_by(RID) %>% filter(time==max(time) & time >= time2-0.2)
    endtime <- max(unique(data.for.graph.latest$time))
    data.for.graph.leadup <- data.for.graph %>% group_by(RID) %>% filter(time!=max(time) & time < endtime)
    #Don't like that this is very dependent on interval
    data.for.graph.leadup.line <- data.for.graph %>% 
      group_by(RID) %>% 
      arrange(RID,time,by_group=TRUE) %>% 
      filter(max(time)-min(time) >= 0.95 | max(time) < 1.2) %>%
      slice(tail(row_number(), 2)) 
    
    N_subs <- nrow(unique(data.for.graph.latest[,"RID"]))
    Title_for_plot <- paste0("Years since baseline: ",format(round(time2,digits = 2),nsmall=1),"\n Number of individuals:", N_subs)
    omnidir_plot  <- ggplot(data=data.for.graph.latest)+
      geom_path(data=data.for.graph.leadup.line,aes_string(x=graph_vars_list[1],y=graph_vars_list[2],group = "RID",color="groupvar"),
                arrow = arrow(length=unit(0.20,"cm"), 
                              ends="last", type = "closed"))+
      geom_path(data=data.for.graph.leadup,aes_string(x=graph_vars_list[1],y=graph_vars_list[2],group = "RID",color="groupvar"))+
      scale_fill_manual(values = mycols)+scale_color_manual(values = mycols)+
      coord_fixed(ratio=0.5)+
      ylab(graph.ylab)+
      xlab(paste0(graph.xlab))+
      geom_hline(yintercept=th1, linetype="dashed", 
                 color = "black", size=1)+
      geom_vline(xintercept=th2, linetype="dashed", 
                 color = "black", size=1)+
      theme_classic()+
      theme(aspect.ratio = 0.5,legend.position = "none",
            axis.title.x = element_text(face="bold",size=14),
            axis.title.y = element_text(face="bold",size=14),
            axis.text.x = element_text(size=13),
            axis.text.y = element_text(size=13),
            plot.title = element_text(size=14,face="bold",hjust=1))+
      ggtitle(Title_for_plot)
    
    if(graph.ylab == graph_choices_ref[3] | graph.ylab == graph_choices_ref[5] | graph.ylab == graph_choices_ref[9] | graph.ylab == graph_choices_ref[14]){
      omnidir_final <- omnidir_plot+scale_y_reverse(limits=c(ymax,ymin), expand = c(0, 0))
    }else {
      omnidir_final <- omnidir_plot+scale_y_continuous(limits=c(ymin,ymax), expand = c(0, 0))  
    }
    
    if(graph.xlab == graph_choices_ref[3] | graph.xlab == graph_choices_ref[5] | graph.xlab == graph_choices_ref[9] | graph.xlab == graph_choices_ref[14]){
      omnidir_final <- omnidir_final+scale_x_reverse(limits=c(xmax,xmin), expand = c(0, 0))
    }else {
      omnidir_final <- omnidir_final+scale_x_continuous(limits=c(xmin,xmax), expand = c(0, 0))  
    }
print(omnidir_final)
    #
    #
    
  }

}





create_raindrop_plot_movie <- function(data,
                                       variablelist,ylimits,
                                       graph.xlab,graph.ylab,
                                       th1=0.0673,th2=90,
                                       originpoints=TRUE){
  require(RColorBrewer)
  require(tidyverse)
  require(ggbreak)
  require(ggforce)
  require(av)
  
  graph_choices_ref <- c(paste0("C2N Precivity AD2 ","plasma %p-tau217 (%)"),
                         paste0("C2N Precivity AD2 ","plasma p-tau217 (pg/mL)"),
                         paste0("C2N Precivity AD2 ","plasma A","\U03B2","42/A","\U03B2","40"),
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
  
  
  
  graph_choices_suffixes <- c("C2N_plasma_ptau217_ratio","C2N_plasma_ptau217", "C2N_plasma_Abeta42_Abeta40",    
                              "Fuji_plasma_ptau217","Fuji_plasma_Ab42_Ab40",   
                              "AlzPath_plasma_ptau217","Janssen_plasma_ptau217","Roche_plasma_ptau181","Roche_plasma_Ab42_Ab40",
                              "Roche_plasma_GFAP",  "Roche_plasma_NfL",       
                              "QX_plasma_ptau181","QX_plasma_GFAP","QX_plasma_Ab42_Ab40","QX_plasma_NfL",
                              "CENTILOIDS","MesialTemporal","TemporoParietal","metaROI","CDSOB")
  
  graph_vars_list <- variablelist
  data <- data[which(complete.cases(data[,graph_vars_list])),]
  color_origin <- ifelse(originpoints==FALSE,0,0.75)
  mycols <- c("A"="#dc143c", "B"="#ffa500", "C"="#3cb371", "D"="#4169e1", "E"="#4b0082","NODATA"="#D3D3D3")
  th2_lab <- round(data[which(data[,graph_vars_list[1]]==th2),][1,graph_vars_list[2]],digits=4)
  interval_xrank <- round(max(data[,graph_vars_list[1]])/5)
  keeps<-c(1,
           interval_xrank*1,
           interval_xrank*2,
           interval_xrank*3,
           interval_xrank*4,
           max(data[,graph_vars_list[1]]))
  breaklabels <- c()
  for(i in 1:length(keeps)){
    current_break <- keeps[i]
    breaklabels[i] <- round(data[which(data[,graph_vars_list[1]]==current_break),][1,graph_vars_list[2]],digits=4)
  }
  keeps[7] <- th2
  breaklabels[7] <- paste0("\n",th2_lab)
  ymax <- ylimits[2]
  ymin <- ylimits[1]
  
 #making edits that have been made to the original function -
  #1 re-frame the plot around baseline for the basis - Done
  #2 ensure that the plot directions and axis are done properly - Checking
  #3 Make sure space is added to plot title - Done
  #4 double check any small fixes that have been made to the original function - Checking
#the plan should be to send some videos over to suzanne at the minimum tomorrow
#let's try to have the functions operational for time trails and raindrop done by 10/3
  graph.data.baseline <- data %>% group_by(RID) %>% summarize(RID=first(RID),
                                                              yvar=first(get(paste0("BL_",graph_vars_list[3]))),
                                                              xvar=first(get(graph_vars_list[1]))) 
  graph.data.baseline <- as.data.frame(graph.data.baseline)
  
  for(i_time in 1:120){
    
    time2 <- (i_time-1)*0.1
    time1 <- time2-1
    
    data.for.graph <- subset(data, time >= time1 & time < time2)
    
    data.for.graph.latest <- data.for.graph %>% group_by(RID) %>% filter(time==max(time) & time >= time2-0.2)
    data.for.graph.leadup <- data.for.graph %>% group_by(RID) %>% filter(time!=max(time) & time < time2-0.025)
    
    
    N_subs <- nrow(unique(data.for.graph.latest[,"RID"]))
    Title_for_plot <- paste0("Years since baseline: ",format(round(time2,digits = 2),nsmall=1),"\n","Number of individuals:", N_subs)
    raindrops_plot  <- ggplot(data=graph.data.baseline)+
      geom_point(data=data.for.graph.latest,aes_string(x=graph_vars_list[1],y=graph_vars_list[3],color="groupvar"),pch=19,size=3)+
      geom_point(data=data.for.graph.leadup,aes_string(x=graph_vars_list[1],y=graph_vars_list[3],color="groupvar"),pch=19,size=1)+
      geom_line(data=data.for.graph.latest,aes_string(x=graph_vars_list[1],y=graph_vars_list[3],group = "RID",color="groupvar"),
                arrow = arrow(length=unit(0.20,"cm"), 
                              ends="last", type = "closed"))+
      scale_fill_manual(values = mycols)+scale_color_manual(values = mycols)+
      geom_point(aes_string(x="xvar",y="yvar"),alpha=color_origin,pch=21,size=2)+
      ylim(ymin,ymax)+
      coord_fixed(ratio=0.5)+
      ylab(graph.ylab)+
      xlab(paste0(graph.xlab," in rank order of baseline value"))+
      geom_hline(yintercept=th1, linetype="dashed", 
                 color = "black", size=1)+
      geom_vline(xintercept=th2, linetype="dashed", 
                 color = "black", size=1)+
      theme_classic()+
      theme(aspect.ratio = 0.5,legend.position = "none",
            axis.title.x = element_text(face="bold",size=14),
            axis.title.y = element_text(face="bold",size=14),
            axis.text.x = element_text(size=13),
            axis.text.y = element_text(size=13),
            plot.title = element_text(size=14,face="bold",hjust=1))+
      ggtitle(Title_for_plot)
    
    
    
    
    if(graph.xlab == graph_choices_ref[3] | graph.xlab == graph_choices_ref[5] | graph.xlab == graph_choices_ref[9] | graph.xlab == graph_choices_ref[14]){
      raindrops_final <- raindrops_plot+scale_x_reverse(breaks=keeps,labels=breaklabels)
      
    } else {
      raindrops_final <- raindrops_plot+scale_x_continuous(breaks=keeps,labels=breaklabels)  
    }
    print(raindrops_final)
    
  }
}



