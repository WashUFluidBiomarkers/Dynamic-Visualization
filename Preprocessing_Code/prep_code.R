#Coded By: Benjamin A Saef
#For FNIH project - Shiny app project data creation
ADNI_amytime <- read.csv(PET_Merged)

#dry run for function
###read in dataset
ADNI_Longitudinal <- read.csv(data_file)
ADNI_Centiloid <- read.csv(AmyloidPet_File)
ADNI_Centiloid_ForUse <- ADNI_Centiloid[,c("RID","SCANDATE","CENTILOIDS")]
ADNI_CSF <- read.csv(CSF_file)
CSF_subjects <- names(table(ADNI_CSF$RID)[which(table(ADNI_CSF$RID) >1)])
CSF_subjects <- CSF_subjects[which(CSF_subjects %in% ADNI_LONG_Raindrop_data_plasma$RID)]
ADNI_CSF <- subset(ADNI_CSF, RID %in% CSF_subjects)
ADNI_CSF$AB42_PTAU181 <- ADNI_CSF$ABETA42 / ADNI_CSF$PTAU
ADNI_CSF <- subset(ADNI_CSF, !is.na(AB42_PTAU181))
ADNI_CSF_ForUse <- ADNI_CSF[,c("RID","EXAMDATE","AB42_PTAU181")]

ADNI_CDR <- read.csv(CDR_file)
ADNI_CDR$CDSOB <- ADNI_CDR$CDMEMORY + ADNI_CDR$CDORIENT + ADNI_CDR$CDJUDGE + ADNI_CDR$CDCOMMUN + ADNI_CDR$CDHOME + ADNI_CDR$CDCARE
ADNI_CDR$CDSOB <- ifelse(ADNI_CDR$CDSOB < 0,NA,ADNI_CDR$CDSOB)
ADNI_CDR_ForUse <- ADNI_CDR[,c("RID","VISDATE","CDSOB")]
ADNI_cortThick <- read.csv(CorticalThickness_File)
ADNI_cortThick_ForUse <- ADNI_cortThick[,c("RID","EXAMDATE","metaROI")]

ADNI_TauPET <- read.csv(TauPet_file)
ADNI_TauPET_foruse <- ADNI_TauPET[,c("RID","SCANDATE","MesialTemporal","TemporoParietal")]

##Create list of RIDs / subject IDs
RID_list <- names(table(ADNI_Longitudinal$RID))

#total # of IDS to scroll through
total_ID_count <- length(table(ADNI_Longitudinal$RID))

#baseline/reference date  (this may need to be adjusted depending on dataset)
set_start <- as.Date("1990-01-01","%Y-%m-%d")

#max date from the dataset
set_end <- as.Date("2023-01-31","%Y-%m-%d")

#doing this since if I don't the merged EXAMDATE variable doesn't look like a date anymore
ADNI_Longitudinal$EXAMDATE <- as.Date(ADNI_Longitudinal$EXAMDATE,"%Y-%m-%d")
ADNI_Centiloid_ForUse$SCANDATE <-  as.Date(ADNI_Centiloid_ForUse$SCANDATE,"%Y-%m-%d")
ADNI_CDR_ForUse$VISDATE <- as.Date(ADNI_CDR_ForUse$VISDATE,"%Y-%m-%d")
ADNI_cortThick_ForUse$EXAMDATE <- as.Date(ADNI_cortThick_ForUse$EXAMDATE,"%Y-%m-%d")
ADNI_TauPET_foruse$SCANDATE <-  as.Date(ADNI_TauPET_foruse$SCANDATE,"%Y-%m-%d")
colnames(ADNI_Centiloid_ForUse)[2] <- "date"
colnames(ADNI_CDR_ForUse)[2] <- "date"
colnames(ADNI_cortThick_ForUse)[2] <- "date"
colnames(ADNI_TauPET_foruse)[2] <- "date"
colnames(ADNI_CSF_ForUse)[2] <- "date"

Centiloid_subjects <- names(table(ADNI_Centiloid_ForUse$RID)[which(table(ADNI_Centiloid_ForUse$RID) >1)])
ADNI_Centiloid_ForUse <- subset(ADNI_Centiloid_ForUse, RID %in% Centiloid_subjects)

CDR_subjects <- names(table(ADNI_CDR_ForUse$RID)[which(table(ADNI_CDR_ForUse$RID) >1)])
ADNI_CDR_ForUse <- subset(ADNI_CDR_ForUse, RID %in% CDR_subjects)

atrophy_subjects <- names(table(ADNI_cortThick_ForUse$RID)[which(table(ADNI_cortThick_ForUse$RID) >1)])
ADNI_cortThick_ForUse <- subset(ADNI_cortThick_ForUse, RID %in% atrophy_subjects)

taupet_subjects <- names(table(ADNI_TauPET_foruse$RID)[which(table(ADNI_TauPET_foruse$RID) >1)])
ADNI_TauPET_foruse <- subset(ADNI_TauPET_foruse, RID %in% taupet_subjects)

#for use in function testing
data_in <- ADNI_Longitudinal


data_nonquanterix <- data_in[,c("RID","EXAMDATE",varlist_noquanterix)]
data_nonquanterix <- subset(data_nonquanterix, !is.na(C2N_plasma_ptau217_ratio) | !is.na(Fuji_plasma_ptau217) | !is.na(AlzPath_plasma_ptau217)  | !is.na(Roche_plasma_ptau181) | !is.na(Roche_plasma_NfL) |
                              !is.na(Janssen_plasma_ptau217) | !is.na(Fuji_plasma_Ab42_Ab40) | !is.na(Roche_plasma_Ab42_Ab40)  | !is.na(Roche_plasma_GFAP) | !is.na(C2N_plasma_Abeta42_Abeta40))

plasma_subjects <- names(table(data_nonquanterix$RID)[which(table(data_nonquanterix$RID) >1)])
data_nonquanterix <- subset(data_nonquanterix, RID %in% plasma_subjects)


data_quanterix <- data_in[,c("RID","EXAMDATE",varlist_quanterix)]
data_quanterix  <- subset(data_quanterix, !is.na(QX_plasma_ptau181) | !is.na(QX_plasma_GFAP) | !is.na(QX_plasma_Ab42_Ab40) | !is.na(QX_plasma_NfL))

quanterix_subjects <- names(table(data_quanterix$RID)[which(table(data_quanterix$RID) >1)])
data_quanterix <- subset(data_quanterix, RID %in% quanterix_subjects)
data_quanterix <- subset(data_quanterix, RID %in% plasma_subjects)


#total # of days between start and end
day_diff_full <- as.numeric(abs(difftime(set_start,
                                         set_end)))
#creating index
days_sequence_index=seq(0,day_diff_full,1)
#creating index by time (Date)
date_sequence_index= as.Date(seq(set_start,set_end,by="day"),"%Y-%m-%d")
#creating the time index dataframe
full_date_dataframe <- data.frame(day=days_sequence_index,
                                  date=date_sequence_index)



#merge dataset with time index dataframe
data_test_plasma <- merge(full_date_dataframe,data_nonquanterix,by.x="date",by.y ="EXAMDATE",all.x = T)

data_test_quanterix <- merge(full_date_dataframe,data_quanterix,by.x="date",by.y ="EXAMDATE",all.x = T)
data_test_cent <- merge(full_date_dataframe,ADNI_Centiloid_ForUse,by="date",all.x = T)
data_test_CDR <- merge(full_date_dataframe,ADNI_CDR_ForUse,by="date",all.x = T)
data_test_cort <- merge(full_date_dataframe,ADNI_cortThick_ForUse,by="date",all.x = T)
data_test_tau <- merge(full_date_dataframe,ADNI_TauPET_foruse,by="date",all.x = T)
data_test_CSF <- merge(full_date_dataframe,ADNI_CSF_ForUse ,by="date",all.x = T)

##Was thinking too hard, do each dataset individually, then merge all together by RID/Date once the EST variables are created
varlist_quanterix <- varlist_plasma[12:15]


data_test_order_plasma <- data_test_plasma[order(data_test_plasma$day,data_test_plasma$RID),]  %>% group_by(RID) %>% mutate(time = (day - first(day))/365.25)
data_test_order_plasma <- as.data.frame(data_test_order_plasma)
data_test_order_quanterix <- data_test_quanterix[order(data_test_quanterix$day,data_test_quanterix$RID),]  %>% group_by(RID) %>% mutate(time = (day - first(day))/365.25)
data_test_order_quanterix <- as.data.frame(data_test_order_quanterix)
data_test_order_cent <- data_test_cent[order(data_test_cent$day,data_test_cent$RID),]  %>% group_by(RID) %>% mutate(time = (day - first(day))/365.25)
data_test_order_cent <- as.data.frame(data_test_order_cent)
data_test_order_CDR <- data_test_CDR[order(data_test_CDR$day,data_test_CDR$RID),]  %>% group_by(RID) %>% mutate(time = (day - first(day))/365.25)
data_test_order_CDR <- as.data.frame(data_test_order_CDR)
data_test_order_cort <- data_test_cort[order(data_test_cort$day,data_test_cort$RID),]  %>% group_by(RID) %>% mutate(time = (day - first(day))/365.25)
data_test_order_cort <- as.data.frame(data_test_order_cort)
data_test_order_tau <- data_test_tau[order(data_test_tau$day,data_test_tau$RID),]  %>% group_by(RID) %>% mutate(time = (day - first(day))/365.25)
data_test_order_tau <- as.data.frame(data_test_order_tau)
data_test_order_CSF <- data_test_CSF[order(data_test_CSF$day,data_test_CSF$RID),]  %>% group_by(RID) %>% mutate(time = (day - first(day))/365.25)
data_test_order_CSF <- as.data.frame(data_test_order_CSF)
#creating by subject time elapsed varibale

varlist_plasma <- c("C2N_plasma_ptau217_ratio","C2N_plasma_ptau217", "C2N_plasma_Abeta42_Abeta40",    
                    "Fuji_plasma_ptau217","Fuji_plasma_Ab42_Ab40",   
                    "AlzPath_plasma_ptau217","Janssen_plasma_ptau217","Roche_plasma_ptau181","Roche_plasma_Ab42_Ab40",
                    "Roche_plasma_GFAP",  "Roche_plasma_NfL",       
                    "QX_plasma_ptau181","QX_plasma_GFAP","QX_plasma_Ab42_Ab40","QX_plasma_NfL")
#write.csv(data_with_subtime,"/C2N/Shiny_Apps/test_ADNI_data.csv",row.names=F,quote=F)
varlist_CDR <- c("CDSOB")
varlist_CSF <- c("AB42_PTAU181")
varlist_cort <- c("metaROI")
varlist_cent <- c("CENTILOIDS")
varlist_taupet <- c("MesialTemporal","TemporoParietal")
#Retooling of process based on SAS code
#1. Create EST - Values + Predicted Values (done on each dataset individually)
#2. Merge everything (all datasets by day)
#3. Trim Everything based on data availability of plasma data (basically, cut all days that don't have a "time passed since start" value for plasma biomarkers)
#4. Create Baseline Values based on trimmed data (this is where the problem happened before as I had wrapped this up in the 
#predicted value creation function - this is why I had thought I needed to redo things at a single ID level even though that wasn't the case!)
#5. Create Baseline Ranks based on Baseline values

###What is needed to execute this?
#Retool single id raindrop prep
#create new function to go through the baseline values creation
#This shouldn't take too long! make sure to have a good portion of this going by end of day  8/28/2024

#testRIDlist <- RID_list[1:5]

single_id_raindrop_prep <- function(data_with_subtime,#dataset with all the variables in varlist
                                    #the ID variable from IDvar
                                    #the date in YYYY/MM/DD(date)
                                    #the day counter (I did # of days since Jan 1 1990 but that's arbitrary)(day)
                                    full_date_dataframe, #Has all possible dates with the day counter(day) and the dates in YYYY/MM/DD(date)             
                                    IDvar, #Variable where IDs are
                                    ID, #Individual ID in question
                                    varlist){
  data_sid <- data_with_subtime[which(data_with_subtime[,IDvar]==ID),]
  max_day <- max(data_sid$day)
  min_day <- min(data_sid$day)
  #changing back to time lapsed variable
  
  data_allframe <- subset(data_with_subtime, day >= min_day & day <= max_day)
  data_frame_id <- data_allframe[which(data_allframe[,IDvar]==ID),] #Taking this, next step, we will need to create the EST variable from this
  # EST_varnames <- c(paste0("EST_",varlist),paste0("BL_EST_",varlist))
  data_frame_id[,IDvar] <- ID
  
  
  
  
  for(i_variable in 1:length(varlist)){
    #assign variable for iteration
    i_var <- varlist[i_variable]
    #Estimated/Interpolated variable ID
    Est_varID  <- paste0("EST_",i_var)
    #Baseline variable ID
    
    #in order to ensure non-adjascent measurements per-id can be used, we do this!
    #this way for each variable we can have different time points determine the interpolation
    data_sid_var <- data_sid[which(!is.na(data_sid[,i_var])),]
    
    for(i_start in 1:(nrow(data_sid_var)-1)){
      days_between_dates <-  (data_sid_var[(i_start+1),"day"]-data_sid_var[i_start,"day"])+1
      if(nrow(data_sid_var) > 1){
        data_chunk <- merge(data_sid_var[c((i_start):(i_start+1)),c(IDvar,"day","date")],full_date_dataframe,by=c("day","date"),all.y=T) 
        data_chunk  <- subset(data_chunk,day >= data_sid_var[(i_start),"day"] & day <= data_sid_var[(i_start+1),"day"])
        
        
        
        
        data_chunk[,IDvar] <- ID
        interpolated_values <-  seq(data_sid_var[i_start,i_var],data_sid_var[i_start+1,i_var],length.out = days_between_dates)
        data_chunk[,Est_varID] <- interpolated_values 
        
        
        
        if(i_start==1){
          current_data  <- data_chunk
        }else{
          current_data  <- rbind(current_data[-nrow(current_data),],data_chunk)
        }  
      }
      else{
        current_data <- data_sid[,c("day","date",IDvar)]
        current_data[,Est_varID] <- NA
      }
    }
    if(i_variable==1 ){
      final_data  <- current_data
      
    } else {
      
      final_data <- merge(final_data,current_data,by=c(IDvar,"day","date"),all.y=T,all.x=T)
      
    }  
    
    
  }
  final_data <- as.data.frame(final_data[order(final_data$day),])
  final_data <- final_data %>% group_by(RID) %>% mutate(time = abs(day - first(day))/365.25)
  final_data <- as.data.frame(final_data)
  
  return(final_data)
  
}

#####Plasma
##Create list of RIDs / subject IDs

varlist_noquanterix <- varlist_plasma[1:11]
RID_list <- names(table(data_nonquanterix$RID))

#total # of IDS to scroll through
total_ID_count <- length(table(data_nonquanterix$RID))

ADNI_LONG_Raindrop_data_plasma_non_quanterix <- single_id_raindrop_prep(data_test_order_plasma,full_date_dataframe,"RID",21,varlist_noquanterix)

for(i_id_index in 2:total_ID_count){
  ID_data  <- single_id_raindrop_prep(data_test_order_plasma,full_date_dataframe,"RID",RID_list[i_id_index],varlist_noquanterix)
  ADNI_LONG_Raindrop_data_plasma_non_quanterix <- rbind(ADNI_LONG_Raindrop_data_plasma_non_quanterix,ID_data)
}

#for(i_var_n in 1:length(varlist_noquanterix)){
#  varchoice <- varlist_noquanterix[i_var_n]

#  N_VAR_NAME <- paste0("N_",varchoice)
#  RID_order <- unique(ADNI_LONG_Raindrop_data_plasma_non_quanterix[order(ADNI_LONG_Raindrop_data_plasma_non_quanterix[,paste0("BL_EST_",varchoice)]),"RID"])

#  ADNI_LONG_Raindrop_data_plasma_non_quanterix[,N_VAR_NAME] <- as.numeric(factor(ADNI_LONG_Raindrop_data_plasma_non_quanterix[,"RID"],levels = RID_order))


#}
save(ADNI_LONG_Raindrop_data_plasma_non_quanterix,
     file="/C2N/Shiny_Apps/ADNI_Raindrop_Data_plasma_non_quanterix.rdata")


####Quanterix
varlist_quanterix <- varlist_plasma[12:15]

##Create list of RIDs / subject IDs
RID_list <- unique(ADNI_Longitudinal[which(!is.na(ADNI_Longitudinal$QX_plasma_Ab42_Ab40) | !is.na(ADNI_Longitudinal$QX_plasma_ptau181) | !is.na(ADNI_Longitudinal$QX_plasma_GFAP) | !is.na(ADNI_Longitudinal$QX_plasma_NfL)),"RID"])
RID_list <- RID_list[which(RID_list %in% plasma_subjects)]
#total # of IDS to scroll through
total_ID_count <- length(RID_list)

ADNI_LONG_Raindrop_data_plasma_quanterix <- single_id_raindrop_prep(data_test_order_quanterix,full_date_dataframe,"RID",21,varlist_quanterix)

for(i_id_index in 2:total_ID_count){
  ID_data  <- single_id_raindrop_prep(data_test_order_quanterix,full_date_dataframe,"RID",RID_list[i_id_index],varlist_quanterix)
  ADNI_LONG_Raindrop_data_plasma_quanterix <- rbind(ADNI_LONG_Raindrop_data_plasma_quanterix,ID_data)
}

save(ADNI_LONG_Raindrop_data_plasma_quanterix,
     file="/C2N/Shiny_Apps/ADNI_Raindrop_Data_plasma_quanterix.rdata")



####CDR

##Create list of RIDs / subject IDs
RID_list <- names(table(data_test_order_CDR$RID))
RID_list <- RID_list[which(RID_list %in% data_test_order_plasma$RID)]
#total # of IDS to scroll through
total_ID_count <- length(RID_list)

ADNI_LONG_Raindrop_data_cdr <- single_id_raindrop_prep(data_test_order_CDR,full_date_dataframe,"RID",RID_list[1],varlist_CDR)

for(i_id_index in 2:total_ID_count){
  ID_data  <- single_id_raindrop_prep(data_test_order_CDR,full_date_dataframe,"RID",RID_list[i_id_index],varlist_CDR)
  ADNI_LONG_Raindrop_data_cdr <- rbind(ADNI_LONG_Raindrop_data_cdr,ID_data)
}


save(ADNI_LONG_Raindrop_data_cdr,
     file="/C2N/Shiny_Apps/ADNI_Time_Trails/ADNI_Raindrop_Data_cdr.rdata")

##### Amyloid
##Create list of RIDs / subject IDs
RID_list <- names(table(data_test_order_cent$RID))
RID_list <- RID_list[which(RID_list %in% data_test_order_plasma$RID)]
#total # of IDS to scroll through
total_ID_count <- length(RID_list)

ADNI_LONG_Raindrop_cent <- single_id_raindrop_prep(data_test_order_cent,full_date_dataframe,"RID",RID_list[1],varlist_cent)

for(i_id_index in 2:total_ID_count){
  ID_data  <- single_id_raindrop_prep(data_test_order_cent,full_date_dataframe,"RID",RID_list[i_id_index],varlist_cent)
  ADNI_LONG_Raindrop_cent <- rbind(ADNI_LONG_Raindrop_cent,ID_data)
}

save(ADNI_LONG_Raindrop_cent,
     file="/C2N/Shiny_Apps/ADNI_Time_Trails/ADNI_Raindrop_Data_Centloid.rdata")


####

##Create list of RIDs / subject IDs
RID_list <- names(table(data_test_order_cort$RID))
RID_list <- RID_list[which(RID_list %in% data_test_order_plasma$RID)]

#total # of IDS to scroll through
total_ID_count <- length(RID_list)

ADNI_LONG_Raindrop_data_cort <- single_id_raindrop_prep(data_test_order_cort,full_date_dataframe,"RID",RID_list[1],varlist_cort)

for(i_id_index in 2:total_ID_count){
  ID_data  <- single_id_raindrop_prep(data_test_order_cort,full_date_dataframe,"RID",RID_list[i_id_index],varlist_cort)
  ADNI_LONG_Raindrop_data_cort <- rbind(ADNI_LONG_Raindrop_data_cort,ID_data)
}

save(ADNI_LONG_Raindrop_data_cort,
     file="/C2N/Shiny_Apps/ADNI_Time_Trails/ADNI_Raindrop_Data_cort.rdata")

####Tau PET

##Create list of RIDs / subject IDs
RID_list <- names(table(data_test_order_tau$RID))
RID_list <- RID_list[which(RID_list %in% plasma_subjects)]

#total # of IDS to scroll through
total_ID_count <- length(RID_list)

ADNI_LONG_Raindrop_data_tau <- single_id_raindrop_prep(data_test_order_tau,full_date_dataframe,"RID",RID_list[1],varlist_taupet)

for(i_id_index in 2:total_ID_count){
  ID_data  <- single_id_raindrop_prep(data_test_order_tau,full_date_dataframe,"RID",RID_list[i_id_index],varlist_taupet)
  ADNI_LONG_Raindrop_data_tau <- rbind(ADNI_LONG_Raindrop_data_tau,ID_data)
}


save(ADNI_LONG_Raindrop_data_tau,
     file="/C2N/Shiny_Apps/ADNI_Time_Trails/ADNI_Raindrop_Data_tau.rdata")

####CSF

##Create list of RIDs / subject IDs
RID_list <- names(table(data_test_order_CSF$RID))
RID_list <- RID_list[which(RID_list %in% data_test_order_plasma$RID)]

#total # of IDS to scroll through
total_ID_count <- length(RID_list)

ADNI_LONG_Raindrop_data_CSF <- single_id_raindrop_prep(data_test_order_CSF,full_date_dataframe,"RID",RID_list[1],varlist_CSF)

for(i_id_index in 2:total_ID_count){
  ID_data  <- single_id_raindrop_prep(data_test_order_CSF,full_date_dataframe,"RID",RID_list[i_id_index],varlist_CSF)
  ADNI_LONG_Raindrop_data_CSF <- rbind(ADNI_LONG_Raindrop_data_CSF,ID_data)
}
save(ADNI_LONG_Raindrop_data_CSF,
     file="/C2N/Shiny_Apps/ADNI_Time_Trails/ADNI_Raindrop_Data_CSF.rdata")



##### Pre-merge work  - need to keep the time variables for each individual measure just in case (will ask Suzanne about this particular issue)
#### Spoke to Suzanne - as Plasma is the lead, plasma will be the time standard
colnames(ADNI_LONG_Raindrop_data_plasma_quanterix)[8] <- "time_quanterix"
colnames(ADNI_LONG_Raindrop_data_tau)[6] <- "time_tau"
colnames(ADNI_LONG_Raindrop_data_CSF)[5] <- "time_csf"
colnames(ADNI_LONG_Raindrop_data_cdr)[5] <- "time_cdr"
colnames(ADNI_LONG_Raindrop_data_cort)[5] <- "time_atrophy"
colnames(ADNI_LONG_Raindrop_cent)[5] <- "time_centiloid"
##### Merge all data by RID Day and time, keep all entries, plasma will be the leader since most variables

ADNI_LONG_Raindrop_data_plasma <- merge(ADNI_LONG_Raindrop_data_plasma_non_quanterix,ADNI_LONG_Raindrop_data_plasma_quanterix,by=c("RID","day","date"),all.x=T,all.y=T)
ADNI_long_Raindrop_merged <- merge(ADNI_LONG_Raindrop_data_plasma,ADNI_LONG_Raindrop_cent,by=c("RID","day","date"),all.x=T,all.y=T)
ADNI_long_Raindrop_merged <- merge(ADNI_long_Raindrop_merged,ADNI_LONG_Raindrop_data_cdr,by=c("RID","day","date"),all.x=T,all.y=T)
ADNI_long_Raindrop_merged <- merge(ADNI_long_Raindrop_merged,ADNI_LONG_Raindrop_data_tau,by=c("RID","day","date"),all.x=T,all.y=T)
ADNI_long_Raindrop_merged <- merge(ADNI_long_Raindrop_merged,ADNI_LONG_Raindrop_data_cort,by=c("RID","day","date"),all.x=T,all.y=T)
ADNI_long_Raindrop_merged <- merge(ADNI_long_Raindrop_merged,ADNI_LONG_Raindrop_data_CSF,by=c("RID","day","date"),all.x=T,all.y=T)
ADNI_long_Raindrop_merged_plasma <- ADNI_long_Raindrop_merged[which(!is.na(ADNI_long_Raindrop_merged$time)),]
phenotype_list <- c(varlist_noquanterix,varlist_quanterix,varlist_CDR,varlist_cent,varlist_CSF,varlist_taupet,varlist_cort)

for(i_pheno in 1:length(phenotype_list)){
  varchoice <- phenotype_list[i_pheno]
  ##Three steps
  #create baseline variable from first observation non-NA observation per subject within range (the range isn't important as we've already
  #filtered out everything that isn't in range)
  #order baseline variable
  #create ranking variable based on baseline
  EST_VAR_NAME <- paste0("EST_",varchoice)
  BL_VAR_NAME <- paste0("BL_EST_",varchoice)
  N_VAR_NAME  <- paste0("N_",varchoice)
  
  BL_KEY_DATA <- ADNI_long_Raindrop_merged_plasma %>% group_by(RID) %>% 
    filter(any(!is.na(get(EST_VAR_NAME)))) %>% 
    summarize(!!BL_VAR_NAME:=first(na.omit(get(EST_VAR_NAME))))
  BL_KEY_DATA <- as.data.frame(BL_KEY_DATA)
  
  RID_order <- unique(BL_KEY_DATA[order(BL_KEY_DATA[,BL_VAR_NAME]),"RID"])
  
  BL_KEY_DATA[,N_VAR_NAME] <- as.numeric(factor(BL_KEY_DATA[,"RID"],levels = RID_order))
  if(i_pheno==1){
    ADNI_long_Raindrop_merged_w_baseline <- merge(ADNI_long_Raindrop_merged_plasma,BL_KEY_DATA,by="RID",all.x=T,all.y=F)
  } else{
    ADNI_long_Raindrop_merged_w_baseline <- merge(ADNI_long_Raindrop_merged_w_baseline,BL_KEY_DATA,by="RID",all.x=T,all.y=F)
    
  }
  
}
ADNI_long_Raindrop_merged_w_baseline <- ADNI_long_Raindrop_merged_w_baseline[order(ADNI_long_Raindrop_merged_w_baseline$RID,ADNI_long_Raindrop_merged_w_baseline$day),]
#merge back with full data by subject ID. since the baseline values don't change

data_every_10th <- ADNI_long_Raindrop_merged_w_baseline %>% group_by(RID) %>% slice(which(row_number() %% 10 == 1))
data_every_10th <- as.data.frame(data_every_10th)
save(data_every_10th,
     file="/C2N/Shiny_Apps/ADNI_Time_Trails/ADNI_Raindrop_Data_10th.rdata")

