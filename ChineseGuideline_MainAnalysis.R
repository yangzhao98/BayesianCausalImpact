rm(list=ls())

library(data.table, quietly = TRUE)
library(bsts, quietly = TRUE)
library(CausalImpact, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(stringr, quietly = TRUE)
library(ggsci, quietly = TRUE)
library(ggpubr, quietly = TRUE)
library(grid, quietly = TRUE)
library(gridExtra, quietly = TRUE)

datPath <- paste(getwd(),"/Data/",sep="")

set.seed(1)

## ---- Outcome: GBD 2019
# Citation of GBD Results tool:
# Global Burden of Disease Collaborative Network.
# Global Burden of Disease Study 2019 (GBD 2019) Results.
# Seattle, United States: Institute for Health Metrics and Evaluation (IHME),2020
# Available from: https://vizhub.healthdata.org/gbd-results/.
# 
datGBD2019 <- fread(paste(datPath,"IHME-GBD_2019_DATA-0f08d3fe-1.csv",sep=""))
with(datGBD2019, table(cause_name,measure_name))
datGBD2019New <- datGBD2019[
    ,`:=`(Year=year,outcome=val)][
        ,.(measure_name,sex_name,age_name,cause_name,metric_name,Year,outcome)]
datGBD2019New[
    ,`:=`(cause=ifelse(cause_name=="Cardiovascular diseases","1 All CVD",
        ifelse(cause_name=="Rheumatic heart disease","2 Rheumatic heart disease",
        ifelse(cause_name=="Ischemic heart disease","3 Ischemic heart disease",
        ifelse(cause_name=="Ischemic stroke","4 Ischemic stroke",
        ifelse(cause_name=="Intracerebral hemorrhage","5 Intracerebral hemorrhage",
        ifelse(cause_name=="Hypertensive heart disease","7 Hypertensive heart disease",
        ifelse(cause_name=="Non-rheumatic valvular heart disease","8 Non rheumatic valvular heart disease",
        ifelse(cause_name=="Cardiomyopathy and myocarditis","9 Cardiomyopathy and myocarditis",
        ifelse(cause_name=="Atrial fibrillation and flutter","10 Atrial fibrillation and flutter",
        ifelse(cause_name=="Aortic aneurysm","11 Aortic aneurysm",
        ifelse(cause_name=="Peripheral artery disease","12 Peripheral artery disease",
        ifelse(cause_name=="Endocarditis","13 Endocarditis",
        ifelse(cause_name=="Subarachnoid hemorrhage","6 Subarachnoid hemorrhage",
        ifelse(cause_name=="Other cardiovascular and circulatory diseases","14 Other CVD and circulatory diseases",
               "15 Negative control outcome")))))))))))))))][
                   ,`:=`(cause_name=NULL)]
datGBD2019New[
    ,`:=`(measure=ifelse(measure_name == "Prevalence", "1 Prevalence",
                  ifelse(measure_name == "Incidence", "2 Incidence",
                  ifelse(measure_name == "Deaths", "3 Mortality", "4 DALY"))),
          sex=ifelse(sex_name == "Both", "1 Both",
              ifelse(sex_name == "Male", "2 Male", "3 Female")))][
                  ,`:=`(cause=as.character(cause),
                        measure=as.character(measure),
                        sex=as.character(sex))]

## Sex-specific statistics
datGBD2019AgeStandardized <- datGBD2019New[
    age_name=="Age-standardized" & metric_name=="Rate"][
        ,`:=`(measure_name=NULL,age_name=NULL,metric_name=NULL,sex_name=NULL)]

datGBD2019AgeStandardizedBoth <- datGBD2019AgeStandardized[sex=="1 Both"]
datGBD2019AgeStandardizedMale <- datGBD2019AgeStandardized[sex=="2 Male"]
datGBD2019AgeStandardizedFemale <- datGBD2019AgeStandardized[sex=="3 Female"]

datGBD2019AgeStandardizedNew <-
    rbindlist(list(datGBD2019AgeStandardizedBoth,
                   datGBD2019AgeStandardizedMale,
                   datGBD2019AgeStandardizedFemale))

rm(list=c("datGBD2019AgeStandardizedBoth",
          "datGBD2019AgeStandardizedMale",
          "datGBD2019AgeStandardizedFemale",
          "datGBD2019AgeStandardized","datGBD2019"))

## Age group-specific statistics
datGBD2019AgeGroup <- datGBD2019New[
    age_name %in% c("25-49 years","50-74 years","75+ years") & 
        metric_name=="Rate" & sex_name == "Both"][
            ,`:=`(measure_name=NULL,metric_name=NULL,sex_name=NULL)][
                ,`:=`(sex=ifelse(age_name=="25-49 years", "4 25-49 years",
                          ifelse(age_name=="50-74 years", "5 50-74 years",
                                 "6 75+ years")),
                      age_name=NULL)]

datGBD2019AgeGroup25 <- datGBD2019AgeGroup[sex=="4 25-49 years"]
datGBD2019AgeGroup50 <- datGBD2019AgeGroup[sex=="5 50-74 years"]
datGBD2019AgeGroup75 <- datGBD2019AgeGroup[sex=="6 75+ years"]

datGBD2019AgeGroupNew <-
    rbindlist(list(datGBD2019AgeGroup25,
                   datGBD2019AgeGroup50,
                   datGBD2019AgeGroup75))
rm(list=c("datGBD2019AgeGroup25","datGBD2019AgeGroup50",
          "datGBD2019AgeGroup75","datGBD2019AgeGroup"))

## Sex- and age group-specific statistics
datGBD2019Update <- rbindlist(list(datGBD2019AgeStandardizedNew,
                                   datGBD2019AgeGroupNew))
datGBD2019Update <- datGBD2019Update[!is.na(Year) & outcome >0]
rm(list=c("datGBD2019AgeStandardizedNew","datGBD2019AgeGroupNew"))


## ---- Controls: Chinese Statistics Yearbook
datChina <- fread(paste(datPath,"ChineseHealthStatistics_20220529.csv",sep=""))
# names(datChina)
datChina2 <- datChina[
    ,`:=`(PopulationSize_Male10K=PopulationSize_10K*Male_Prop/100,
          PopulationSize_Female10K=PopulationSize_10K*(100-Male_Prop)/100,
          PopulationSize_Both10K=PopulationSize_10K,
          Illiterate_BothProp=Illiterate_Prop,
          PrimarySchool_BothProp=PrimarySchool_Prop,
          JuniorSecondarySchool_BothProp=JuniorSecondarySchool_Prop,
          SeniorTechnicalSecondarySchool_BothProp=
              SeniorTechnicalSecondarySchool_Prop,
          JuniorCollegeAndAbove_BothProp=JuniorCollegeAndAbove_Prop,
          LifeExpectancyAtBirthBoth=LifeExpectancyAtBirth)][
              ,c("TurnoverRateOfHospitalBeds_Times",
                 "OccupancyRateOfMedicalBeds_Pct",
                 "AvgLengthOfStayInMedical_Days",
                 "TurnoverRateOfMedicalBeds_Times",
                 "NumVisitsPerDoctorPerDay",
                 "NumInpatientsPerDoctorPerDay",
                 "PerCapitaHealthExpenditure",
                 "MortalityRateOfInpatients",
                 "HealthInsuranceCoverage_UrbanPop10K",
                 "PopulationSize_10K",
                 "Illiterate_Prop",
                 "PrimarySchool_Prop",
                 "JuniorSecondarySchool_Prop",
                 "SeniorTechnicalSecondarySchool_Prop",
                 "JuniorCollegeAndAbove_Prop",
                 "LifeExpectancyAtBirth"):=NULL]
varList <- names(datChina2)
varList <- varList[varList != "Male_Prop"];varList
varListBoth <- varList[grepl("Both",varList)];varListBoth
varListMale <- varList[grepl("Male",varList)];varListMale
varListFemale <- varList[grepl("Female",varList)];varListFemale
varListCommon <- varList[!varList %in% c(varListBoth,varListMale,varListFemale)]

## Both
datChina2Both <- datChina2[,c(varListCommon,varListBoth),with=FALSE]
varNameBoth <- names(datChina2Both)
varNameBothNew <- gsub("Both","",varNameBoth)
setnames(datChina2Both, names(datChina2Both),varNameBothNew)
datChina2Both[,`:=`(sex="1 Both")]

## Male
datChina2Male <- datChina2[,c(varListCommon,varListMale),with=FALSE]
varNameMale <- names(datChina2Male)
varNameMaleNew <- gsub("Male","",varNameMale)
setnames(datChina2Male, names(datChina2Male),varNameMaleNew)
datChina2Male[,`:=`(sex="2 Male")]

## Female
datChina2Female <- datChina2[,c(varListCommon,varListFemale),with=FALSE]
varNameFemale <- names(datChina2Female)
varNameFemaleNew <- gsub("Female","",varNameFemale)
setnames(datChina2Female, names(datChina2Female), varNameFemaleNew)
datChina2Female[,`:=`(sex="3 Female")]
datChina2New <- rbindlist(list(datChina2Both,datChina2Male,datChina2Female),
                          use.names = TRUE) 

rm(list=c("datChina","datChina2",
          "varList","varListBoth","varListMale","varListFemale","varListCommon",
          "datChina2Both","varNameBoth","varNameBothNew",
          "datChina2Male","varNameMale","varNameMaleNew",
          "datChina2Female","varNameFemale","varNameFemaleNew"))

library(gtsummary)
library(tidyverse)
suppTable1 <- as.data.frame(datChina2New) %>%
    dplyr::select(-Year) %>%
    tbl_summary(by=sex,
                type = all_continuous() ~ "continuous2",
                statistic=list(all_continuous() ~ "{median} ({p25}, {p75})")) 

library(xlsx, quietly = TRUE)
# write.xlsx2(as_gt(suppTable1),
#             file=paste(getwd(),"/Result/0.1 Controls.xlsx",sep=""),
#             sheetName="Controls",col.names=TRUE,row.names=TRUE,append=TRUE)


## ---- Data combination
datSCSex <- merge(datGBD2019Update[!grepl("years", sex)], 
                  datChina2New, by=c("Year","sex"))
datSCAge <- merge(datGBD2019Update[grepl("years",sex)],
                  datChina2New[grepl("Both", sex)][,`:=`(sex=NULL)],
                  by=c("Year"))
datSC <- rbindlist(list(datSCSex, datSCAge), use.names = TRUE)
datSCNew <- datSC[,`:=`(group=paste(cause, "-", sex, "-", measure, sep=""),
                        date=as.Date(paste(Year,"-12-31",sep="")),
                        Year=NULL)][order(cause,sex,measure)][
                            ,`:=`(cause=NULL,sex=NULL,measure=NULL)]
datSCNew <- as.data.frame(datSCNew)
rm(list=c("datSCSex","datSCAge","datSC",
          "datChina2New","datGBD2019New","datGBD2019Update"))
datSC <- datSCNew


## ---- Main analysis
source(paste(getwd(),"/RCode/ChineseGuidelineUtilities.R",sep=""))

# Setup outcome and covariates
factor_name <- "group"
date_name <- "date"
group <- unique(datSCNew$group)
data_start_date <- min(datSCNew[,date_name]); data_start_date
data_end_date <- max(datSCNew[,date_name]); data_end_date
data_intervention_date <- as.Date("2011-01-01"); data_intervention_date
pre_period <- c(data_start_date,data_intervention_date); pre_period
post_period<- c(data_intervention_date,data_end_date); post_period
eval_period1 <- c(as.Date("2011-12-31"),data_end_date); eval_period1
time_points <- seq.Date(as.Date(data_start_date),as.Date(data_end_date),by="year");time_points
digitsNum <- 2

# Preprocessing data
ds <- setNames(lapply(unique(datSCNew$group),
                      FUN=logTransform,
                      factor_name=factor_name,
                      date_name=date_name,
                      start_date=data_start_date,
                      prelog_data=datSCNew),
               group);length(ds)

data_start <- match(data_start_date,ds[[1]][,date_name]); data_start
time_points <- ds[[1]][,date_name][data_start:nrow(ds[[1]])]; time_points
idx_var <- c("date","group","outcome")

covars <- setNames(
    lapply(ds, 
           FUN=function(ds_group) {
               covar <- ds_group[data_start:nrow(ds_group),
                                 !names(ds_group) %in% idx_var]
               # The new rural cooperative medical care after 2004.
               covar$newRuralCooperativeMedicalCare <- (time_points>"2007-12-31")+0
               covar <- as.data.frame(lapply(covar[,apply(covar,2,var) != 0], scale))
               return(covar)}),
    group); head(covars[[1]][,1:5])

outcome <- sapply(ds, FUN=function(data) {scale(data[,"outcome"])})
dsOutcome <- colSums(outcome)
outcome_mean <- sapply(ds, FUN=function(data) {mean(data[,"outcome"])})
outcome_sd <- sapply(ds, FUN=function(data) {sd(data[,"outcome"])})
outcome_plot <- exp(t(t(outcome)*outcome_sd + outcome_mean))
outcome_offset <- sapply(ds, FUN=function(data) {
    data[,"outcome"] - data[,grepl("PopulationSize",names(data))] })
outcome_offset_mean <- colMeans(outcome_offset)
outcome_offset_sd <- sapply(ds, FUN=function(data) {
    sd(data[,"outcome"] - data[,grepl("PopulationSize",names(data))])})

# Define model 1 (model 1 -> main analysis)
covars_M1 <- covars
    # setNames(
    # lapply(covars,
    #        FUN=function(covar) {covar[,!grepl("PopulationSize_",names(covar))]}),
    # group)
data_M1 <- setNames(lapply(group,
                           FUN=makeTimeSeries,
                           outcome=outcome,
                           covars=covars_M1,
                           time_points=time_points,
                           scale_outcome=FALSE),
                    group);names(data_M1[[1]])

# Define model 2 (simple model -> sensitivity analysis 1)
# including population size as an offset only
covars_M2 <- setNames(lapply(covars,FUN=function(covar) {
               as.data.frame(list(constant = rep(1, times = nrow(covar))))}), 
               group); names(covars_M2[[1]])
data_M2 <- setNames(lapply(group,
                           FUN=makeTimeSeries,
                           outcome=outcome_offset,
                           covars=covars_M2,
                           time_points=time_points,
                           scale_outcome=TRUE),
                    group); head(data_M2[[1]])

# Define model 3 (ITS -> sensitivity analysis 2)
# including time index with population size as an offset
# no other controls were considered
covars_M3 <- setNames(lapply(covars, FUN=function(covar) {
        as.data.frame(list(time_index = 1:nrow(covar)))}), 
        group); names(covars_M3[[1]])
data_M3 <- setNames(lapply(group,
                           FUN=makeTimeSeries,
                           outcome=outcome_offset,
                           covars=covars_M3,
                           time_points=time_points,
                           scale_outcome=TRUE),
                    group); head(data_M3[[1]])

# Define model 4 (sensitivity analysis 3)
# including time index and population size only
# on other controls were considered
covars_M4 <- setNames(lapply(covars, FUN=function(covar) {
    covar <- as.data.frame(covar[,grepl("PopulationSize_",names(covar)),
                                 drop=FALSE])
    covar$time_index = 1:nrow(covar)
    return(covar)
    }), group); names(covars_M4[[1]])
data_M4 <- setNames(lapply(group,
                           FUN=makeTimeSeries,
                           outcome=outcome,
                           covars=covars_M4,
                           time_points=time_points,
                           scale_outcome=FALSE),
                    group); head(data_M4[[1]])

# Define model 5 (sensitivity analysis 4)
# including all controls excepting population growth and ageing
covars_M5 <- setNames(lapply(covars, FUN=function(covar) {
    covar[,!grepl("Population",names(covar)) &
              !grepl("RatePer1000Pop",names(covar)) &
              !grepl("LifeExpectancyAtBirth",names(covar))]
    }), group); names(covars_M5[[1]])
data_M5 <- setNames(lapply(group,
                           FUN=makeTimeSeries,
                           outcome=outcome,
                           covars=covars_M5,
                           time_points=time_points,
                           scale_outcome=FALSE),
                    group)

rm(list=c("covars","covars_M2","covars_M3","covars_M4","covars_M5"))

# save.image(file=paste(getwd(),"/Result/2011CSCGuideline_TimeSeriesData_20220708.RData"))


# Run the main analysis
library(parallel)
gc()
n_cores <- detectCores(); n_cores
cl <- makeCluster(n_cores)
clusterEvalQ(cl, library(CausalImpact, quietly = TRUE))
clusterExport(cl,c("ds","doCausalImpact",
                   "data_M1","data_M2","data_M3","data_M4","data_M5",
                   "group","data_intervention_date","time_points"))
impact_M1 <- setNames(parLapply(cl,data_M1,doCausalImpact),group)
impact_M2 <- setNames(parLapply(cl,data_M2,doCausalImpact,
                                offset=TRUE),group)
impact_M3 <- setNames(parLapply(cl,data_M3,doCausalImpact,
                                offset=TRUE,trend=TRUE),group)
impact_M4 <- setNames(parLapply(cl,data_M4,doCausalImpact),group)
impact_M5 <- setNames(parLapply(cl,data_M5,doCausalImpact),group)
stopCluster(cl)

# save(
#     # impact_M1,
#     list=c("impact_M1"),
#     file=paste(getwd(),
#                "/Result/2011CSCGuideline_MainAnalysis_Model1_20220708.RData",
#                sep=""))
# 
# save(
#     list=c("impact_M2"),
#     file=paste(getwd(),
#                "/Result/2011CSCGuideline_MainAnalysis_Model2_20220708.RData",
#                sep=""))
# 
# save(
#     list=c("impact_M3"),
#     file=paste(getwd(),
#                "/Result/2011CSCGuideline_MainAnalysis_Model3_20220708.RData",
#                sep=""))
# 
# save(
#     list=c("impact_M4"),
#     file=paste(getwd(),
#                "/Result-Current-10000/2011CSCGuideline_MainAnalysis_Model4_20220708.RData",
#                sep=""))
# 
# save(
#     list=c("impact_M5"),
#     file=paste(getwd(),
#                "/Result-Current-10000/2011CSCGuideline_MainAnalysis_Model5_20220708.RData",
#                sep=""))

# # Run sensitivity analysis with different models
# group1 <- group[as.numeric(substr(group,1,2))==1]
# group2 <- group[as.numeric(substr(group,1,2))==2]
# group3 <- group[as.numeric(substr(group,1,2))==3]
# group4 <- group[as.numeric(substr(group,1,2))==4]
# group5 <- group[as.numeric(substr(group,1,2))==5]
# group6 <- group[as.numeric(substr(group,1,2))==6]
# group7 <- group[as.numeric(substr(group,1,2))==7]
# group8 <- group[as.numeric(substr(group,1,2))==8]
# group9 <- group[as.numeric(substr(group,1,2))==9]
# group10 <- group[as.numeric(substr(group,1,2))==10]
# group11 <- group[as.numeric(substr(group,1,2))==11]
# group12 <- group[as.numeric(substr(group,1,2))==12]
# group13 <- group[as.numeric(substr(group,1,2))==13]
# group14 <- group[as.numeric(substr(group,1,2))==14]
# group15 <- group[as.numeric(substr(group,1,2))==15]
# cl <- makeCluster(n_cores)
# clusterEvalQ(cl, library(CausalImpact, quietly = TRUE))
# clusterExport(cl,c("ds","doCausalImpact","sensitivityAnalysis",
#                    "outcome","group","data_intervention_date","time_points"))
# SA_impact_M1_group1 <- setNames(parLapply(cl,
#                                           group1,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group1)
# SA_impact_M1_group2 <- setNames(parLapply(cl,
#                                           group2,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group2)
# SA_impact_M1_group3 <- setNames(parLapply(cl,
#                                           group3,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group3)
# SA_impact_M1_group4 <- setNames(parLapply(cl,
#                                           group4,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group4)
# SA_impact_M1_group5 <- setNames(parLapply(cl,
#                                           group5,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group5)
# SA_impact_M1_group6 <- setNames(parLapply(cl,
#                                           group6,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group6)
# SA_impact_M1_group7 <- setNames(parLapply(cl,
#                                           group7,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group7)
# SA_impact_M1_group8 <- setNames(parLapply(cl,
#                                           group8,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group8)
# SA_impact_M1_group9 <- setNames(parLapply(cl,
#                                           group9,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group9)
# SA_impact_M1_group10 <- setNames(parLapply(cl,
#                                           group10,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group10)
# SA_impact_M1_group11 <- setNames(parLapply(cl,
#                                           group1,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group11)
# SA_impact_M1_group12 <- setNames(parLapply(cl,
#                                           group12,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group12)
# SA_impact_M1_group13 <- setNames(parLapply(cl,
#                                           group13,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group13)
# SA_impact_M1_group14 <- setNames(parLapply(cl,
#                                           group14,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group14)
# SA_impact_M1_group15 <- setNames(parLapply(cl,
#                                           group15,
#                                           sensitivityAnalysis,
#                                           outcome=outcome,
#                                           covars=covars_M1,
#                                           impact=impact_M1),
#                                 group15)
# stopCluster(cl)
# 
# save.image(
#     list=c("SA_impact_M1_group1",
#            "SA_impact_M1_group2",
#            "SA_impact_M1_group3",
#            "SA_impact_M1_group4",
#            "SA_impact_M1_group5"),
#     file=paste(getwd(),
#                "/Result/2011CSCGuideline_SensitivityAnalysis_groups1_5_20220708.RData"))
# 
# save.image(
#     list=c("SA_impact_M1_group6",
#            "SA_impact_M1_group7",
#            "SA_impact_M1_group8",
#            "SA_impact_M1_group9",
#            "SA_impact_M1_group10"),
#     file=paste(getwd(),
#                "/Result/2011CSCGuideline_SensitivityAnalysis_groups6_10_20220708.RData"))
# 
# save.image(
#     list=c("SA_impact_M1_group11",
#            "SA_impact_M1_group12",
#            "SA_impact_M1_group13",
#            "SA_impact_M1_group14",
#            "SA_impact_M1_group15"),
#     file=paste(getwd(),
#                "/Result/2011CSCGuideline_SensitivityAnalysis_groups11_15_20220708.RData"))
# 
# save.image(file=paste(getwd(),"/Result/ChineseGuideline_MainAnalysis_20220708_Update.RData")
# 
# 