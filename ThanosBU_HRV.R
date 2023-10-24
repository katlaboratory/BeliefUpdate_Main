###### LIBRARIES #####
library("Hmisc")
library(readr)
library(lme4)
library(sjPlot)
library(dplyr)
library(Metrics)
library(ggplot2)
library(psych)

source("summarySE.R")
source("R_rainclouds.R")

###### FUNCTIONS #### 
GetResultsTable<-function(dataset)
{
  
  lm1<-lm(POST_PROSP_AVG~PRIOR,data=dataset);
  lm2<-lm(POST_PROSP_AVG~IAcc,data=dataset);
  lm3<-lm(POST_PROSP_AVG~RETRO,data=dataset);
  lm4<-lm(POST_PROSP_AVG~POST_PROSP_Pred_l,data=dataset);
  lm5<-lm(POST_PROSP_AVG~POST_PROSP_Pred_g,data=dataset);
  lm6<-lm(POST_PROSP_AVG~POST_PROSP_Pred_gl,data=dataset);
  bicTable<-round(BIC(lm1,lm2,lm3,lm4,lm5,lm6),1);
  aicTable<-round(AIC(lm1,lm2,lm3,lm4,lm5,lm6),1)
  
  df <- data.frame(Model=character(),N=integer(),MAE=double(),
                   R2=double(),BIC=double(),AIC=double(),
                   stringsAsFactors=FALSE)
  
  df<-rbind(df,data.frame(Model="Baseline: 0 Learning",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$PRIOR),2),
                          R2=round(summary(lm1)$r.squared,3),BIC=bicTable[1,"BIC"],AIC=aicTable[1,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="Baseline: 100% Learning Local",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$IAcc),2),
                          R2=round(summary(lm2)$r.squared,3),BIC=bicTable[2,"BIC"],AIC=aicTable[2,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="Baseline: 100% Learning Global",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$RETRO),2),
                          R2=round(summary(lm3)$r.squared,3),BIC=bicTable[3,"BIC"],AIC=aicTable[3,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="LocalEvidence",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$POST_PROSP_Pred_l),2),
                          R2=round(summary(lm4)$r.squared,3),BIC=bicTable[4,"BIC"],AIC=aicTable[4,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="GlobalEvidence",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$POST_PROSP_Pred_g),2),
                          R2=round(summary(lm5)$r.squared,3),BIC=bicTable[5,"BIC"],AIC=aicTable[5,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="GlobalEvidLocalPrec",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$POST_PROSP_Pred_gl),2),
                          R2=round(summary(lm6)$r.squared,3),BIC=bicTable[6,"BIC"],AIC=aicTable[6,"AIC"],
                          stringsAsFactors=FALSE))
  return(df)
}

GetResultsTable<-function(dataset)
{
  
  lm1<-lm(POST_PROSP_AVG~PRIOR,data=dataset);
  lm2<-lm(POST_PROSP_AVG~IAcc,data=dataset);
  lm3<-lm(POST_PROSP_AVG~RETRO,data=dataset);
  lm4<-lm(POST_PROSP_AVG~POST_PROSP_pred_iacc_1,data=dataset);
  lm5<-lm(POST_PROSP_AVG~POST_PROSP_pred_retro_1,data=dataset);
  lm6<-lm(POST_PROSP_AVG~POST_PROSP_pred_iacc_2,data=dataset);
  lm7<-lm(POST_PROSP_AVG~POST_PROSP_pred_retro_2,data=dataset);
  
  bicTable<-round(BIC(lm1,lm2,lm3,lm4,lm5,lm6,lm7),1);
  aicTable<-round(AIC(lm1,lm2,lm3,lm4,lm5,lm6,lm7),1)
  
  df <- data.frame(Model=character(),N=integer(),MAE=double(),
                   R2=double(),BIC=double(),AIC=double(),
                   stringsAsFactors=FALSE)
  
  df<-rbind(df,data.frame(Model="Baseline: 0 Learning",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$PRIOR),2),
                          R2=round(summary(lm1)$r.squared,3),BIC=bicTable[1,"BIC"],AIC=aicTable[1,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="Baseline: 100% Learning Accuracy",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$IAcc),2),
                          R2=round(summary(lm2)$r.squared,3),BIC=bicTable[2,"BIC"],AIC=aicTable[2,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="Baseline: 100% Learning Retro",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$RETRO),2),
                          R2=round(summary(lm3)$r.squared,3),BIC=bicTable[3,"BIC"],AIC=aicTable[3,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="AccuracyEvidence",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$POST_PROSP_pred_iacc_1),2),
                          R2=round(summary(lm4)$r.squared,3),BIC=bicTable[4,"BIC"],AIC=aicTable[4,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="RetroEvidence",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$POST_PROSP_pred_retro_1),2),
                          R2=round(summary(lm5)$r.squared,3),BIC=bicTable[5,"BIC"],AIC=aicTable[5,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="AccuracyEvidenceEDI",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$POST_PROSP_pred_iacc_2),2),
                          R2=round(summary(lm6)$r.squared,3),BIC=bicTable[6,"BIC"],AIC=aicTable[6,"AIC"],
                          stringsAsFactors=FALSE))
  df<-rbind(df,data.frame(Model="RetroEvidenceEDI",N=nrow(dataset),
                          MAE=round(mae(dataset$POST_PROSP_AVG,dataset$POST_PROSP_pred_retro_2),2),
                          R2=round(summary(lm7)$r.squared,3),BIC=bicTable[7,"BIC"],AIC=aicTable[7,"AIC"],
                          stringsAsFactors=FALSE))
  
  return(df)
}
###### DATA #### 
# need to combine the four cohort data first
dtStudy_ITA <- read_csv("Data/AN_ITALIANS_BU_ANALYSES.csv")
dtStudy_WATER <- read_csv("Data/AN_ALKISTIS_BU_ANALYSES_ALL.csv")
dtStudy_GR <- read_csv("Data/AN_GREEKS_BU_ANALYSES.csv")
dtStudy_LUCY <- read_csv("Data/AN_LUCY_BU_ANALYSES_ALL.csv")

unique(colnames(dtStudy_ITA))
unique(colnames(dtStudy_WATER))

# binding this way bc they do not have all the same column names, i.e., not everyone did the same exp. 
# some have diff qnaires etc. 

dtStudy_ITA[setdiff(names(dtStudy_WATER), names(dtStudy_ITA))] <- NA
dtStudy_WATER[setdiff(names(dtStudy_ITA), names(dtStudy_WATER))] <- NA

dtMerged<-rbind(dtStudy_ITA,dtStudy_WATER)
dtMerged[setdiff(names(dtStudy_GR), names(dtMerged))] <- NA
dtStudy_GR[setdiff(names(dtMerged), names(dtStudy_GR))] <- NA

dtMerged<-rbind(dtMerged,dtStudy_GR)
dtMerged[setdiff(names(dtStudy_LUCY), names(dtMerged))] <- NA
dtStudy_LUCY[setdiff(names(dtMerged), names(dtStudy_LUCY))] <- NA
dtMerged<-rbind(dtMerged,dtStudy_LUCY)
colnames(dtMerged)
length(unique(colnames(dtMerged)))


dtRawHR<-read_csv("Data/EmpaticaHRData.csv")
dtRawIBI<-read_csv("Data/EmpaticaIBIData.csv")
dtRawTags<-read_csv("Data/EmpaticaTagsData.csv")
subjectsList<-unique(dtMerged$ID)

dtRawHR<-filter(dtRawHR,SUBJ_ID %in%subjectsList)
dtRawIBI<-filter(dtRawIBI,SUBJ_ID %in%subjectsList)
dtRawTags<-filter(dtRawTags,SUBJ_ID %in%subjectsList)


dtRawTags$Phase<-NA_integer_
previousSubj<-""
cnt<-0
for(i in seq(1:nrow(dtRawTags)))
{
  if(i==1)
  {
    previousSubj<-dtRawTags[i,"SUBJ_ID"]
  }
  else
    previousSubj<- dtRawTags[i-1,"SUBJ_ID"]
  
  if(previousSubj == dtRawTags[i,"SUBJ_ID"])
  {
    dtRawTags[i,"Phase"]<-cnt
    cnt<- cnt+1
  }
  else 
  {
    dtRawTags[i,"Phase"]<-0
    cnt<- 1
  }
  
}
summary(as.factor(dtRawTags$Phase))


dtRawHR$Phase<-NA_integer_
dtRawIBI$Phase<-NA_integer_

for(i in seq(1,9))
{
  colNb<-i-1
  newCol<-paste("phase",colNb,sep="")
  dtRawHR<-merge(dtRawHR, select(filter(dtRawTags, Phase==colNb),SUBJ_ID,newCol=timeUnix), by="SUBJ_ID",all.x=T )
}
colnames(dtRawHR)[5:13]<-c("phase0","phase1","phase2","phase3","phase4","phase5","phase6","phase7","phase8")

dtRawHR[dtRawHR$timeUnix<dtRawHR$phase0,]$Phase<-0
dtRawHR[!is.na(dtRawHR$phase0) &!is.na(dtRawHR$phase1) & dtRawHR$timeUnix>=dtRawHR$phase0 & dtRawHR$timeUnix<dtRawHR$phase1,]$Phase<-1
dtRawHR[!is.na(dtRawHR$phase1) &!is.na(dtRawHR$phase2) & dtRawHR$timeUnix>=dtRawHR$phase1 & dtRawHR$timeUnix<dtRawHR$phase2,]$Phase<-2
dtRawHR[!is.na(dtRawHR$phase2) &!is.na(dtRawHR$phase3) & dtRawHR$timeUnix>=dtRawHR$phase2 & dtRawHR$timeUnix<dtRawHR$phase3,]$Phase<-3
dtRawHR[!is.na(dtRawHR$phase3) &!is.na(dtRawHR$phase4) & dtRawHR$timeUnix>=dtRawHR$phase3 & dtRawHR$timeUnix<dtRawHR$phase4,]$Phase<-4
dtRawHR[!is.na(dtRawHR$phase4) &!is.na(dtRawHR$phase5) & dtRawHR$timeUnix>=dtRawHR$phase4 & dtRawHR$timeUnix<dtRawHR$phase5,]$Phase<-5
dtRawHR[!is.na(dtRawHR$phase5) &!is.na(dtRawHR$phase6) & dtRawHR$timeUnix>=dtRawHR$phase5 & dtRawHR$timeUnix<dtRawHR$phase6,]$Phase<-6
dtRawHR[!is.na(dtRawHR$phase6) &!is.na(dtRawHR$phase7) & dtRawHR$timeUnix>=dtRawHR$phase6 & dtRawHR$timeUnix<dtRawHR$phase7,]$Phase<-7
dtRawHR[!is.na(dtRawHR$phase7) &!is.na(dtRawHR$phase8) & dtRawHR$timeUnix>=dtRawHR$phase7 & dtRawHR$timeUnix<dtRawHR$phase8,]$Phase<-8
dtRawHR[!is.na(dtRawHR$phase8) & dtRawHR$timeUnix>=dtRawHR$phase8 ,]$Phase<-9

select(filter(dtRawHR, Phase==1),SUBJ_ID,newCol=timeUnix)

for(i in seq(1,9))
{
  colNb<-i-1
  newCol<-paste("phase",colNb,sep="")
  dtRawIBI<-merge(dtRawIBI, select(filter(dtRawTags, Phase==colNb),SUBJ_ID,newCol=timeUnix), by="SUBJ_ID",all.x=T )
}
colnames(dtRawIBI)[6:14]<-c("phase0","phase1","phase2","phase3","phase4","phase5","phase6","phase7","phase8")



dtRawIBI[!is.na(dtRawIBI$phase0) & dtRawIBI$timeUnix<dtRawIBI$phase0,]$Phase<-0
dtRawIBI[!is.na(dtRawIBI$phase0) &!is.na(dtRawIBI$phase1) & dtRawIBI$timeUnix>=dtRawIBI$phase0 & dtRawIBI$timeUnix<dtRawIBI$phase1,]$Phase<-1
dtRawIBI[!is.na(dtRawIBI$phase1) &!is.na(dtRawIBI$phase2) & dtRawIBI$timeUnix>=dtRawIBI$phase1 & dtRawIBI$timeUnix<dtRawIBI$phase2,]$Phase<-2
dtRawIBI[!is.na(dtRawIBI$phase2) &!is.na(dtRawIBI$phase3) & dtRawIBI$timeUnix>=dtRawIBI$phase2 & dtRawIBI$timeUnix<dtRawIBI$phase3,]$Phase<-3
dtRawIBI[!is.na(dtRawIBI$phase3) &!is.na(dtRawIBI$phase4) & dtRawIBI$timeUnix>=dtRawIBI$phase3 & dtRawIBI$timeUnix<dtRawIBI$phase4,]$Phase<-4
dtRawIBI[!is.na(dtRawIBI$phase4) &!is.na(dtRawIBI$phase5) & dtRawIBI$timeUnix>=dtRawIBI$phase4 & dtRawIBI$timeUnix<dtRawIBI$phase5,]$Phase<-5
dtRawIBI[!is.na(dtRawIBI$phase5) &!is.na(dtRawIBI$phase6) & dtRawIBI$timeUnix>=dtRawIBI$phase5 & dtRawIBI$timeUnix<dtRawIBI$phase6,]$Phase<-6
dtRawIBI[!is.na(dtRawIBI$phase6) &!is.na(dtRawIBI$phase7) & dtRawIBI$timeUnix>=dtRawIBI$phase6 & dtRawIBI$timeUnix<dtRawIBI$phase7,]$Phase<-7
dtRawIBI[!is.na(dtRawIBI$phase7) &!is.na(dtRawIBI$phase8) & dtRawIBI$timeUnix>=dtRawIBI$phase7 & dtRawIBI$timeUnix<dtRawIBI$phase8,]$Phase<-8
dtRawIBI[!is.na(dtRawIBI$phase8) & dtRawIBI$timeUnix>=dtRawIBI$phase8 ,]$Phase<-9

summary(as.factor(dtRawHR$Phase))
summary(as.factor(dtRawIBI$Phase))


dtIBI<-dtRawIBI
summary(dtIBI$Phase)
dtIBI<-dtIBI %>%arrange(SUBJ_ID,timeUnix)
dtIBI<-dtIBI%>%mutate(deltaIBI=IBI-lag(IBI),samePhase= Phase==lag(Phase),unixTimeLag=timeUnix-lag(timeUnix) )
summary(dtIBI$samePhase)
dtIBI[!is.na(dtIBI$samePhase) & dtIBI$samePhase==FALSE,]$deltaIBI<-NA_real_
summary(dtIBI$deltaIBI)
dtIBI$deltaIBI_2<-dtIBI$deltaIBI*dtIBI$deltaIBI

dtIBIPhases<-filter(dtIBI,!is.na(samePhase))%>%group_by(SUBJ_ID,Phase)%>%
  summarise(Nibi=n(),RMSSD=sqrt(mean(deltaIBI_2,na.rm=T)),unixTimeLag=sum(unixTimeLag,na.rm=T) ) 


dtSubjPhase_HR<- dtRawHR %>% group_by(SUBJ_ID,Phase) %>% summarise(HR_mean=mean(HR,na.rm=T))
dtSubjPhase_IBI<- dtRawIBI %>% group_by(SUBJ_ID,Phase) %>% summarise(IBI_mean=mean(IBI,na.rm=T),IBI_sd=sd(IBI,na.rm=T),IBI_N=n())



dtTest<-filter(dtRawIBI) %>% group_by(SUBJ_ID,Phase) %>% summarise(
  HRV=sd(IBI,na.rm=T),
  HRV_N=n(),
  IBI=mean(IBI,na.rm=T),)

hist((dtTest%>%group_by(SUBJ_ID) %>% summarise(
  N=n()))$N)

filter(dtTest,Phase>0)
unique(dtSubjPhase_HR$SUBJ_ID)
summary(dtSubjPhase_HR$Phase)

dtAll<-merge(dtMerged,dplyr::select(filter(dtSubjPhase_HR,Phase==0), SUBJ_ID, HR_mean) , by.x=c("ID"),by.y=c("SUBJ_ID"),all.x=T)
dtAll<-merge(dtAll,dplyr::select(filter(dtIBIPhases,Phase==0), SUBJ_ID, Nibi, HRV_RMSSD=RMSSD), by.x=c("ID"),by.y=c("SUBJ_ID"),all.x=T)
dtAll<-merge(dtAll,dplyr::select(filter(dtSubjPhase_IBI,Phase==0), SUBJ_ID, IBI_mean, IBI_sd, IBI_N), by.x=c("ID"),by.y=c("SUBJ_ID"),all.x=T)

dtRaw <- dtAll 
summary(as.factor(dtRaw$GROUP))
dtRaw%>%group_by(GROUP)%>%summarise(HRV_mean=mean(HRV_RMSSD,na.rm=T),Nibi=mean(Nibi,na.rm=T),HR_mean=mean(HR_mean,na.rm=T))
# we want to excl anyone who doesn't belong to one of the three main groups
dtRaw<-filter(dtRaw,GROUP %in% c("HC","AN", "AN_WR"))

# the below was in the BACN analysis script, do I need it here too?
#strangeIDs<-c("32","3","11","17")
#dtRaw<-read_csv("Water_HCT_Data_Conference_20220508.csv")

# relevelling so we have HC as intercept 
dtRaw$GROUP<-as.factor(dtRaw$GROUP)
dtRaw$GROUP<-relevel(dtRaw$GROUP,"HC")

###### Variable Calculations #### 

# IAcc
dtRaw$IAcc_25<-1-abs((dtRaw$ACT_25-dtRaw$EST_25)/dtRaw$ACT_25)
dtRaw$IAcc_30<-1-abs((dtRaw$ACT_30-dtRaw$EST_30)/dtRaw$ACT_30)
dtRaw$IAcc_45<-1-abs((dtRaw$ACT_45-dtRaw$EST_45)/dtRaw$ACT_45)
dtRaw$IAcc_65<-1-abs((dtRaw$ACT_65-dtRaw$EST_65)/dtRaw$ACT_65)

# combine IAcc
dtRaw$IAcc<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  Iacc<-100*mean(c(as.numeric(dtRaw[indx,"IAcc_25"]),as.numeric(dtRaw[indx,"IAcc_30"]),as.numeric(dtRaw[indx,"IAcc_45"]),
                   as.numeric(dtRaw[indx,"IAcc_65"])),na.rm=T)
  return(Iacc)
}))

# HR Accuracy for Self- and Other HB knowledge
dtRaw$ACT_Scaled_25<-dtRaw$ACT_25*60/25
dtRaw$ACT_Scaled_30<-dtRaw$ACT_30*60/30
dtRaw$ACT_Scaled_45<-dtRaw$ACT_45*60/45
dtRaw$ACT_Scaled_65<-dtRaw$ACT_65*60/65

dtRaw$ACT_Scaled_60<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  ACT_Scaled_60<-mean(c(as.numeric(dtRaw[indx,"ACT_Scaled_25"]),as.numeric(dtRaw[indx,"ACT_Scaled_30"]),as.numeric(dtRaw[indx,"ACT_Scaled_45"]),
                   as.numeric(dtRaw[indx,"ACT_Scaled_65"])),na.rm=T)
  return(ACT_Scaled_60)
}))
hist(dtRaw$ACT_Scaled_60)

# Estimating a self accuracy score based on self hb knowledge and HB 
dtRaw$Self_HB_Accuracy <- abs((dtRaw$ACT_Scaled_60-dtRaw$AVG_HB_SELF)/dtRaw$ACT_Scaled_60)

# Estimating an other accuracy score based on other hb knowledge and HB
dtRaw$Other_HB_Accuracy <- abs((dtRaw$ACT_Scaled_60-dtRaw$AVG_HB_OTHER)/dtRaw$ACT_Scaled_60)

dtRaw$Other_HB_Accuracy <- abs((80-dtRaw$AVG_HB_OTHER)/80)
# keep either mean of our sample, or of general population, which is 80 

# Time Accuracy
dtRaw$TIME_ACCURACY_25<-1-abs((25-dtRaw$TIME_25)/25)
dtRaw$TIME_ACCURACY_30<-1-abs((30-dtRaw$TIME_30)/30)
dtRaw$TIME_ACCURACY_45<-1-abs((45-dtRaw$TIME_45)/45)
dtRaw$TIME_ACCURACY_65<-1-abs((65-dtRaw$TIME_65)/65)
dtRaw$TIME_ACCURACY_60<-1-abs((60-dtRaw$TIME_60)/60)

# combine Time Accuracy
dtRaw$Time_Accuracy<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  Time_Accuracy<-100*mean(c(as.numeric(dtRaw[indx,"TIME_ACCURACY_25"]),as.numeric(dtRaw[indx,"TIME_ACCURACY_30"]),as.numeric(dtRaw[indx,"TIME_ACCURACY_45"]),
                   as.numeric(dtRaw[indx,"TIME_ACCURACY_65"])),na.rm=T)
  return(Time_Accuracy)
}))

# for correlations intero awareness

dtRaw$ACC_CONF_Corr<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  acc<-c(as.numeric(dtRaw[indx,"IAcc_25"]),as.numeric(dtRaw[indx,"IAcc_30"]),as.numeric(dtRaw[indx,"IAcc_45"]),
         as.numeric(dtRaw[indx,"IAcc_65"]))
  conf<-c(as.numeric(dtRaw[indx,"EST_25_CONF"]),as.numeric(dtRaw[indx,"EST_30_CONF"]),as.numeric(dtRaw[indx,"EST_45_CONF"]),
          as.numeric(dtRaw[indx,"EST_65_CONF"]))
  if(sum(!is.na(acc))>=3&sum(!is.na(conf))>=3)
    ACC_CONF_Corr<-as.numeric(cor.test(acc,conf)$estimate)
  else
    ACC_CONF_Corr<-NA_real_
  return(ACC_CONF_Corr)
}))

# Performance Error Percentage 
dtRaw$PERF_ERROR_PCT <- abs(100*((dtRaw$RETRO - dtRaw$IAcc)/dtRaw$IAcc))
hist(dtRaw$PERF_ERROR_PCT, breaks=100)
hist(log(dtRaw$PERF_ERROR_PCT), breaks=100)

dtRaw$PERF_ERROR_PCT_LOG <- log(dtRaw$PERF_ERROR_PCT)

# Prediction error calculations 

# Posterior Prospective Conf Calculations
# here we have double and half post prosp conf
dtRaw$POST_PROSP_AVG_CONF<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  POST_PROSP_AVG_CONF<-mean(c(as.numeric(dtRaw[indx,"POST_PROSP_L_CONF"]),as.numeric(dtRaw[indx,"POST_PROSP_S_CONF"])),na.rm=T)
  return(POST_PROSP_AVG_CONF)
}))

# here we have double and half post prosp belief
dtRaw$POST_PROSP_AVG<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  POST_PROSP_AVG<-mean(c(as.numeric(dtRaw[indx,"POST_PROSP_L"]),as.numeric(dtRaw[indx,"POST_PROSP_S"])),na.rm=T)
  return(POST_PROSP_AVG)
}))
dtRaw$BU<-dtRaw$POST_PROSP_AVG-dtRaw$PRIOR

# here we have post HCT conf, i.e., how conf are you w the number of beats you felt
dtRaw$EST_CONF_AVG<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  EST_CONF_AVG<-mean(c(as.numeric(dtRaw[indx,"EST_25_CONF"]),as.numeric(dtRaw[indx,"EST_30_CONF"]),as.numeric(dtRaw[indx,"EST_45_CONF"]),
                       as.numeric(dtRaw[indx,"EST_65_CONF"])),na.rm=T)
  return(EST_CONF_AVG)
}))

# ITPE for Q5
hist(dtRaw$EDI_ID)
dtRaw$EDI_ID
SD_EDI_ID_IAcc <- sd(dtRaw$IAcc - (100-(((dtRaw$EDI_ID)/36)*100)), na.rm = T)
dtRaw$EDI_ID_SCALED <- (100-(((dtRaw$EDI_ID)/36)*100))
dtRaw$ITPE_z <- (dtRaw$IAcc - dtRaw$EDI_ID_SCALED)/SD_EDI_ID_IAcc
hist(dtRaw$EDI_ID_SCALED)
hist(dtRaw$ITPE_z)
hist(dtRaw$IAcc)

# diffs between groups 
lm0<-lm(IAcc~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

lm0<-lm(EDI_ID~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

lm0<-lm(EDI_ID_SCALED~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

lm0<-lm(IAcc - EDI_ID_SCALED~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# diffs between self-report and IAcc smaller in AN bc they are quite bad in EDI_ID but do quite well 
# in fact IAcc in AN slightly better than HCs
# ITPE measure problematic - a scaling issue 

# or, if we do separate z-scores for ITPE
# we have to make them mean-centred otherwise the directionality is odd
# first we get a z-score for EDI_ID
# in EDI_ID, the higher the score, the worse the deficit
dtRaw$EDI_ID
SD_EDI_ID <- sd((100-(((dtRaw$EDI_ID)/36)*100)), na.rm = T)
mean_EDI_ID <- mean((100-(((dtRaw$EDI_ID)/36)*100)), na.rm = T)
dtRaw$EDI_ID_SCALED <- (100-(((dtRaw$EDI_ID)/36)*100))
dtRaw$EDI_ID_z <- (mean_EDI_ID - dtRaw$EDI_ID_SCALED)/SD_EDI_ID
hist(dtRaw$EDI_ID_z)

# then we get a z-score for IAcc
dtRaw$IAcc
SD_IAcc <- sd(dtRaw$IAcc, na.rm = T)
mean_IAcc <- mean(dtRaw$IAcc, na.rm = T)
dtRaw$IAcc_z <- (mean_IAcc - dtRaw$IAcc)/SD_IAcc
hist(dtRaw$IAcc_z)

# difference 
dtRaw$ITPE_Other_z <- dtRaw$IAcc_z - dtRaw$EDI_ID_z
hist(dtRaw$ITPE_Other_z)
# keep this. z score of each component and then their difference. 

###### Summary Stats ####
###### calculating some quick stats, but need to add more 
summary(dtRaw$GROUP)

as.data.frame(dtRaw %>% group_by(GROUP) %>% dplyr::summarise(BMI=round(mean(BMI,na.rm=T),1),
                                                             AGE=round(mean(AGE,na.rm=T),1) ))

as.data.frame(dtRaw %>% group_by(GROUP) %>% dplyr::summarise(N=n(),PRIOR_CONF=paste(round(mean(PRIOR_CONF,na.rm=T),1),"(",
                                                                                    round(sd(PRIOR_CONF,na.rm=T),2),")",sep=""),
                                                             PRIOR=paste(round(mean(PRIOR,na.rm=T),1),"(",
                                                                         round(sd(PRIOR,na.rm=T),2),")",sep=""),
                                                             IAcc=paste(round(mean(IAcc,na.rm=T),2),"(",
                                                                        round(sd(IAcc,na.rm=T),2),")",sep=""),
                                                             TIME_ACCURACY=round(mean(TIME_ACCURACY_65,na.rm=T),2),

))

###### Initial control analyses w BMI and age between groups #### 
# add STUDY 
# firstly, need to filter out AN_BP
dtTemp<-filter(dtRaw,GROUP %in% c("HC","AN", "AN_WR"))

# BMI diffs between groups
lm0<-lm(BMI~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# no diffs between AN_WR and HCs but sig diff, as expected between AN and HCs

###### Initial, baseline belief comparisons #### 

# prior prospective beliefs comparisons 
lm0<-lm(PRIOR~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
hist(filter(dtTemp,GROUP!="AN_BP")$PRIOR, breaks=10)
# n.s. diffs at the prior belief level

lmer0<-lmer(PRIOR~GROUP+ AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
tab_model(lmer0)
# no variance, in supp. material 

# then we look at the perf error pct  
lm0<-lm(PERF_ERROR_PCT_LOG~GROUP,data=filter(dtTemp,GROUP!="AN_BP" & PERF_ERROR_PCT_LOG != Inf))
summary(lm0)
# log bc not nornally distributed 

# thanos says to keep both and do the same w the priors, this was not pre-registered
lm0<-lm(RETRO - IAcc ~GROUP,data=filter(dtTemp,GROUP!="AN_BP"))
summary(lm0)
summary (dtTemp$RETRO - dtTemp$IAcc)
# the rose tinted window 
# hc overestimate; clinical underestimate 

# check if scores are predictive irrespective of group 

hist(dtTemp$RETRO - dtTemp$IAcc, breaks = 100)
# HC did worse than they think, but in AN the diff between reality and belief is smaller
# same in AN_WR - compared to HCs. smaller to the point of being neg
# will then need to do post hocs for each of the groups

lm0<-lm(RETRO - IAcc ~1,data=filter(dtTemp,GROUP=="HC"))
summary(lm0)

lm0<-lm(RETRO - IAcc ~1,data=filter(dtTemp,GROUP=="AN"))
summary(lm0)

lm0<-lm(RETRO - IAcc ~1,data=filter(dtTemp,GROUP=="AN_WR"))
summary(lm0)

# sig diff between HCs and AN
# finding: the diff is smaller in the AN, but odd bc going from pos to neg

# posterior prospective avg (i.e., double and half length tgt)
lm0<-lm(POST_PROSP_AVG~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

lm0<-lmer(POST_PROSP_AVG~GROUP+ (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
tab_model(lm0)
# same as above, more or less. a bit better w study here. Add in supplementary material

# both groups more pess than HCs
hist(dtTemp$POST_PROSP_AVG, breaks=100)

# correlation between IAcc and perf conf 
cor.test (dtRaw$IAcc, dtRaw$EST_CONF_AVG)
# repeat w one score per ppt, 

cor.test (filter(dtRaw, GROUP =="HC")$IAcc, filter(dtRaw, GROUP =="HC")$EST_CONF_AVG)
# neg correlation, the higher the conf, the lower the IAcc

cor.test (filter(dtRaw, GROUP =="AN")$IAcc, filter(dtRaw, GROUP =="AN")$EST_CONF_AVG)
# pos correlation, the higher the conf, the lower the conf

cor.test (filter(dtRaw, GROUP =="AN_WR")$IAcc, filter(dtRaw, GROUP =="AN_WR")$EST_CONF_AVG)
# neg correlation

lm0<-lm(ACC_CONF_Corr~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# after we do correlations per person, we compare between groups. We see AN have a greater correlation but n.s.
# probs bc HCs overestimated their IAcc and had greater conf. AN smaller diff between beliefs and IAcc.
# if we were do to posthocs AN would probs be sig. 
# trend, greater correlation, i.e. greater awareness 
# this is w 3. but can check w 4 as well. 

lm0<-lm(ACC_CONF_Corr~1,data=filter(dtTemp,GROUP=="AN") )
summary(lm0)
# actually, AN n.s. Leave it, no need to include in results. Large SE 

# correlation between IAcc and EDI intero subscale 
cor.test (dtRaw$IAcc, dtRaw$EDI_ID)

cor.test (filter(dtRaw, GROUP =="HC")$EDI_ID, filter(dtRaw, GROUP =="HC")$IAcc)
# here pos correlation

cor.test (filter(dtRaw, GROUP =="AN")$EDI_ID, filter(dtRaw, GROUP =="AN")$IAcc)
# here the lower the IAcc, the lwoer the EDI

cor.test (filter(dtRaw, GROUP =="AN_WR")$EDI_ID, filter(dtRaw, GROUP =="AN_WR")$IAcc)
# same as HCs

cor.test (dtRaw$EST_CONF_AVG, dtRaw$EDI_ID)
# here a neg correlation, the higher the edi, the lower the conf 
# not making much sense..should be the higher the edi, the higher the conf

# per group - all negative correlations
cor.test (filter(dtRaw, GROUP =="HC")$EDI_ID, filter(dtRaw, GROUP =="HC")$EST_CONF_AVG)

cor.test (filter(dtRaw, GROUP =="AN")$EDI_ID, filter(dtRaw, GROUP =="AN")$EST_CONF_AVG)

cor.test (filter(dtRaw, GROUP =="AN_WR")$EDI_ID, filter(dtRaw, GROUP =="AN_WR")$EST_CONF_AVG)
# all negative..the higher the edi, the lower the conf.. 

# in prior prosp, it should be a pos correlation 

cor.test (filter(dtRaw, GROUP =="HC")$EDI_ID, filter(dtRaw, GROUP =="HC")$PRIOR)
# neg but small 

cor.test (filter(dtRaw, GROUP =="AN")$EDI_ID, filter(dtRaw, GROUP =="AN")$PRIOR)
# neg but p n.s. and large variance
cor.test (filter(dtRaw, GROUP =="AN_WR")$EDI_ID, filter(dtRaw, GROUP =="AN_WR")$PRIOR)
# here pos

# ITPE
# lm0<-lm(ITPE_z~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
# summary(lm0)
# this is not a proper z score so do not use as a z score
# AN higher than HCs, so do they over/under estimate less than HCs? they're closer to 0...

# if we calculate EDI_ID and IAcc z-scores separately then subtract
lm0<-lm(ITPE_Other_z~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# sig. higher in AN group - so they underestimate
# if IAcc is greater, then you did better than EDI_ID. if smaller, then worse than beliefs
# here, looks like the beliefs (EDI_ID) of AN are greater than IAcc

# main effects per group
lm0<-lm(ITPE_Other_z~1,data=filter(dtTemp,GROUP=="HC") )
summary(lm0)
# an underestimation of their performance 

lm0<-lm(ITPE_Other_z~1,data=filter(dtTemp,GROUP=="AN") )
summary(lm0)
# here sig., which means that they had greater EDI than IAcc. So higher beliefs than IAcc
# the beliefs (EDI) from IAcc are farther apart as opposed to HCs. In HCs it's clser. not a finding
# just for me to understand
# an overestimation of their performance 

lm0<-lm(ITPE_Other_z~1,data=filter(dtTemp,GROUP=="AN_WR") )
summary(lm0)

# EDI diffs per group 
lm0<-lm(EDI_ID~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

###### Initial, baseline confidence comparisons ####
# prior prosp conf
lm0<-lm(PRIOR_CONF~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

# est hb conf, i.e., how conf are you w the number of beats you felt, retrospective conf on performance
lm0<-lm(EST_CONF_AVG~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

# retro conf
lm0<-lm(RETRO_CONF~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

# avg conf
lm0<-lm(POST_PROSP_AVG_CONF~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

###### IAcc and Time Error comparisons ####
lm0<-lm(100*HRV_RMSSD~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
tab_model(lm0,digits=3)
lm0<-lm(HR_mean~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
tab_model(lm0,digits=3)
dtTemp<-dtRaw
dtTemp25<-dplyr::select(dtTemp, ID,STUDY, IAcc=IAcc_25, GROUP,AGE);dtTemp25$TrialSecs<-"25"
dtTemp30<-dplyr::select(dtTemp, ID,STUDY,IAcc=IAcc_30, GROUP,AGE);dtTemp30$TrialSecs<-"30"
dtTemp45<-dplyr::select(dtTemp, ID,STUDY,IAcc=IAcc_45, GROUP,AGE);dtTemp45$TrialSecs<-"45"
dtTemp65<-dplyr::select(dtTemp, ID,STUDY,IAcc=IAcc_65, GROUP,AGE);dtTemp65$TrialSecs<-"65"
dtTemp_Long<-rbind(dtTemp25,dtTemp30);dtTemp_Long<-rbind(dtTemp_Long,dtTemp45);
dtTemp_Long<-rbind(dtTemp_Long,dtTemp65);

# IAcc first
lm0<-lm(IAcc~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# Thanos TOST and bayesian 

lm0<-lm(IAcc~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
library(bayestestR)
equivalence_test(lm0)
# for this, the HO column says undecided for AN. This means we dk if the null H got rejections.
library(baymedr)
equiv_bf(x=filter(dtTemp,GROUP=="HC" & !is.nan(IAcc)&!is.na(IAcc))$IAcc,
         y=filter(dtTemp,GROUP=="AN"& !is.nan(IAcc)&!is.na(IAcc))$IAcc,
         interval=c(-5,5),interval_std=FALSE)
# here, 3.6 bayes factor suggests there's moderate evidence but not sig.
# we cannot say w stat sig that there is no diffs between IAcc in the groups

library(TOSTER)
xx<-filter(dtTemp,GROUP=="HC" & !is.nan(IAcc)&!is.na(IAcc))$IAcc
yy<-filter(dtTemp,GROUP=="AN"& !is.nan(IAcc)&!is.na(IAcc))$IAcc
mXX<-mean(xx);sXX<-sd(xx);nXX<-length(xx);mYY<-mean(yy);sYY<-sd(yy);nYY<-length(yy);
mXX;sXX;nXX;mYY;sYY;nYY
TOSTtwo(mYY,mXX,sYY,sXX,nYY,nXX,low_eqbound_d=-1.5,high_eqbound_d=1.5)
TOSTtwo(mYY,mXX,sYY,sXX,nYY,nXX,low_eqbound_d=-0.6402,high_eqbound_d=0.6402,var.equal=FALSE,alpha=0.05)
TOSTtwo(mYY,mXX,sYY,sXX,nYY,nXX,low_eqbound_d=-0.18,high_eqbound_d=0.18,var.equal=FALSE,alpha=0.05)

dataTOSTtwo(filter(dtTemp,GROUP%in%c("HC","AN") & !is.nan(IAcc)&!is.na(IAcc)),
            deps="IAcc",group="GROUP",plots=TRUE)
# this is a 3rd test but it is currently not running 

# use this!
lm0<-lmer(100*IAcc~GROUP + (1|ID),data=filter(dtTemp_Long,GROUP!="AN_BP") )
tab_model(lm0)

lm0<-lmer(100*IAcc~GROUP + (1|ID) + (1|STUDY),data=filter(dtTemp_Long,GROUP!="AN_BP") )
tab_model(lm0)
# need to keep ID as random effect bc repeated measures 
# trialsec no variance when added as a random effect, but when removed the effect is lost

# then analyses per group to see if BMI has an effect - one of the criticisms
# but in our groups, BMI does not affect performance 

lm0<-lm(IAcc ~ BMI,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

lm0<-lm(IAcc ~ BMI*GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

# BMI is used to separate ppts into groups so also ran analyses per group
# only keep these - the other two don't make sense
lm0<-lm(IAcc ~ BMI,data=filter(dtTemp,GROUP=="HC"))
summary(lm0)

lm0<-lm(IAcc ~ BMI,data=filter(dtTemp,GROUP=="AN"))
summary(lm0)

lm0<-lm(IAcc ~ BMI,data=filter(dtTemp,GROUP=="AN_WR"))
summary(lm0)

# then we control for time error 

# Time Error
lm0<-lm(Time_Accuracy~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# sig in AN

# time error effects on IAcc, across all groups
lm0<-lm(IAcc~GROUP*Time_Accuracy,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# n.s. effect 

# then we check for HB self and other awareness 

# do separately or do PCA 
# for the self we can find an accuracy measure bc we have their actual HB
# for other we can use general guidelines (70-100) or sample average
# sample average might be ideal (ACT_25, _30, _45, _65), then scale to 60 w the rule of three
# scale everything to 60s and then compare to self and other HB 

# scaled for self
lm0<-lm(Self_HB_Accuracy~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# sn.s. effect. Self hb accuracy = score of scaled actual HB and self-beliefs 

lm0<-lm(Other_HB_Accuracy~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# n.s. effects here 

lm0<-lm(IAcc~GROUP*Self_HB_Accuracy,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# sig. effect of accuracy 

# need to check HRV effects

# do not use this
#lm0<-lmer(100*IAcc~GROUP+ TrialSecs + (1|ID),data=filter(dtTemp_Long,GROUP!="AN_BP") )
#tab_model(lm0)

# lm0<-lm(IAcc ~ AVG_HB_OTHER*GROUP + AVG_HB_SELF*GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
# summary(lm0)
# n.s. effects, but also a useless analysis

###### Baseline belief comparisons w traits #### 
# prior prospective beliefs w traits of intero sensibility, and IUS
# maybe only do between AN and HC and AN_WR and HC, if the diffs are of interest 
# mood qnaires, covariation 
lm0<-lm(PRIOR~GROUP * EDI_ID,data=filter(dtTemp,GROUP!="AN_BP"))
summary(lm0)
# n.s. effect of intero deficits EDI subscale

lm0<-lm(PRIOR~GROUP * IUS12_TOTAL,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

# as IUS increases, the difference in the priors decreases in AN-WR vs HC
# the interaction is sig. and in the post-hoc was see that priors in AN_WR decrease as IUS increases
# don't elaborate on the interaction finding bc it's weird

# bc interaction was sig
lm0<-lm(PRIOR~IUS12_TOTAL ,data=filter(dtTemp,GROUP %in% c("HC")))
summary(lm0)

lm0<-lm(PRIOR~IUS12_TOTAL ,data=filter(dtTemp,GROUP %in% c("AN_WR")))
summary(lm0)
# as IUS increases, the priors decrease 

# n.s. effect of IUS, sig. in AN-WR x IUS 

# still need to explore insight scores per clinical group 

# posteriors
lm0<-lm(POST_PROSP_AVG~GROUP*EDI_ID,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# no sig. effect of EDI_ID

lm0<-lm(POST_PROSP_AVG~GROUP*IUS12_TOTAL,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# sig. IUS x AN_WR interaction

lm0<-lm(POST_PROSP_AVG~IUS12_TOTAL ,data=filter(dtTemp,GROUP %in% c("HC")))
summary(lm0)

lm0<-lm(POST_PROSP_AVG~IUS12_TOTAL ,data=filter(dtTemp,GROUP %in% c("AN_WR")))
summary(lm0)
# same as in priors

# need to add insight scores per group to explore potential effects

###### Baseline conf comparisons w traits ####
# now we repeat the same for intero sensibility and IUS

# prior prosp conf and traits
lm0<-lm(PRIOR_CONF~GROUP*EDI_ID,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# no sig. intero effect 

lm0<-lm(PRIOR_CONF~GROUP*IUS12_TOTAL,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# n.s. IUS effect 

# est hb conf, and traits
lm0<-lm(EST_CONF_AVG~GROUP*EDI_ID,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# sig EDI_ID and AN-WR interaction

# post hocs
lm0<-lm(EST_CONF_AVG~EDI_ID,data=filter(dtTemp,GROUP%in% c("HC")) )
summary(lm0)

lm0<-lm(EST_CONF_AVG~EDI_ID,data=filter(dtTemp,GROUP%in% c("AN_WR")) )
summary(lm0)
# sig. here.as EDI increases, conf decreases  

lm0<-lm(EST_CONF_AVG~GROUP*IUS12_TOTAL,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# n.s. IUS effect 

# retro conf and traits
lm0<-lm(RETRO_CONF~GROUP*EDI_ID,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# no sig. intero effect

lm0<-lm(RETRO_CONF~GROUP*IUS12_TOTAL,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# n.s. IUS effect 

# avg conf and traits 
lm0<-lm(POST_PROSP_AVG_CONF~GROUP*EDI_ID,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# n.s.

lm0<-lm(POST_PROSP_AVG_CONF~GROUP*IUS12_TOTAL,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
# sig. AN-WR and IUS interaction

lm0<-lm(POST_PROSP_AVG_CONF~IUS12_TOTAL,data=filter(dtTemp,GROUP%in% c("HC")) )
summary(lm0)
# trend to sig

lm0<-lm(POST_PROSP_AVG_CONF~IUS12_TOTAL,data=filter(dtTemp,GROUP%in% c("AN_WR")) )
summary(lm0)
# trend to sig
# opposite directions; w post difference but then bc it's neg, not large enough to be sig.
# but diff of IUS between groups is sig. 
##### BU  ####
summary(dtRaw$EDI_ID);summary(dtRaw$EDI_ID_SCALED)
dtRaw$BU<-dtRaw$POST_PROSP_AVG-dtRaw$PRIOR
dtRaw$Prec_Evidence_1<-dtRaw$EST_CONF_AVG
dtRaw$Prec_Evidence_2<-dtRaw$EDI_ID_SCALED
dtRaw$Prec_Prior<-dtRaw$PRIOR_CONF
dtRaw$LR_1<-dtRaw$Prec_Evidence_1/(dtRaw$Prec_Evidence_1+dtRaw$Prec_Prior)
dtRaw$LR_2<-dtRaw$Prec_Evidence_2/(dtRaw$Prec_Evidence_2+dtRaw$Prec_Prior)
dtRaw$POST_PROSP_pred_retro_1<-dtRaw$PRIOR+dtRaw$LR_1*(dtRaw$RETRO-dtRaw$PRIOR)
dtRaw$POST_PROSP_pred_iacc_1<-dtRaw$PRIOR+dtRaw$LR_1*(dtRaw$IAcc-dtRaw$PRIOR)
dtRaw$POST_PROSP_pred_retro_2<-dtRaw$PRIOR+dtRaw$LR_2*(dtRaw$RETRO-dtRaw$PRIOR)
dtRaw$POST_PROSP_pred_iacc_2<-dtRaw$PRIOR+dtRaw$LR_2*(dtRaw$IAcc-dtRaw$PRIOR)

cor.test(dtRaw$POST_PROSP_AVG,dtRaw$IAcc)
cor.test(dtRaw$POST_PROSP_AVG,dtRaw$POST_PROSP_pred_retro_1)
cor.test(dtRaw$POST_PROSP_AVG,dtRaw$POST_PROSP_pred_iacc_1)
cor.test(dtRaw$POST_PROSP_pred_retro_1,dtRaw$POST_PROSP_pred_iacc_1)
cor.test(dtRaw$POST_PROSP_pred_retro_1,dtRaw$POST_PROSP_pred_retro_2)
cor.test(dtRaw$POST_PROSP_AVG,dtRaw$RETRO)
cor.test(dtRaw$LR_1,dtRaw$LR_2)
# no stat differences between the global predicted vs retro, global pred vs actual post, retro vs actual post
t.test(dtRaw$RETRO,dtRaw$POST_PROSP_pred_retro_1,paired = T)
t.test(dtRaw$POST_PROSP_AVG,dtRaw$POST_PROSP_pred_iacc_1,paired = T)
t.test(dtRaw$POST_PROSP_pred_retro_1,dtRaw$POST_PROSP_pred_iacc_1,paired = T)
t.test(dtRaw$POST_PROSP_AVG,dtRaw$RETRO,paired = T)
hist(dtRaw$LR_1,breaks=100)
hist(dtRaw$LR_2,breaks=100)
# "actual" learning rates of groups
lm0<-lm(BU~GROUP*PRIOR+IAcc-GROUP,data=dtRaw)
summary(lm0)
# AN have a smaller LR than HCs, if using actual IAcc as evidence
lm0<-lm(BU~GROUP*PRIOR+RETRO-GROUP,data=dtRaw)
summary(lm0)
# here, when retro as evidece, then they have a greater LR. trend to sig. 

# MODEL COMPARISON
GetResultsTable(filter(dtRaw,!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )
# model comparison for all 
GetResultsTable(filter(dtRaw,GROUP=="HC"&!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )
# model comparison for HC 
GetResultsTable(filter(dtRaw,GROUP=="AN"&!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )
# model comparison for AN
GetResultsTable(filter(dtRaw,GROUP=="AN_WR"&!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )
# model comparison for AN_WR
GetResultsTable(filter(dtRaw) )
# same as first line

#Learning Rates between group comparison
lm0<-lm(LR_1~GROUP,data=dtRaw)
summary(lm0)
lm0<-lm(LR_2~GROUP,data=dtRaw)
summary(lm0)

#SUMMARY BU
as.data.frame(dtRaw%>%group_by(GROUP)%>%summarise(N=n(), LR_1=round(mean(LR_1,na.rm=T),2), 
                                                  LR_2=round(mean(LR_2,na.rm=T),2)))
rcorr(as.matrix(dplyr::select(dtRaw,POST_PROSP_AVG,POST_PROSP_pred_retro_1,POST_PROSP_pred_iacc_1,
                              POST_PROSP_pred_retro_2,POST_PROSP_pred_iacc_2)))
rcorr(as.matrix(dplyr::select(filter(dtRaw,GROUP=="HC"),POST_PROSP_AVG,POST_PROSP_pred_retro_1,POST_PROSP_pred_iacc_1,
                              POST_PROSP_pred_retro_2,POST_PROSP_pred_iacc_2)))
rcorr(as.matrix(dplyr::select(filter(dtRaw,GROUP=="AN"),POST_PROSP_AVG,POST_PROSP_pred_retro_1,POST_PROSP_pred_iacc_1,
                              POST_PROSP_pred_retro_2,POST_PROSP_pred_iacc_2)))

##### INSIGHT ####
# insight output is odd so need to spend ~1 day cleaning
#dtRaw$INSIGHT_PT_GA_SCALE<-mean(dtRaw$INSIGHT_1A_SEVERITY_AN_PATIENT_GA + dtRaw$INSIGHT_2A_APPROACH_AN_PATIENT_GA + dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_PATIENT_GA + dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_PATIENT_GA)
#dtRaw$INSIGHT_PT_FUTURE <- mean(dtRaw$INSIGHT_10A_IMPROVE_CONDITION_AN_PATIENT_FP + dtRaw$INSIGHT_12A_FOOD_APPROACH_AN_PATIENT_FP)
#dtRaw$INSIGHT_CL_GA_SCALE<-mean(dtRaw$INSIGHT_1A_SEVERITY_AN_CLINICIAN_GA + dtRaw$INSIGHT_2A_APPROACH_AN_CLINICIAN_GA + dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_CLINICIAN_GA + dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_CLINICIAN_GA)
#dtRaw$INSIGHT_PT_FUTURE <- mean(dtRaw$INSIGHT_10A_IMPROVE_CONDITION_AN_CLINICIAN_FP + dtRaw$INSIGHT_12A_FOOD_APPROACH_AN_CLINICIAN_FP)


##### GRAPHS ####