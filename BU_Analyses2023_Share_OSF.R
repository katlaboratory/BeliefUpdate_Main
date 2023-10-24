rm(list=ls())
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

GetFactorName<-function (factorAnalysis, variableName)
{
  return(names(which.max(abs(factorAnalysis$loadings[variableName,]) )))
}

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
dtStudy_WATER <- read_csv("Data/AN_HCLDN1_BU_ANALYSES_ALL.csv")
dtStudy_GR <- read_csv("Data/AN_GREEKS_BU_ANALYSES.csv")
dtStudy_HC <- read_csv("Data/AN_HCLDN1_BU_ANALYSES_ALL.csv")

unique(colnames(dtStudy_ITA))
unique(colnames(dtStudy_WATER))

# binding this way bc they do not have all the same column names; some have diff qnaires etc. 

dtStudy_ITA[setdiff(names(dtStudy_WATER), names(dtStudy_ITA))] <- NA
dtStudy_WATER[setdiff(names(dtStudy_ITA), names(dtStudy_WATER))] <- NA

dtMerged<-rbind(dtStudy_ITA,dtStudy_WATER)
dtMerged[setdiff(names(dtStudy_GR), names(dtMerged))] <- NA
dtStudy_GR[setdiff(names(dtMerged), names(dtStudy_GR))] <- NA

dtMerged<-rbind(dtMerged,dtStudy_GR)
dtMerged[setdiff(names(dtStudy_HC), names(dtMerged))] <- NA
dtStudy_HC[setdiff(names(dtMerged), names(dtStudy_HC))] <- NA
dtMerged<-rbind(dtMerged,dtStudy_HC)
colnames(dtMerged)
length(unique(colnames(dtMerged)))

dtRaw <- dtMerged 
summary(dtRaw$GROUP)

# excl. the AN_BP
dtRaw<-filter(dtRaw,GROUP %in% c("HC","AN", "AN_WR"))

# relevelling so we have HC as intercept 
dtRaw$GROUP<-as.factor(dtRaw$GROUP)
dtRaw$GROUP<-relevel(dtRaw$GROUP,"HC")

###### Variable Calculations #### 

# IAcc
dtRaw$IAcc_25<-1-abs((dtRaw$ACT_25-dtRaw$EST_25)/dtRaw$ACT_25)
dtRaw$IAcc_30<-1-abs((dtRaw$ACT_30-dtRaw$EST_30)/dtRaw$ACT_30)
dtRaw$IAcc_45<-1-abs((dtRaw$ACT_45-dtRaw$EST_45)/dtRaw$ACT_45)
dtRaw$IAcc_65<-1-abs((dtRaw$ACT_65-dtRaw$EST_65)/dtRaw$ACT_65)

# hr at rest
dtRaw$HRrest <- (dtRaw$ACT_25 + dtRaw$ACT_30 + dtRaw$ACT_45 + dtRaw$ACT_65)/2.75
hist(dtRaw$HRrest)

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

# Self and Other HB accuracy 
# Estimating a self accuracy score based on self hb knowledge and HB 
dtRaw$Self_HB_Accuracy <- abs((dtRaw$ACT_Scaled_60-dtRaw$AVG_HB_SELF)/dtRaw$ACT_Scaled_60)

# Estimating an other accuracy score based on other hb knowledge and HB
dtRaw$Other_HB_Accuracy <- abs((80-dtRaw$AVG_HB_OTHER)/80)
# mean of general population, which is 80 

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

# WSCT SCALED 
dtRaw$WCST_SCALED <- (dtRaw$WCST * 100)/64 
hist(dtRaw$WCST_SCALED)

# WSCT PERSEV SCALED 
dtRaw$WCST_P_SCALED <- (dtRaw$WCST_PRES * 100)/64 
hist(dtRaw$WCST_P_SCALED) 

# WSCT PERSEV ERROR SCALED 
dtRaw$WCST_PE_SCALED <- (dtRaw$WCST_PRES_ERR * 100)/64 
hist(dtRaw$WCST_PE_SCALED) 

# Performance Error Percentage 
dtRaw$PERF_ERROR_PCT <- abs(100*((dtRaw$RETRO - dtRaw$IAcc)/dtRaw$IAcc))
hist(dtRaw$PERF_ERROR_PCT, breaks=100)
hist(log(dtRaw$PERF_ERROR_PCT), breaks=100)

dtRaw$PERF_ERROR_PCT_LOG <- log(dtRaw$PERF_ERROR_PCT)

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

# ITPE 
# we do separate z-scores for ITPE
# first we get a z-score for EDI_ID
# in EDI_ID, the higher the score, the worse the deficit
dtRaw$EDI_ID
dtRaw$EDI_ID_SCALED <- (100-(((dtRaw$EDI_ID)/36)*100))
SD_EDI_ID <- sd(dtRaw$EDI_ID_SCALED, na.rm = T)
mean_EDI_ID <- mean(dtRaw$EDI_ID_SCALED, na.rm = T)
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

# retro diff w IAcc
dtRaw$RETRO_PE <- dtRaw$RETRO - dtRaw$IAcc
dtRaw$PEretro <- dtRaw$RETRO - dtRaw$PRIOR
dtRaw$LRretro<-dtRaw$BU/dtRaw$PEretro

dtRaw$BU<-dtRaw$POST_PROSP_AVG-dtRaw$PRIOR
dtRaw$Prec_Evidence_1<-dtRaw$EST_CONF_AVG
dtRaw$Prec_Evidence_2<-dtRaw$EDI_ID_SCALED
dtRaw$Prec_Prior<-dtRaw$PRIOR_CONF

# precision rates calculated using proxies for prec
dtRaw$LR_1<-dtRaw$Prec_Evidence_1/(dtRaw$Prec_Evidence_1+dtRaw$Prec_Prior)
dtRaw$LR_2<-dtRaw$Prec_Evidence_2/(dtRaw$Prec_Evidence_2+dtRaw$Prec_Prior)
dtRaw$POST_PROSP_pred_retro_1<-dtRaw$PRIOR+dtRaw$LR_1*(dtRaw$RETRO-dtRaw$PRIOR)
dtRaw$POST_PROSP_pred_iacc_1<-dtRaw$PRIOR+dtRaw$LR_1*(dtRaw$IAcc-dtRaw$PRIOR)
dtRaw$POST_PROSP_pred_retro_2<-dtRaw$PRIOR+dtRaw$LR_2*(dtRaw$RETRO-dtRaw$PRIOR)
dtRaw$POST_PROSP_pred_iacc_2<-dtRaw$PRIOR+dtRaw$LR_2*(dtRaw$IAcc-dtRaw$PRIOR)

# PCA bc qnaires v highly correlated 

dtPCA<-dplyr::select(filter(dtRaw), DASS21_DEPRESSION, DASS21_STRESS, DASS21_ANXIETY, 
                     IUS12_TOTAL, OCIR_TOTAL, TAS20_TOTAL, EDI_ID)
fa1<-fa(dtPCA, nfactors=7, fm="pa" ,rotate="varimax",SMC=FALSE )

dtRaw$DEPRESSION_pca<-fa1$scores[,GetFactorName(fa1, "DASS21_DEPRESSION")]
dtRaw$STRESS_pca<-fa1$scores[,GetFactorName(fa1, "DASS21_STRESS")]
dtRaw$ANXIETY_pca<-fa1$scores[,GetFactorName(fa1, "DASS21_ANXIETY")]
dtRaw$IUS_pca<-fa1$scores[,GetFactorName(fa1, "IUS12_TOTAL")]
dtRaw$OCI_pca<-fa1$scores[,GetFactorName(fa1, "OCIR_TOTAL")]
dtRaw$TAS_pca<-fa1$scores[,GetFactorName(fa1, "TAS20_TOTAL")]
dtRaw$EDI_ID_pca <- fa1$scores[,GetFactorName(fa1, "EDI_ID")]

dtPCA<-dplyr::select(filter(dtRaw),
                     IUS12_TOTAL, OCIR_TOTAL, TAS20_TOTAL, EDI_ID)
fa1<-fa(dtPCA, nfactors=4, fm="pa" ,rotate="varimax",SMC=FALSE )

dtRaw$IUS_pca2<-fa1$scores[,GetFactorName(fa1, "IUS12_TOTAL")]
dtRaw$OCI_pca2<-fa1$scores[,GetFactorName(fa1, "OCIR_TOTAL")]
dtRaw$TAS_pca2<-fa1$scores[,GetFactorName(fa1, "TAS20_TOTAL")]
dtRaw$EDI_ID_pca2 <- fa1$scores[,GetFactorName(fa1, "EDI_ID")]

###### Summary Stats and Desc Stats ####
summary(dtRaw$GROUP)

as.data.frame(dtRaw %>% group_by(GROUP) %>% dplyr::summarise(BMI=round(mean(BMI,na.rm=T),2),
                                                             AGE=round(mean(AGE,na.rm=T),2) ))

dtPrint1<-as.data.frame(dtRaw %>% group_by(GROUP) %>% dplyr::summarise(N=n(),PRIOR_CONF=paste(round(mean(PRIOR_CONF,na.rm=T),1),"(",
                                                                                              round(sd(PRIOR_CONF,na.rm=T),2),")",sep=""),
                                                                       PRIOR=paste(round(mean(PRIOR,na.rm=T),1),"(",
                                                                                   round(sd(PRIOR,na.rm=T),2),")",sep=""),
                                                                       RETRO_PE=paste(round(mean(RETRO_PE,na.rm=T),1),"(",
                                                                                      round(sd(RETRO_PE,na.rm=T),2),")",sep=""),
                                                                       POST_PROSP_AVG=paste(round(mean(POST_PROSP_AVG,na.rm=T),2),"(",
                                                                                            round(sd(POST_PROSP_AVG,na.rm=T),2),")",sep=""),
                                                                       POST_PROSP_AVG_CONF=paste(round(mean(POST_PROSP_AVG_CONF,na.rm=T),2),"(",
                                                                                                 round(sd(POST_PROSP_AVG_CONF,na.rm=T),2),")",sep=""),
                                                                       IAcc=paste(round(mean(IAcc,na.rm=T),2),"(",
                                                                                  round(sd(IAcc,na.rm=T),2),")",sep=""),
                                                                       EST_CONF_AVG=paste(round(mean(EST_CONF_AVG,na.rm=T),2),"(",
                                                                                          round(sd(EST_CONF_AVG,na.rm=T),2),")",sep=""),
                                                                       RETRO=paste(round(mean(RETRO,na.rm=T),2),"(",
                                                                                   round(sd(RETRO,na.rm=T),2),")",sep=""),
                                                                       PERF_ERROR_PCT_LOG=paste(round(mean(PERF_ERROR_PCT_LOG,na.rm=T),2),"(",
                                                                                                round(sd(PERF_ERROR_PCT_LOG,na.rm=T),2),")",sep=""),
                                                                       Time_Accuracy=paste(round(mean(Time_Accuracy,na.rm=T),2),"(",
                                                                                           round(sd(Time_Accuracy,na.rm=T),2),")",sep=""),
                                                                       BMI=paste(round(mean(BMI,na.rm=T),2),"(",
                                                                                 round(sd(BMI,na.rm=T),2),")",sep=""),
                                                                       AGE=paste(round(mean(AGE,na.rm=T),2),"(",
                                                                                 round(sd(AGE,na.rm=T),2),")",sep=""),
                                                                       EDEQ_RESTRAINT=paste(round(mean(EDEQ_RESTRAINT,na.rm=T),2),"(",
                                                                                            round(sd(EDEQ_RESTRAINT,na.rm=T),2),")",sep=""),
                                                                       EDEQ_EATING_CONCERN=paste(round(mean(EDEQ_EATING_CONCERN,na.rm=T),2),"(",
                                                                                                 round(sd(EDEQ_EATING_CONCERN,na.rm=T),2),")",sep=""),
                                                                       EDEQ_SHAPE_CONCERN=paste(round(mean(EDEQ_SHAPE_CONCERN,na.rm=T),2),"(",
                                                                                                round(sd(EDEQ_SHAPE_CONCERN,na.rm=T),2),")",sep=""),
                                                                       EDEQ_WEIGHT_CONCERN=paste(round(mean(EDEQ_WEIGHT_CONCERN,na.rm=T),2),"(",
                                                                                                 round(sd(EDEQ_WEIGHT_CONCERN,na.rm=T),2),")",sep=""),
                                                                       EDEQ_OVERALL=paste(round(mean(EDEQ_OVERALL,na.rm=T),2),"(",
                                                                                          round(sd(EDEQ_OVERALL,na.rm=T),2),")",sep=""),
                                                                       EDI_ID=paste(round(mean(EDI_ID,na.rm=T),2),"(",
                                                                                    round(sd(EDI_ID,na.rm=T),2),")",sep=""),
                                                                       OCIR_TOTAL=paste(round(mean(OCIR_TOTAL,na.rm=T),2),"(",
                                                                                        round(sd(OCIR_TOTAL,na.rm=T),2),")",sep=""),
                                                                       IUS12_PROSPECTIVE=paste(round(mean(IUS12_PROSPECTIVE,na.rm=T),2),"(",
                                                                                               round(sd(IUS12_PROSPECTIVE,na.rm=T),2),")",sep=""),
                                                                       IUS12_INHIBITORY=paste(round(mean(IUS12_INHIBITORY,na.rm=T),2),"(",
                                                                                              round(sd(IUS12_INHIBITORY,na.rm=T),2),")",sep=""),
                                                                       IUS12_TOTAL=paste(round(mean(IUS12_TOTAL,na.rm=T),2),"(",
                                                                                         round(sd(IUS12_TOTAL,na.rm=T),2),")",sep=""),
                                                                       TAS20_DESCRIBE=paste(round(mean(TAS20_DESCRIBE,na.rm=T),2),"(",
                                                                                            round(sd(TAS20_DESCRIBE,na.rm=T),2),")",sep=""),
                                                                       TAS20_IDENTIFY=paste(round(mean(TAS20_IDENTIFY,na.rm=T),2),"(",
                                                                                            round(sd(TAS20_IDENTIFY,na.rm=T),2),")",sep=""),
                                                                       TAS20_EXTERNAL=paste(round(mean(TAS20_EXTERNAL,na.rm=T),2),"(",
                                                                                            round(sd(TAS20_EXTERNAL,na.rm=T),2),")",sep=""),
                                                                       TAS20_TOTAL=paste(round(mean(TAS20_TOTAL,na.rm=T),2),"(",
                                                                                         round(sd(TAS20_TOTAL,na.rm=T),2),")",sep=""),
                                                                       DASS21_DEPRESSION=paste(round(mean(DASS21_DEPRESSION,na.rm=T),2),"(",
                                                                                               round(sd(DASS21_DEPRESSION,na.rm=T),2),")",sep=""),
                                                                       DASS21_ANXIETY=paste(round(mean(DASS21_ANXIETY,na.rm=T),2),"(",
                                                                                            round(sd(DASS21_ANXIETY,na.rm=T),2),")",sep=""),
                                                                       DASS21_STRESS=paste(round(mean(DASS21_STRESS,na.rm=T),2),"(",
                                                                                           round(sd(DASS21_STRESS,na.rm=T),2),")",sep=""),
                                                                       WCST_SCALED=paste(round(mean(WCST_SCALED,na.rm=T),2),"(",
                                                                                         round(sd(WCST_SCALED,na.rm=T),2),")",sep=""),
                                                                       WCST_P_SCALED=paste(round(mean(WCST_P_SCALED,na.rm=T),2),"(",
                                                                                           round(sd(WCST_P_SCALED,na.rm=T),2),")",sep=""),
                                                                       AVG_HB_SELF=paste(round(mean(AVG_HB_SELF,na.rm=T),2),"(",
                                                                                         round(sd(AVG_HB_SELF,na.rm=T),2),")",sep=""),
                                                                       AVG_HB_OTHER=paste(round(mean(AVG_HB_OTHER,na.rm=T),2),"(",
                                                                                          round(sd(AVG_HB_OTHER,na.rm=T),2),")",sep=""),
                                                                       ITPEz=paste(round(mean(ITPE_Other_z,na.rm=T),2),"(",
                                                                                   round(sd(ITPE_Other_z,na.rm=T),2),")",sep=""),
                                                                       HRrest=paste(round(mean(HRrest,na.rm=T),2),"(",
                                                                                    round(sd(HRrest,na.rm=T),2),")",sep=""),
                                                                       
                                                                       
                                                                       
                                                                       
                                                                       
))

write_csv(dtPrint1,"Output/Descriptives.csv")

###### Initial control analyses  #### 
dtTemp<-filter(dtRaw,GROUP %in% c("HC","AN", "AN_WR"))
# Age differences 
lm0 <- lm(AGE ~ GROUP, data=dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# BMI differences 
lm0 <- lm(BMI ~ GROUP, data=dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# HB at rest
lm0 <- lm(HRrest ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# HB Self
lm0 <- lm(AVG_HB_SELF ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# HB Other
lm0 <- lm(AVG_HB_OTHER ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# WCST % Correct 
lm0 <- lm(WCST_SCALED ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# WCST % Preservative
lm0 <- lm(WCST_P_SCALED ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# EDEQ
lm0 <- lm(EDEQ_OVERALL ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# EDI-ID
lm0 <- lm(EDI_ID ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# IUS
lm0 <- lm(IUS12_TOTAL ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# OCIR
lm0 <- lm(OCIR_TOTAL ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# DASS Depression 
lm0 <- lm(DASS21_DEPRESSION ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# DASS Anxiety 
lm0 <- lm(DASS21_ANXIETY ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# DASS Stress
lm0 <- lm(DASS21_STRESS ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# TAS 20 
lm0 <- lm(TAS20_TOTAL ~ GROUP, dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

###### Baseline belief comparisons #### 
# Main Analyses
# prior prospective beliefs comparisons 
lmer0<-lmer(PRIOR~GROUP + AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=2)

# regressions for retro 
lm0<-lm(PERF_ERROR_PCT_LOG~GROUP + AGE,data=filter(dtTemp,GROUP!="AN_BP" & PERF_ERROR_PCT_LOG != Inf))
summary(lm0)
tab_model(lm0, show.se=2)

lm0<-lm(RETRO - IAcc ~ GROUP + AGE,data=filter(dtTemp,GROUP!="AN_BP"))
summary(lm0)
tab_model(lm0, show.se=T)

# then we look at the perf error pct  
lmer0<-lmer(PERF_ERROR_PCT_LOG~GROUP + AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP" & PERF_ERROR_PCT_LOG != Inf))
summary(lmer0)
tab_model(lmer0, show.se=2)
# log bc not nornally distributed. no diffs between groups

lmer0<-lmer(RETRO - IAcc ~ GROUP + AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP"))
summary(lmer0)
tab_model(lmer0, show.se=T)
summary (dtTemp$RETRO - dtTemp$IAcc)
# hc overestimate; clinical groups underestimate 

# we did the diff bc better distribution w kurtosis
hist(dtTemp$RETRO - dtTemp$IAcc)
hist(dtTemp$PERF_ERROR_PCT)
hist(dtTemp$PERF_ERROR_PCT_LOG)

# posterior prospective avg (i.e., double and half length tgt)
lmer0<-lmer(POST_PROSP_AVG~GROUP+ AGE+ (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T)
# AN pessimistic

###### ITPE Analyses ####
# correlation between IAcc and perf conf 
cor.test (dtRaw$IAcc, dtRaw$EST_CONF_AVG)

# after we do correlations per person, we compare between groups. We see AN have a greater correlation but n.s.
# probs bc HCs overestimated their IAcc and had greater conf. AN smaller diff between beliefs and IAcc.

cor.test (filter(dtRaw, GROUP =="HC")$IAcc, filter(dtRaw, GROUP =="HC")$EST_CONF_AVG)
cor.test (filter(dtRaw, GROUP =="AN")$IAcc, filter(dtRaw, GROUP =="AN")$EST_CONF_AVG)
cor.test (filter(dtRaw, GROUP =="AN_WR")$IAcc, filter(dtRaw, GROUP =="AN_WR")$EST_CONF_AVG)

# EDI_correlations
cor.test (dtRaw$IAcc, dtRaw$EDI_ID_SCALED)
cor.test (filter(dtRaw, GROUP =="HC")$IAcc, filter(dtRaw, GROUP =="HC")$EDI_ID_SCALED)
cor.test (filter(dtRaw, GROUP =="AN")$IAcc, filter(dtRaw, GROUP =="AN")$EDI_ID_SCALED)
cor.test (filter(dtRaw, GROUP =="AN_WR")$IAcc, filter(dtRaw, GROUP =="AN_WR")$EDI_ID_SCALED)

lm0<-lm(ITPE_Other_z~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)
tab_model(lm0)

# exploratory
lm0<-lm(abs(ITPE_Other_z)~GROUP,data=dtTemp)
summary(lm0)
summary(filter(dtTemp,GROUP=="HC")$ITPE_Other_z)

# main effects per group
lm21212<-lm(ITPE_Other_z~1,data=filter(dtTemp,GROUP=="HC") )
summary(lm21212)

lm21213<-lm(ITPE_Other_z~1,data=filter(dtTemp,GROUP=="AN") )
summary(lm21213)

lm21214<-lm(ITPE_Other_z~1,data=filter(dtTemp,GROUP=="AN_WR") )
summary(lm21214)

# here just conf correlations
lm0<-lm(ACC_CONF_Corr~GROUP,data=filter(dtTemp,GROUP!="AN_BP") )
summary(lm0)

# correlation between IAcc and EDI intero subscale 
cor.test (dtRaw$IAcc, dtRaw$EDI_ID)
cor.test (filter(dtRaw, GROUP =="HC")$EDI_ID_SCALED, filter(dtRaw, GROUP =="HC")$IAcc)
cor.test (filter(dtRaw, GROUP =="AN")$EDI_ID_SCALED, filter(dtRaw, GROUP =="AN")$IAcc)
cor.test (filter(dtRaw, GROUP =="AN_WR")$EDI_ID_SCALED, filter(dtRaw, GROUP =="AN_WR")$IAcc)
cor.test (dtRaw$EST_CONF_AVG, dtRaw$EDI_ID_SCALED)

cor.test (filter(dtRaw, GROUP =="HC")$EDI_ID_SCALED, filter(dtRaw, GROUP =="HC")$EST_CONF_AVG)
cor.test (filter(dtRaw, GROUP =="AN")$EDI_ID_SCALED, filter(dtRaw, GROUP =="AN")$EST_CONF_AVG)
cor.test (filter(dtRaw, GROUP =="AN_WR")$EDI_ID_SCALED, filter(dtRaw, GROUP =="AN_WR")$EST_CONF_AVG)

cor.test (filter(dtRaw, GROUP =="HC")$EDI_ID_SCALED, filter(dtRaw, GROUP =="HC")$PRIOR)
cor.test (filter(dtRaw, GROUP =="AN")$EDI_ID_SCALED, filter(dtRaw, GROUP =="AN")$PRIOR)
cor.test (filter(dtRaw, GROUP =="AN_WR")$EDI_ID_SCALED, filter(dtRaw, GROUP =="AN_WR")$PRIOR)

# EDI diffs per group 
lmer0<-lmer(EDI_ID_SCALED~GROUP + AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T)

Descriptive_stats <- dtTemp %>% # DESCRIPTIVES MEAN AND SD FOR CONDITIONS
  group_by(GROUP) %>%
  summarise(ITPE_Other_z_mean = mean(ITPE_Other_z, na.rm = TRUE),
            ITPE_Other_z_SD = sd(ITPE_Other_z, na.rm = TRUE)
  )

# prediction error between groups
lmer0<-lmer(IAcc-PRIOR~GROUP + AGE + (1|STUDY),data=dtTemp)
summary(lmer0)
tab_model(lmer0, show.se=T)

lm0<-lm(POST_PROSP_AVG-IAcc~GROUP,data=dtTemp)
summary(lm0)
tab_model(lm0)

###### Additional analyses for secondary bhvral results ####
# check if scores are predictive irrespective of group 
hist(dtTemp$RETRO - dtTemp$IAcc, breaks = 100)
# HC did worse than they think, but in AN the diff between reality and belief is smaller
# same in AN_WR - compared to HCs. smaller to the point of being neg

###### Initial, baseline confidence comparisons ####
# prior prosp conf
lmer0<-lmer(PRIOR_CONF~GROUP + AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T)

# est hb conf, i.e., how conf are you w the number of beats you felt, retrospective conf on performance
lmer0<-lmer(EST_CONF_AVG~GROUP + AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T)
# AN have lower conf 

# prospective avg conf
lmer0<-lmer(POST_PROSP_AVG_CONF~GROUP + AGE + (1|STUDY),data=filter(dtTemp,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T)
# AN lower

###### IAcc comparisons ####
dtTemp<-dtRaw
dtTemp25<-dplyr::select(dtTemp, ID,STUDY,IAcc=IAcc_25,Time_Accuracy=TIME_ACCURACY_25,GROUP,AGE);dtTemp25$TrialSecs<-"25"
dtTemp30<-dplyr::select(dtTemp, ID,STUDY,IAcc=IAcc_30,Time_Accuracy=TIME_ACCURACY_30,GROUP,AGE);dtTemp30$TrialSecs<-"30"
dtTemp45<-dplyr::select(dtTemp, ID,STUDY,IAcc=IAcc_45,Time_Accuracy=TIME_ACCURACY_45,GROUP,AGE);dtTemp45$TrialSecs<-"45"
dtTemp65<-dplyr::select(dtTemp, ID,STUDY,IAcc=IAcc_65,Time_Accuracy=TIME_ACCURACY_65,GROUP,AGE);dtTemp65$TrialSecs<-"65"
dtTemp_Long<-rbind(dtTemp25,dtTemp30);dtTemp_Long<-rbind(dtTemp_Long,dtTemp45);
dtTemp_Long<-rbind(dtTemp_Long,dtTemp65);

library(bayestestR)
equivalence_test(lm0)

library(baymedr)
equiv_bf(x=filter(dtTemp,GROUP=="HC" & !is.nan(IAcc)&!is.na(IAcc))$IAcc,
         y=filter(dtTemp,GROUP=="AN"& !is.nan(IAcc)&!is.na(IAcc))$IAcc,
         interval=c(-5,5),interval_std=FALSE)
# here, 3.6 bayes factor suggests there's moderate evidence but not sig.

# in the main analyses we control for AGE, then below we run an lm w all other factors
lmer0<-lmer(100*IAcc~GROUP + AGE + (1|ID) + (1|STUDY),data=filter(dtTemp_Long,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T)

# then analyses per group to see if BMI has an effect - one of the criticisms
# BMI is used to separate ppts into groups so also ran analyses only per group

lm0<-lm(IAcc ~ BMI,data=filter(dtTemp,GROUP=="HC"))
summary(lm0)
tab_model(lm0)

lm0<-lm(IAcc ~ BMI,data=filter(dtTemp,GROUP=="AN"))
summary(lm0)
tab_model(lm0)

lm0<-lm(IAcc ~ BMI,data=filter(dtTemp,GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0)
# none are sig

# control for all the control factors here (time, self, and other hb)
lm0<-lm(IAcc~GROUP + Time_Accuracy + Other_HB_Accuracy + Self_HB_Accuracy,data=dtTemp)
summary(lm0)
tab_model(lm0, show.se=2)

# diffs per trial duration 
IACC_25 <- lm(IAcc_25 ~ GROUP, data=dtTemp)
summary(IACC_25)
tab_model(IACC_25, show.se=2)

IACC_30 <- lm(IAcc_30 ~ GROUP, data=dtTemp)
summary(IACC_30)
tab_model(IACC_30, show.se=2)

IACC_45 <- lm(IAcc_45 ~ GROUP, data=dtTemp)
summary(IACC_45)
tab_model(IACC_45, show.se=2)

IACC_65 <- lm(IAcc_65 ~ GROUP, data=dtTemp)
summary(IACC_65)
tab_model(IACC_65, show.se=2, digits=2)

###### MAIN BELIEFS AND TRAITS ####
filter(dtRaw,GROUP!="AN_BP")%>% group_by(GROUP)%>%summarise(DEPRESSION=paste(round(mean(DASS21_DEPRESSION,na.rm=T),2),"(",round(sd(DASS21_DEPRESSION,na.rm=T),2),")",sep=""),
                                                            ANXIETY=paste(round(mean(DASS21_ANXIETY,na.rm=T),2),"(",round(sd(DASS21_ANXIETY,na.rm=T),2),")",sep=""),
                                                            STRESS=paste(round(mean(DASS21_STRESS,na.rm=T),2),"(",round(sd(DASS21_STRESS,na.rm=T),2),")",sep=""),
                                                            IUS12=paste(round(mean(IUS12_TOTAL,na.rm=T),2),"(",round(sd(IUS12_TOTAL,na.rm=T),2),")",sep=""),
                                                            OCIR=paste(round(mean(OCIR_TOTAL,na.rm=T),2),"(",round(sd(OCIR_TOTAL,na.rm=T),2),")",sep=""),
                                                            TAS20=paste(round(mean(TAS20_TOTAL,na.rm=T),2),"(",round(sd(TAS20_TOTAL,na.rm=T),2),")",sep=""),
                                                            POST_PROSP_AVG=mean(POST_PROSP_AVG,na.rm=T))

dtCorrs<-dplyr::select(filter(dtRaw,GROUP!="AN_BP"), DASS21_DEPRESSION, DASS21_STRESS, DASS21_ANXIETY, 
                       IUS12_TOTAL, OCIR_TOTAL, TAS20_TOTAL, POST_PROSP_AVG)

corTest<-rcorr(as.matrix(dtCorrs))

# now we look at how traits explain behaviour
# we did 2 pca's bc not all ppts had done DASS, so these were not incl. in the first one 

dtPlot<-dtRaw
dtPlot$DepressionLevel<-NA_character_
dtPlot[!is.na(dtPlot$DASS21_DEPRESSION) & dtPlot$DASS21_DEPRESSION<=20,]$DepressionLevel<-"Normal - Moderate"
dtPlot[!is.na(dtPlot$DASS21_DEPRESSION) & dtPlot$DASS21_DEPRESSION>=21,]$DepressionLevel<-"Severe - Extremely Severe"
dtPlot$DepressionLevel<-as.factor(dtPlot$DepressionLevel)

# figures
ggplot(dtRaw, aes(x=DEPRESSION_pca,y=POST_PROSP_AVG))+
  geom_smooth(method="lm",span=0.6)+
  ylab('Posterior Prospective Self-Efficacy Belief')+
  xlab('Depression (Principal Component) Score')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 14))+
  ggsave('Graphs/PostDepr.png', width = 8, height = 6, dpi=1000)

ggplot(dtRaw, aes(x=STRESS_pca,y=POST_PROSP_AVG))+
  geom_smooth(method="loess",span=0.6)+
  ylab('Posterior Prospective Self-Efficacy Belief')+
  xlab('Stress (Principal Component) Score')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 14))+
  ggsave('Graphs/PostStress.png', width = 8, height = 6, dpi=1000)

#  in post prosp 
lmer0<-lmer(POST_PROSP_AVG ~ DEPRESSION_pca + STRESS_pca + ANXIETY_pca + IUS_pca + OCI_pca + TAS_pca + EDI_ID_pca + (1|STUDY), data =dtRaw)
summary(lmer0)
tab_model(lmer0, show.se=2)

lmer0<-lmer(POST_PROSP_AVG ~ IUS_pca2 + OCI_pca2 + TAS_pca2 + EDI_ID_pca2 + (1|STUDY), data=dtRaw)
summary(lmer0)
tab_model(lmer0, show.se=2)

# do depr and stress explain group effects 
lm0<-lm(POST_PROSP_AVG ~  GROUP + DEPRESSION_pca, data =dtRaw)
summary(lm0)
tab_model(lm0, show.se=2)

lm0<-lm(POST_PROSP_AVG ~  GROUP + STRESS_pca, data =dtRaw)
summary(lm0)
tab_model(lm0, show.se=2)

# first we look at depression per group - this for state or trait expl. 
lm0<-lm(POST_PROSP_AVG ~  DEPRESSION_pca, data =filter(dtRaw, GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)

lm0<-lm(POST_PROSP_AVG ~  DEPRESSION_pca, data =filter(dtRaw, GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se=2)

p<-c(0.841, 0.017)
p.adjust(p, method="holm")

# in AN n.s. bc all high in depression. in AN-WR, priors reduce as depr increases
# same in post prosp. and we have group diffs in dassdepr scores

# here we look at stress 
lm0<-lm(POST_PROSP_AVG ~  STRESS_pca, data =filter(dtRaw, GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)

lm0<-lm(POST_PROSP_AVG ~  STRESS_pca, data =filter(dtRaw, GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se=2)

p<-c(0.593, 0.102)
p.adjust(p, method="holm")

###### BU ####
# add retro conf as a proxy for precision, not just EDI_ID and est_conf - we said no to this bc conceptually v similar to retro beliefs

cor.test(dtRaw$POST_PROSP_AVG,dtRaw$IAcc)
cor.test(dtRaw$POST_PROSP_AVG,dtRaw$POST_PROSP_pred_retro_1)
cor.test(dtRaw$POST_PROSP_AVG,dtRaw$POST_PROSP_pred_iacc_1)
cor.test(dtRaw$POST_PROSP_pred_retro_1,dtRaw$POST_PROSP_pred_iacc_1)
cor.test(dtRaw$POST_PROSP_pred_retro_1,dtRaw$POST_PROSP_pred_retro_2)
cor.test(dtRaw$POST_PROSP_AVG,dtRaw$RETRO)
cor.test(dtRaw$LR_1,dtRaw$LR_2)
# this is to test what we have done 

# no stat differences between the global predicted vs retro, global pred vs actual post, retro vs actual post
t.test(dtRaw$RETRO,dtRaw$POST_PROSP_pred_retro_1,paired = T)
t.test(dtRaw$POST_PROSP_AVG,dtRaw$POST_PROSP_pred_iacc_1,paired = T)
t.test(dtRaw$POST_PROSP_pred_retro_1,dtRaw$POST_PROSP_pred_iacc_1,paired = T)
t.test(dtRaw$POST_PROSP_AVG,dtRaw$POST_PROSP_pred_retro_1,paired = T)
hist(dtRaw$LR_1,breaks=100)
hist(dtRaw$LR_2,breaks=100)

# "actual" learning rates of groups
dtTempExpl<-dtRaw

# prediction D
lm1111<-lm(BU~GROUP*PEretro - GROUP,data=dtTempExpl)
summary(lm1111)
tab_model(lm1111, show.se=T)

# for prediction C
lm00<-lm(abs(LRretro-LR_1)~GROUP,data=filter(dtRaw, abs(LRretro-LR_1)<4))
summary(lm00)
hist(dtRaw$LRretro-dtRaw$LR_1, breaks=100)
tab_model(lm00, show.se=T)
# An actual lr more far off than the opt; less bayesian opt but n.s.

lm00<-lm(abs(LRretro-LR_2)~GROUP,data=filter(dtRaw, abs(LRretro-LR_2)<4))
summary(lm00)
tab_model(lm00, show.se=T)
hist(log(abs(dtTempExpl$LRretro-dtTempExpl$LR_2)), breaks=100)

# The HC have higher precision weighted learning rates than the AN (sign. in one choice of evidence prec. proxy and not sign. in the other)
# The winning models have a better fit for the HC comparing to the AN for both choices of evidence prec. proxies,
# which means that the HC are more bayesian optimal in their belief updating.
# In further exploration, which supports the above, we see that the actual LR of the AN differ more comparing to the 
# bayesian optimal ones in the AN vs the HC, but this is not significant (for both choices of evid. prec. proxies).

# MODEL COMPARISON - Prediction A and compare accuracy evidence output vs retro evidence output 4 and 5 and then the two last ones that have EDI
# then for prediction B compare output lines 5 and 7 
# model comparison for all 
dt1 <- GetResultsTable(filter(dtRaw,!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )
# the HCs for our winning model are more bayesian optimal than the AN-WR and the AN-WR are more BO than the AN. Howveer, these diffs are n.s. (see comparison below)
# test w POSTERIOR_PRED_ERR_2 
# model comparison for HC 
dt2 <- GetResultsTable(filter(dtRaw,GROUP=="HC"&!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )
# model comparison for AN
dt3 <- GetResultsTable(filter(dtRaw,GROUP=="AN"&!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )
# model comparison for AN_WR
dt4 <- GetResultsTable(filter(dtRaw,GROUP=="AN_WR"&!is.na(POST_PROSP_pred_iacc_1)&!is.na(POST_PROSP_pred_retro_1)&!is.na(POST_PROSP_pred_iacc_2)&!is.na(POST_PROSP_pred_retro_2)) )

# same as first line
write_csv(dt1, "Output/dtTable1.csv")
write_csv(dt2, "Output/dtTable2.csv")
write_csv(dt3, "Output/dtTable3.csv")
write_csv(dt4, "Output/dtTable4.csv")

# this below is to check the sig of the model comparison fit between groups. results were n.s. but same as MAE which is g
dtRaw$POSTERIOR_PRED_ERR_RETRO_1<-abs(dtRaw$POST_PROSP_AVG-dtRaw$POST_PROSP_pred_retro_1)
dtRaw$POSTERIOR_PRED_ERR_ACC_1<-abs(dtRaw$POST_PROSP_AVG-dtRaw$POST_PROSP_pred_iacc_1)
dtRaw$POSTERIOR_PRED_ERR_RETRO_2<-abs(dtRaw$POST_PROSP_AVG-dtRaw$POST_PROSP_pred_retro_2)
dtRaw$POSTERIOR_PRED_ERR_ACC_2<-abs(dtRaw$POST_PROSP_AVG-dtRaw$POST_PROSP_pred_iacc_2)
# which group had a greater diffs between predicted and actual posterior prospective. 
# add this for prediction A.
# this is bc edi was the better proxy
lm1113<-lm(POSTERIOR_PRED_ERR_RETRO_2~GROUP,data=dtRaw)
summary(lm1113)
tab_model(lm1113, show.se=T)

# this belongs to our prediction A
lm891113<-lm(POSTERIOR_PRED_ERR_RETRO_1~GROUP,data=dtRaw)
summary(lm891113)
tab_model(lm891113, show.se=T)

# here we compare for prediction A - model comparison fit
# first is same precision, diff evidences, perf conf as precision
lm1114<-lm((POSTERIOR_PRED_ERR_ACC_1-POSTERIOR_PRED_ERR_RETRO_1)~1,data=dtRaw)
summary(lm1114)
tab_model(lm1114, show.se=T)
t.test(dtRaw$POSTERIOR_PRED_ERR_ACC_1-dtRaw$POSTERIOR_PRED_ERR_RETRO_1)
lm1115<-lm((POSTERIOR_PRED_ERR_ACC_1-POSTERIOR_PRED_ERR_RETRO_1)~1,data=filter(dtRaw,GROUP=="HC"))
summary(lm1115)
tab_model(lm1115, show.se=T)
# both sig. 

# here we compare the diff precisions, prediction a, extra analysis
lm1116<-lm((POSTERIOR_PRED_ERR_ACC_2-POSTERIOR_PRED_ERR_RETRO_2)~1,data=dtRaw)
summary(lm1116)
tab_model(lm1116, show.se=T)
lm1117<-lm((POSTERIOR_PRED_ERR_ACC_2-POSTERIOR_PRED_ERR_RETRO_2)~1,data=filter(dtRaw,GROUP=="HC"))
summary(lm1117)
tab_model(lm1117, show.se=T)
# both sig. 
# these tests are for prediction A to say the diffs are also stat sig. no need for AN and ANWR bc we have not predicted sth for them

# prediction B
# exploratory; say in pred a retro was better evidence; used it in pred b to see if sig.
# but here it isn't
lm1116<-lm((POSTERIOR_PRED_ERR_RETRO_1-POSTERIOR_PRED_ERR_RETRO_2)~1,data=dtRaw)
summary(lm1116)
tab_model(lm1116, show.se=T)
lm1117<-lm((POSTERIOR_PRED_ERR_RETRO_1-POSTERIOR_PRED_ERR_RETRO_2)~1,data=filter(dtRaw,GROUP=="HC"))
summary(lm1117)
tab_model(lm1117, show.se=T)

#Learning Rates between group comparison - this is prediction C w winning model
# if we assume the bayesian model that we use is the process that is actually taking place
# a higher LR means they will take the PE more into account when they are updating their beliefs
# prediction error w any evidence bc we are computing LR w precisions
# LR 1 is w IAcc conf
lm1118<-lm(LR_1~GROUP,data=dtRaw)
summary(lm1118)
tab_model(lm1118, show.se=T)
# HCs have a greater LR 

# LR 2 is w EDI 
lm1119<-lm(LR_2~GROUP,data=dtRaw)
summary(lm1119)
tab_model(lm1119, show.se=T)

#SUMMARY BU
as.data.frame(dtRaw%>%group_by(GROUP)%>%summarise(N=n(), LR_1=round(mean(LR_1,na.rm=T),2), 
                                                  LR_2=round(mean(LR_2,na.rm=T),2)))
rcorr(as.matrix(dplyr::select(dtRaw,POST_PROSP_AVG,POST_PROSP_pred_retro_1,POST_PROSP_pred_iacc_1,
                              POST_PROSP_pred_retro_2,POST_PROSP_pred_iacc_2)))
rcorr(as.matrix(dplyr::select(filter(dtRaw,GROUP=="HC"),POST_PROSP_AVG,POST_PROSP_pred_retro_1,POST_PROSP_pred_iacc_1,
                              POST_PROSP_pred_retro_2,POST_PROSP_pred_iacc_2)))
rcorr(as.matrix(dplyr::select(filter(dtRaw,GROUP=="AN"),POST_PROSP_AVG,POST_PROSP_pred_retro_1,POST_PROSP_pred_iacc_1,
                              POST_PROSP_pred_retro_2,POST_PROSP_pred_iacc_2)))

###### BU Descriptives #### 
dtPrint2 <- as.data.frame(dtRaw %>% group_by(GROUP) %>% dplyr::summarise(N=n(),Prec_Evidence_1=paste(round(mean(Prec_Evidence_1,na.rm=T),1),"(",
                                                                                                     round(sd(Prec_Evidence_1,na.rm=T),2),")",sep=""),
                                                                         Prec_Evidence_2=paste(round(mean(Prec_Evidence_2,na.rm=T),1),"(",
                                                                                               round(sd(Prec_Evidence_2,na.rm=T),2),")",sep=""),
                                                                         LR_1=paste(round(mean(LR_1,na.rm=T),1),"(",
                                                                                    round(sd(LR_1,na.rm=T),2),")",sep=""),
                                                                         LR_2=paste(round(mean(LR_2,na.rm=T),1),"(",
                                                                                    round(sd(LR_2,na.rm=T),2),")",sep=""),
                                                                         POSTERIOR_PRED_ERR_RETRO_1=paste(round(mean(POSTERIOR_PRED_ERR_RETRO_1,na.rm=T),1),"(",
                                                                                                          round(sd(POSTERIOR_PRED_ERR_RETRO_1,na.rm=T),2),")",sep=""),
                                                                         POSTERIOR_PRED_ERR_ACC_1=paste(round(mean(POSTERIOR_PRED_ERR_ACC_1,na.rm=T),1),"(",
                                                                                                        round(sd(POSTERIOR_PRED_ERR_ACC_1,na.rm=T),2),")",sep=""),
                                                                         POSTERIOR_PRED_ERR_RETRO_2=paste(round(mean(POSTERIOR_PRED_ERR_RETRO_2,na.rm=T),1),"(",
                                                                                                          round(sd(POSTERIOR_PRED_ERR_RETRO_2,na.rm=T),2),")",sep=""),
                                                                         POSTERIOR_PRED_ERR_ACC_2=paste(round(mean(POSTERIOR_PRED_ERR_ACC_2,na.rm=T),1),"(",
                                                                                                        round(sd(POSTERIOR_PRED_ERR_ACC_2,na.rm=T),2),")",sep=""),
)) 

write_csv(dtPrint2,"Output/BU.csv")

###### INSIGHT Calculations ####
# ITEM AND SUBSCALE CALCULATIONS
# ILLNESS SEVERITY 

dtRaw$ILLNESS_SEVERITY_DIFF <- dtRaw$INSIGHT_1A_SEVERITY_AN_PATIENT_GA - dtRaw$INSIGHT_1A_SEVERITY_AN_CLINICIAN_GA

Mean_ILLNESS_SEVERITY <- abs(mean(dtRaw$ILLNESS_SEVERITY_DIFF, na.rm = T))
dtRaw$ILLNESS_SEVERITY <- (dtRaw$ILLNESS_SEVERITY_DIFF)/ Mean_ILLNESS_SEVERITY
hist(dtRaw$ILLNESS_SEVERITY)

# APPROACH TO FOOD 
dtRaw$Mean_FOOD_APPROACH <- abs(mean(dtRaw$INSIGHT_2A_APPROACH_AN_PATIENT_GA - dtRaw$INSIGHT_2A_APPROACH_AN_CLINICIAN_GA, na.rm = T))

dtRaw$FOOD_APPROACH <- (dtRaw$INSIGHT_2A_APPROACH_AN_PATIENT_GA - dtRaw$INSIGHT_2A_APPROACH_AN_CLINICIAN_GA)/ dtRaw$Mean_FOOD_APPROACH
hist(dtRaw$FOOD_APPROACH)

# THIN VS AVG WOMAN 
dtRaw$Mean_THINNESS <- abs(mean(dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_PATIENT_GA - dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_CLINICIAN_GA, na.rm = T))

dtRaw$THINNESS <- (dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_PATIENT_GA - dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_CLINICIAN_GA)/ dtRaw$Mean_THINNESS
hist(dtRaw$THINNESS)

# PATHOLOGICAL EATING 
dtRaw$Mean_PATHOLOGICAL_EATING <- abs(mean(dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_PATIENT_GA - dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_CLINICIAN_GA, na.rm = T))

dtRaw$PATHOLOGICAL_EATING <- (dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_PATIENT_GA - dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_CLINICIAN_GA)/ dtRaw$Mean_PATHOLOGICAL_EATING
hist(dtRaw$PATHOLOGICAL_EATING)

# INSIGHT GA SUBSCALE 
dtRaw$INSIGHT_GA_SCALE<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_GA_SCALE<-mean(c(as.numeric(dtRaw[indx,"ILLNESS_SEVERITY"]),as.numeric(dtRaw[indx,"FOOD_APPROACH"]),
                           as.numeric(dtRaw[indx,"THINNESS"]),
                           as.numeric(dtRaw[indx,"PATHOLOGICAL_EATING"])),na.rm=T)
  return(INSIGHT_GA_SCALE)
}))
hist(dtRaw$INSIGHT_GA_SCALE)

# Health Consequences 
# eating health consequences
dtRaw$Mean_EATING_HEALTH <- abs(mean(dtRaw$INSIGHT_5A_EATING_AN_PATIENT_HC - dtRaw$INSIGHT_5A_EATING_AN_CLINICIAN_HC, na.rm = T))

dtRaw$EATING_HEALTH <- (dtRaw$INSIGHT_5A_EATING_AN_PATIENT_HC - dtRaw$INSIGHT_5A_EATING_AN_CLINICIAN_HC)/ dtRaw$Mean_EATING_HEALTH
hist(dtRaw$EATING_HEALTH)

# weight health consequences 
dtRaw$Mean_WEIGHT_HEALTH <- abs(mean(dtRaw$INSIGHT_6A_WEIGHTSHAPE_AN_PATIENT_HC - dtRaw$INSIGHT_6A_WEIGHTSHAPE_AN_CLINICIAN_HC, na.rm = T))

dtRaw$WEIGHT_HEALTH <- (dtRaw$INSIGHT_6A_WEIGHTSHAPE_AN_PATIENT_HC - dtRaw$INSIGHT_6A_WEIGHTSHAPE_AN_CLINICIAN_HC)/ dtRaw$Mean_WEIGHT_HEALTH
hist(dtRaw$WEIGHT_HEALTH)

# INSIGHT HEALTH SUBSCALE 
dtRaw$INSIGHT_HEALTH_SUBSCALE<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_HEALTH_SUBSCALE<-mean(c(as.numeric(dtRaw[indx,"EATING_HEALTH"]),as.numeric(dtRaw[indx,"WEIGHT_HEALTH"])),na.rm=T)
  return(INSIGHT_HEALTH_SUBSCALE)
}))
hist(dtRaw$INSIGHT_HEALTH_SUBSCALE)

# Perspective Taking 
# others commenting on thinness
dtRaw$Mean_OTHERS_THINNESS <- abs(mean(dtRaw$INSIGHT_7A_OTHERS_THINNESS_AN_PATIENT_PT - dtRaw$INSIGHT_7A_OTHERS_THINNESS_AN_CLINICIAN_PT, na.rm = T))

dtRaw$OTHERS_THINNESS <- (dtRaw$INSIGHT_7A_OTHERS_THINNESS_AN_PATIENT_PT - dtRaw$INSIGHT_7A_OTHERS_THINNESS_AN_CLINICIAN_PT)/ dtRaw$Mean_OTHERS_THINNESS
hist(dtRaw$OTHERS_THINNESS)

# others commenting on portions 
dtRaw$Mean_OTHERS_EATING <- abs(mean(dtRaw$INSIGHT_8A_OTHERS_EATING_AN_PATIENT_PT - dtRaw$INSIGHT_8A_OTHERS_EATING_AN_CLINICIAN_PT, na.rm = T))

dtRaw$OTHERS_EATING <- (dtRaw$INSIGHT_8A_OTHERS_EATING_AN_PATIENT_PT - dtRaw$INSIGHT_8A_OTHERS_EATING_AN_CLINICIAN_PT)/ dtRaw$Mean_OTHERS_EATING
hist(dtRaw$OTHERS_EATING)

# INSIGHT PERSPECTIVE SUBSCALE
dtRaw$INSIGHT_PERSPECTIVE_SUBSCALE<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_PERSPECTIVE_SUBSCALE<-mean(c(as.numeric(dtRaw[indx,"OTHERS_THINNESS"]),as.numeric(dtRaw[indx,"OTHERS_EATING"])),na.rm=T)
  return(INSIGHT_PERSPECTIVE_SUBSCALE)
}))
hist(dtRaw$INSIGHT_PERSPECTIVE_SUBSCALE)

# Fears and Hopes 
dtRaw$INSIGHT_PT_FUTURE<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_PT_FUTURE<-mean(c(as.numeric(dtRaw[indx,"INSIGHT_10A_IMPROVE_CONDITION_AN_PATIENT_FP"]),as.numeric(dtRaw[indx,"INSIGHT_12A_FOOD_APPROACH_AN_PATIENT_FP"])),na.rm=T)
  return(INSIGHT_PT_FUTURE)
}))

# reversing scores 
dtRaw$INSIGHT_9_LOSE_CONTROL_R <- 10 - dtRaw$INSIGHT_9A_LOSE_CONTROL_AN_PATIENT_FP
summary(dtRaw$INSIGHT_9_LOSE_CONTROL_R)

dtRaw$INSIGHT_11_FEAR_WEIGHT_R <- 10 - dtRaw$INSIGHT_11A_FEAR_WEIGHT_AN_PATIENT_FP
summary(dtRaw$INSIGHT_11_FEAR_WEIGHT_R)

# grouping fears and hopes subscale 
dtRaw$INSIGHT_FEARS<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_FEARS<-mean(c(as.numeric(dtRaw[indx,"INSIGHT_9_LOSE_CONTROL_R"]),as.numeric(dtRaw[indx,"INSIGHT_11_FEAR_WEIGHT_R"])),na.rm=T)
  return(INSIGHT_FEARS)
}))

dtRaw$INSIGHT_COUNTERFACTUAL_BELIEFS<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_COUNTERFACTUAL_BELIEFS<-mean(c(as.numeric(dtRaw[indx,"INSIGHT_9_LOSE_CONTROL_R"]),as.numeric(dtRaw[indx,"INSIGHT_10A_IMPROVE_CONDITION_AN_PATIENT_FP"]),
                                         as.numeric(dtRaw[indx,"INSIGHT_11_FEAR_WEIGHT_R"]), as.numeric(dtRaw[indx,"INSIGHT_12A_FOOD_APPROACH_AN_PATIENT_FP"])),na.rm=T)
  return(INSIGHT_COUNTERFACTUAL_BELIEFS)
}))

# histograms for all patient standalone qs 
hist(dtRaw$INSIGHT_1A_SEVERITY_AN_PATIENT_GA)
hist(dtRaw$INSIGHT_2A_APPROACH_AN_PATIENT_GA)
hist(dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_PATIENT_GA)
hist(dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_PATIENT_GA)
hist(dtRaw$INSIGHT_5A_EATING_AN_PATIENT_HC)
hist(dtRaw$INSIGHT_6A_WEIGHTSHAPE_AN_PATIENT_HC)
hist(dtRaw$INSIGHT_7A_OTHERS_THINNESS_AN_PATIENT_PT)
hist(dtRaw$INSIGHT_8A_OTHERS_EATING_AN_PATIENT_PT)
hist(dtRaw$INSIGHT_9A_LOSE_CONTROL_AN_PATIENT_FP)
hist(dtRaw$INSIGHT_10A_IMPROVE_CONDITION_AN_PATIENT_FP)
hist(dtRaw$INSIGHT_11A_FEAR_WEIGHT_AN_PATIENT_FP)
hist(dtRaw$INSIGHT_12A_FOOD_APPROACH_AN_PATIENT_FP)
hist(dtRaw$INSIGHT_FEARS)
hist(dtRaw$INSIGHT_PT_FUTURE)
hist(dtRaw$INSIGHT_COUNTERFACTUAL_BELIEFS)

# histograms for all clinician standalone qs
hist(dtRaw$INSIGHT_1A_SEVERITY_AN_CLINICIAN_GA)
hist(dtRaw$INSIGHT_2A_APPROACH_AN_CLINICIAN_GA)
hist(dtRaw$INSIGHT_3A_THINNESS_VS_AVERAGE_AN_CLINICIAN_GA)
hist(dtRaw$INSIGHT_4A_EATING_BEHAVIOUR_AN_CLINICIAN_GA)
hist(dtRaw$INSIGHT_5A_EATING_AN_CLINICIAN_HC)
hist(dtRaw$INSIGHT_6A_WEIGHTSHAPE_AN_CLINICIAN_HC)
hist(dtRaw$INSIGHT_7A_OTHERS_THINNESS_AN_CLINICIAN_PT)
hist(dtRaw$INSIGHT_8A_OTHERS_EATING_AN_CLINICIAN_PT)

###### INSIGHT COUNTERFACTUAL BELIEFS in AN #### 
# counterbeliefs - patient only 
# hopes grouped 
lm0<-lm(PRIOR~INSIGHT_PT_FUTURE,data=filter(dtRaw,GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)
# n.s. 

lm0<-lm(POST_PROSP_AVG~INSIGHT_PT_FUTURE,data=filter(dtRaw,GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)
# n.s. 

# fears grouped 
lm0<-lm(PRIOR~INSIGHT_FEARS,data=filter(dtRaw,GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)
# n.s. 

lm0<-lm(POST_PROSP_AVG~INSIGHT_FEARS,data=filter(dtRaw,GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)
# sig. ; fears grouped associated w higher post prosp. i.e., the less afraid, the higher the beliefs

# future hopes and fears grouped 
lm0<-lm(PRIOR~INSIGHT_COUNTERFACTUAL_BELIEFS,data=filter(dtRaw,GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)
# n.s. 

lm0<-lm(POST_PROSP_AVG~INSIGHT_COUNTERFACTUAL_BELIEFS,data=filter(dtRaw,GROUP=="AN"))
summary(lm0)
tab_model(lm0, show.se=2)
# trend to sig.; the more optimistic the counterfactual beliefs, the higher the post prosp beliefs 

###### ANWR Counterfactual Calculations First ####

# Fears and Hopes 
dtRaw$INSIGHT_PT_FUTURE_ANWR<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_PT_FUTURE_ANWR<-mean(c(as.numeric(dtRaw[indx,"INSIGHT_10A_IMPROVE_CONDITION_ANWR_PATIENT_FP_PRESENT"]),as.numeric(dtRaw[indx,"INSIGHT_12A_FOOD_APPROACH_ANWR_PATIENT_FP_PRESENT"])),na.rm=T)
  return(INSIGHT_PT_FUTURE_ANWR)
}))

# reversing scores 
dtRaw$INSIGHT_9_LOSE_CONTROL_R_ANWR <- 10 - dtRaw$INSIGHT_9A_LOSE_CONTROL_ANWR_PATIENT_FP_PRESENT
summary(dtRaw$INSIGHT_9_LOSE_CONTROL_R_ANWR)

dtRaw$INSIGHT_11_FEAR_WEIGHT_R_ANWR <- 10 - dtRaw$INSIGHT_11A_FEAR_WEIGHT_ANWR_PATIENT_FP_PRESENT
summary(dtRaw$INSIGHT_11_FEAR_WEIGHT_R_ANWR)

# grouping fears and hopes subscale 
dtRaw$INSIGHT_FEARS_ANWR<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_FEARS_ANWR<-mean(c(as.numeric(dtRaw[indx,"INSIGHT_9_LOSE_CONTROL_R_ANWR"]),as.numeric(dtRaw[indx,"INSIGHT_11_FEAR_WEIGHT_R_ANWR"])),na.rm=T)
  return(INSIGHT_FEARS_ANWR)
}))

dtRaw$INSIGHT_COUNTERFACTUAL_BELIEFS_ANWR<-unlist(lapply(seq_along(dtRaw$ID),function(indx){
  INSIGHT_COUNTERFACTUAL_BELIEFS_ANWR<-mean(c(as.numeric(dtRaw[indx,"INSIGHT_9_LOSE_CONTROL_R_ANWR"]),as.numeric(dtRaw[indx,"INSIGHT_10A_IMPROVE_CONDITION_ANWR_PATIENT_FP_PRESENT"]),
                                              as.numeric(dtRaw[indx,"INSIGHT_11_FEAR_WEIGHT_R_ANWR"]), as.numeric(dtRaw[indx,"INSIGHT_12A_FOOD_APPROACH_ANWR_PATIENT_FP_PRESENT"])),na.rm=T)
  return(INSIGHT_COUNTERFACTUAL_BELIEFS_ANWR)
}))

###### ANWR Analyses w Counterfactual as DV ####
# counterbeliefs - patient only 

# hopes grouped 
lm0<-lm(PRIOR~INSIGHT_PT_FUTURE_ANWR,data=filter(dtRaw,GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se=2)
# n.s. 

lm0<-lm(POST_PROSP_AVG~INSIGHT_PT_FUTURE_ANWR,data=filter(dtRaw,GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se=2)
# n.s. 

# fears grouped 
lm0<-lm(PRIOR~INSIGHT_FEARS_ANWR,data=filter(dtRaw,GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se = 2)
# n.s. 

lm0<-lm(POST_PROSP_AVG~INSIGHT_FEARS_ANWR,data=filter(dtRaw,GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se = 2)
# sig. ; post prosp higher predicts less fears grouped

# future hopes and fears grouped 
lm0<-lm(PRIOR~INSIGHT_COUNTERFACTUAL_BELIEFS_ANWR,data=filter(dtRaw,GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se = 2)
# n.s. 

lm0<-lm(POST_PROSP_AVG~INSIGHT_COUNTERFACTUAL_BELIEFS_ANWR,data=filter(dtRaw,GROUP=="AN_WR"))
summary(lm0)
tab_model(lm0, show.se = 2)
# trend to sig.; the more optimistic the post prosp, the higher the counterfactual beliefs

###### INSIGHT CORRELATIONS ####
# INSIGHT ITEMS CORRELATIONS
dtCorrFrame<-dplyr::select(dtRaw,ILLNESS_SEVERITY,FOOD_APPROACH, THINNESS, PATHOLOGICAL_EATING, 
                           INSIGHT_GA_SCALE, EATING_HEALTH, WEIGHT_HEALTH, INSIGHT_HEALTH_SUBSCALE, 
                           OTHERS_THINNESS, OTHERS_EATING, INSIGHT_PERSPECTIVE_SUBSCALE, PRIOR, POST_PROSP_AVG, 
                           IAcc, LR_1, LR_2
)
dtCorr<-as.matrix(dtCorrFrame) 
dtInsightALL<-round(rcorr(dtCorr)$r,2)
write_csv(as.data.frame(dtInsightALL),"Output/corrinsight.csv")

###### EDI-3 COUNTERFACTUAL AND POST PROSP BELIEFS ####
# COUNTERFACTUAL BELIEFS CORRELATIONS
# for AN group first 
dtCorrFrame<-dplyr::select(filter(dtRaw, GROUP =="AN"),EDI_DT_16,EDI_DT_49, 
                           EDI_B_64, EDI_ID_44,
                           POST_PROSP_AVG
)
dtCorr<-as.matrix(dtCorrFrame) 
dtEDIAN<-round(rcorr(dtCorr)$r,2)
write_csv(as.data.frame(dtEDIAN),"Output/corrEDIan.csv")
# n.s. correlations in general 

# then in AN-WR group 
dtCorrFrame<-dplyr::select(filter(dtRaw, GROUP =="AN_WR"),EDI_DT_16,EDI_DT_49, 
                           EDI_B_64, EDI_ID_44, 
                           POST_PROSP_AVG
)
dtCorr<-as.matrix(dtCorrFrame) 
dtEDIANWR<-round(rcorr(dtCorr)$r,2)
write_csv(as.data.frame(dtEDIANWR),"Output/corrEDIanwr.csv")
# same as above 

###### Cronbach's alpha calculations ####
colnamesTAS<-colnames(dtRaw)[grepl("TAS20_Q",colnames(dtRaw))]
dtCorr<-dtRaw[,colnamesTAS]
rcorr(as.matrix(dtCorr))
TAS_cron <-(dtCorr%>% 
              alpha(na.rm = TRUE,check.keys=TRUE) )$total["raw_alpha"]
# tas alpha, 0.87

colnamesIUS<-colnames(dtRaw)[grepl("IUS12_Q",colnames(dtRaw))]
IUS_cron <-(dtRaw[,colnamesIUS]%>% 
              alpha(na.rm = TRUE) )$total["raw_alpha"]
# ius alpha 0.94

colnamesEDEQ<-colnames(dtRaw)[grepl("EDEQ_",colnames(dtRaw)) & !grepl("OVERALL",colnames(dtRaw)) & !grepl("WEIGHT_CONCERN",colnames(dtRaw)) & !grepl("SHAPE_CONCERN",colnames(dtRaw)) & !grepl("EATING_CONCERN",colnames(dtRaw)) & !grepl("RESTRAINT",colnames(dtRaw)) & !grepl("PILL",colnames(dtRaw))& !grepl("WEIGHT",colnames(dtRaw)) & !grepl("HOWMANY_MISSED",colnames(dtRaw))& !grepl("HEIGHT",colnames(dtRaw))& !grepl("MISSED_CYCLE",colnames(dtRaw))]
dtCorr<-dtRaw[,colnamesEDEQ]
rcorr(as.matrix(dtCorr))
EDEQ_cron <-(dtCorr%>% 
               alpha(na.rm = TRUE) )$total["raw_alpha"]

# EDI total
colnamesEDI<-colnames(dtRaw)[grepl("EDI_",colnames(dtRaw)) & !grepl("_71",colnames(dtRaw)) & !grepl("_z",colnames(dtRaw)) & !grepl("_SCALED",colnames(dtRaw))]
EDI_cron <-(dtRaw[,colnamesEDI]%>% 
              alpha(na.rm = TRUE) )$total["raw_alpha"]
# edi total is 0.96

# EDI_ID
colnamesEDIID<-colnames(dtRaw)[grepl("EDI_ID_",colnames(dtRaw)) & !grepl("_z",colnames(dtRaw)) & !grepl("_SCALED",colnames(dtRaw))]
EDI_ID_cron <-(dtRaw[,colnamesEDIID]%>% 
                 alpha(na.rm = TRUE) )$total["raw_alpha"]
# edi id is 0.945

# OCI-R
colnamesOCIR<-colnames(dtRaw)[grepl("OCIR_Q",colnames(dtRaw))]
OCIR_cron <-(dtRaw[,colnamesOCIR]%>% 
               alpha(na.rm = TRUE) )$total["raw_alpha"]
# oci alpha is 0.91

# DASS
colnamesDASS<-colnames(dtRaw)[grepl("DASS21_",colnames(dtRaw)) & !grepl("_DEPRESSION",colnames(dtRaw)) & !grepl("_ANXIETY",colnames(dtRaw)) & !grepl("_STRESS",colnames(dtRaw))]
DASS_cron <-(dtRaw[,colnamesDASS]%>% 
               alpha(na.rm = TRUE) )$total["raw_alpha"]
# DASS alpha is 0.95

###### CONTROL TESTS W WCST ####
lm0<-lm(WCST_SCALED~GROUP,data=filter(dtRaw,GROUP!="AN_BP") )
summary(lm0)
tab_model(lm0, show.se=T) 

lmer0<-lm(WCST_P_SCALED~GROUP, data=filter(dtRaw,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T)


lmer0<-lm(LR_1~GROUP + WCST_SCALED, data=filter(dtRaw,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T, digits=3)

lmer0<-lm(LR_1~GROUP + WCST_P_SCALED, data=filter(dtRaw,GROUP!="AN_BP") )
summary(lmer0)
tab_model(lmer0, show.se=T, digits=3)
# effect size similar, but much smaller sample which is why this happens

# validity for actual lr, and only in hcs
lm00<-lm(LRretro~WCST_PE_SCALED,data=filter(dtRaw, abs(LRretro)<4))
summary(lm00)
tab_model(lm00, show.se=T)

lm00<-lm(LRretro~WCST_PE_SCALED,data=filter(dtRaw, abs(LRretro)<4), GROUP=="HC")
summary(lm00)
tab_model(lm00, show.se=T)

lm00<-lm(LRretro~GROUP + WCST_PE_SCALED,data=filter(dtRaw, abs(LRretro)<4))
summary(lm00)
tab_model(lm00, show.se=T)

###### Traits and LR #### 
# DEPRESSION and STRESS 
lm1118<-lm(LR_1~ DEPRESSION_pca + STRESS_pca ,data=dtRaw)
summary(lm1118)
tab_model(lm1118, show.se=T)

lm1118<-lm(LR_1~ GROUP +DEPRESSION_pca + STRESS_pca ,data=dtRaw)
summary(lm1118)
tab_model(lm1118, show.se=T)

lm1118<-lm(LR_1~ DEPRESSION_pca +STRESS_pca ,data=filter(dtRaw, GROUP=="AN"))
summary(lm1118)
tab_model(lm1118, show.se=T)

lm1118<-lm(LR_1~ DEPRESSION_pca +STRESS_pca ,data=filter(dtRaw, GROUP=="AN_WR"))
summary(lm1118)
tab_model(lm1118, show.se=T)

p<-c(0.933, 0.225,0.006, 0.362)
p.adjust(p, method="holm")
# 0.933 0.675 0.024 0.724

lm1118<-lm(EST_CONF_AVG~ GROUP + DEPRESSION_pca +STRESS_pca ,data=dtRaw)
summary(lm1118)
tab_model(lm1118, show.se=T)

lm1118<-lm(LRretro~ DEPRESSION_pca +STRESS_pca ,data=filter(dtRaw, abs(LRretro)<4))
summary(lm1118)
tab_model(lm1118, show.se=T)

lm00<-lm(abs(LRretro-LR_1)~ DEPRESSION_pca +STRESS_pca,data=filter(dtRaw, abs(LRretro-LR_1)<4))
summary(lm00)
tab_model(lm00, show.se=T)

###### GRAPHS ####

# Load packages ----

# relevelling
dtTemp<-within(dtTemp,GROUP <- relevel(GROUP, ref = "AN_WR"));
dtTemp<-within(dtTemp,GROUP <- relevel(GROUP, ref = "AN"));

dtRaw<-within(dtRaw,GROUP <- relevel(GROUP, ref = "AN_WR"));
dtRaw<-within(dtRaw,GROUP <- relevel(GROUP, ref = "AN"));

# Prospective Performance Estimates
sC_summaryStats <- summarySE(dtTemp, measurevar = "PRIOR",na.rm = TRUE,
                             groupvars=c("GROUP"))

ggplot(dtTemp,aes(x=GROUP,y=PRIOR, fill = GROUP, colour = GROUP))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(position=position_dodge(width=0.12),data = sC_summaryStats, 
             aes(x =  as.numeric(GROUP),
                 y = PRIOR_mean,
                 group = GROUP, colour = GROUP),
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sC_summaryStats, 
                aes(x =  as.numeric(GROUP), y = PRIOR_mean,
                    group = GROUP,   ymin = PRIOR_mean-ci,
                    ymax = PRIOR_mean+ci, colour = GROUP), width = 0.2, size = 0.8)+
  geom_hline(yintercept=0, linetype = "dashed")+
  ylab('Prior Prospective Self-Efficacy Belief')+
  xlab('Group')+
  expand_limits(y=c(0 ,100))+
  ylim(0,100)+
  scale_colour_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 16))+
  ggsave('Graphs/Priors.png', width = 8, height = 6, dpi=1000)

# IAcc
sC_summaryStats <- summarySE(dtTemp, measurevar = "IAcc",na.rm = TRUE,
                             groupvars=c("GROUP"))

ggplot(dtTemp,aes(x=GROUP,y=IAcc, fill = GROUP, colour = GROUP))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(position=position_dodge(width=0.12),data = sC_summaryStats, 
             aes(x =  as.numeric(GROUP),
                 y = IAcc_mean,
                 group = GROUP, colour = GROUP),
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sC_summaryStats, 
                aes(x =  as.numeric(GROUP), y = IAcc_mean,
                    group = GROUP,   ymin = IAcc_mean-ci,
                    ymax = IAcc_mean+ci, colour = GROUP), width = 0.2, size = 0.8)+
  geom_hline(yintercept=0, linetype = "dashed")+
  ylab('Performance (IAcc)')+
  xlab('Group')+
  expand_limits(y=c(0 ,100))+
  ylim(0,100)+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 16))+
  ggsave('Graphs/IAcc.png', width = 8, height = 6, dpi=1000)

# graph on posterior prospectives
sC_summaryStats <- summarySE(dtTemp, measurevar = "POST_PROSP_AVG",na.rm = TRUE,
                             groupvars=c("GROUP"))

ggplot(dtTemp,aes(x=GROUP,y=POST_PROSP_AVG, fill = GROUP, colour = GROUP))+
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2, trim = FALSE)+
  geom_point(position = position_jitter(width = .15), size = .25)+
  geom_point(position=position_dodge(width=0.12),data = sC_summaryStats, 
             aes(x =  as.numeric(GROUP),
                 y = POST_PROSP_AVG_mean,
                 group = GROUP, colour = GROUP),
             size = 3, shape = 18)+
  geom_errorbar(position=position_dodge(width=0.12),data = sC_summaryStats, 
                aes(x =  as.numeric(GROUP), y = POST_PROSP_AVG_mean,
                    group = GROUP,   ymin = POST_PROSP_AVG_mean-ci,
                    ymax = POST_PROSP_AVG_mean+ci, colour = GROUP), width = 0.2, size = 0.8)+
  geom_hline(yintercept=0, linetype = "dashed")+
  ylab('Posterior Prospective Self-Efficacy Belief')+
  xlab('Group')+
  expand_limits(y=c(0 ,100))+
  ylim(0,100)+
  scale_colour_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.caption = element_text(hjust = 0.5), text = element_text(size = 16))+
  ggsave('Graphs/Posteriors.png', width = 8, height = 6, dpi=1000)

