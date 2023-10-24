# -*- coding: utf-8 -*-
"""
Created on Thu Feb  3 12:47:23 2022

@author: ThanosKoukoutsakis
"""
import pandas as pd
import numpy as np
from os import listdir

outFileNameTAGS="MergedFiles_Output/EmpaticaTagsData.csv"
outFileNameHR="MergedFiles_Output/EmpaticaHRData.csv"
outFileNameIBI="MergedFiles_Output/EmpaticaIBIData.csv"
dataPath="EMPATICA_THANOS"
fileNames=list()
for fn in listdir(dataPath):
    fileNames.append(fn)
dtOutTags=pd.DataFrame(columns = ["timeUnix","SUBJ_ID"])  
dtOutHR=pd.DataFrame(columns = ["HR","timeUnix","SUBJ_ID"])   
dtOutIBI=pd.DataFrame(columns = ["dtIBI","IBI","timeUnix","SUBJ_ID"]) 

for dtf in fileNames[1:]:
    if("tags" in dtf):
        filename=dtf
        dtRaw2=pd.read_csv("EMPATICA_THANOS/"+filename)

        subjectId=filename.split('_')[0]

        dtTags=pd.DataFrame(data=(dtRaw2[:]), index=None)
        dtTags.reset_index(drop=True, inplace=True)
        dtTags.columns.values[0] = "timeUnix"
        dtTags["SUBJ_ID"]=np.full(len(dtTags),subjectId)
        dtOutTags=dtOutTags.append(dtTags)
        print("tags "+filename)
    elif("HR" in dtf):
        filename=dtf
        dtRaw2=pd.read_csv("EMPATICA_THANOS/"+filename)

        subjectId=filename.split('_')[0]

        dtHR=pd.DataFrame(data=(dtRaw2[1:]), index=None)
        dtHR.reset_index(drop=True, inplace=True)
        t0=(float(dtHR.columns.values[0]))
        dtHR.columns.values[0] = "HR"
        dtHR["timeUnix"]=np.zeros(len(dtHR))
        dtHR["SUBJ_ID"]=np.full(len(dtHR),subjectId)
        print("HR "+filename +" " +  str(t0))
        for cnt in range(0,len(dtHR)):
            dtHR.loc[cnt,"timeUnix"]=t0+cnt*1
        dtOutHR=dtOutHR.append(dtHR)
    if("IBI" in dtf):
        filename=dtf
        dtRaw2=pd.read_csv("EMPATICA_THANOS/"+filename)

        subjectId=filename.split('_')[0]

        dtIBI=pd.DataFrame(data=(dtRaw2[0:]), index=None)
        dtIBI.reset_index(drop=True, inplace=True)
        t0=(float(dtIBI.columns.values[0]))
        dtIBI.columns.values[0] = "dtIBI"
        dtIBI.columns.values[dtIBI.columns.values==' IBI']='IBI'
        dtIBI["timeUnix"]=np.zeros(len(dtIBI))
        dtIBI["SUBJ_ID"]=np.full(len(dtIBI),subjectId)
        print("IBI "+filename +" " +  str(t0))
        for cnt in range(0,len(dtIBI)):
            dtIBI.loc[cnt,"timeUnix"]=t0+dtIBI.loc[cnt,"dtIBI"]
        dtOutIBI=dtOutIBI.append(dtIBI)
        
dtOutTags.to_csv(outFileNameTAGS, index=False)
dtOutHR.to_csv(outFileNameHR, index=False)
dtOutIBI.to_csv(outFileNameIBI, index=False)