import pandas as pd
from os import listdir

dataPath="Emoticon_Interface"
fileNames=list()
for fn in listdir(dataPath):
    fileNames.append(fn)
    
dtOutput=pd.read_csv(dataPath+"/"+fileNames[0], sep='\t')#, lineterminator='\r')
for dtf in fileNames[1:]:
    dtOutput=dtOutput.append(pd.read_csv(dataPath+"/"+dtf, sep='\t'))#, lineterminator='\r'))
    
outFilePathName="MergedFiles_Output/EmoticonInterfaceData.csv"
dtOutput.to_csv(outFilePathName, index=False)
    