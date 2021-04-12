import numpy as np
import pandas as pd
import os

files = os.listdir()
files = list(filter(lambda x: x.endswith(".csv"), files))
fileNames = []
for index in range(len(files)):
    fileNames.append(str(files[index]))
    files[index] = pd.read_csv(files[index], low_memory=False)
    files[index].dropna(inplace=True)
    files[index] = files[index].iloc[1:]
    files[index]["datetime"] = pd.to_datetime(files[index]["datetime"], format="%m/%d/%Y %H:%M")
    files[index].reset_index(drop=True, inplace=True)

files = np.array(files, dtype=pd.DataFrame)