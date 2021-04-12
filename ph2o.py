import numpy as np
import pandas as pd
import os

files = os.listdir()
files = list(filter(lambda x: x.endswith(".csv"), files))
fileNames = []
for file in files:
    fileNames.append(str(file))
    file = pd.read_csv(file, 
    low_memory=False, 
    usecols=[2, 4], 
    skiprows= lambda x: 2 if x == 2 else x%2 == 1, 
    parse_dates=["datetime"], 
    date_parser=lambda x: pd.to_datetime(x, format="%m/%d/%Y %H:%M"),
    index_col=["datetime"])

files = np.array(files, dtype=pd.DataFrame)
