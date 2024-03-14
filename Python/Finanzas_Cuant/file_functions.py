import pandas as pd
import numpy as np
import matplotlib as mpl
import scipy
import importlib
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2

import file_classes
importlib.reload(file_classes)
import file_functions
importlib.reload(file_functions)

for i in range(3):
    x = [1, 0, 1]
    for j in range(i):
        x.append(0)
        x.append(1)
    x = np.array(x) + i
    y = np.arange(-(i+1), i+2)[::-1]
    
def load_timeseries(ric):
    directory = "C:\\Users\JOSUE\Documents\SeminariPythoncuantitativo\Finanzas_Cuant\data-master\\"
    path = directory + ric + ".csv" 
    raw_data = pd.read_csv(path)
    t = pd.DataFrame()
    t['Date'] = pd.to_datetime(raw_data['Date'], dayfirst = True) #Dia/Mes/Anio
    t['Close'] = raw_data['Close']
    t.sort_values(by = "Date", ascending = True)
    t['Close_previous'] = t['Close'].shift(1)
    t['Return_close'] = t['Close']/t['Close_previous'] - 1
    t.dropna(inplace = True)
    t.reset_index(drop = True, inplace = True)
    
    return t

def load_synchronise_timeseries(ric_x, ric_y):
    #Get timeseries of x and y
    table_x = file_functions.load_timeseries(ric_x)
    table_y = file_functions.load_timeseries(ric_y)
    #synchronise timeseries
    timestamp_x = list(table_x['Date'].values)
    timestamp_y = list(table_y['Date'].values)
    timestamps = list(set(timestamp_x) & set(timestamp_y))     
    #Synchrinised time  series for benchhmark
    table_x_sync = table_x[table_x['Date'].isin(timestamps)]
    table_x_sync.sort_values(by = "Date", ascending = True)
    table_x_sync =  table_x_sync.reset_index(drop = True)
    table_y_sync = table_y[table_y['Date'].isin(timestamps)]
    table_y_sync.sort_values(by = "Date", ascending = True)
    table_y_sync =  table_y_sync.reset_index(drop = True)
    #Table of return for ric and benchmark
    t = pd.DataFrame()
    t['Date'] = table_x_sync['Date']
    t['price_x'] = table_x_sync['Close']
    t['return_x']= table_x_sync['Return_close']
    t['price_y'] = table_y_sync['Close']
    t['return_y']= table_y_sync['Return_close']       
    
    return t