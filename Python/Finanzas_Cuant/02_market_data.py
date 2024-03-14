import pandas as pd
import numpy as np
import matplotlib as mpl
import scipy
import importlib
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2

#Get market data
#Remember to modify the path to match your own directory
ric = "BBVA.MC"
directory = "D:\Descargas\\"
path = directory + ric + ".csv"
raw_data = pd.read_csv(path)

#Create table of returns
t = pd.DataFrame()
t['Date'] = pd.to_datetime(raw_data['Date'], dayfirst = True) #Dia/Mes/Anio
t['Close'] = raw_data['Close']
t.sort_values(by = "Date", ascending = True)
t['Close_previous'] = t['Close'].shift(1)

t['Return_close'] = t['Close']/t['Close_previous'] - 1
t.dropna(inplace = True)
t.reset_index(drop = True, inplace = True)

#Plot time-series of price
plt.figure()
plt.plot(t['Date'],t['Close'])
plt.xlabel("Price")
plt.ylabel("Time")

'''
Goal: create a Jarque-Bera normality test
'''

x = t['Return_close'].values
x_description = "market data " + ric
nb_sims = len(x)

x_mean = np.mean(x)
x_std = np.std(x)
x_skew = skew(x)
x_kurtosis = kurtosis(x) #excess kurtosis
x_jb_stat =nb_sims/6*(x_skew**2 + 1/4*x_kurtosis**2)   
x_p_value = 1 - chi2.cdf(x_jb_stat, df = 2) 
x_is_normal = (x_p_value > .05)
chi2.ppf(.95, 2) #Equivalently x_jb_stat > 5.991

# jb_list = []
# jb_list.append(x_jb_stat)

print("---- Real Market Data ---")
print("Ric is" + ric)
print("skewness is: " + str(x_skew))
print("kurtosis is: " + str(x_kurtosis))
print("JB statistic is: " + str(x_jb_stat))
print("p-value is: " + str(x_p_value))
print("x is normal: " + str(x_is_normal))      
      
#plot histogram
plt.figure()
plt.hist(x, bins = 50)
plt.title(x_description)
plt.show()