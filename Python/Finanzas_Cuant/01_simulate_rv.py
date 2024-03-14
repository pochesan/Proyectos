import pandas as pd
import numpy as np
import matplotlib as mpl
import scipy
import importlib
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2

nb_sims = 10**6
df = 2
dist_name = "normal" # student normal exponential uniform
dist_type = "simulated RV" # real custom

if dist_name == "normal":  
    x = np.random.standard_normal(nb_sims)
    x_description = dist_type + " " + dist_name
elif dist_name == "exponential":
    x = np.random.standard_exponential(nb_sims)
    x_description = dist_type + " " + dist_name
elif dist_name == "uniform":
    x = np.random.uniform(0,1,nb_sims)
    x_description = dist_type + " " + dist_name
elif dist_name == "student":
    x = np.random.standard_t(df, size = nb_sims)
    x_description = dist_type + " " + dist_name + " | df = " + str(df)
elif dist_name == "chi-square":
    x = np.random.chisquare(df,  size = nb_sims)
    x_description = dist_type + " " + dist_name + " | df = " + str(df)

'''
Goal: create a Jarque-Bera normality test
'''
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

print("skewness is: " + str(x_skew))
print("kurtosis is: " + str(x_kurtosis))
print("JB statistic is: " + str(x_jb_stat))
print("p-value is: " + str(x_p_value))
print("x is normal: " + str(x_is_normal))      
      
#Plot time-series of price
plt.figure()
plt.plot(x)
plt.xlabel("Price")
plt.ylabel("Time")

#plot histogram
plt.figure()
plt.hist(x, bins = 100)
plt.title(x_description)
plt.show()