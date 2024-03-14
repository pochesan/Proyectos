import pandas as pd
import numpy as np
import matplotlib as mpl
import scipy
import importlib
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2, linregress
from scipy.optimize import minimize

#Import our own files and reload
import file_classes
importlib.reload(file_classes)
import file_functions
importlib.reload(file_functions)

# #Define the function to minimize 
# def cost_function(x):
#         f = (x[0] - 7.0)**2 + (x[1] + 5)**2  + (x[2] - 13)**2
#         return f

# #initialise optimization 
# x = np.ones([3,1])   

# #Compute optimization
# optimal_result = minimize(cost_function, x) 

# #print
# print('------')
# print('Optimization result')
# print(optimal_result)

def cost_function(x, roots, coeffs):
        f = 0
        for n in range(len(x)):
            f += coeffs[n] * (x[n] - roots[n])**2 
        return f

#Input parameters
dimensions = 5 
roots = np.random.randint(low = -20, high = 20, size = 5)
coeffs = np.ones([dimensions,1])

#Initialise optimization
x = np.zeros([dimensions,1])

#Compute optimization
optimal_result = minimize(cost_function, x0 = x, args = (roots, coeffs))

#Print  
print('------')
print('Optimization result:')
print(optimal_result)
print('Roots')
print(roots)
print('------')