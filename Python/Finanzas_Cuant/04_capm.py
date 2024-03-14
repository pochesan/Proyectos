import pandas as pd
import numpy as np
import matplotlib as mpl
import scipy
import importlib
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2, linregress

#Import our own files and reload
import file_classes
importlib.reload(file_classes)
import file_functions
importlib.reload(file_functions)

# inputs
benchmark = '^STOXX50E' # variable x
security = 'BBVA.MC' # variable y 
nb_decimals = 4

capm = file_classes.capm_manager(benchmark, security, nb_decimals)
capm.load_timeseries()
capm.plot_timeseries()
capm.compute()
capm.plot_linear_regression()
print(capm)
 