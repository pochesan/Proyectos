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
inputs = file_classes.hedge_inputs() 
inputs.benchmark = '^STOXX50E' 
inputs.security = 'FP.PA' # Reuters Identification Code
inputs.hedge_securities = ['RDSa.AS', "BP.L"]
inputs.delta_portafolio = 10 # million USD
inputs.nb_decimals = 4

#Compute
hedge = file_classes.hedge_manager(inputs)
hedge.load_betas() #Get the betas for portfolio and hedges
hedge.compute() #Compute optimal hedge via CAPM


#Aproximate solution via minization cost function

# #Matriz (solución existe y es única)
# 1*S_1 + 1*S_2 = -delta_portafolio
# 1.4718*S_1 + 1.2322*S_2 = -beta_portafolio_usd 