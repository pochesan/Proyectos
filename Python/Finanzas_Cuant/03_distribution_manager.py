import pandas as pd
import numpy as np
import matplotlib as mpl
import scipy
import importlib
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2

#Import our own files and reload
import file_classes #Importar archivo
importlib.reload(file_classes) #Se recargan

#Diccionario
# inputs = {
# "data_type" : 'real', # simulation real custom
# "variable_name" : "VWS.CO", # normal student VWS.CO
# "degrees_freedom" : None,
# "nb_sims" : None, #Only Used in student and chi-square
# "bool_plot_timeseries" : False,
# "bool_plot_histogram" : False,
# }

# #Dataframe
# inputs_df = pd.DataFrame() 
# inputs_df['data_type'] = ["real"]
# inputs_df['variable_name'] = ["STOXX50E.CO"]
# inputs_df['degrees_freedom'] = [None]
# inputs_df['nb_sims'] = [None]

#Variables independientes
# data_type = 'real' # simulation real custom
# variable_name = 'VWS.CO' # normal student VWS.CO
# degrees_freedom = 'None'
# nb_sims = 'None' #Only Used in student and chi-square

inputs = file_classes.distribution_input()
inputs.data_type = "real"
inputs.variable_name = 'BBVA.MC'
inputs.degrees_freedom = None
inputs.nb_sims = None

dm = file_classes.distribution_manager(inputs)
dm.load_timeseries() #Carga las series de tiempo #Polymorphise
dm.compute() #Calcula
dm.plot_histogram() #Plotea histograma
print(dm)   