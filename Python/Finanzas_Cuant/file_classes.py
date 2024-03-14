import pandas as pd
import numpy as np
import matplotlib as mpl
import scipy
import importlib
import matplotlib.pyplot as plt
from scipy.stats import skew, kurtosis, chi2, linregress

import file_classes
importlib.reload(file_classes)
import file_functions
importlib.reload(file_functions)


class distribution_manager():
    
    def __init__ (self, inputs):
       self.inputs = inputs #Distribution_inputs
       self.data_table = []
       self.description = None
       self.nb_rows = 0
       self.mean = None
       self.std = None
       self.skew = None
       self.kurtosis = None #Excess kurtosis
       self.jb_stat = None  
       self.p_value = None
       self.is_normal = None
       self.sharpe = None
       self.var_95 = None
       self.percentile_25 = None
       self.percentile_75 = None
       self.median = None
       
       
    def __str__ (self):
       str_self = self.description + ' / size ' + str(self.nb_rows) + '\n' + self.plot_str()
       return str_self
       
    def load_timeseries(self):
        
        data_type = self.inputs.data_type
         
        if data_type == "simulation":
            
            nb_sims = self.inputs.nb_sims
            dist_name = self.inputs.variable_name
            degrees_freedom = self.inputs.degrees_freedom
            
            if dist_name == "normal":  
                x = np.random.standard_normal(nb_sims)
                self.description = data_type + " " + dist_name
            elif dist_name == "exponential":
                x = np.random.standard_exponential(nb_sims)
                self.description = data_type + " " + dist_name
            elif dist_name == "uniform":
                x = np.random.uniform(0,1,nb_sims)
                self.description = data_type + " " + dist_name
            elif dist_name == "student":
                x = np.random.standard_t(df = degrees_freedom, size = nb_sims)
                self.description = data_type + " " + dist_name + " | df = " + str(degrees_freedom)
            elif dist_name == "chi-square":
                x = np.random.chisquare(df = degrees_freedom,  size = nb_sims)
                self.description = data_type + " " + dist_name + " | df = " + str(degrees_freedom)

            self.nb_rows = nb_sims
            self.vec_returns = x
            
        elif data_type == "real":    
             
             ric = self.inputs.variable_name
             t = file_functions.load_timeseries(ric)
             
             self.data_table = t
             self.description = "market data " + ric
             self.nb_rows = t.shape[0]
             self.vec_returns = t['Return_close'].values
             
    def plot_histogram(self):
        #plot histogram
        plt.figure()
        plt.hist(self.vec_returns, bins = 100)
        plt.title(self.description)
        plt.xlabel(self.plot_str())
        plt.show()
        
    def compute(self):
       self.mean = np.mean(self.vec_returns)
       self.std = np.std(self.vec_returns)
       self.skew = skew(self.vec_returns)
       self.kurtosis = kurtosis(self.vec_returns) #excess kurtosis
       self.jb_stat = self.nb_rows/6*(self.skew**2 + 1/4*self.kurtosis**2)   
       self.p_value = 1 - chi2.cdf(self.jb_stat, df = 2) 
       self.is_normal = (self.p_value > .05)
       chi2.ppf(.95, 2) #Equivalently self.jb_stat > 5.991
       self.sharpe = self.mean/self.std * np.sqrt(252) #Anualizado
       self.var_95 = np.percentile(self.vec_returns, 5)
       self.cvar_95 = np.mean(self.vec_returns[self.vec_returns <= self.var_95])
       self.percentile_25 = self.percentile(25)
       self.median = np.median(self.vec_returns)
       self.percentile_75 = self.percentile(75)
       
    def plot_str(self):
        nb_decimals = 4
        plot_str = 'mean ' + str(np.round(self.mean,nb_decimals))\
            + ' | std dev ' + str(np.round(self.std,nb_decimals))\
            + ' | skewness ' + str(np.round(self.skew,nb_decimals))\
            + ' | kurtosis ' + str(np.round(self.kurtosis,nb_decimals)) + "\n"\
            + 'Jarque Bera ' + str(np.round(self.jb_stat,nb_decimals))\
            + ' |  p-value ' + str(np.round(self.p_value,nb_decimals))\
            + ' | is normal ' + str(self.is_normal) + "\n"\
            + 'Sharpe Annual ' + str(np.round(self.sharpe,nb_decimals))\
            + ' | VaR 95% ' + str(np.round(self.var_95,nb_decimals))\
            + ' | CVaR 95% ' + str(np.round(self.cvar_95,nb_decimals)) + "\n"\
            + 'Percentile 25% ' + str(np.round(self.percentile_25,nb_decimals))\
            + ' | Median ' + str(np.round(self.median,nb_decimals))\
            + ' | Percentile 75% ' + str(np.round(self.percentile_75,nb_decimals))
        return plot_str
        
    def percentile (self, pct):
        percentile = np.percentile(self.vec_returns, pct)
        #return np.percentil(self.vec_returns, pct) Mas facilre
        return percentile


class capm_manager():
    
    def __init__(self, benchmark, security, nb_decimals):
        self.benchmark = benchmark
        self.security = security
        self.nb_decimals = nb_decimals
        self.data_table = None
        self.alpha = None
        self.beta = None
        self.correlation = None 
        self.r_squared = None
        self.p_value = None
        self.std_err = None
        self.null_hypothesis = None
        self.predictor_linreg = None
     
    def __str__(self):
        return self.plot_str()
    
    def plot_str(self):
        str_self = 'Linear regression | security ' + self.security\
            + ' | benchmark ' + self.benchmark + '\n'\
            + 'alpha (intercept) ' + str(self.alpha)\
            + ' | beta (slope) ' + str(self.beta) + '\n'\
            + 'p-value ' + str(self.p_value)\
            + ' | null hypothesis ' + str(self.null_hypothesis) + '\n'\
            + 'correlation ' + str(self.correlation)\
            + ' | r-square ' + str(self.r_squared)  
        
        return str_self
    
    def load_timeseries(self):
        self.data_table = file_functions.load_synchronise_timeseries(ric_x = self.benchmark, ric_y = self.security)
   
    def compute(self):
        y = self.data_table['return_y'].values
        x = self.data_table['return_x'].values
        slope, intercept, correlation, p_value, std_err = linregress(x, y)
        self.alpha = np.round(intercept, self.nb_decimals)
        self.beta = np.round(slope, self.nb_decimals)
        self.correlation = np.round(correlation, self.nb_decimals) #Correlation
        self.r_squared = np.round(correlation**2, self.nb_decimals) #Percentage of variance of y explained by x
        self.p_value = np.round(p_value, self.nb_decimals)
        self.std_err = np.round(std_err, self.nb_decimals)
        self.null_hypothesis = p_value > 0.05 #False if Beta is statistically significant
        self.predictor_linreg = slope*x + intercept
    
    def plot_timeseries(self):
        #plot 2 timeseries with 2 vertical axes
        plt.figure(figsize = (12,5))
        plt.title("Time series of prices")
        plt.xlabel("Time")
        plt.ylabel("Prices")
        ax = plt.gca()
        ax1 = self.data_table.plot(kind = "line", x = "Date", y = "price_x", ax = ax, grid = True,\
                                    color = "blue", label = self.benchmark)
        ax2 = self.data_table.plot(kind = "line", x = "Date", y = "price_y", ax = ax, grid = True,\
                                    color = "red", secondary_y = True, label = self.security)   
        ax1.legend(loc = 2)
        ax2.legend(loc = 1)
        plt.show()
    
    def plot_linear_regression(self):
        str_title = 'Scatterplot of returns' + '\n' + self.plot_str()
        y = self.data_table['return_y'].values
        x = self.data_table['return_x'].values
        plt.figure()
        plt.title(str_title)
        plt.scatter(x,y)
        plt.plot(x, self.predictor_linreg, color = 'green')
        plt.ylabel(self.security)    
        plt.xlabel(self.benchmark)
        plt.grid()
        plt.show() 
        
        
class distribution_input():
    
    def __init__(self):
        self.data_type = None # simulation real custom
        self.variable_name = None # normal student VWS.CO
        self.degrees_freedom = None # only used in simulation i.e student, chi-square
        self.nb_sims = None # only in simulation    


class hedge_manager():
    
    def __init__(self, inputs):
        self.inputs = inputs #hedge_inputs
        self.nb_decimals = inputs.nb_decimals
        self.benchmark = inputs.benchmark #The market in CAPM, in general ^STOXX50E
        self.security = inputs.security # Portafolio to hedge
        self.hedge_securities = inputs.hedge_securities #get Universe
        self.delta_portafolio = inputs.delta_portafolio # million USD #In mn USD, deafult 10
        self.nb_hedges = [len(self.hedge_securities),1]
        self.beta_portafolio = None
        self.beta_portafolio_usd = None 
        self.betas = None
        self.optimal_hedge = None
        self.hedge_delta = None
        self.hedge_beta_usd = None
        
    def load_betas(self):
        
        #Compute betas for the portafolio
        benchmark = self.benchmark
        security = self.security
        hedge_securities = self.hedge_securities
        delta_portafolio = self.delta_portafolio
        nb_decimals = self.nb_decimals
        
        #Compute Beta for the portafolio
        capm = file_classes.capm_manager(benchmark, security, nb_decimals)
        capm.load_timeseries()
        capm.compute()
        print(capm)
        beta_portafolio = capm.beta 
        beta_portafolio_usd = beta_portafolio * delta_portafolio # mn USD
        
        #print input
        print('----')
        print('Input Portfolio:')
        print('Delta mnUSD for ' + security + ' is ' + str(delta_portafolio))
        print('Beta for ' + security + ' vs ' + benchmark + ' is ' + str(beta_portafolio))
        print('Beta mnUSD for ' + security + ' vs ' + benchmark + ' is ' + str(beta_portafolio_usd))
        
        #Compute
        shape = [len(hedge_securities),1]
        betas = np.zeros(shape)
        counter = 0 
        print('----')
        print('Input hedges: ')
        for hedge_security in hedge_securities:
            capm = file_classes.capm_manager(benchmark, hedge_security, nb_decimals)
            capm.load_timeseries()
            capm.compute()
            beta = capm.beta 
            print('Beta for hedge[' + str(counter) + '] = ' + hedge_security + ' vs ' + benchmark + ' is ' + str(beta))
            betas[counter] = beta 
            counter += 1
            
        self.beta_portafolio = beta_portafolio
        self.beta_portafolio_usd = beta_portafolio_usd
        self.betas = betas

    def compute(self):
        
        #Exact solution using matrix algebra
        shape = [len(self.hedge_securities)]
        deltas = np.ones(shape)
        betas = self.betas
        targets = -np.array([[self.delta_portafolio], [self.beta_portafolio_usd]])
        mtx = np.transpose(np.column_stack((deltas, betas))) 
        self.optimal_hedge = np.linalg.inv(mtx).dot(targets) #Invierto y luego multiplico por objetivos
        self.hedge_delta = np.sum(self.optimal_hedge)
        self.hedge_beta_usd = np.transpose(betas).dot(self.optimal_hedge).item()

        #Print result
        print('-----')
        print('Optimisation result')
        print('-----')
        print("Delta: " + str(self.delta_portafolio))
        print('Beta USD: ' + str(self.beta_portafolio_usd))
        print('-----')
        print('Hedge delta: ' + str(self.hedge_delta))
        print('Hedge beta: ' + str(self.hedge_beta_usd))
        print('-----')
        print('Optimal hedge:')
        print(self.optimal_hedge) #Cantidad de activos por los 2 bancos para cubrir activo
        print('-----')
           

class hedge_inputs():
    
    def __init__(self):
        self.benchmark = None #The market in CAPM, in general ^STOXX50E
        self.security = None # Portafolio to hedge
        self.hedge_securities = None #Hedge Universe
        self.delta_portafolio = None # million USD #In mn USD, deafult 10
        self.regularisation_parameter = None #To be defined later
        self.nb_decimals = None
        