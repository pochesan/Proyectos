##############################################################################
#Materia: M�todos cuantitativos en finanzas

#C�digo que extrae datos de los precios hist�ricos de 5 emisoras de la B.M.V.,
#agrupa los datos y calcula 3 metodolog�as de VaR distintas
#para 2 portafolios con distintas distribuciones en sus pesos
#Posteriormente gr�fica lo necesario para realizar un an�lisis de riesgo a los
#portafolios y finalmente, compara las m�ximas perdidas esperadas dado un cierto
#nivel de confianza con la p�rdida real dada al d�a posterior de valuaci�n.

#Alumnos: �ngel Josu� Mej�a N�jera
#         Eduardo Ram�rez Rubio
##############################################################################

#Librerias
import numpy as np
import pandas as pd
import math
import yfinance as yf
import seaborn as sns
import scipy as sp
from scipy.stats import norm
import matplotlib.pyplot as plt
import matplotlib.ticker as mtick
import warnings
#Funci�n que quita warnings del append()
warnings.filterwarnings("ignore")

#Fijamos semilla
np.random.seed(10)

###################### 2. C�LCULO DE RENDIMIENTOS, VARIANZA Y COVARIANZA POR EMISORA ######################

#Datos iniciales
#Emisoras de la Bolsa Mexicana de Valores
ric = ['AGUA.MX','GENTERA.MX','PE&OLES.MX','PINFRA.MX','WALMEX.MX']
#Pesos iguales del portafolio de inversi�n
pesos =  [0.2, 0.2, 0.2,0.2,0.2]
#Fecha de inicio de evaluaci�n
start = '2017-05-06'
#Fecha de fin de evaluaci�n (5 a�os de data hist�rica)
#Tomamos 7 de mayo para que la funci�n nos regrese el precio
#de cierre del 6 de mayo y asi poder comparar luego con el precio
#al 9 de mayo(7 y 8 son inh�biles)
end = '2022-05-07' 

#Se descargan los datos de 5 a�os de historia y se guardan en un DataFrame
#Note que el �ltimo dato 
raw_data = yf.download(ric, start = start, end = end)['Close'].reset_index()
#Se guarda el �ltimo valor hist�rico para determinar la distribuci�n de p�rdidas
today =  raw_data.tail(1).reset_index(drop = True) 
##Notemos que el �ltimo valor obtenido corresponde al d�a 5 de mayo de 2022

#Calculamos rendimientos por emisora
rf = pd.DataFrame()
            
#Funci�n para calcular rendimientos diarios continuos
def returns(columns_names,data):
    for i in columns_names:
        data[i] = np.log(raw_data[i]/raw_data[i].shift(1))
#Usamos la funci�n para calcular los rendimientos
returns(ric, rf)

#Calculamos rendimientos esperados por emisora
rf.mean() #Funciones omiten NA's
#Calculamos volatilidad de los rendimientos por emisora
rf.std()
#Calculamos varianza de los rendimientos por emisora
rf.var()

########################### 3. DESARROLLO ###########################

########################### PORTAFOLIO 1 (PESOS IGUALES) ###########################
    
#Capital inicial de $1,000,000 pesos mexicanos
capital = 1000000

#Determinamos nuestra posici�n (n�mero de acciones) por emisora 
#A d�a de hoy (5 de mayo de 2022). 
#Suponemos s�lo podemos comprar acciones enteras
n_acciones = [math.floor((capital*.2)/today.iloc[0,1]), 
              math.floor((capital*.2)/today.iloc[0,2]),
              math.floor((capital*.2)/today.iloc[0,3]),
              math.floor((capital*.2)/today.iloc[0,4]),
              math.floor((capital*.2)/today.iloc[0,5])]

#Obtenemos el valor de inversi�n para cada activo
posicion_today = np.multiply(n_acciones, today[ric])

#Obtenemos el valor total de nuestro portafolio hoy 
#Su valor es de aproximadamente un mill�n de pesos
portafolio = posicion_today.iloc[0,].sum()

#Eliminar el �ltimo dato de la data hist�rica porque ese dato es el actual
raw_data.drop(1258, axis = 0, inplace = True)                  

###################### 3.1.1 RENDIMIENTOS DIARIOS ##################

#Creamos un nuevo dataframe donde guardaremos todo lo que necesitemos
df = pd.DataFrame()
#Agregar la fecha
df['date'] = raw_data['Date']              

#Hagamos una funcion para calcular los rendimientos cont�nuos diarios 
#Se agregan a la tabla
def returns(columns_names,data, raw_data):
    for i in columns_names:
        data[i + '_returns'] = np.log(raw_data[i]/raw_data[i].shift(1))

#Usamos la funci�n para calcular los rendimientos
returns(ric, df, raw_data)

#Eliminamos los NA's
df.dropna(inplace = True)

###################### 3.1.2 AN�LISIS DE CORRELACI�N ##################

#Obtenemos precios de cierre del IPC_
IPC_data = yf.download('^MXX', start = start, end = end)['Close'].reset_index()
IPC_rendimientos =  np.log(IPC_data['Close']/IPC_data['Close'].shift(1))

#Calculamos rendimientos esperados del IPC
IPC_rendimientos.mean()
#Calculamos volatilidad del IPC
IPC_rendimientos.std()
#Calculamos varianza del IPC
IPC_rendimientos.var()

#Creamos data frame con rendimientos incluyendo el �ndice de representatividad
#del mercado (IPC_MEX_BMV)
gf = rf.assign(MXX = IPC_rendimientos)

#Creamos matriz de correlaciones entre los rendimientos diarios continuos
#de las 5 emisora y el �ndice de representatividad IPC
#Observamos baja correlaci�n entre los activos que conforman el portafolio
#y una alta correlaci�n de cada activo con el IPC
corr_matrix = gf.corr()

#Calculamos betas del portafolio para tener una idea de la volatilidad
#del mismo con respecto al mercado
Betas = (rf.std()/IPC_rendimientos.std())*corr_matrix.iloc[5,0:5]
Beta_portafolio = (0.2*Betas).sum()

#Calculamos VaR hist�rico diario con datos a 5 a�os y rendimientos diarios continuos
IPCdf = pd.DataFrame()
IPCdf['date'] = IPC_data['Date'] 
IPCdf['Rendimientos'] = IPC_rendimientos
IPCdf.dropna(inplace = True)
todayIPC =  IPC_data.tail(1)  
IPC_position = (math.floor((capital)/todayIPC.iloc[0,1]))*todayIPC.iloc[0,1]
IPCdf['IPC_pos_scenario'] = todayIPC.iloc[0,1] * np.exp(IPCdf['Rendimientos'])
IPCdf['IPC_scenario'] = np.multiply(IPCdf['IPC_pos_scenario'], math.floor((capital)/todayIPC.iloc[0,1]))
IPCdf['IPC_loss'] = (IPCdf['IPC_scenario'] - IPC_position) * -1
VaR_95_IPC = np.percentile(IPCdf['IPC_loss'] , 99.5) 
#Valor porcentual del var
(VaR_95_IPC/IPC_position) * 100

###################### 3.1.3 ESCENARIOS ##################

#Hacemos una funci�n para crear los escenarios de rendimientos a un d�a
def scenarios(columns_names, data):
    for i in columns_names:
        data[i + '_scenario'] = today[i][0] * np.exp(data[i + '_returns']) #Tasa de rendimiento cont�nua diaria

#Usamos la funci�n
scenarios(ric, df)        

###################### 3.1.4 VALOR DE LA POSICI�N EN CADA ESCENARIO ##################

#Vamos a multiplicar la cantidad de acciones que tenemos en cada empresa por su
#precio-escenario o estimado.
#De este modo, obtenemos un conjunto de escenarios con los posibles valores estimados
#de nuestra posici�n por emisora para el d�a de ma�ana a partir del d�a de hoy (5 de mayo 2022)
df[['AGUA.MX_val_pos_scenario','GENTERA.MX_val_pos_scenario',
    'PE&OLES.MX_val_pos_scenario','PINFRA.MX_val_pos_scenario',
    'WALMEX.MX_val_pos_scenario']] = np.multiply(n_acciones, df[['AGUA.MX_scenario','GENTERA.MX_scenario',
                                                                 'PE&OLES.MX_scenario','PINFRA.MX_scenario',
                                                                 'WALMEX.MX_scenario']])

#Crear el valor del portafolio por escenario
df['val_portafolio_scenario'] = (df['AGUA.MX_val_pos_scenario'] + df['GENTERA.MX_val_pos_scenario'] + df['PE&OLES.MX_val_pos_scenario'] 
                                + df['PINFRA.MX_val_pos_scenario'] + df['WALMEX.MX_val_pos_scenario'])

###################### 3.1.5 FUNCI�N DE P�RDIDAS ##################

#Determinamos la funci�n de p�rdidas con la diferencia entre la posici�n del escenario estimado a d�a de ma�ana
#y el valor real de la posici�n a d�a de hoy (5 de mayo de 2022). De esta forma, creamos varios
#escenarios de cu�nta fue la p�rdida (o ganancia) de un d�a para otro
#Notemos que las p�rdidas representan valores positivos y las ganancias, negativos.
def loss(columns_names, data, posicion_today): 
    for i in columns_names:
        data[i + '_loss'] = (data[i + '_val_pos_scenario'] - posicion_today[i][0])*-1
        
#Calculamos las p�rdidas        
loss(ric, df, posicion_today)        

#Calculamos p�rdidas por todo el portafolio
df['portafolio_loss'] = (df['AGUA.MX_loss'] + df['GENTERA.MX_loss'] + df['PE&OLES.MX_loss'] 
                         + df['PINFRA.MX_loss'] + df['WALMEX.MX_loss'])

###################### 3.1.6 MATRIZ DE VARIANZAS Y COVARIANZAS ######################

#Matriz de varianzas y covarianzas de los rendimientos continuos diarios
var = np.cov(np.transpose(df.iloc[:,1:6]))

###################### 3.1.7 MATRIZ DE CHOLESKY ######################
#Obtenemos la matriz de Cholesky
cholesky = np.linalg.cholesky(var)
#Verificamos que s� es una "ra�z cuadrada" de la matriz de var y cov
np.dot(cholesky,np.transpose(cholesky))

###################### 3.1.8 SIMULACION NORMAL MULTIVARIADA ######################

#Generamos las simulaciones de la normal estandar multivariada
def sim_normal(columns_names,data):
    for i in columns_names:
        data[i + '_SimNorm'] = np.random.standard_normal(size=1257)
        
sim_normal(ric,df)

###################### 3.1.9 CHOLESKY * N MULT ######################

#Multiplicamos cholesky por la normal
df[['AGUA.MX_cholesky','GENTERA.MX_cholesky','PE&OLES.MX_cholesky',
    'PINFRA.MX_cholesky','WALMEX.MX_cholesky']] = np.transpose(np.dot(cholesky,np.transpose(df[['AGUA.MX_SimNorm','GENTERA.MX_SimNorm','PE&OLES.MX_SimNorm',
                                                                                                'PINFRA.MX_SimNorm','WALMEX.MX_SimNorm']])))

###################### 3.1.10 ESCENARIOS MONTECARLO ######################

#Hacemos una funcion parra crear los escenarios
def scenariosM(columns_names, data):
    for i in columns_names:
        data[i + '_scenarioM'] = today[i][0]*np.exp(np.mean(data[i + '_returns']) - (np.var(data[i + '_returns'])/2) + data[i + '_cholesky'])

#Usamos la funcion
scenariosM(ric, df)

###################### 3.1.11 VALOR DE LA POSICI�N EN EL ESCENARIO MONTECARLO ######################

#Vamos a multiplicar la cantidad de acciones que tenemos en cada empresa por su
#precio escenario o predecido
df[['AGUA.MX_val_pos_scenarioM','GENTERA.MX_val_pos_scenarioM','PE&OLES.MX_val_pos_scenarioM',
    'PINFRA.MX_val_pos_scenarioM','WALMEX.MX_val_pos_scenarioM']] = np.multiply(n_acciones, df[['AGUA.MX_scenarioM', 'GENTERA.MX_scenarioM', 'PE&OLES.MX_scenarioM',
                                                                                              'PINFRA.MX_scenarioM','WALMEX.MX_scenarioM']])
                                                                                               
#Crear el valor de portafolio por escenario Montecarlo
df['val_portafolio_scenarioM'] = (df['AGUA.MX_val_pos_scenarioM'] + df['GENTERA.MX_val_pos_scenario'] + df['PE&OLES.MX_val_pos_scenario']
                                 + df['PINFRA.MX_val_pos_scenarioM'] + df['WALMEX.MX_val_pos_scenarioM'])
 
###################### 3.1.12 FUNCI�N DE P�RDIDAS MONTECARLO ######################

def lossM(columns_names,data, posicion_today):
    for i in columns_names:
        data[i + '_lossM'] = -(data[i + '_val_pos_scenarioM'] - posicion_today[i][0])
        
#Calculamos las perdidas
lossM(ric,df, posicion_today)

#Calculamos perdidas por todo el portafolio con simulaciones Montecarlo
df['portafolio_lossM'] = (df['AGUA.MX_lossM'] + df['GENTERA.MX_loss'] + df['PE&OLES.MX_loss'] 
                          + df['PINFRA.MX_lossM'] + df['WALMEX.MX_lossM'] )



########################### SELECCI�N DE PESOS �PTIMOS POR SIMULACI�N ###########################
#Construimos portafolio de m�nima varianza por simulaciones aleatorias
Rendimientos = rf.dropna()
N_activos = len(Rendimientos.columns)

#Creamos variables vacios donde guardar los resultados
rend_por2 = []
std_por2 = []
W_por2 = []

#Generamos simulaciones de 100,000 portafolios
#El proceso tarda unos minutos

for i in range(0, 100000):
    #Aleatorio uniforme(0,1)
    W = np.random.random(N_activos)
    #Normalizamos activos (suma da 1)
    W /= np.sum(W)
    #Agregamos vector de pesos ala variable a minimizar
    W_por2.append(W)
    #Calculamos rendimiento esperado ponderado por d�a y por escenario
    rend_por2.append(np.dot(Rendimientos.mean(),W))
    #Calculamos desviaci�n est�ndar del rendimiento de un portafolio
    #ponderado aleatoriamente
    std_por2.append(np.sqrt(np.dot(W.T, np.dot(Rendimientos.cov(),W))))

#Guardamos los datos en un dataframe
dic = {'Rendimientos':rend_por2, 'Volatilidad':std_por2}
for i, Activo in enumerate(Rendimientos.columns.tolist()):
        dic[Activo + '_Peso'] = [w[i] for w in W_por2]

#Datos ordenados por simulaci�n
Matriz_Sim = pd.DataFrame(dic) 

#Visualizamos conjunto factible de nuestros posibles portafolios
Matriz_Sim.plot(x = 'Volatilidad', y = 'Rendimientos', kind = 'scatter')      
    
#Del scatterplot, visualizamos que el portafolio que mayor rendimiento otorga (de entre los simulados)
#No es el de mayor riesgo, de hecho, su riesgo es bastante aceptable con respecto a otros portafolios
#que entregan menores rendimientos esperados pero a menor riesgo. 
#Por lo que dado nuestro perfil de inversores moderados, es buena idea elegir este como el portafolio �ptimo.

#Notemos que el portafolio de m�nima varianza o riesgo, nos otorga rendimientos esperados continuos diarios negativos de casi 0
var_min = Matriz_Sim.iloc[Matriz_Sim['Volatilidad'].idxmin()]

#Visualizamos portafolio de m�ximos rendimientos
Port_opt = Matriz_Sim[Matriz_Sim['Rendimientos'] == max(Matriz_Sim['Rendimientos'])]

#Calculamos aumento de varianza del portafolio �ptimo elegido con respecto al de m�nima varianza
max_vol_port2 = Port_opt.iloc[0,1]
min_vol_port2 = var_min.iloc[1,]              
percentagevol = ((max_vol_port2 / min_vol_port2) - 1) * 100

#Calculamos aumento de rendimiento esperado diario cont�nuo con respecto al de m�nima varianza
max_exre_port2 = np.exp(Port_opt.iloc[:,0])
min_exre_port2 = np.exp(var_min.iloc[0,])
percentagered = (( max_exre_port2 / min_exre_port2) - 1) * 100  

#Observamos que quiz� no es tan buena idea optar por el portafolio de m�ximo rendimiento
#debido al pobre aumento de rendimiento con respecto al aumento de variabilidad
#por lo que decidimos optar por el siguiente enfoque:

#Ciclo for que obtiene la frontera eficiente (ineficiente el c�digo pero cumple xd)
Order_df = Matriz_Sim[Matriz_Sim['Volatilidad'] <= max_vol_port2].sort_values('Volatilidad')

#Iniciamos data frame que guardar� portafolios de la frontera eficiente
vv = pd.DataFrame(Order_df.iloc[0,:]).transpose()
j = 0
for i in range(1, len(Order_df)) :
    if Order_df.iloc[i,:][0] > vv.iloc[j,0] : 
        vv = vv.append(Order_df.iloc[i,:])
        j = j + 1;   

#Visualizamos frontera eficiente de nuestros posibles portafolios
vv.plot(x = 'Volatilidad', y = 'Rendimientos', kind = 'scatter') 

#Ajustamos polinomio de grado 4 a la frontera eficiente
def func(x, b, c, d, e, f):
    return (b * (x**4)) + (c * (x**3)) + (d * (x**2)) + (e*x) + f
popt, pcov = sp.optimize.curve_fit(func, vv['Volatilidad'], vv['Rendimientos'])

#Visualizamos frontera eficiente
plt.plot(vv['Volatilidad'], func(vv['Volatilidad'], *popt))
plt.title("Ajuste polinomial de grado 4 a frontera eficiente")
plt.show()

#Calculamos la derivada para obtener los puntos de mayor crecimiento
def derivada_func(x, b, c, d, e):
    return  (b * (4*x**3)) + (c * (3*x**2)) + (d * (2*x)) + (e)

#Graficamos derivada de la funci�n ajustada
plt.plot(vv['Volatilidad'], derivada_func(vv['Volatilidad'], *popt[0:4]))
plt.title("Derivada ajuste polinomial")
plt.xlabel('Volatilidad')
plt.ylabel('Rendimiento')
#Notamos que a una volatilidad de .0125 aproximadamente, la derivada tiene un punto de inflexi�n
#y luego tiene un crecimiento casi nulo, por lo que es buena opci�n tomar esta volatilidad
#como riesgo objetivo para el portafolio �ptimo dado un perfil moderado de riesgo.
 
Port_opt = vv.loc[abs(vv['Volatilidad'] - .0125).idxmin(),:]

#Calculamos aumento de varianza del portafolio �ptimo elegido con respecto al de m�nima varianza
vol_opt = Port_opt.iloc[1,]            
percentagevol = ((vol_opt / min_vol_port2) - 1) * 100

#Calculamos aumento de rendimiento esperado diario cont�nuo con respecto al de m�nima varianza
exre_opt = np.exp(Port_opt.iloc[0,])
percentagered = (( exre_opt/ min_exre_port2) - 1) * 100  

#Extraemos pesos del portafolio cuyo riesgo es aproximadamente 0.0125
Pesos_opt = Port_opt.iloc[2:8,]

#Rendimiento y volatilidad del portafolio 1
WW = np.array([0.2,0.2,0.2,0.2,0.2])

#Calculamos rendimientos esperados del portafolio 1
rend_port1 = np.dot(Rendimientos.mean(),WW)

#Calculamos desviaci�n est�ndar del rendimiento del portafolio 1
std_port1 = np.sqrt(np.dot(WW.T, np.dot(Rendimientos.cov(),WW)))

########################### PORTAFOLIO 2 (Portafolio �ptimo) ###########################

#Determinamos nuestra posici�n (n�mero de acciones) por emisora 
#A d�a de hoy (5 de mayo de 2022). 
#Suponemos s�lo podemos comprar acciones enteras
n_acciones2 = [math.floor((capital*Pesos_opt[0])/today.iloc[0,1]), 
              math.floor((capital*Pesos_opt[1])/today.iloc[0,2]),
              math.floor((capital*Pesos_opt[2])/today.iloc[0,3]),
              math.floor((capital*Pesos_opt[3])/today.iloc[0,4]),
              math.floor((capital*Pesos_opt[4])/today.iloc[0,5])]

#Obtenemos el valor de inversi�n para cada activo
posicion_today2 = np.multiply(n_acciones2, today[ric])

#Obtenemos el valor total de nuestro portafolio hoy 
#Su valor es de aproximadamente un mill�n de pesos
portafolio2 = posicion_today2.iloc[0,].sum()            

###################### 3.2.1 RENDIMIENTOS DIARIOS ##################

#Creamos un nuevo dataframe donde guardaremos todo lo que necesitemos
df2 = pd.DataFrame()
#Agregar la fecha
df2['date'] = raw_data['Date']              

#Usamos la funci�n para calcular los rendimientos cont�nuos diarios 
returns(ric, df2, raw_data)

#Eliminamos los NA's
df2.dropna(inplace = True)

###################### 3.2.2 ESCENARIOS ##################

#Usamos la funci�n para crear escenarios de rendimientos a un d�a 
scenarios(ric, df2)        

###################### 3.2.3 VALOR DE LA POSICI�N EN CADA ESCENARIO ##################

#Vamos a multiplicar la cantidad de acciones que tenemos en cada empresa por su
#precio-escenario o estimado.
#De este modo, obtenemos un conjunto de escenarios con los posibles valores estimados
#de nuestra posici�n por emisora para el d�a de ma�ana a partir del d�a de hoy (5 de mayo 2022)
df2[['AGUA.MX_val_pos_scenario','GENTERA.MX_val_pos_scenario',
    'PE&OLES.MX_val_pos_scenario','PINFRA.MX_val_pos_scenario',
    'WALMEX.MX_val_pos_scenario']] = np.multiply(n_acciones2, df2[['AGUA.MX_scenario','GENTERA.MX_scenario',
                                                                 'PE&OLES.MX_scenario','PINFRA.MX_scenario',
                                                                 'WALMEX.MX_scenario']])

#Crear el valor del portafolio por escenario
df2['val_portafolio_scenario'] = (df2['AGUA.MX_val_pos_scenario'] + df2['GENTERA.MX_val_pos_scenario'] + df2['PE&OLES.MX_val_pos_scenario'] 
                                + df2['PINFRA.MX_val_pos_scenario'] + df2['WALMEX.MX_val_pos_scenario'])

###################### 3.2.4 FUNCI�N DE P�RDIDAS ######################

#Calculamos las p�rdidas mediante funci�n de p�rdidas creadas     
loss(ric, df2, posicion_today2)        

#Calculamos p�rdidas por todo el portafolio
df2['portafolio_loss'] = (df2['AGUA.MX_loss'] + df2['GENTERA.MX_loss'] + df2['PE&OLES.MX_loss'] 
                         + df2['PINFRA.MX_loss'] + df2['WALMEX.MX_loss'])

###################### 3.2.5 SIMULACION NORMAL MULTIVARIADA ######################

#La matriz de varianzas y covarianzas se hace sobre los rendimientos, por lo que es la 
#misma para ambos portafolios

#Usamos funci�n para generar datos normales est�ndar multivariados       
sim_normal(ric,df2)

###################### 3.2.6 CHOLESKY * N MULT ######################

#Multiplicamos cholesky por los datos normales para el portafolio �ptimo
df2[['AGUA.MX_cholesky','GENTERA.MX_cholesky','PE&OLES.MX_cholesky',
    'PINFRA.MX_cholesky','WALMEX.MX_cholesky']] = np.transpose(np.dot(cholesky,np.transpose(df2[['AGUA.MX_SimNorm','GENTERA.MX_SimNorm','PE&OLES.MX_SimNorm',
                                                                                                'PINFRA.MX_SimNorm','WALMEX.MX_SimNorm']])))

###################### 3.2.7 ESCENARIOS MONTECARLO ######################

#Usamos la funcion para crear escenarios Montecarlo para el portafolio �ptimo
scenariosM(ric, df2)

###################### 3.2.8 VALOR DE LA POSICI�N EN EL ESCENARIO MONTECARLO ######################

#Vamos a multiplicar la cantidad de acciones que tenemos en cada empresa por su
#precio escenario o predecido
df2[['AGUA.MX_val_pos_scenarioM','GENTERA.MX_val_pos_scenarioM','PE&OLES.MX_val_pos_scenarioM',
    'PINFRA.MX_val_pos_scenarioM','WALMEX.MX_val_pos_scenarioM']] = np.multiply(n_acciones2, df2[['AGUA.MX_scenarioM', 'GENTERA.MX_scenarioM', 'PE&OLES.MX_scenarioM',
                                                                                              'PINFRA.MX_scenarioM','WALMEX.MX_scenarioM']])
                                                                                               
#Crear el valor de portafolio por escenario Montecarlo
df2['val_portafolio_scenarioM'] = (df2['AGUA.MX_val_pos_scenarioM'] + df2['GENTERA.MX_val_pos_scenario'] + df2['PE&OLES.MX_val_pos_scenario']
                                 + df2['PINFRA.MX_val_pos_scenarioM'] + df2['WALMEX.MX_val_pos_scenarioM'])
 
###################### 3.2.9 FUNCI�N DE P�RDIDAS MONTECARLO ######################

#Calculamos las p�rdidas para el portafolio �ptimo
lossM(ric,df2, posicion_today2)

#Calculamos perdidas por todo el portafolio con simulaciones Montecarlo
df2['portafolio_lossM'] = (df2['AGUA.MX_lossM'] + df2['GENTERA.MX_loss'] + df2['PE&OLES.MX_loss'] 
                          + df2['PINFRA.MX_lossM'] + df2['WALMEX.MX_lossM'] )

###################### 3.3 CALCULO DE LOS VAR's ######################

#Funci�n que calcula VaR al 85% + i% de confianza (i va de 0 a 14) y al 99.5% dependiendo
#el tipo de VaR deseado, ya sea Hist�rico [VaR_type = 'H'], Param�trico [VaR_type = 'P'] o
#MonteCarlo [VaR_type = 'M'].
#La funci�n recibe el dataframe con las p�rdidas simuladas del
#portafolio para el d�a siguiente y el valor del portafolio al d�a actual (6 de mayo de 2022) 
#as� como el n�mero del portafolio usado (1 o 2)
#La funci�n devuelve un dataframe que contiene los distintos VaR' en valor nominal y porcentual
#y grafica la distribuci�n de p�rdidas seg�n el tipo de VaR deseado.
def VaR(VaR_type, data, portafolio,p): 
    VaR_df = pd.DataFrame()
    if VaR_type == 'H':
        for i in range(85, 100):
            VaR_df['VaR_H_' + str(i) + '%']  = [np.percentile(data['portafolio_loss'] , i),
                                                             np.percentile(data['portafolio_loss'] , i) / portafolio * 100]
            if(i == 99): VaR_df['VaR_H_' + str(99.5) + '%'] = [np.percentile(data['portafolio_loss'] , 99.5),
                                                             np.percentile(data['portafolio_loss'] , 99.5) / portafolio * 100]
        sns.histplot(data = data['portafolio_loss'], kde = True, color = 'green')
        plt.xlabel('P�rdida d�aria del portafolio simulada')
        plt.ylabel('Frecuencia')
        plt.title('Histograma de la variable de p�rdidas por m�todo ' + VaR_type + ' para el portafolio ' + str(p))
        plt.grid()
        return(VaR_df)    
    elif VaR_type == 'P':
        
        mu = sum(data['portafolio_loss'])/len(data['portafolio_loss'])
        std = np.std(data['portafolio_loss'])
        for i in range(85, 100):
            VaR_df['VaR_P_' + str(i) + '%']  = [mu + std*norm.ppf(i/100, 0, 1),
                                                             (mu + std*norm.ppf(i/100, 0, 1) )/ portafolio * 100]
            if(i == 99): VaR_df['VaR_P_' + str(99.5) + '%'] = [mu + std*norm.ppf(99.5/100, 0, 1), 
                                                             (mu + std*norm.ppf(99.5/100, 0, 1)) / portafolio * 100]
        sns.histplot(data = (mu + std * np.random.standard_normal(size=1257)), kde = True, color = 'green')
        plt.xlabel('P�rdida d�aria del portafolio simulada')
        plt.ylabel('Frecuencia')
        plt.title('Histograma de la variable de p�rdidas por m�todo ' + VaR_type + ' para el portafolio ' + str(p))
        plt.grid()
        print(mu,std)
        return(VaR_df)
    elif VaR_type == 'M':
        for i in range(85, 100):
            VaR_df['VaR_M_' + str(i) + '%']  = [np.percentile(data['portafolio_lossM'] , i),
                                                             np.percentile(data['portafolio_lossM'] , i) / portafolio * 100]
            if(i == 99): VaR_df['VaR_M_' + str(99.5) + '%'] = [np.percentile(data['portafolio_lossM'] , 99.5),
                                                             np.percentile(data['portafolio_lossM'] , 99.5) / portafolio * 100]
        sns.histplot(data = data['portafolio_lossM'], kde = True, color = 'green')
        plt.xlabel('P�rdida d�aria del portafolio simulada')
        plt.ylabel('Frecuencia')
        plt.title('Histograma de la variable de p�rdidas por m�todo ' + VaR_type + ' para el portafolio ' + str(p))
        plt.grid()
        return(VaR_df) 

#Obtenemos gr�ficas de histogramas y guardamos dataframes
#Para el m�todo param�trico imprime media y desviaci�n est�ndar para comparar
gg = []
for i in ['H','P','M']:
    gg.append(VaR(i,df,portafolio,1))
    plt.show()
for i in ['H','P','M']:
    gg.append(VaR(i,df2,portafolio2,2))
    plt.show()

#Ciclo para obtener valores para eje x
x_lab_var = []
for i in range(85, 100):
    x_lab_var.append(str(i) + '%')
x_lab_var.append('99.5%')
  
#Obtenemos gr�ficas de los VaR's nominales
plt.rc('xtick', labelsize = 7)  
for i, VaR_type in enumerate(['H','P','M','H','P','M']):       
    plt.plot(gg[i].iloc[0,:])  
    plt.xlabel('VaR%')
    plt.xticks(np.arange(16), x_lab_var)
    plt.ylabel('P�rdida Nominal')
    if i <= 2: 
        plt.title('Plot de VaR para distintos niveles de confianza tipo ' + VaR_type + ' para el portafolio ' + str(1))
    else: 
        plt.title('Plot de VaR para distintos niveles de confianza tipo ' + VaR_type + ' para el portafolio ' + str(2))
    plt.grid()
    plt.show()

#Obtenemos gr�ficas de los VaR's porcentuales
for i, VaR_type in enumerate(['H','P','M','H','P','M']):       
    ax = gg[i].iloc[1,:].plot()
    ax.yaxis.set_major_formatter(mtick.PercentFormatter()) 
    plt.xlabel('VaR%')
    plt.xticks(np.arange(16), x_lab_var)
    plt.ylabel('Rendimiento negativo m�ximo esperado un d�a')
    if i <= 2: 
        plt.title('Plot de VaR para distintos niveles de confianza tipo ' + VaR_type + ' para el portafolio ' + str(1))
    else: 
        plt.title('Plot de VaR para distintos niveles de confianza tipo ' + VaR_type + ' para el portafolio ' + str(2))
    plt.grid()
    plt.show()

#Cargamos en un dataframe los distintos valores de VaR para distintos tipos y portafolios
Comparacion = pd.DataFrame(gg[0].iloc[1,:]).reset_index(drop = True).transpose()
for i in range(1,6):
    Comparacion = Comparacion.append(gg[i].iloc[1,:].reset_index(drop = True).transpose())  

#Cargamos �ndices para etiquetas y luego seteamos columna como �ndice
Comparacion['Type VaR'] = ['Hist�rico', 'Param�trico','Montecarlo',
                    'Hist�rico', 'Param�trico','Montecarlo']
Comparacion.set_index('Type VaR', inplace = True)

#Ploteamos en una misma gr�fica los 3 tipos de VaR's correspondientes al portafolio 1
Comparacion.iloc[0:3,:].transpose().plot(legend = True).yaxis.set_major_formatter(mtick.PercentFormatter()) 
plt.xticks(np.arange(16), x_lab_var)
plt.xlabel('VaR%')
plt.ylabel('Rendimiento negativo m�ximo esperado un d�a')
plt.title('Plot de 3 tipos de VaR para distintos niveles de confianza para el portafolio 1')
plt.show()

#Ploteamos en una misma gr�fica los 3 tipos de VaR's correspondientes al portafolio 2
Comparacion.iloc[3:6,:].transpose().plot(legend = True).yaxis.set_major_formatter(mtick.PercentFormatter()) 
plt.xticks(np.arange(16), x_lab_var)
plt.xlabel('VaR%')
plt.ylabel('Rendimiento negativo m�ximo esperado un d�a')
plt.title('Plot de 3 tipos de VaR para distintos niveles de confianza para el portafolio 2')
plt.show()

###################### 3.4 COMPARATIVO CON DATOS REALES DEL D�A SIGUIENTE ######################
#Fecha del "d�a siguiente" con respecto a la fecha valuada
#Note que los d�as 7 y 8 son d�as inh�biles
start_next = '2022-05-09'
end_next = '2022-05-10' 

#Se descarga la informaci�n de los precios de los activos para el 9 de mayo de 2022
next_day = yf.download(ric, start = start_next, end = end_next)['Close'].reset_index()

#Calculamos valor real de los dos portafolios
value_9may_1 = np.multiply(n_acciones, next_day.iloc[:,1:6]).sum(axis = 1)
value_9may_2 = np.multiply(n_acciones2, next_day.iloc[:,1:6]).sum(axis = 1)

#Obtenemos valor de la p�rdida para ambos portafolios a d�a 9 de mayo de 2022
#tanto nominal como porcentual
loss_9may_1 = (value_9may_1 - portafolio) * -1
loss_9may_1 = loss_9may_1.append(((value_9may_1 - portafolio) * -1)/portafolio * 100)
loss_9may_2 = (value_9may_2 - portafolio2) * -1
loss_9may_2 = loss_9may_2.append(((value_9may_2 - portafolio2) * -1)/portafolio2 * 100)

#Ploteamos en una misma gr�fica los 3 tipos de VaR's correspondientes al portafolio 1
Comparacion.iloc[0:3,:].transpose().plot(marker='o').yaxis.set_major_formatter(mtick.PercentFormatter()) 
plt.axhline(y = loss_9may_1.iloc[1,], linestyle = '--')
plt.legend(('Hist�rico', 'Param�trico', 'Montecarlo', "P�rdida al 09/05/2022"),title='Type VaR:')
plt.xticks(np.arange(16), x_lab_var)
plt.xlabel('VaR%')
plt.ylabel('Rendimiento negativo m�ximo esperado un d�a')
plt.title('Plot de 3 tipos de VaR para distintos niveles de confianza para el portafolio 1')
plt.show()

#Ploteamos en una misma gr�fica los 3 tipos de VaR's correspondientes al portafolio 2
Comparacion.iloc[3:6,:].transpose().plot(marker='o').yaxis.set_major_formatter(mtick.PercentFormatter()) 
plt.axhline(y = loss_9may_2.iloc[1,], linestyle = '--')
plt.legend(('Hist�rico', 'Param�trico', 'Montecarlo', "P�rdida al 09/05/2022"),title='Type VaR:')
plt.xticks(np.arange(16), x_lab_var)
plt.xlabel('VaR%')
plt.ylabel('Rendimiento negativo m�ximo esperado un d�a')
plt.title('Plot de 3 tipos de VaR para distintos niveles de confianza para el portafolio 2')
plt.show()

###################### 4 C�LCULO VaR AL 100% Montecarlo ######################

VaR_100_M_1 = [np.percentile(df['portafolio_lossM'] , 100), np.percentile(df['portafolio_lossM'] , 100) / portafolio * 100]

VaR_100_M_2 = [np.percentile(df2['portafolio_lossM'] , 100), np.percentile(df2['portafolio_lossM'] , 100) / portafolio * 100]
