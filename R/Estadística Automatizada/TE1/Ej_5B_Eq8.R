####### 5. Modelos lineales generalizados para datos categoricos #######
#Limpieza de entorno
rm(list = ls(all.names = TRUE))
gc()
options(digits=4)

#Importamos librerias
library(purrr)   # Para utilizar la función map(), map2(), pmap(), ...
library(ggplot2) # Para gráficas.
library(car)     # Para utilizar la función boxTidwell() y ncvTest().
library(GGally) # Graficas globales
library(broom) #Test normalidad
library(nortest) #Test normalidad (J.B)
library(latex2exp) #Test aleatoriedad
library(DHARMa) #Residuales simulados
library(multcomp) #Pruebas simultaneas 
library(dplyr)   # Para el manejo de datos.
library(lessR)  #BarChart
library(VGAM) #Regresion multinomial
#La base de datos Preg5.csv contiene informacion sobre el nivel de 
#satisfaccion (Sat) de un conjunto de individuos que rentan una 
#vivienda. El interes es identificar si entre los factores que 
#definen este nivel estan: 
#el tipo de vivienda (Type), 
#la percepción sobre su influencia en las decisiones sobre el 
#mantenimiento de la vivienda (Infl) y 
#el contacto que tienen con el resto de inquilinos (Cont).

#Cargamos datos
setwd("C:/Users/JOSUE/Documents/Est.Automatizada/TE1")
#Seleccionamos variables de interes y factorizamos
#Y = Sat; (Tomamos como referencia la categoria High)
#X = (Type, Infl, Cont) (Las respectivas referencias son:
#                        Apartment, High, High)
Data5B <- read.csv("Preg5.csv") %>% dplyr::select(-X)
#Factorizamos
Data5B[sapply(Data5B, is.character)] <- lapply(Data5B[sapply(Data5B, is.character)], 
                                               as.factor)
#Agrupamos datos en todas las combinanciones posibles
count_Data5B= Data5B %>% group_by(Type, Infl, Cont, Sat)  %>% count()
#Resumen de Datos
summary(Data5B)
str(Data5B)
head(count_Data5B)
#i)-------------------------------------------------------------------------
# Agrupamos los datos por trio de filas y calculamos las frecuencias relativas
Data5B_Probs <- count_Data5B %>%
  group_by(Type, Infl, Cont) %>% summarise(P = n / sum(n)) %>% 
  ungroup %>% mutate(Sat = count_Data5B$Sat) %>% relocate(Sat, .after = Cont)
#Notemos que son 72 categorias posibles en total
print(Data5B_Probs, n=72)
#Grafico de Barras Apilado por cada categoria
#Copiamos data
Data5BGrap = Data5B

#Ajustamos levels para que quepan en el grafico
levels(Data5BGrap$Infl) <- list(iH  = "High", iL = "Low", iM = "Medium")
levels(Data5BGrap$Cont) <- list(cH  = "High", cL = "Low")
levels(Data5BGrap$Type) <- list(Apa  = "Apartment", Atr = "Atrium", Ter = "Terrace", Tow = "Tower")
#Agrupamos categorias unicas
Data5BGrap$TIC = factor(paste(Data5BGrap$Type,Data5BGrap$Infl,Data5BGrap$Cont, sep="."))
#Guardamos variables y cambiamos orden de levels
Type.Infl.Cont = forcats::fct_rev(Data5BGrap$TIC)
Sat = Data5BGrap$Sat

#Graficamos
X11()
BarChart(Type.Infl.Cont , by = Sat, stack100 = TRUE,
         color="black", fill="Okabe-Ito", value_size=1.2,
         transparency=0.26, horiz=TRUE, quiet = TRUE)
#Observamos que la preponderancia es que la mayoria de los encuestados
#estan satisfechos, especialmente para aquellos que poseen apartamentos
#En cambio, para los que poseen terraza no es el caso.
#ii)-------------------------------------------------------------------------
#Convertimos a dataframe el tibble
Data5B = data.frame(Data5B)
#Ajuste de modelo logístico multinomial considerando todas las 
#posibles interacciones entre Type, Infl y Cont.
Ajuste5B_Com <- vglm(Sat ~ Infl*Type*Cont,
                     family = multinomial(refLevel = "High"),
                     data = Data5B)
coef(Ajuste5B_Com, matrix = TRUE)

#Ajuste de modelo sin interacciones
Ajuste5B_Red <- vglm(Sat ~ Infl+Type+Cont,
                     family = multinomial(refLevel = "High"),
                     data = Data5B)
coef(Ajuste5B_Red, matrix = TRUE)

#Ajuste de modelo auxiliar para la prueba de significancia de
#la regresion
Ajuste5B_Aux <- vglm(Sat ~ 1,
                     family = multinomial(refLevel = "High"),
                     data = Data5B)
coef(Ajuste5B_Aux, matrix = TRUE)

#Prueba de significancia
anova(Ajuste5B_Com, Ajuste5B_Aux, type = "I")
#Se rechaza H0, es decir, al menos una de las variables
#del modelo completo aporta suficiente informacion

#Comprobamos si el modelo reducido es suficiente para
#el modelado. Observemos que el conjunto de Betas del
#modelo reducido es un subconjunto del completo (anidado)
anova(Ajuste5B_Com, Ajuste5B_Red, type = "I", test = "LRT")
#No se rechaza la hipotesis nula, es decir, 
#no hay evidencia de que las betas que solo se 
#encuentran presentes en el modelo completo
#sean diferenres de cero
#Por lo tanto, podemos quedarnos con el modelo
#reducido, es decir, no hay correlacion
#entre el tipo de departamento, influencia y contacto  
AIC5B_Red = AIC(Ajuste5B_Red)
AIC5B_Com = AIC(Ajuste5B_Com)
BIC5B_Red = BIC(Ajuste5B_Red)
BIC5B_Com= BIC(Ajuste5B_Com)
c(AIC5B_Com,AIC5B_Red,BIC5B_Com,BIC5B_Red)
#Notamos una sustancial mejora en los índices del
#modelo completo al modelo reducido, especialmente
#en el BIC que da mayor penalizacion a un mayor
#numero de covariables
#Por lo tanto, es optimo usar el modelo reducido
#iii)-------------------------------------------------------------------------
#Dado que la variable Sat toma valores: (High, Low, Medium),
#podriamos considerarla una variable categorica ordinal y con ello
#usar modelos logisticos acumulativos para modelar la probabilidad

#Ajustamos modelo reducido considerando que la variable Sat es una variable
#categorica ordenada (No tomamos en cuenta supuesto de curvas paralelas)
Ajuste5B_Ord <- vglm(Sat ~ Type + Infl + Cont,
                     family = cumulative(parallel = FALSE),
                     data = Data5B)
coef(Ajuste5B_Ord, matrix = TRUE)

#Creamos dataframe con coeficientes
coef_matrix <- coef(Ajuste5B_Ord, matrix = TRUE) # Guardar la salida en una variable
coef_df <- as.data.frame(coef_matrix) # Transformar en data.frame
# Dar nombres a las columnas
names(coef_df) <- c("P[Y<=1]", "(P[Y<=2]")
# Agregar una columna para los nombres de las variables predictoras
coef_df$Beta <- rownames(coef_df)
# Reiniciar los índices del data.frame
row.names(coef_df) <- NULL
# Mostrar el data.frame
coef_df

#Guardamos AIC
AIC5B_Ord = AIC(Ajuste5B_Ord)
BIC5B_Ord = BIC(Ajuste5B_Ord)

#Ajustamos modelo reducido considerando que la variable Sat es una variable
#categorica ordenada y dando por hecho supuesto de curvas paralelas
Ajuste5B_OrdP <- vglm(Sat ~ Type + Infl + Cont,
                      family = cumulative(parallel = TRUE),
                      data = Data5B)
coef(Ajuste5B_OrdP, matrix = TRUE)
#Notemos que los coeficientes son iguales tango para P(Y <= 1) como
#P(Y<=2)

#Hacemos prueba anova para ver si modelo con supuesto de curvas paralelas
#es suficiente para modelar la Probabilidad. Observemos que este modelo
#esta anidado en el que no toma en cuenta el supuesto
anova(Ajuste5B_Ord, Ajuste5B_OrdP, type = "I", test = "LRT")
#Se rechaza H0, es decir, que hay evidencia suficiente para decir
#que el modelo mas completa aporta informacion necesaria para el 
#modelado de la probabilidad

#Ante esta situacion, si elegimos el modelo con curvas paralelas, podriamos
#perder efectos importantes aportados por los coeficientes adjuntos al
#componente lineal de  P(Y <= 2), en el modelado. Por lo que, decidimos
#finalmente, comparar el modelo reducido de ii) y con el supuesto de que
#la variable categorica es ordenada.
#Note que, como ambos modelos poseen mismo numero de coeficientes, solo es
#necesario comparar con la metrica AIC para determinar cual posee mejor
#bondad de ajuste.

#Comparacion de AIC entre modelo sin Y categorica ordenada vs 
#modelo con Y categorica ordenada
c(AIC5B_Red, AIC5B_Ord)
#Dado que el modelo multinomial sin aplicar que la variable Y es ordinal,
#posee menor AIC, obtamos por usar este para modelar la probabilidad
#iv)-------------------------------------------------------------------------
#Coeficientes de modelo logistico multinomial
#la referencia pi1 = High y pi2 = Low, pi3 = Medium
coefi = coef(Ajuste5B_Red, matrix = TRUE)
#Obtengamos probas cuando infl = "High", Type = "Apartment" y Cont = "High"
#eta(Y = 2; infl = "High", Type = "Apartment" y Cont = "High" ) =
#^(2) expresa el superindice
# pi2 = exp(B0^(2))/(1+exp(B0^(2)) + exp(B0^(3)))
B0s <- c(coefi[1,1], coefi[1,2])
pi2_AHH = exp(coefi[1,1]) / (1 + sum(exp(B0s)))
pi2_AHH
# pi3 = exp(B0^(3))/(1+exp(B0^(3)) + exp(B0^(3)))
pi3_AHH = exp(coefi[1,2]) / (1 + sum(exp(B0s)))
pi3_AHH
pi1_AHH = 1 - (pi3_AHH + pi2_AHH)
pi1_AHH

#Obtenemos todas las probabilidades estimadas
#Combinaciones unicas
Combis = unique(Data5B[c(3,2,4)]) %>% group_by(Type, Infl, Cont) %>% arrange(Type, .by_group=TRUE)
#Obtenemos valores estimados a escala original
Probas5B = predict(Ajuste5B_Red, Combis, type = "response")
#Convertimos datos a dataframe de una sola columna y renombramos
Probas = data.frame(as.vector(t(Probas5B)))
Probas = rename(Probas, from = as.vector.t.Probas5B.., to = Probabilidad)

#Obtenemos valores de las etiquetas por cada categoria
Combination = Data5BGrap %>% unique () %>% group_by(TIC) %>% 
  select(TIC) %>% as.data.frame() %>% arrange(TIC)
#Creamos dataframe con todo lo anterior y agregamos variable Sat
Estimation = cbind(Combination, Probas)
Estimation = Estimation %>% mutate(Sat = rep(c("High", "low", "Medium"), 24))

#Graficamos barplot con las 72 categorias posibles y su estimacion
X11()
ggplot(Estimation, aes(y = forcats::fct_rev(TIC), 
                       x = Probabilidad, fill = forcats::fct_rev(Sat), 
                       label = paste0(as.character(round(Probabilidad*100,0)), "%"))) + 
  geom_bar(position='stack', stat='identity', width = 0.8, colour = "black", alpha = .65) + 
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white") +
  ggtitle("Estimación Puntual") +
  labs(subtitle ="Probabilidad por cada categoria dada la Satisfacción") +
  labs(y ="Type.Infl.Cont", x = "Probabilidad") + 
  scale_x_continuous(labels = scales::percent) +
  labs(fill = "Satisfacción") +
  theme_minimal()
#Interpretacion en el rmarkdown
#Fin owo

