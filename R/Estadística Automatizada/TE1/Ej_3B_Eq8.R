####### 3. Modelos lineales generalizados para datos binarios #######
#Limpieza de entorno
rm(list = ls(all.names = TRUE))
gc()
options(digits=4)

#Importamos librerias
library(dplyr)   # Para el manejo de datos.
library(purrr)   # Para utilizar la función map(), map2(), pmap(), ...
library(ggplot2) # Para gráficas.
library(car)     # Para utilizar la función boxTidwell() y ncvTest().
library(GGally) # Graficas globales
library(broom) #Test normalidad
library(nortest) #Test normalidad (J.B)
library(latex2exp) #Test aleatoriedad
library(DHARMa) #Residuales simulados
library(multcomp) #Pruebas simultaneas 

#La base de datos Preg3B.csv contiene información sobre 862 insectos que fueron expuestos a diferentes dosis
#(Deposit, mg) de tres insecticidas (Insecticide). La asignación a una dosis y al tipo de insecticida se realizó
#de forma aleatoria. Después de seis días se analizó si los insectos se habían muerto, de manera que la base
#de datos contiene también el número de insectos muertos (Killed) y el número total de insectos expuestos
#(Number) por cada dosis e insecticida.

#Cargamos datos
setwd("C:/Users/JOSUE/Documents/Est.Automatizada/TE1")

#Deposit = Dosis de exposición al insecticida
#Insecticide = Insecticida (A, B, C)
#Y = Si el insecto está muerto o no (1-Yes, 0-No)

#Leemos datos, agrupamos y desagregamos para 
#ajustar los modelos
Original3B<- read.csv("Preg3B.csv")
head(Original3B)
Data3B <- read.csv("Preg3B.csv") %>% group_by(Insecticide, Deposit) %>% 
  do(data.frame(unos= rep(1, .$Number), 
                Y = rbind(cbind(rep("Yes", .$Killed)),
                          cbind(rep("No", as.numeric(.$Number) - as.numeric(.$Killed))) 
                )))
summary(Data3B)
Data3B=as.data.frame(Data3B)
#Factorizamos las variables categoricas
Data3B[sapply(Data3B, is.character)] <- lapply(Data3B[sapply(Data3B, is.character)], 
                                               as.factor)
#Omitimos la columna auxiliar de 1's
Data3B <- Data3B[, c(4,1,2)]
summary(Data3B) 
head(Data3B)
levels(Data3B$Insecticide) #La referencia categórica es A
#i)-------------------------------------------------------------------------
# Gráfica de dispersión
ggplot(Original3B, aes(x = Deposit, y = Killed / Number, color = Insecticide)) +
  geom_point() +
  labs(x = "Dosis del Insecticida", y = "Proporción de Insectos Muertos",
       color = "Insecticida")
#ii)-------------------------------------------------------------------------

#Ajustamos modelos con ligas logit, probit y cloglog, para ambas covariables e interacciones
# Eta(Y; X1 = A, X2 = ln(Deposit)) = B0 + B3*X2
# Eta(Y; X1 = B, X2 = ln(Deposit)) = (B0 + B1) + (B3 + B4)*X2
# Eta(Y; X1 = C, X2 = ln(Deposit)) = (B0 + B2) + (B3 + B5)*X2
AjusteLog = glm(Y ~ Insecticide * I(log(Deposit)), family = binomial(link="logit"), data=Data3B)
summary(AjusteLog) 
AIC(AjusteLog)
AjustePro = glm(Y ~ Insecticide * I(log(Deposit)), family = binomial(link="probit"), data=Data3B)
summary(AjustePro) #Modelo con menor AIC

AjusteClog = glm(Y ~ Insecticide * I(log(Deposit)), family = binomial(link="cloglog"), data=Data3B)
summary(AjusteClog) #Modelo con mayor AIC

#Vector de AIC's
c(AIC(AjusteLog),AIC(AjustePro),AIC(AjusteClog))

#Verificamos supuestos de los modelos
#Ajustamos semilla
set.seed(123)

##Ajustamos modelo con liga Logit
AjusteLogRes <- simulateResiduals(fittedModel = AjusteLog)
#Modelo es adecuado en cuanto al supuesto de que el parametro
#parámetro de dispersión es igual a 1
#Residual deviance / degrees of freedom debe ser similar a 1
deviance(AjusteLog)/df.residual(AjusteLog) #Valor cercano a 1
X11()
plot(AjusteLogRes) 
#No se presentan problemas con los supuestos, 
#La bondad de ajuste no se rechaza por lo que
#podríamos decir que no hay evidencia en contra
#de que la distribucion uniforme ajuste a los residuales
#Además de que no se rechaza que se este modelando
#bien la varianza dado el parametro de dispersion
#En cuanto al componente lineal, no parece evidente
#que haya un problema con la linealidad.

#Verificamos linealidad del componente eta, variable por variable
plotResiduals(AjusteLogRes, Data3B$Insecticide) #No se ven problemas de linealidad
X11()
plotResiduals(AjusteLogRes, Data3B$Deposit) #No se ven problemas de linealidad
#Posible problema con la homogeneidad de
#la varianza(Se rechaza varianza constante)

##Ajustamos modelo con liga Probit
AjusteProRes <- simulateResiduals(fittedModel = AjustePro)
#Calculo manual est. parametro de dispersión
deviance(AjustePro)/df.residual(AjustePro) #Valor cercano a 1
X11()
plot(AjusteProRes) 
#No se presentan problemas con los supuestos, 

#Verificamos linealidad del componente eta, variable por variable
plotResiduals(AjusteProRes, Data3B$Insecticide) #No se ven problemas de linealidad
X11()
plotResiduals(AjusteProRes, Data3B$Deposit) #No se ven problemas de linealidad
#Posible problema con la homogeneidad de
#la varianza(Se rechaza varianza constante)

##Ajustamos modelo con liga Cloglog
AjusteClogRes <- simulateResiduals(fittedModel = AjusteClog)
#Calculo manual est. parametro de dispersión
deviance(AjusteClog)/df.residual(AjusteClog) #Valor cercano a 1
X11()
plot(AjusteClogRes) 
#No se presentan problemas con los supuestos
#Sin embargo, la parte de la linealidad se ve peor
#modela con respecto a los otros 2 modelos

#Dado que los modelos con liga logit y probit desempeñan
#tentativamente igual en cuanto al cumplimiento de los supuestos
#y dado la mas sencilla interpretabilidad del modelo logit, eligimos
#trabajar con este ultimo

#Prueba de significancia de la regresion (Similar a prueba ANOVA)
K=matrix(c(0,1,0,0,0,0,
           0,0,1,0,0,0,
           0,0,0,1,0,0,
           0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=5, byrow=TRUE)
m=c(0,0,0,0,0)
summary(glht(AjusteLog, linfct=K, rhs=m), test=Chisqtest())
#Se rechaza H0, lo que implica que se puede proceder
#al análisis del modelo pues las variables en conjunto
#ayudan a modelar la probabilidad de Y
#Es decir, al menos el coeficiente de una variable es 
#diferente de cero en el componente lineal)

###Reduccion de modelo con liga logit
# Eta(Y; X1 = A, X2) = B0 + B3*X2
# Eta(Y; X1 = B, X2) = (B0 + B1) + (B3 + B4)*X2
# Eta(Y; X1 = X, X2) = (B0 + B2) + (B3 + B5)*X2

#Primero verificamos y las 3 curvas son paralelas, es decir:
#Queremos verificar que B3 = (B3 + B4) = (B3 + B5)
#Esto es equivalente a: 0 = B4 = B5
#Si no se rechaza, podriamos optar por un modelo con igualdad de 
#pendientes o rectas paralelas

#Prueba lineal general
K=matrix(c(0,0,0,0,1,0,
           0,0,0,0,0,1), ncol=6, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(AjusteLog, linfct=K, rhs=m), test=Chisqtest())
#No se rechaza que podamos considerar al modelo con pendientes iguales
#Es decir, podemos decir que B4,B5 = 0 
#(No hay evidencia de que las interacciones resulten significativas)

#Para confirmar, realizamos pruebas simultaneas para 
#verificar si las pendientes entre pares de curvas son
#diferentes
K=matrix(c(0,0,0,0,1,0,
           0,0,0,0,0,1,
           0,0,0,0,1,-1), ncol=6, nrow=3, byrow=TRUE)
m=c(0,0,0)
summary(glht(AjusteLog, linfct=K, rhs=m))
#Notamos que no se rechaza que todas las pendientes sean iguales

#Con lo anterior, podemos decir que las 3 rectas ajustadas (dada
#el tipo de insecticida), solo difieren en su ordenada al origen

#Ajustamos modelo reducido
# Eta(Y; X1 = A, X2) = B0 + B3*X2
# Eta(Y; X1 = B, X2) = (B0 + B1) + B3*X2
# Eta(Y; X1 = C, X2) = (B0 + B2) + B3*X2
AjusteLog1 = glm(Y ~ Insecticide + I(log(Deposit)), family = binomial(link="logit"), data=Data3B)
summary(AjusteLog1)
#Con base en el summary, podemos determinar que el insecticida B no aporta
#informacion relevante a la modelacion de la probabilidad dado que las 
#demas variables estan presentes en la misma
#Entonces, como no hay evidencia de que B1!=0, podemos ajustar un
#nuevo modelo reducido bajo dicha consideracion.

#En otras palabras, no se rechaza que NO haya diferencias significativas
#a la hora de modelar el componente lineal de las probabilidad de que un
#insecto No se muera, al aplicar el insecticida A o B.

#Ajustamos segundo modelo reducido
#Renombramos Betas
# Eta(Y; X1 = A, X2) = B0 + B2*X2
# Eta(Y; X1 = B, X2) = B0 + B2*X2
# Eta(Y; X1 = C, X2) = (B0 + B1) + B2*X2
AjusteLog2 = glm(Y ~ I(Insecticide == "C") + I(log(Deposit)), family = binomial(link="logit"), data=Data3B)
summary(AjusteLog2)
AICLogit1 = AIC(AjusteLog2)
#Observamos que ya no se puede reducir el modelo

#Comprobamos si es suficiente o no usar el modelo reducido
#para explicar la probabilidad
anova(AjusteLog, AjusteLog2, test = "Chisq") #No se rechaza H0
#Por lo tanto el modelo reducido es plausible para los datos

#Ajustamos el modelo reducido para las otras ligas
AjustePro2=glm(Y ~ I(Insecticide == "C") + I(log(Deposit)), family = binomial(link="probit"), data=Data3B)
AjusteClog2=glm(Y ~ I(Insecticide == "C") + I(log(Deposit)), family = binomial(link="cloglog"), data=Data3B)
c(AIC(AjusteLog2), AIC(AjustePro2), AIC(AjusteClog2))
#Notamos que la diferencia entre el modelo con liga logit y probit es de un punto AIC
#pero dada la interpretabilidad que nos concede el modelo logit, optamos por este.

#Por lo tanto, las rectas ajustadas son (Con liga logit):
#\hat{Eta}(Y;group=A, Ln(Deposit))=\hat{Eta}(Y;group=B, Ln(Deposit)) = B0 + B2*X2 =
# -4.273 + 2.889*Ln(Deposit)
#\hat{Eta}(Y;group=C, Ln(Deposit)) = (B0 + B1) + B2*X2 =
# (-4.273 + 2.651) + 2.889*Ln(Deposit) = -1.622 + 2.889*Ln(Deposit)

#Se puede observar que el componente lineal eta para el insecticida C es mayor, para toda cantidad de 
#exposicion del insecticida sobre los insectos, que la correspondiente, de los insecticidas A y B.
#Para los 3 grupos, el ln de la exposicion al insecticida afecta en la misma proporcion.
#Aplicando la inversa, que es creciente, podemos determinar que hay un crecimiento en
#la probabilidad de que un insecto muera al aplicar el insecticida C a que si se aplica cualquier otro.

#Analizamos de nuevo los supuestos
AjusteLogRes2 <- simulateResiduals(fittedModel = AjusteLog2)
deviance(AjusteLog2)/df.residual(AjusteLog2) #Valor cercano a 1
X11()
plot(AjusteLogRes2)  #No se presentan problemas con los supuestos

#Verificamos linealidad del componente eta, variable por variable
plotResiduals(AjusteLogRes2, Data3B$Insecticide) #No se ven problemas de linealidad
X11()
plotResiduals(AjusteLogRes2, Data3B$Deposit) #Posible problema con homocedasticidad

#Intervalos simultaneos en escala original
K=matrix(c(1,0,1,
           1,1,1), ncol=3, nrow=2, byrow=TRUE)
ICeta=confint(glht(AjusteLog2, linfct=K), level=.95)

Fg_1 <- family(AjusteLog2)$linkinv
ICmuLI=Fg_1(ICeta$confint[1:2,2])
ICmuLS=Fg_1(ICeta$confint[1:2,3])
Estmu=Fg_1(ICeta$confint[1:2,1])
rbind(ICmuLI,Estmu,ICmuLS)

#Gráfica Resumen
DatosIC=data.frame(t(rbind(ICmuLI,Estmu,ICmuLS)))
DatosIC$x=c(0,1)
DatosIC$X=c("AvB","C")

ggplot(DatosIC, aes(X, Estmu)) + geom_point() + 
  geom_errorbar(aes(ymin = ICmuLI, ymax = ICmuLS))+ theme_bw()

#iii)-------------------------------------------------------------------------

#Adicionamos interaccion de Insecticide con ln(Deposit)^2 a los modelos de ii)
#Ajustamos modelos con ligas logit, probit y cloglog, para ambas covariables e interacciones
# Eta(Y; X1 = A, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B3*X2 + B4*X3
# Eta(Y; X1 = B, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B1) + (B3 + B5)*X2 + (B4 + B7)*X3
# Eta(Y; X1 = C, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B2) + (B3 + B6)*X2 + (B4 + B8)*X3
AjusteLogCua = glm(Y ~ (Insecticide * I(log(Deposit))) + (Insecticide * I(log(Deposit)^2)), 
                   family = binomial(link="logit"), data=Data3B)
summary(AjusteLogCua) 


AjusteProCua = glm(Y ~ (Insecticide * I(log(Deposit))) + (Insecticide * I(log(Deposit)^2)), 
                   family = binomial(link="probit"), data=Data3B)
summary(AjusteProCua) #Modelo con mayor AIC

AjusteClogCua = glm(Y ~ (Insecticide * I(log(Deposit))) + (Insecticide * I(log(Deposit)^2)), 
                    family = binomial(link="cloglog"), data=Data3B)
summary(AjusteClogCua) #Modelo con menor AIC

c(AIC(AjusteLogCua),AIC(AjusteProCua),AIC(AjusteClogCua))
#Verificamos supuestos de los modelos

##Ajustamos modelo con liga Logit
AjusteLogCuaRes <- simulateResiduals(fittedModel = AjusteLogCua)
#Calculo manual est. parametro de dispersión
deviance(AjusteLogCua)/df.residual(AjusteLogCua) #Valor cercano a 1
X11()
plot(AjusteLogCuaRes) 
#No se presentan problemas con los supuestos, 
library(DHARMa)
#Verificamos linealidad del componente eta, variable por variable
plotResiduals(AjusteLogCua, Data3B$Insecticide) #No se ven problemas de linealidad
X11()
plotResiduals(AjusteLogCua, Data3B$Deposit) #No se ven problemas de linealidad
#Posible problema con la homocedasticidad (Se rechaza varianza constante)

##Ajustamos modelo con liga Probit
AjusteProCuaRes <- simulateResiduals(fittedModel = AjusteProCua)
#Calculo manual est. parametro de dispersión
deviance(AjusteProCua)/df.residual(AjusteProCua) #Valor cercano a 1
X11()
plot(AjusteProCuaRes) 
#No se presentan problemas con los supuestos, 

#Verificamos linealidad del componente eta, variable por variable
plotResiduals(AjusteProCuaRes, Data3B$Insecticide) #No se ven problemas de linealidad
X11()
plotResiduals(AjusteProCuaRes, Data3B$Deposit) #No se ven problemas de linealidad
#Posible problema con la homogeneidad de
#la varianza(Se rechaza varianza constante)

##Ajustamos modelo con liga Cloglog
AjusteClogCuaRes <- simulateResiduals(fittedModel = AjusteClogCua)
#Calculo manual est. parametro de dispersión
deviance(AjusteClogCua)/df.residual(AjusteClogCua) #Valor cercano a 1
X11()
plot(AjusteClogCuaRes) 
#No se presentan problemas con los supuestos

#Verificamos linealidad del componente eta, variable por variable
plotResiduals(AjusteClogCuaRes, Data3B$Insecticide) #No se ven problemas de linealidad
X11()
plotResiduals(AjusteClogCuaRes, Data3B$Deposit) #No se ven problemas de linealidad
#Posible problema con la homogeneidad de
#la varianza(Se rechaza varianza constante)

#Notamos que la adicion del termino cuadrado y sus interacciones con la variable categorica
#conllevaron una mejora sustancial en el cumplimiento del supuesto de linealidad y una
#mejora en el supuesto de distribucion uniforme de los residuales.
#Ante esto, observamos que los 3 modelos cumplen perfectamente con los supuestos, pero dado
#el AIC que solo posee una diferencia de 0.5 unidades con respecto al modelo con menor AIC
#,que en este caso fue el cloglog, y la mejor interpretabilidad del modelo logit.
#Elegimos quedarnos con este.


#Prueba de significancia de la regresion (Similar a prueba ANOVA)
K=matrix(c(0,1,0,0,0,0,0,0,0,
           0,0,1,0,0,0,0,0,0,
           0,0,0,1,0,0,0,0,0,
           0,0,0,0,1,0,0,0,0,
           0,0,0,0,0,1,0,0,0,
           0,0,0,0,0,0,1,0,0,
           0,0,0,0,0,0,0,1,0,
           0,0,0,0,0,0,0,1,0), ncol=9, nrow=8, byrow=TRUE)
m=c(0,0,0,0,0,0,0,0)
summary(glht(AjusteLogCua, linfct=K, rhs=m), test=Chisqtest())
#Se rechaza H0, lo que implica que se puede proceder
#al análisis del modelo.

###Reduccion de modelo con liga logit
# Eta(Y; X1 = A, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B3*X2 + B4*X3
# Eta(Y; X1 = B, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B1) + (B3 + B5)*X2 + (B4 + B7)*X3
# Eta(Y; X1 = C, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B2) + (B3 + B6)*X2 + (B4 + B8)*X3

#Primero verificamos si las 3 curvas son paralelas dado X3, es decir:
#Queremos verificar que B4 = (B4 + B7) = (B4 + B8)
#Esto es equivalente a: 0 = B7 = B8
#Si no se rechaza, podriamos optar por un modelo con igualdad de 
#pendientes o rectas paralelas

#Prueba lineal general
K=matrix(c(0,0,0,0,0,0,0,1,0,
           0,0,0,0,0,0,0,0,1), ncol=9, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(AjusteLogCua, linfct=K, rhs=m), test=Chisqtest())
#No se rechaza que podamos considerar al modelo con pendientes iguales
#Es decir, podemos decir que B7,B8 = 0 

#Ajustamos Modelo Reducido
# Eta(Y; X1 = A, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B3*X2 + B4*X3
# Eta(Y; X1 = B, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B1) + (B3 + B5)*X2 + B4*X3
# Eta(Y; X1 = C, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B2) + (B3 + B6)*X2 + B4*X3
AjusteLogCuaSP1 = glm(Y ~ (Insecticide * I(log(Deposit))) + (Insecticide + I(log(Deposit)^2)), 
                      family = binomial(link="logit"), data=Data3B)
summary(AjusteLogCuaSP1) 

#Ahora verificamos si las 3 curvas son paralelas dado X2, es decir:
#Queremos verificar que B3 = (B3 + B5) = (B3 + B6) <-> 0 = B5 = B6

#Prueba lineal general
K=matrix(c(0,0,0,0,0,1,0,
           0,0,0,0,0,0,1), ncol=7, nrow=2, byrow=TRUE)
m=c(0,0)
summary(glht(AjusteLogCuaSP1, linfct=K, rhs=m), test=Chisqtest())
#No se rechaza que podamos considerar al modelo con pendientes iguales
#Es decir, podemos decir que B5,B6 = 0 

#Ajustamos segundo Modelo Reducido
# Eta(Y; X1 = A, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B3*X2 + B4*X3
# Eta(Y; X1 = B, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B1) + B3*X2 + B4*X3
# Eta(Y; X1 = C, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B2) + B3*X2 + B4*X3
AjusteLogCuaSP2 = glm(Y ~ Insecticide + I(log(Deposit)) + I(log(Deposit)^2), 
                      family = binomial(link="logit"), data=Data3B)
summary(AjusteLogCuaSP2) 
#Con base en el summary, podemos determinar que el insecticida B no aporta
#informacion relevante al modelo así que podemos hacer B1 = 0.

#Ajustamos el tercer Modelo Reducido
#Renombramos Betas
# Eta(Y; X1 = A, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B2*X2 + B3*X3
# Eta(Y; X1 = B, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B2*X2 + B3*X3
# Eta(Y; X1 = C, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B1) + B2*X2 + B3*X3
AjusteLogCuaSP3 = glm(Y ~ I(Insecticide == "C") + poly(log(Deposit),2, raw = TRUE), 
                      family = binomial(link="logit"), data=Data3B)
summary(AjusteLogCuaSP3) #Observamos que ya no se puede reducir el modelo
#Guardamos AIC
AICLogCuaSP3 = AIC(AjusteLogCuaSP3)
summary(AjusteLogCua)
#Comprobamos si es suficiente o no usar el modelo reducido
#para explicar la probabilidad
anova(AjusteLogCua, AjusteLogCuaSP3, test = "Chisq") #No se rechaza H0
#Por lo tanto el modelo reducido es plausible para los datos

#Ajustamos el modelo reducido para las otras ligas
AjusteProCuaSP3=glm(Y ~ I(Insecticide == "C") + poly(log(Deposit),2, raw = TRUE), 
                    family = binomial(link="probit"), data=Data3B)
AjusteClogCuaSP3=glm(Y ~ I(Insecticide == "C") + poly(log(Deposit),2, raw = TRUE), 
                     family = binomial(link="cloglog"), data=Data3B)
c(AIC(AjusteLogCuaSP3), AIC(AjusteProCuaSP3), AIC(AjusteClogCuaSP3))
#Notamos que no hay diferencia entre el modelo con liga logit y probit; además de 
#ambos tener el menor AIC, pero dada la interpretabilidad que nos concede el modelo 
#logit, optamos por este.

#Por lo tanto, las rectas ajustadas son (Con liga logit):
#\hat{Eta}(Y;Insecticide=A, Ln(Deposit), Ln(Deposit)^2)=
#\hat{Eta}(Y;Insecticide=B, Ln(Deposit), Ln(Deposit)^2) = B0 + B2*X2 + B3*X3 =
# -6.820 + 6.891*Ln(Deposit) + -1.434*Ln(Deposit)^2
#\hat{Eta}(Y;Insecticide=C, Ln(Deposit), Ln(Deposit)^2) = (B0 + B1) + B2*X2 + B3*X3  =
# (-6.820 + 2.804) + 6.891*Ln(Deposit) + -1.434*Ln(Deposit)^2

#Dado que el coeficiente B1 es positivo (que es la única diferencia entre las curvas),
#es de esperar que haya un crecimiento en la probabilidad de que un insecto muera al 
#aplicar el insecticida C a que si se aplica cualquier otro.

#Analizamos de nuevo los supuestos
AjusteLogCuaSPRes <- simulateResiduals(fittedModel = AjusteLogCuaSP3)
deviance(AjusteLogCuaSP3)/df.residual(AjusteLogCuaSP3) #Valor cercano a 1
X11()
plot(AjusteLogCuaSPRes)  #No se presentan problemas con los supuestos

#Verificamos linealidad del componente eta, variable por variable
plotResiduals(AjusteLogCuaSPRes, Data3B$Insecticide) #No se ven problemas de linealidad
X11()
plotResiduals(AjusteLogCuaSPRes, Data3B$Deposit) #Posible problema con homocedasticidad
#iV)-------------------------------------------------------------------------
#Modelo más adecuado
#Dado que la adicion del termino cuadrado al componente lineal mejoro suficientemente
#el cumplimiento del supuesto de linealidad y el de bondad de ajuste, aunado a que
#este modelo posee un menor AIC a pesar de contener una covariable mas.
#Consideramos que dicho modelo es el mejor
#Comparacion AIC's
c(AICLogit1,AICLogCuaSP3)

#a. Grafico con estimacion puntual

#Combinaciones posibles
head(Original3B[,4:5])

#Rectas generadas para cada Insecticida
Coef = unname(coef(AjusteLogCuaSP3))

EstCurveAB = function(x){
  ProbKill = exp(Coef[1]+Coef[3]*log(x)+Coef[4]*log(x)^2)/
    (1+exp(Coef[1]+Coef[3]*log(x)+Coef[4]*log(x)^2))
  return(ProbKill)
}

EstCurveC = function(x){
  ProbKill = exp(Coef[1]+Coef[2]+Coef[3]*log(x)+Coef[4]*log(x)^2)/
    (1+exp(Coef[1]+Coef[2]+Coef[3]*log(x)+Coef[4]*log(x)^2))
  return(ProbKill)
}

#Obtenemos estimaciones puntuales y agrupamos por categoricas
Probas = predict(AjusteLogCuaSP3, Original3B[,4:5], type = "response")
Probas = data.frame(Deposit = Original3B[,5], Insecticide = Original3B[,4], 
                    Est = Probas)
Probas$TypeCurve <- ifelse(Probas$Insecticide %in% c("A", "B"), "AB", "C")
Probas$Estimacion <- ifelse(Probas$Insecticide %in% c("A", "B"), "Est. AB", "Est. C")

#Grafica de dispersion con datos originales, curvas de regresion y
#estimaciones puntuales
ggplot(Original3B, aes(x = Deposit, y = Killed / Number, color = Insecticide)) +
  geom_point(size = 3, shape = 1) +
  geom_function(fun = EstCurveAB, aes(colour = "Insecticida A y B")) +
  geom_function(fun = EstCurveC, aes(colour = "Insecticida C")) +
  geom_point(data = Probas, aes(x = Deposit, y = Est, shape = Estimacion), size = .9) +
  labs(x = "Dosis del Insecticida", y = "Proporción de Insectos Muertos",
       color = "Insecticida") + 
  theme_bw()

#b. Dosis mínima para cada insecticida con la que se puede indicar que
#el 75 % de los insectos se muere.

#Usando modelo sin reducir (modelo completo)
# Eta(Y; X1 = A, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B3*X2 + B4*X3
# Eta(Y; X1 = B, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B1) + (B3 + B5)*X2 + (B4 + B7)*X3
# Eta(Y; X1 = C, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B2) + (B3 + B6)*X2 + (B4 + B8)*X3
summary(AjusteLogCua)
#Coeficientes
Bs = unname(coef(AjusteLogCua))

#Funcion liga inversa
Estimation = function(x, Insecticide){
  if(Insecticide == "A"){
    ProbKill = exp(Bs[1]+Bs[4]*log(x)+Bs[5]*log(x)^2)/
      (1+exp(Bs[1]+Bs[4]*log(x)+Bs[5]*log(x)^2))
    return(ProbKill)
  }else if(Insecticide == "B"){
    ProbKill = exp((Bs[1]+Bs[2])+(Bs[4]+Bs[6])*log(x)+(Bs[5]+Bs[8])*log(x)^2)/
      (1+exp((Bs[1]+Bs[2])+(Bs[4]+Bs[6])*log(x)+(Bs[5]+Bs[8])*log(x)^2))
    return(ProbKill)
  }else{
    ProbKill = exp((Bs[1]+Bs[3])+(Bs[4]+Bs[7])*log(x)+(Bs[5]+Bs[9])*log(x)^2)/
      (1+exp((Bs[1]+Bs[3])+(Bs[4]+Bs[7])*log(x)+(Bs[5]+Bs[9])*log(x)^2))
    return(ProbKill)
  }
}

#Funcion de diferencia
Diff = function(x, Insecticide, Prob){
  return(Estimation(x, Insecticide) - Prob)
}

#Encontrar la raíz de la función de diferencia para Insecticida A
Roots <- list()
for (prob in seq(.5, .75, .0001)) {
  result <- try(uniroot(Diff, interval = c(2, 8.1), Insecticide = "A", prob)$root, silent = TRUE)
  Roots <- append(Roots, result)
}
RootA = max(unlist(Roots[sapply(Roots, is.numeric)]))

#Encontrar la raíz de la función de diferencia para Insecticida B
RootB = uniroot(Diff, interval = c(2, 8.1), Insecticide = "B", 0.75)$root
#Encontrar la raíz de la función de diferencia para Insecticida C
RootC = uniroot(Diff, interval = c(2, 8.1), Insecticide = "C", 0.75)$root

#Comprobacion Estimaciones
Estimation(RootA, "A")
Estimation(RootB, "B")
Estimation(RootC, "C")

#Dosis minima con el que el 75% de los insectos se muere:
c(RootA, RootB, RootC)


#Usando modelo Reducido
# Eta(Y; X1 = AvB, X2 = ln(Deposit), X3 = ln(Deposit)^2) = B0 + B2*X2 + B3*X3
# Eta(Y; X1 = C, X2 = ln(Deposit), X3 = ln(Deposit)^2) = (B0 + B1) + B2*X2 + B3*X3
summary(AjusteLogCuaSP3)

Bs = unname(coef(AjusteLogCuaSP3))

#Funcion liga inversa
Estimation = function(x, Insecticide){
  if(Insecticide == "AvB"){
    ProbKill = exp(Bs[1]+Bs[3]*log(x)+Bs[4]*log(x)^2)/
      (1+exp(Bs[1]+Bs[3]*log(x)+Bs[4]*log(x)^2))
    return(ProbKill)
  }else{
    ProbKill = exp((Bs[1]+Bs[2])+Bs[3]*log(x)+Bs[4]*log(x)^2)/
      (1+exp((Bs[1]+Bs[2])+Bs[3]*log(x)+Bs[4]*log(x)^2))
    return(ProbKill)
  }
}

#Funcion de diferencia
Diff = function(x, Insecticide, Prob){
  return(Estimation(x, Insecticide) - Prob)
}

#Encontrar la raíz de la función de diferencia para Insecticida A o B
RootAvB = uniroot(Diff, interval = c(2, 8), Insecticide = "AvB", 0.75)$root
#Encontrar la raíz de la función de diferencia para Insecticida C
RootC = uniroot(Diff, interval = c(2, 8), Insecticide = "C", 0.75)$root

Estimation(RootAvB, "AvB")
Estimation(RootC, "C")

#Dosis minima con el que el 75% de los insectos se muere
#Modelo Reducido
c(RootAvB, RootC)

#c. ¿se puede indicar que un insecticida es el mejor? (Considerando la menor de
#las dosis encontradas)
#Sí

#Menor dosis encontrada
MinRoot = min(c(RootAvB, RootC))

#Planteamos la prueba de hipotesis de que la probabilidad de matar a un insecto 
#con el insecticida C es mayor que al usar el insecticida A o B
#H0: eta(Y; X1 = AvB, X2 = ln(2.503), X3 = ln(2.503)^2) >= eta(Y; X1 = C, X2 = ln(2.503), X3 = ln(2.503)^2)
#vs
#Ha: eta(Y; X1 = AvB, X2 = ln(2.503), X3 = ln(2.503)^2) < eta(Y; X1 = C, X2 = ln(2.503), X3 = ln(2.503)^2)
#<->
#H0: B0 + B2*ln(2.503) + B3*ln(2.503)^2 >= (B0 + B1) + B2*ln(2.503) + B3*ln(2.503)^2
#Ha: B0 + B2*ln(2.503) + B3*ln(2.503)^2 < (B0 + B1) + B2*ln(2.503) + B3*ln(2.503)^2
#<->
#H0: 0 >= B1
#Ha: 0 < B1

K=matrix(c(0,1,0,0), ncol=4, nrow=1, byrow=TRUE)
m=c(0)
summary(glht(AjusteLogCuaSP3, linfct=K, rhs=m, alternative="greater"))
#Se rechaza H0, entonces, bajo un nivel de significancia del %5 podemos decir
#que la probabilidad de matar un insecto con el insecticida C es mayor 
#que al usar el A o el B, notando que la inversa de la funcion liga es creciente
#por lo cual se mantiene la relacion de orden.

#Intervalos simultaneos en escala original
#Modelo Reducido
K=matrix(c(1,0,1,1,
           1,1,1,1), ncol=4, nrow=2, byrow=TRUE)
ICeta=confint(glht(AjusteLogCuaSP3, linfct=K), level=.95)

Fg_1 <- family(AjusteLogCuaSP3)$linkinv
ICmuLI=Fg_1(ICeta$confint[1:2,2])
ICmuLS=Fg_1(ICeta$confint[1:2,3])
Estmu=Fg_1(ICeta$confint[1:2,1])
rbind(ICmuLI,Estmu,ICmuLS)

#Gráfica Resumen
DatosIC=data.frame(t(rbind(ICmuLI,Estmu,ICmuLS)))
DatosIC$x=c(0,1)
DatosIC$X=c("AvB","C")

ggplot(DatosIC, aes(X, Estmu)) + geom_point() + 
  geom_errorbar(aes(ymin = ICmuLI, ymax = ICmuLS))+ theme_bw()
#De la grafica que se puede concluir que el insecticida C probe una mayor
#probabilidad de matar un insecto que los insecticidas A y B

#d. ¿se puede indicar que los insecticidas A y B tienen un desempeño similar? 
#Sí, anexo en iii)
