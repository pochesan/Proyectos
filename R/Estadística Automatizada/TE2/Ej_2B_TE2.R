######## Ejercicio 2 ##########
#Limpiamos entorno
rm(list = ls(all.names = TRUE))
gc()

## Datos
library(mlbench)

## Manejo y limpieza de datos
library(dplyr)
library(tidyr)
library(forcats)
library(broom)

## Gráficas
library(ggplot2)
library(GGally)
library(ggpubr)
library(corrplot)
library(RColorBrewer)

## Selección de variables
library(MASS)
library(bestglm)
library(glmnet)

# Verificación de supuestos
library(DHARMa)

#Datos corregidos del National Institute of 
#Diabetes and Digestive and Kidney Diseases
help("PimaIndiansDiabetes2")
data(PimaIndiansDiabetes2)
#Observamos que el dataset contiene
#768 observaciones, 8 variables
#independientes y la variable dependiente
#binaria

#Xi (Continuas)
#pregnant-Number of times pregnant
#glucose-Plasma glucose concentration (glucose tolerance test)
#pressure-Diastolic blood pressure (mm Hg)
#triceps-Triceps skin fold thickness (mm)
#insulin-2-Hour serum insulin (mu U/ml)
#mass-Body mass index (weight in kg/(height in m)\^2)
#pedigree-Diabetes pedigree function
#age-Age (years)

#Yi (Bernoulli)
#diabetes-Class variable (test for diabetes)
#2- neg - negativo (Referencia)
#2-pos - positivo

#Guardamos Datos
Datos = PimaIndiansDiabetes2
#Borramos Database original del enviroment
rm(PimaIndiansDiabetes2)
#Resumen de variables
str(Datos)#Detectamos NA's
summary(Datos) #Todas las covariables son numericas

#Preprocesamiento
#Calculamos NA's
sum(apply(t(!is.na(Datos)),2, function(x) all(x))) #Se conservan 392 observaciones
length(na.omit(Datos)$pregnant) #Confirmamos
#Eliminamos NA's
DatosL = na.omit(Datos)
#DatosL$diabetes <-factor(DatosL$diabetes,levels = c("0","1"),labels = c("No","Yes"))

#Resumen de variables
summary(DatosL)
str(DatosL)

#Gráficas
#Gráfica de conteo de diabetes
data.frame(table(DatosL$diabetes))  %>% 
  rename("Diabetes" = "Var1",
         "Frecuencia" = "Freq") %>%
  ggplot(aes(Diabetes, Frecuencia, fill=Diabetes)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  theme(legend.position = "right")+
  xlab("")+
  geom_text(aes(label=Frecuencia), 
            vjust=-0.5 , hjust=0.7, color="black", 
            position = position_dodge(0.9),size=3) +
  scale_fill_brewer(palette="Reds")
#El número de no diabéticos es casi el doble que el de diabéticos.

#Relacion general entre las variables
ggpairs(DatosL, columns = 1:ncol(DatosL), aes(color = diabetes, alpha = 0.5))
#Notamos valores atipicos en el numero de embarazos para valores positivos de diabetes
#Observamos que las personas con mayor edad presentan diabetes, mientras que los mas
#jovenes no
#Observamos que las personas que poseen diabetes concentran un mayor nivel de glucosa

#Las anteriores observaciones se pueden observar mejor en el siguiente plot
#Boxplots por cada covariable tomando en cuenta si el paciente presenta
#diabetes o no
variables <- colnames(DatosL)[-length(colnames(DatosL))]

#Graficos de boxplot por cada covariable
plots <- lapply(variables, function(var) {
  ggplot(DatosL, aes_string(y = var, color = "diabetes")) +
    geom_boxplot(size = 1.5)
})

#Ajustamos boxplots en un solo arrange
ggarrange(plotlist = plots, nrow = 2, ncol = 4, common.legend = TRUE)

#Analizamos correlaciones con grafico de calor
covariables = subset(DatosL, select = -c(diabetes))
covariables %>% cor(method = "pearson") %>% 
  round(digits = 3) %>% 
  corrplot(method = "ellipse",
           type = "upper",
           order="hclust",
           mar=c(0,0,2,0), 
           diag = FALSE,
           addCoef.col = "black",
           number.cex = 0.75,
           tl.cex = 0.75,
  ) 
#No observamos correlaciones tan significativas por lo
#que se puede confiar en que no habra problemas de 
#colinealidad

#Tabla comparativa de medias y desv. estandar [media(desv.est)]
MeansData = aggregate(DatosL[,1:8], by = list(DatosL$diabetes), function(x) mean(x, na.rm = TRUE))
SdData = aggregate(DatosL[,1:8], by = list(DatosL$diabetes), function(x) sd(x, na.rm = TRUE))
MySdPos = paste(round(MeansData[2,2:9],2),"[",round(SdData[2,2:9],2),"]", sep = "")
MySdNeg = paste(round(MeansData[1,2:9],2),"[",round(SdData[1,2:9],2),"]", sep = "")
MySdf = data.frame(Positivo = MySdPos, Negativo = MySdNeg, Variables = colnames(DatosL)[-9])

#### i #### 
#Seleccion de Variables solo Efectos Principales
#Consideramos modelo logit

#Acomodo de datos para bestglm
Databest = DatosL %>% relocate(diabetes, .after = age)
#Acomodo de datos para glmnet
#Generamos matrix de disenio (solo efectos principales)
DatanetX = model.matrix(diabetes ~., data = DatosL)
XsInt = DatanetX[,-1]
#Guardamos variable dependiente
DataY = DatosL$diabetes

#### a) Mejor subconjunto ####
best.log = bestglm(Databest, IC = 'BIC', family = binomial('logit'), method = 'exhaustive')
summary(best.log$BestModel) #Todas las covariables son significativas
#Obtenemos BIC y coeficientes
coef(best.log$BestModel)
BIC(best.log$BestModel)
#Listas para guardar mejores modelos y BIC
BestModel = list(coef(best.log$BestModel))
BestBIC = list(BIC(best.log$BestModel))

#### b) StepWise #### 
#Metrica BIC
#Definimos modelo nulo
NullDatos = glm(diabetes ~ 1, family = binomial('logit'), data = DatosL)

#Definimos modelo completo
FullDatos = glm(diabetes ~., family = binomial('logit'), data = DatosL)
summary(FullDatos)

#### Forward
forward.log = step(object = NullDatos, scope = list(upper = FullDatos, lower = NullDatos), 
                   trace = FALSE, direction = 'forward', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.log)))
BestBIC = append(BestBIC, BIC(forward.log))

#### Backward
backward.log = step(object =  FullDatos, scope = list(upper = FullDatos, lower = NullDatos), 
                   trace = FALSE, direction = 'backward', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(backward.log)))
BestBIC = append(BestBIC, BIC(backward.log))

#### Both
Both.log = step(object =  FullDatos, scope = list(upper = FullDatos, lower = NullDatos), 
                   trace = FALSE, direction = 'both', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(Both.log)))
BestBIC = append(BestBIC, BIC(Both.log))

#En los 3 stepwise obtenemos el mismo modelo con todas las covariables significativas
#### c) Lasso ####

#Aplicamos penalizacion lasso con variables estandarizadas
#usando solo efectos principales y liga logit
laso.log = glmnet(x = XsInt, y = DataY, standardize = TRUE, family = binomial('logit'), nlambda = 200)
#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.log)
coef(laso.log)

#Funcion con ciclo for que filtra los coeficientes cero
#dados por la penalizacion lasso y recalcula
#los parametros beta por EMV para cada lambda
LassBIC = function(LenL, Matrix, model, Y, liga){
  #Listas vacias
  ListMod = list(NA)
  ListBIC = list(NA)
  #Ciclo for de filtrado y ajuste de modelo
  for (i in 1:LenL) {
    aux =  Matrix[, coef(model)[, i] != 0]
    auxMod = glm(formula = Y ~ aux - 1, 
                 family = binomial(liga))
    ListMod[[i]] = auxMod
    ListBIC[[i]] = BIC(auxMod)
  }
  return(list(ListMod,ListBIC))
}

#Guardamos numero de lambdas probadas
lenlamb = length(laso.log$lambda)

#Aplicamos funcion
Lasso = LassBIC(lenlamb, DatanetX, laso.log, DataY, 'logit')
#Guardamos modelo con minimo BIC
LassMinBIC = which.min(unlist(Lasso[[2]]))
LassMinMod = Lasso[[1]][[LassMinBIC]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinMod)))
BestBIC = append(BestBIC, BIC(LassMinMod))

#### ii ####
#Seleccion de Variables Efectos Principales más interacciones y cuadrados
#Consideramos modelo logit

#Acomodo de datos para glmnet

#Terminos de la matrix (agregamos interacciones y cuadrados)
form <- formula(paste('diabetes ~ . ^2 + ', 
                      paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))

#Generamos matrix de disenio 
DatanetXEP = model.matrix(form, data = DatosL)
#Quitamos interceptor
XsIntEP = DatanetXEP[,-1]
#Comprobamos que no exista colinealidad con interceptor
#Es decir, que no sea una covariable con puros ceros
summary(XsIntEP) #No hay problemas

#Guardamos variable dependiente
DataYEP = DatosL$diabetes

#### a) StepWise #### 
#Metrica BIC
#Definimos modelo nulo
NullDatosEP = glm(diabetes ~ 1, family = binomial('logit'), data = DatosL)
#Definimos modelo completo con interacciones y terminos cuadrados
FullDatosEP = glm(form, family = binomial('logit'), data = DatosL)
#### Forward
forward.logEP = step(object = NullDatosEP, scope = list(upper = FullDatosEP, lower = NullDatosEP), 
                   trace = FALSE, direction = 'forward', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.logEP)))
BestBIC = append(BestBIC, BIC(forward.logEP))

#### Backward
backward.logEP = step(object =  FullDatosEP, scope = list(upper = FullDatosEP, lower = NullDatosEP), 
                    trace = FALSE, direction = 'backward', k = log(dim(DatosL)[1]))
summary(backward.logEP) #Obtenemos una variable no significativa
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(backward.logEP)))
BestBIC = append(BestBIC, BIC(backward.logEP))

#### Both
Both.logEP = step(object =  FullDatosEP, scope = list(upper = FullDatosEP, lower = NullDatosEP), 
                trace = FALSE, direction = 'both', k = log(dim(DatosL)[1]))
summary(Both.logEP)#Mismo modelo que con backward
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(Both.logEP)))
BestBIC = append(BestBIC, BIC(Both.logEP))
#El Modelo Forward presenta el modelo con minimo AIC

#Modelo both sin variable insulina
Both.logEP1 = glm(diabetes ~ glucose + mass + pedigree + age + I(age^2) + I(insulin*pedigree) + I(insulin*age), family = binomial('logit'), data = DatosL)
#Menor BIC de los modelos comprobados
BIC(Both.logEP1) 
#Notamos que el modelo forward esta anidado en el modelo both sin insulina
anova(Both.logEP1, forward.logEP, test="Chisq")
#Se rechaza la hipotesis nula, es decir, el modelo mayor aporta variables significativas
#al modelo, ademas de presentar menor BIC

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(Both.logEP1)))
BestBIC = append(BestBIC, BIC(Both.logEP1))

#### b) Lasso ####
#Aplicamos penalizacion lasso con variables estandarizadas
#usando solo efectos principales y liga logit
laso.logEP = glmnet(x = XsIntEP, y = DataYEP, standardize = TRUE, family = binomial('logit'), nlambda = 300)
#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.logEP) #287 lambdas probadas
coef(laso.logEP)

#Guardamos numero de lambdas probadas
lenlambEP = length(laso.logEP$lambda)

#Aplicamos funcion de filtrado Lasso
LassoEP = LassBIC(lenlambEP, DatanetXEP, laso.logEP, DataYEP, 'logit')

#Guardamos modelo con minimo BIC
LassMinBICEP = which.min(unlist(LassoEP[[2]]))
LassMinModEP = LassoEP[[1]][[LassMinBIC]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinModEP)))
BestBIC = append(BestBIC, BIC(LassMinModEP))

#### iii ####
#### A) ####
#Seleccion de Variables usando distintas ligas
#### liga probit ####
##Mejor Subconjunto (i)
best.pro = bestglm(Databest, IC = 'BIC', family = binomial('probit'), method = 'exhaustive')
summary(best.pro$BestModel) #Todas las covariables son significativas
#Listas para guardar mejores modelos y BIC
BestModel = append(BestModel, list(coef(best.pro$BestModel)))
BestBIC = append(BestBIC, BIC(best.pro$BestModel))

##StepWise (i) #### Forward
#Definimos modelo nulo
NullDatospro = glm(diabetes ~ 1, family = binomial('probit'), data = DatosL)
#Definimos modelo completo
FullDatospro = glm(diabetes ~., family = binomial('probit'), data = DatosL)
#Ajuste Modelo
forward.pro = step(object = NullDatospro, scope = list(upper = FullDatospro, lower = NullDatospro), 
                   trace = FALSE, direction = 'forward', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.pro)))
BestBIC = append(BestBIC, BIC(forward.pro))

##StepWise (ii) #### Forward
#Definimos modelo nulo
NullDatosproEP = glm(diabetes ~ 1, family = binomial('probit'), data = DatosL)
#Definimos modelo completo con interacciones y terminos cuadrados
FullDatosproEP = glm(form, family = binomial('probit'), data = DatosL)
#Ajuste Modelo
forward.proEP = step(object = NullDatosproEP, scope = list(upper = FullDatosproEP, lower = NullDatosproEP), 
                      trace = FALSE, direction = 'forward', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.proEP)))
BestBIC = append(BestBIC, BIC(forward.proEP))

##Lasso (i)
laso.probi = glmnet(x = XsInt, y = DataY, standardize = TRUE, family = binomial('probit'), nlambda = 100)
#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.probi) #62 lambdas probadas
coef(laso.probi)
#Guardamos numero de lambdas probadas
lenlambprob = length(laso.probi$lambda)
#Aplicamos funcion de filtrado Lasso
Lassopro = LassBIC(lenlambprob, DatanetX, laso.probi, DataY, 'probit')
#Guardamos modelo con minimo BIC
LassMinBICpro = which.min(unlist(Lassopro[[2]]))
LassMinModpro = Lassopro[[1]][[LassMinBICpro]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinModpro)))
BestBIC = append(BestBIC, BIC(LassMinModpro))

##Lasso (ii)
laso.probiEP = glmnet(x = XsIntEP, y = DataYEP, standardize = TRUE, family = binomial('probit'), nlambda = 300)
#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.probiEP) #284 lambdas probadas
coef(laso.probiEP)
#Guardamos numero de lambdas probadas
lenlambprobEP = length(laso.probiEP$lambda)
#Aplicamos funcion de filtrado Lasso
LassoproEP = LassBIC(lenlambprobEP, DatanetXEP, laso.probiEP, DataYEP, 'probit')
#Guardamos modelo con minimo BIC
LassMinBICproEP = which.min(unlist(LassoproEP[[2]]))
LassMinModproEP = LassoproEP[[1]][[LassMinBICproEP]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinModproEP)))
BestBIC = append(BestBIC, BIC(LassMinModproEP))
#### liga cloglog ####
##Mejor Subconjunto (i)
best.clog = bestglm(Databest, IC = 'BIC', family = binomial('cloglog'), method = 'exhaustive')
summary(best.clog$BestModel) #Todas las covariables son significativas
#Listas para guardar mejores modelos y BIC
BestModel = append(BestModel, list(coef(best.clog$BestModel)))
BestBIC = append(BestBIC, BIC(best.clog$BestModel))

##StepWise (i) #### Forward
#Definimos modelo nulo
NullDatosclog = glm(diabetes ~ 1, family = binomial('cloglog'), data = DatosL)
#Definimos modelo completo
FullDatosclog = glm(diabetes ~., family = binomial('cloglog'), data = DatosL)
#Ajuste Modelo
forward.clog = step(object = NullDatosclog, scope = list(upper = FullDatosclog, lower = NullDatosclog), 
                   trace = FALSE, direction = 'forward', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.clog)))
BestBIC = append(BestBIC, BIC(forward.clog))

##StepWise (ii) #### Forward
#Definimos modelo nulo
NullDatosclogEP = glm(diabetes ~ 1, family = binomial('cloglog'), data = DatosL)
#Definimos modelo completo con interacciones y terminos cuadrados
FullDatosclogEP = glm(form, family = binomial('cloglog'), data = DatosL)
#Ajuste Modelo
forward.clogEP = step(object = NullDatosclogEP, scope = list(upper = FullDatosclogEP, lower = NullDatosclogEP), 
                     trace = FALSE, direction = 'forward', k = log(dim(DatosL)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.clogEP)))
BestBIC = append(BestBIC, BIC(forward.clogEP))

##Lasso (i)
laso.clog = glmnet(x = XsInt, y = DataY, standardize = TRUE, family = binomial('cloglog'), nlambda = 100)
#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.clog) #58 lambdas probadas
coef(laso.clog)
#Guardamos numero de lambdas probadas
lenlambclog = length(laso.clog$lambda)
#Aplicamos funcion de filtrado Lasso
Lassoclog = LassBIC(lenlambclog, DatanetX, laso.clog, DataY, 'cloglog')
#Guardamos modelo con minimo BIC
LassMinBICclog= which.min(unlist(Lassoclog[[2]]))
LassMinModclog = Lassoclog[[1]][[LassMinBICclog]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinModclog)))
BestBIC = append(BestBIC, BIC(LassMinModclog))

##Lasso (ii)
laso.clogEP = glmnet(x = XsIntEP, y = DataYEP, standardize = TRUE, family = binomial('cloglog'), nlambda = 300)
#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.clogEP) #284 lambdas probadas
coef(laso.clogEP)
#Guardamos numero de lambdas probadas
lenlambclogEP = length(laso.clogEP$lambda)
#Aplicamos funcion de filtrado Lasso
LassoclogEP = LassBIC(lenlambclogEP, DatanetXEP, laso.clogEP, DataYEP, 'cloglog')
#Guardamos modelo con minimo BIC
LassMinBICclogEP = which.min(unlist(LassoclogEP[[2]]))
LassMinModclogEP = LassoclogEP[[1]][[LassMinBICclogEP]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinModclogEP)))
BestBIC = append(BestBIC, BIC(LassMinModclogEP))

### B) Procesamiento log()
#### Preprocesamiento ####
summary(DatosL) #Observamos ningún cero salvo en la variable pregnant
#Transformamos los datos, dado que la variable pregnant cuenta con ceros
#agregamos 1 para no tener -inf y a su vez no sesgar tanto la distribucion
#de los datos y su correlacion con las demas covariables, sobre todo con 
#los ceros (conforme x->0, ln(x) -> -inf)
DatosLog <- DatosL %>%
  mutate(pregnant = log(pregnant + 1), across(-c(pregnant,diabetes), log))
summary(DatosLog)

#Relacion general entre las variables
X11()
ggpairs(DatosLog, columns = 1:ncol(DatosLog), aes(color = diabetes, alpha = 0.5))

#Variable para bestglm
Databestlog = DatosLog
#Acomodo de datos para glmnet
#Generamos matrix de disenio (solo efectos principales)
DatanetXlog = model.matrix(diabetes ~., data = DatosLog)
XsIntlog = DatanetXlog[,-1]
#Guardamos variable dependiente
DataYlog = DatosLog$diabetes

#Terminos de la matrix (agregamos interacciones y cuadrados)
form.log <- formula(paste('diabetes ~ . ^2 + ', 
                          paste(paste0('I(', colnames(DatosLog)[-9], '^2)'), collapse = " + ")))
#Generamos matrix de disenio 
DatanetX.logEP = model.matrix(form.log, data = DatosLog)
#Quitamos interceptor
XsInt.logEP= DatanetX.logEP[,-1]
#Comprobamos que no exista colinealidad con interceptor
#Es decir, que no sea una covariable con puros ceros
summary(XsInt.logEP) #No hay problemas

#Guardamos variable dependiente
DataY.logEP = DatosL$diabetes

##### Seleccion de Variables con liga logit (Efectos Principales) ####

##Mejor subconjunto##
best.log.log = bestglm(Databestlog, IC = 'BIC', family = binomial('logit'), method = 'exhaustive')
summary(best.log.log$BestModel) #Todas las covariables son significativas
#Listas para guardar mejores modelos y BIC
BestModel = append(BestModel, list(coef(best.log.log$BestModel)))
BestBIC = append(BestBIC, BIC(best.log.log$BestModel))

##StepWise Forward-BIC## 
#Definimos modelo nulo
NullDatos.log = glm(diabetes ~ 1, family = binomial('logit'), data = DatosLog)
#Definimos modelo completo
FullDatos.log = glm(diabetes ~., family = binomial('logit'), data = DatosLog)
##Ajustamos modelo stepwise
forward.log.log = step(object = NullDatos.log, scope = list(upper = FullDatos.log, lower = NullDatos.log), 
                   trace = FALSE, direction = 'forward', k = log(dim(DatosLog)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.log.log)))
BestBIC = append(BestBIC, BIC(forward.log.log))

##Penalizacion Lasso con variables estandarizadas##
##usando solo efectos principales y liga logit##
laso.log.log = glmnet(x = XsIntlog, y = DataYlog, standardize = TRUE, family = binomial('logit'), nlambda = 200)

#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.log.log) #111 lambdas probadas
coef(laso.log.log)

#Guardamos numero de lambdas probadas
lenlamb.log = length(laso.log.log$lambda)

#Aplicamos funcion de filtrado Lasso
Lasso.log = LassBIC(lenlamb.log, DatanetXlog, laso.log.log, DataYlog, 'logit')

#Guardamos modelo con minimo BIC
LassMinBIC.log = which.min(unlist(Lasso.log[[2]]))
LassMinMod.log = Lasso.log[[1]][[LassMinBIC.log]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinMod.log)))
BestBIC = append(BestBIC, BIC(LassMinMod.log))

##### Seleccion de Variables con liga logit (Efectos Principales, interacciones y ^2) ####
##StepWise Forward-BIC## 
#Definimos modelo nulo
NullDatos.logEP = glm(diabetes ~ 1, family = binomial('logit'), data = DatosLog)
#Definimos modelo completo con interacciones y terminos cuadrados
FullDatos.logEP = glm(form.log, family = binomial('logit'), data = DatosLog)
#### Ajuste StepWise Forward
forward.log.logEP = step(object = NullDatos.logEP, scope = list(upper = FullDatos.logEP, lower = NullDatos.logEP), 
                     trace = FALSE, direction = 'forward', k = log(dim(DatosLog)[1]))
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(forward.log.logEP)))
BestBIC = append(BestBIC, BIC(forward.log.logEP))

##Penalizacion lasso con variables estandarizadas##
##usando efectos principales, interacciones, ^2 y liga logit##
laso.log.logEP = glmnet(x = XsInt.logEP, y = DataY.logEP, standardize = TRUE, family = binomial('logit'), nlambda = 400)
#Observamos todos los modelos obtenidos y sus respectivas lambdas
print(laso.log.logEP) #333 lambdas probadas

#Guardamos numero de lambdas probadas
lenlamb.logEP = length(laso.log.logEP$lambda)

#Aplicamos funcion de filtrado Lasso
Lasso.logEP = LassBIC(lenlamb.logEP, DatanetX.logEP, laso.log.logEP, DataY.logEP, 'logit')

#Guardamos modelo con minimo BIC
LassMinBIC.logEP = which.min(unlist(Lasso.logEP[[2]]))
LassMinMod.logEP = Lasso.logEP[[1]][[LassMinBIC.logEP]]

#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(LassMinMod.logEP)))
BestBIC = append(BestBIC, BIC(LassMinMod.logEP))

#### iv ####
##Comprobamos Residuales Dharma##
#Ajustamos semilla
set.seed(123)
#Graficamos  residuales en orden de aparicion
#y checamos significancia de las covariables
plot(simulateResiduals(best.log$BestModel)) #1 #Cumple
summary(best.log$BestModel) #Todo significativo
plot(simulateResiduals(forward.log)) #2 #Cumple
summary(forward.log) #Todo significativo
plot(simulateResiduals(backward.log)) #3 #Cumple
summary(backward.log) #Todo significativo
plot(simulateResiduals(Both.log)) #4 #Cumple
summary(Both.log) #Todo significativo
plot(simulateResiduals(LassMinMod)) #5 #Cumple
summary(LassMinMod) #Todo significativo
plot(simulateResiduals(forward.logEP)) #6 #Cumple
summary(forward.logEP) #Todo significativo
plot(simulateResiduals(backward.logEP)) #7 #Cumple
summary(backward.logEP) #Una variable no significativa
plot(simulateResiduals(Both.logEP)) #8 #Cumple
summary(Both.logEP) #Una variable no significativa
plot(simulateResiduals(Both.logEP1)) #9 #Cumple
summary(Both.logEP) #Todo Significativo (sin embargo se quito una variable importante (insulina))
plot(simulateResiduals(LassMinModEP)) #10 #Cumple
summary(LassMinModEP) #2 variables no significativas

#Otras distribuciones
plot(simulateResiduals(best.pro$BestModel)) #11 #Cumple
summary(best.pro$BestModel) #Todas las variables significativas
plot(simulateResiduals(forward.pro)) #12 #Cumple
summary(forward.pro) #Todas las variables significativas
plot(simulateResiduals(forward.proEP)) #13 #Cumple
summary(forward.proEP) #Todas las variables significativas
plot(simulateResiduals(LassMinModpro)) #14 #Cumple
summary(LassMinModpro) #Todas las variables significativas
plot(simulateResiduals(LassMinModproEP)) #15 #Cumple
summary(LassMinModproEP) #Una variable no significativa (modelo muy reducido para reducirlo mas)
#BIC(glm(diabetes~ I(glucose*mass) + I(glucose*age), family = binomial('probit'), data = DatosL)) =376.422

plot(simulateResiduals(best.clog$BestModel)) #16 #Cumple
summary(best.clog$BestModel) #Todas las variables significativas
plot(simulateResiduals(forward.clog)) #17 #Cumple
summary(forward.clog) #Todas las variables significativas
plot(simulateResiduals(forward.clogEP)) #18 #Cumple
summary(forward.clogEP) #Todas las variables significativas
plot(simulateResiduals(LassMinModclog)) #19 #No cumple por outlier
summary(LassMinModclog) #Todas las variables significativas
plot(simulateResiduals(LassMinModclogEP)) #20 #No cumple por outlier
summary(LassMinModclogEP) #Una variable no significativa (modelo muy reducido para reducirlo mas)
#BIC(glm(diabetes~ I(glucose*mass) + I(glucose*age), family = binomial('cloglog'), data = DatosL)) =390.6729

#Con log
plot(simulateResiduals(best.log$BestModel)) #21 #Cumple
summary(best.log$BestModel) #Todas las variables significativas
plot(simulateResiduals(forward.log.log)) #22 #Cumple
summary(forward.log.log) #Todas las variables significativas
plot(simulateResiduals(LassMinMod.log)) #23 #Cumple
summary(LassMinMod.log) #Todas las variables significativas
plot(simulateResiduals(forward.log.logEP)) #24 #Cumple
summary(forward.log.logEP) #Todas las variables significativas
plot(simulateResiduals(LassMinMod.logEP)) #25 #Cumple
summary(LassMinMod.logEP)#Una variable no significativa
#El anterior modelo reducido
Bestlasso = glm(diabetes~ pedigree + I(glucose*mass) + I(glucose*age), family = binomial('logit'), data = DatosLog)
summary(Bestlasso)
#Con base en el summary, podemos determinar que la variable glucose^2 no aporta
#informacion relevante a la modelacion de la probabilidad dado que las 
#demas variables estan presentes en la misma
#Entonces, como no hay evidencia de que B2!=0, podemos ajustar un
#nuevo modelo reducido bajo dicha consideracion.
#Guardamos modelo y BIC
BestModel = append(BestModel, list(coef(Bestlasso)))
BestBIC = append(BestBIC, BIC(Bestlasso))
plot(simulateResiduals(Bestlasso)) #26 No hay problemas con supuestos

#Guardamos si cumplio supuestos y tipo de liga
Supuesto <- ifelse(seq_along(BestModel) %in% c(19, 20), "No Cumple", "Cumple")
Liga = c('logit','logit','logit','logit','logit','logit','logit','logit','logit','logit','probit',
           'probit','probit','probit','probit','cloglog','cloglog','cloglog','cloglog','cloglog',
           'logit','logit','logit','logit','logit','logit')
Ajuste = c('Bestglm','Forward','Backward','Both','Lasso','Forward','Backward','Both','Lasso','Both_Reducción','Bestglm',
                  'Forward','Lasso','Forward','Lasso','Bestglm','Forward','Lasso','Forward','Lasso',
                  'log_Bestglm','log_Forward','log_Lasso','log_Forward','log_Lasso','Log_Lasso_Reducción')
#Guardamos en una lista de listas todos los datos
BestModelCom <- Map(c, BestModel, Supuesto, Liga, Ajuste)

#Ordenamos por BIC la lista de mejores modelos
BestBIC[order(unlist(BestBIC))]
BestM = BestModel[order(unlist(BestBIC))]
#Con info completa
BestM1 = BestModelCom[order(unlist(BestBIC))]

#Creamos tabla con datos
#25 mejores modelos
#Guardamos modelos
TablaBest = data.frame(Modelo = sapply(sapply(BestM, names), function(x) paste(x[-1], collapse = ", ")))
#Eliminamos palabra aux de las variables
TablaBest$Modelo = gsub("aux", "", TablaBest$Modelo)
#Agregamos columnas
TablaBest %>% mutate(Ajuste = sapply(BestM1, function(x) c(x[length(x)])),
                     Liga = sapply(BestM1, function(x) c(x[length(x) - 1])),
                     Supuestos = sapply(BestM1, function(x) x[length(x) - 2]),
                     BIC = unlist(BestBIC[order(unlist(BestBIC))])
                     )
