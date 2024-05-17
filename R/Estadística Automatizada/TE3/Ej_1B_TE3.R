library(tidyverse)
library(caret)
library(MASS)
library(glmnet)


# Cargamos los datos y quitamos datos extraños
data(fat, package = "faraway")

head(fat)
str(fat)
summary(fat$weight)
plot(fat$weight)

summary(fat$height)
plot(fat$height)

fat <- fat %>%
  dplyr::select(!c(siri, density, free)) %>%
  filter(brozek != 0, height > 40, weight < 350)

plot(fat$weight)
plot(fat$height)


# Definimos los parametros del 5-CV
set.seed(1)
K <- 5
n <- dim(fat)[1]
labK <- rep(1:K, length.out = n)
Pliegues <- sample(labK)

#### i Modelo Gaussiano con liga identidad ####

###### Modelo con efectos principales ######
# Descripcion del modelo
modelo1 <- lm(brozek ~ ., data = fat)
summary(modelo1)

mod1KCV <- function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod1t = lm(brozek ~ ., data = Dat[train,])
  predm1t = predict(mod1t, Dat[test,])
  MSE=mean((Dat$brozek[test]-predm1t)^2)
  return(MSE)
}

MSE.K.mod1 = sapply(1:K, mod1KCV, Plie = Pliegues, Dat = fat)
(MSE.KCV.mod1=mean(MSE.K.mod1))
# 17.08772


###### Modelo con efectos principales e interacciones ######
# Descripcion del modelo
modelo2 <- lm(brozek ~ .^2, data = fat)
summary(modelo2)

mod2KCV <- function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod1t = lm(brozek ~ .^2, data = Dat[train,])
  predm1t = predict(mod1t, Dat[test,])
  MSE=mean((Dat$brozek[test]-predm1t)^2)
  return(MSE)
}

MSE.K.mod2 = sapply(1:K, mod2KCV, Plie = Pliegues, Dat = fat)
(MSE.KCV.mod2 = mean(MSE.K.mod2))
# 56.6425


#### Modelo con efectos principales, interacciones y cuadrado de las variables ####
# Descripcion del modelo
compLin <- as.formula(paste0("brozek ~ .^2 + ", paste0("I(", colnames(fat)[-1], "^2)", collapse = " + ")))
modelo3 <- lm(compLin, data = fat)
summary(modelo3)

mod3KCV <- function(x, Plie, Dat){
  train <- which(Plie != x)
  test = (-train)
  mod1t = lm(compLin, data = Dat[train,])
  predm1t = predict(mod1t, Dat[test,])
  MSE=mean((Dat$brozek[test]-predm1t)^2)
  return(MSE)
}


MSE.K.mod3 = sapply(1:K, mod3KCV, Plie = Pliegues, Dat = fat)
(MSE.KCV.mod3 = mean(MSE.K.mod3))
# 90.41858



#### ii Seleccion de variables con criterio BIC ####
set.seed(123)
mod3RHM=function(x, Plie, Dat, forme, upform){
  train <- which(Plie != x)
  test <- (-train)
  assign("DatosAux", Dat[train,], envir = .GlobalEnv) #Cuidado stepAIC o step buscan la base de datos en el environment global cuando se usa scope 
  modAux <- lm(forme, data = DatosAux)
  penAux <- log(dim(DatosAux)[1])
  modtr <- stepAIC(modAux, scope =list(upper = upform, lower = ~1), trace = FALSE,direction ="both", k=penAux)
  predte <- predict(modtr, Dat[test,])
  MSE <- mean((fat$brozek[test] - predte)^2)
  return(MSE)
}

###### Modelo con efectos principales ######
forme1 <- as.formula("brozek ~ .")
upform1 <- as.formula("~ .")

# se requiere definir la penalización para BIC
pen <- log(dim(fat)[1])

modelo1BIC <- stepAIC(modelo1, scope = list(upper = upform1, lower = ~ 1), trace = FALSE, direction = "both", k = pen)
summary(modelo1BIC)

# Medición del poder predictivo
MSE_B_modelo1BIC <- NA
for(ik in 1:K){
  MSE_B_modelo1BIC[ik] <- mod3RHM(ik, Plie = Pliegues, Dat = fat, forme = forme1, upform = upform1)
}
(MSE_RHM_modelo1BIC <- mean(MSE_B_modelo1BIC))
# 16.88585


###### Modelo con efectos principales e interacciones ######
forme2 <- as.formula("brozek ~ .^2")
upform2 <- as.formula("~ .^2")

# se requiere definir la penalización para BIC
pen <- log(dim(fat)[1])

modelo2BIC <- stepAIC(modelo2, scope = list(upper = upform2, lower = ~ 1), trace = FALSE, direction = "both", k = pen)
summary(modelo2BIC)

# Medición del poder predictivo
MSE_B_modelo2BIC <- NA
for(ik in 1:K){
  MSE_B_modelo2BIC[ik] <- mod3RHM(ik, Plie = Pliegues, Dat = fat, forme = forme2, upform = upform2)
}
(MSE_RHM_modelo2BIC <- mean(MSE_B_modelo2BIC))
# 83.47374


#### Modelo con efectos principales, interacciones y cuadrado de las variables ####
forme3 <- as.formula(paste0("brozek ~ .^2 + ", paste0("I(", colnames(fat)[-1], "^2)", collapse = " + ")))
upform3 <- as.formula(paste0("~ .^2 + ", paste0("I(", colnames(fat)[-1], "^2)", collapse = " + ")))

# se requiere definir la penalización para BIC
pen <- log(dim(fat)[1])

modelo3BIC <- stepAIC(modelo3, scope = list(upper = upform3, lower = ~ 1), trace = FALSE, direction = "both", k = pen)
summary(modelo3BIC)

# Medición del poder predictivo
MSE_B_modelo3BIC <- NA
for(ik in 1:K){
  MSE_B_modelo3BIC[ik] <- mod3RHM(ik, Plie = Pliegues, Dat = fat, forme = forme3, upform = upform3)
}
(MSE_RHM_modelo3BIC <- mean(MSE_B_modelo3BIC))
# 206.4342


###### iii Seleccion de variables con metodo lasso ######
set.seed(1234)
mod4RHM=function(x, Plie, Dat, forme){
  train <- which(Plie != x)
  test <- (-train)
  Xmod4ttotal <- model.matrix(forme, data=Dat)[,-1]
  Xmod4t <- Xmod4ttotal[train, ]
  Ymod4t <- Dat[train, "brozek"] 
  mod4t.lasso.tun <- cv.glmnet(Xmod4t, Ymod4t, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
  predte <- predict(mod4t.lasso.tun, newx = Xmod4ttotal[test,], type = "response", s = "lambda.min")
  MSE <- mean((Dat$brozek[test]-predte)^2)
  return(MSE)
}


###### Modelo con efectos principales ######
set.seed(1)
MSE_B_modelo1lasso <- sapply(1:K, mod4RHM, Plie = Pliegues, Dat = fat, forme = forme1)
(MSE_RHM_modelo1lasso <- mean(MSE_B_modelo1lasso))
# 16.98819

###### Modelo con efectos principales e interacciones ######
set.seed(1)
MSE_B_modelo2lasso <- sapply(1:K, mod4RHM, Plie = Pliegues, Dat = fat, forme = forme2)
(MSE_RHM_modelo2lasso <- mean(MSE_B_modelo2lasso))
# 16.31596

###### Modelo con efectos principales, interacciones y cuadrado de las variables ######
set.seed(1)
MSE_B_modelo3lasso <- sapply(1:K, mod4RHM, Plie = Pliegues, Dat = fat, forme = forme3)
(MSE_RHM_modelo3lasso <- mean(MSE_B_modelo3lasso))
# 16.30106


###### iii Modelo gamma con liga inversa y metodo Lasso ######
modeloG1 <- glm(brozek ~ ., family = Gamma, data = fat)
modeloG2 <- glm(brozek ~ .^2, family = Gamma, data = fat)
modeloG3 <- glm(compLin, family = Gamma, data = fat)

Gmod3RHM <- function(x, Plie, Dat, forme){
  train <- which(Plie != x)
  test <- (-train)
  Xmod4ttotal <- model.matrix(forme, data=Dat)[,-1]
  Xmod4t <- Xmod4ttotal[train, ]
  Ymod4t <- Dat[train, "brozek"] 
  mod4t.lasso.tun <- cv.glmnet(Xmod4t, Ymod4t, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = Gamma("inverse"), nlambda = 50)
  predte <- predict(mod4t.lasso.tun, newx = Xmod4ttotal[test,], type = "response", s = "lambda.min")
  MSE <- mean((Dat$brozek[test]-predte)^2)
  return(MSE)
}

###### Modelo con efectos principales ######
set.seed(1)
forme1 <- as.formula("brozek ~ .")
MSE_B_modeloG1lasso <- sapply(1:K, Gmod3RHM, Plie = Pliegues, Dat = fat, forme = forme1)
(MSE_RHM_modeloG1lasso <- mean(MSE_B_modeloG1lasso))
# 32.96021

###### Modelo con efectos principales e interacciones ######
set.seed(1)
forme2 <- as.formula("brozek ~ .^2")
MSE_B_modeloG2lasso <- sapply(1:K, Gmod3RHM, Plie = Pliegues, Dat = fat, forme = forme2)
(MSE_RHM_modeloG2lasso <- mean(MSE_B_modeloG2lasso))
#24.74873


###### Modelo con efectos principales e interacciones ######
set.seed(1)
forme3 <- as.formula(paste0("brozek ~ .^2 + ", paste0("I(", colnames(fat)[-1], "^2)", collapse = " + ")))
MSE_B_modeloG3lasso <- sapply(1:K, Gmod3RHM, Plie = Pliegues, Dat = fat, forme = forme3)
(MSE_RHM_modeloG3lasso <- mean(MSE_B_modeloG3lasso))
# 24.15794


# Comparación final
MSE.KCV.mod1 # 17.08772
MSE.KCV.mod2 # 56.6425
MSE.KCV.mod3 # 90.41858
MSE_RHM_modelo1BIC # 16.88585
MSE_RHM_modelo2BIC # 83.47374
MSE_RHM_modelo3BIC # 206.4342
MSE_RHM_modelo1lasso # 16.98819
MSE_RHM_modelo2lasso # 16.31596
MSE_RHM_modelo3lasso # 16.30106
MSE_RHM_modeloG1lasso # 32.96021
MSE_RHM_modeloG2lasso # 24.74873
MSE_RHM_modeloG3lasso # 24.15794

#### Comentarios ####

# Para este ejercicio el modelo con mejor poder predictivo es:
# Modelo con liga identidad
# Distribucion gausiana
# Componente lineal: efectos principales, interacciones y variables al cuadrado
# Seleccion de variables con lasso

# Observamos que en los modelos sin seleccion de variables y con seleccion de variables usando
# el criterio BIC, el poder predictivo empeora conforme se vuelve más complejo el componente lineal
# Por su parte, los modelos que usan la seleccion de variables con el metodo lasso tienen un mejor
# poder predictivo conforme se vuelve más complejo el componente lineal.
# Esto puede deberse a que los modelos con componente lineal más complejo tienden a sobreajustar
# y no se desempeñan bien en nuevas observaciones
# Sin embargo, el metodo lasso hace una buena seleccion de variables para poder predecir de buena forma
# las nuevas observaciones sin necesidad de sobreajustar

# Finalmente, los modelos con liga identidad y distribucion gausiana tuvieron un mejor poder
# predictivo que sus respectivos modelos con liga inversa y distribucion gamma.


# Variables seleccionadas por los modelos con Lasso
Xmod1ttotal <- model.matrix(forme1, data = fat)[,-1]
Ymod1t <- fat[, "brozek"] 
mod1t.lasso.tun <- cv.glmnet(Xmod1ttotal, Ymod1t, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
coef(mod1t.lasso.tun)
# intercept, age, height, abdom, wrist


Xmod2ttotal <- model.matrix(forme2, data = fat)[,-1]
Ymod2t <- fat[, "brozek"] 
mod2t.lasso.tun <- cv.glmnet(Xmod2ttotal, Ymod1t, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
coef(mod2t.lasso.tun)
# intercept, abdom, age:abdom, height:wrist, 


Xmod3ttotal <- model.matrix(forme3, data = fat)[,-1]
Ymod3t <- fat[, "brozek"] 
mod3t.lasso.tun <- cv.glmnet(Xmod3ttotal, Ymod1t, nfolds = 5, type.measure ="mse", gamma = 0, relax = FALSE, family = gaussian("identity"), nlambda = 50)
coef(mod3t.lasso.tun)
# intercept, abdom, height^2, age:adipos, age:abdom, height:wrist
