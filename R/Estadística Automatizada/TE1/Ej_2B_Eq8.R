####### 2. Modelos lineales generalizados para datos continuos #######
#Cargamos librerias
library(tidyverse) #Manejo de datos
library(ggplot2) #Estilizacion graficas
library(DHARMa) #Verificacion de supuestos
library(multcomp) #Pruebas simultaneas
library(VGAM) #Ajuste de MLG
library(MASS) #Funciones auxiliares

#Limpieza de entorno
rm(list = ls(all.names = TRUE))
gc()
options(digits=4)

# Cargamos los datos
datos <- read.csv("Preg1B.csv")
datos$sex <- factor(datos$sex, levels = c(1, 2), labels = c("Hombre", "Mujer")) 

#### 2.1 ####

# Funcion para crear los polinimios y potencias de la covariable cov
# gPoly = grado máximo del polinomio
# gPot = grado máximo de la potencia
malla <- function(cov, gPoly, gPot) {
  Poly <- paste0("poly(", cov, ", ", 1:gPoly, ", raw = TRUE)")
  
  Pot <- paste0("I(", cov, "^(", seq(from = -gPot, to = gPot, by = 0.5), "))")
  idx0 <- (length(seq(from = -gPot, to = gPot, by = 0.5)) + 1)/2
  Pot[idx0] <- paste0("I(log(", cov, "))")
  
  return(c(Poly, Pot))
}

# Los polinomios y potencias para cada variable
bmiMalla <- malla("bmi", gPoly = 5, gPot = 3)
ageMalla <- malla("age", gPoly = 5, gPot = 3)
sexMalla <- c("sex", "NULL")

# Todas las posibles combinaciones entre las covariables
mallaTotal <- expand.grid(sexMalla, bmiMalla, ageMalla, stringsAsFactors = FALSE)

# Creamos los caracteres como apareceran en la formula
CompLin <- apply(mallaTotal, 1, function(x) {paste0("bpdiast ~ ", paste(x, collapse = " + "))})


# Posibles distribuciones
Distribuciones <- c("gaussian", "Gamma", "inverse.gaussian")

# Posibles funciones liga
# i) inverse
# ii) identity
# iii) log
# iv) 1/mu^2 (sólo IG)
FunLigas <- c("identity", "log", "inverse", "1/mu^2")


# Creamos algunas variables
nFunLigas <- length(FunLigas)
nDist <- length(Distribuciones)
nCompLin <- length(CompLin)

nModelos <- nCompLin * (2 * 3 + 4) # Total de modelos (tres funciones ligas para 2 dist y 4 para una IG) 
ModelList <- vector(mode = "list", length = nModelos)  # guardar resultados del ajuste, objeto glm
AICList <- vector(mode = "list", length = nModelos)    # guardar el AIC del modelo
BICList <- vector(mode = "list", length = nModelos)    # guardar el BIC del modelo
FormList <- vector(mode = "list", length = nModelos)   # guardar la fórmula usada para el ajuste


index=0
for(k in 1:nCompLin) {
  formstring <- CompLin[k]
  form <- formula(formstring)
  for(j in 1:nDist) {
    for(l in 1:nFunLigas) {
      # definición del argumento family
      if(FunLigas[l]=="1/mu^2") {
        if(Distribuciones[j]=="inverse.gaussian") {
          index <- index+1
          Dist <- get(Distribuciones[j])  #obtener la función a usar
          Mod.A.Prueba <- glm(form, data = datos, family = Dist(link=FunLigas[l]))
          ModelList[[index]] <- Mod.A.Prueba
          AICList[[index]] <- AIC(Mod.A.Prueba)
          BICList[[index]] <- BIC(Mod.A.Prueba)
          FormList[[index]] <- formstring
        }
      } else {
        index <- index+1
        Dist <- get(Distribuciones[j])
        Mod.A.Prueba <- glm(form, data=datos, family = Dist(link=FunLigas[l]))
        ModelList[[index]] <- Mod.A.Prueba
        AICList[[index]] <- AIC(Mod.A.Prueba)
        BICList[[index]] <- BIC(Mod.A.Prueba)
        FormList[[index]] <- formstring
      }
    }
  }
}


# Modelo con menor AIC
MinAIC <- which.min(unlist(AICList)) # indice

ModMinAIC <- ModelList[[MinAIC]]
summary(ModMinAIC)

ModMinAIC$family

AICList[[MinAIC]]
BICList[[MinAIC]]
FormList[[MinAIC]]

# Modelo con menor AIC

# E[Y] = beta_0 + beta_1 * x_Mujer + beta_2 * x_BMI^2 + beta_3 * x_Age + beta_4 * x_Age^2


# Modelo con menor BIC
MinBIC <- which.min(unlist(BICList))

ModMinBIC <- ModelList[[MinBIC]] # indice
summary(ModMinBIC)

ModMinBIC$family

AICList[[MinBIC]]
BICList[[MinBIC]]
FormList[[MinBIC]]

# Modelo con menor BIC

# E[Y] = beta_0 + beta_1 * x_Mujer + beta_2 * x_BMI^2 + beta_3 * x_Age^{-0.5}


# Ambos modelos tienen una distribución Gamma con función liga identidad y misma forma
# para las covariables sexo y BMI.
# Por lo que consideramos el modelo con menor BIC, ya que además su AIC no difere mucho
# al modelo con menor AIC.


# El modelo es

# E[Y] = beta_0 + beta_1 * x_Mujer + beta_2 * x_BMI^2 + beta_3 * x_Age^{-0.5}

# donde x_Mujer es la indicadora si la persona es mujer o no.


#### 2.2 ####

# De acuerdo al criterio de la primera derivada, la presión arterial diastólica es
# creciente con respecto al BMI, tomando al sexo y edad fijos, si 2 * beta_2 * x_BMI > 0,
# esto es, sí beta_BMI > 0 ya que x_BMI siempre es mayor a 0

# Realizamos la prueba de hipótesis
# H0: beta_2 <= 0 vs
# H1: beta_2 > 0

K <- matrix(c(0, 0, 1, 0), byrow = TRUE, nrow = 1)
m = 0

summary(glht(ModMinBIC, linfct = K, rhs = m, alternative = "greater"))

# Tenemos un p-value < 0.05. Por lo que rechazamos H0 en favor a H1.

# Podria decirse que la presión arterial diastólica es creciente 
# con respecto al BMI, tomando al sexo y edad fijos


# Guardamos coeficientes
b0 <- ModMinBIC[["coefficients"]][["(Intercept)"]]
b1 <- ModMinBIC[["coefficients"]][["sexMujer"]]
b2 <- ModMinBIC[["coefficients"]][["I(bmi^(2))"]]
b3 <- ModMinBIC[["coefficients"]][["I(age^(-0.5))"]]

# Rectas de regresión
# E[Y] = beta_0 + beta_2 * x_BMI^2 + beta_3 * x_Age^{-0.5}
RA <- function(x, age) {
  b0 + b2*(x^2) + b3*(age^-0.5)
}
# E[Y] = beta_0 + beta_1 + beta_2 * x_BMI^2 + beta_3 * x_Age^{-0.5}
RB <- function(x, age) {
  (b0 + b1) + b2*(x^2) + b3*(age^-0.5)
}

# Generamos los valores estimados para cada combinacion
# entre edad y sexo
bmi_df <- data.frame(bmi = datos$bmi)
for (sex in c("Hombre", "Mujer")) {
  for (age in c(30, 50, 64)) {
    col_name <- paste(sex, age, sep = "_")
    if (sex == "Hombre") {
      bmi_df[col_name] <- RA(datos$bmi, age)
    } else {
      bmi_df[col_name] <- RB(datos$bmi, age)
    }
  }
}

#Extendemos a lo largo el dataframe para graficar
asas <- tidyr::gather(bmi_df, key = measure, value = regression, 
                      colnames(bmi_df)[-1])

# Graficamos dispersion y los 3 pares de rectas  ajustadas
# Cambiamos nombre de columna
colnames(datos)[colnames(datos) == "sex"] <- "Sexo"
p = ggplot(datos, aes(x = bmi,
                       y = bpdiast)) +
  geom_point(aes(color = Sexo,
                 shape = Sexo), size = 1, alpha = 0.5) +
  labs(title = "Presion Arterial v.s BMI a Diferentes Edades",
       x = "bmi",
       y = "bpdiast") +
  geom_line(data= asas, aes(x = bmi, y = regression, group = measure, color = measure)) +
  labs(color = "Sexo y Edad")

X11()
p

colnames(datos)[colnames(datos) == "Sexo"] <- "sex"

# En todas las rectas se puede ver una tendencia creciente


# Dado la prueba de hipotesis y la grafica anterior, es posible afirmar que el promedio 
# de la presión arterial diastólica (bpdiast) es creciente con respecto la indice de
# masa corporal (BMI)


#### 2.3 ####

# Modelo 1: E[ln(Y)] = beta_0 + beta_1 * x_BMI^1.7 + beta_3 * x_Age^{-0.8}
# Y ~ Normal
modelo1 <-  lm(formula = log(bpdiast) ~ I(bmi^1.7) + sex + I(age^-0.8), data = datos)
summary(modelo1)

# Modelo 2: E[Y] = beta_0 + beta_1 * x_Mujer + beta_2 * x_BMI^2 + beta_3 * x_Age^{-0.5}
# Y ~ Gamma
modelo2 <- ModMinBIC
summary(modelo2)

# Comprobacion de supuestos. No parece haber problemas con ningun modelo
X11()
plot(modelo1)

X11()
plot(modelo2)


# Criterios AIC
# AIC modelo 1 = 3055
AIC(modelo2) # menor AIC
 

# Interpretación

# 1. Las observaciones son datos positivos por lo que tiene sentido modelarlos
# con una distrubición que solo tome valores positivos como la Gamma en el modelo 2.
# El modelo 1 hace uso de la distribución normal que toma valores positivos y negativos.


# 2. En el modelo 1 modelamos el valor esperado de una transformación de los datos (log),
# por lo que no es posible hacer conclusiones directas sobre las observaciones.
# Para el modelo 2 modelamos valor esperado de los datos. Es  posible hacer 
# conclusiones más directas sobre los datos.


# Dadas las consideraciones de la distribución, el criterio AIC y las interpretabilidad
# es preferible usar el modelo 2

