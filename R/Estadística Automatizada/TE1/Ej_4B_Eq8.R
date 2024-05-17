####### 4. Modelos lineales generalizados para datos de conteos #######
#Limpieza de entorno
rm(list = ls(all.names = TRUE))
gc()
options(digits=4)
#Cargamos librerias
library(tidyverse) #Manejo de datos
library(ggplot2) #Estilizacion graficas
library(DHARMa) #Verificacion de supuestos
library(multcomp) #Pruebas simultaneas
library(VGAM) #Ajuste de MLG
library(MASS) #Funciones auxiliares

# Cargamos los datos
datos <- read.csv("Preg4.csv")

datos$Age <- as.factor(datos$Age)
datos$City <- as.factor(datos$City)

#### Ejercicio 4 ####

#### 4.1 ####

# Gráfica de dispersión de la incidencia de casos considerando el grupo de edad y la ciudad
ggplot(data = datos) +
  geom_point(mapping = aes(x = Age, y = Cases / Pop, col = City))

# En general se observa que a mayor edad hay una mayor incidencia de casos cáncer de pulmón
# Sin nos enfocamos por ciudad, la relación aún se mantiene


#### 4.2 ####

# Ajustamos un modelo Poisson considerando las covariables Age, City y sus interacciones
modelo1 <- glm(Cases ~ (Age + City)^2, family = poisson(link = "log"), data = datos, offset = log(Pop))
summary(modelo1)

# No parece un buen modelo. Vemos problemas en la verificación de supuestos
set.seed(123)
plot(simulateResiduals(fittedModel = modelo1))


# Ajustamos un modelo Poisson considerando la covariable Age
modelo2 <- glm(Cases ~ Age, family = poisson(link = "log"), data = datos, offset = log(Pop))
summary(modelo2)

# Parece un buen modelo. No se observan problemas en la verificación de supuestos
set.seed(123)
plot(simulateResiduals(fittedModel = modelo2))


# Realizamos una prueba de hipotésis para ver si los coeficientes de la covariables
# distintas de Age son distintas de cero o no
# H0: coefientes de covariables distintas a Age = 0 vs. H1: coeficientes != 0
# H0: modelo2 es el correcto vs. H1: modelo 1 es el correcto
anova(modelo1, modelo2, test = "Chisq")

# El p-value = 0.3202 > 0.05, así que no rechazamos H0 a un nivel de 0.05
# No hay evidencia en contra del modelo 2


# Vemos el AIC y BIC de los dos modelos
AIC(modelo1)
AIC(modelo2) # menor AIC

BIC(modelo1)
BIC(modelo2) # menor BIC

# Con ambos criterios el modelo 2 (sin interacciones) es mejor


#### 4.3 ####

# Ajustamos un modelo binomial negativo con la covariable de Age
modelo3 <- glm.nb(Cases ~ Age + offset(log(Pop)), data = datos)
summary(modelo3)


# Checamos los supuestos para ambos modelos: ambos modelos parecen bien
set.seed(123)
X11()
plot(simulateResiduals(fittedModel = modelo2))

set.seed(123)
X11()
plot(simulateResiduals(fittedModel = modelo3))


# Calculamos el AIC y BIC para ambos modelos.
AIC(modelo2) # menor AIC
AIC(modelo3)

BIC(modelo2) # menor BIC
BIC(modelo3)

# En ambos casos el modelo 2 (Poisson) tiene menores valores de AIC y BIC.
# El modelo 2 parece ser mejor que el modelo 3

# Calculamos intervalos simultaneos
K <- matrix(c(1,0,0,0,0,
              1,1,0,0,0,
              1,0,1,0,0,
              1,0,0,1,0,
              1,0,0,0,1), ncol = 5, byrow = TRUE)
ICeta <- confint(glht(modelo2, linfct = K), level = 0.95)

IC <- as.data.frame(exp(ICeta$confint))
IC$Age <- levels(datos$Age)

ggplot() +
  geom_point(data = IC, mapping = aes(x = Age, y = Estimate), col = "red") +
  geom_segment(data = IC, mapping = aes(x = Age, y = lwr, xend = Age, yend = upr), col = "red") +
  geom_point(data = datos, mapping = aes(x = Age, y = Cases / Pop)) +
  labs(x = "Grupos de edad", y = "Casos/Pob") 
  

# En cuanto a los valores estimados por el modelo podemos ver una tendencia
# creciente, pero dado que los intervalos de confianza se translapan no es
# posible afirmar con seguridad una incidencia de casos creciente dada la edad

# Además observamos que aumenta el tamaño de los intervalos de confianza conforme
# aumenta la edad


#### 4.4 ####

datos$Ageprima <- (as.numeric(substr(datos$Age, 4, 5)) + as.numeric(substr(datos$Age, 1, 2))) / 2
datos$Ageprima2 <- datos$Ageprima^2

# Ajustamos los modelos
modeloP1 <- glm(Cases ~ Ageprima, family = poisson, offset = log(Pop), data = datos)
summary(modeloP1)

modeloP2 <- glm(Cases ~ Ageprima + Ageprima2, family = poisson, offset = log(Pop), data = datos)
summary(modeloP2)

modeloNB1 <- glm.nb(Cases ~ Ageprima + offset(log(Pop)), data = datos)
summary(modeloNB1)

modeloNB2 <- glm.nb(Cases ~ Ageprima + Ageprima2 + offset(log(Pop)), data = datos)
summary(modeloNB2)


# Hacemos la comprobación de supuestos

# Problemas con el componente lineal
set.seed(123)
X11()
plot(simulateResiduals(fittedModel = modeloP1))

# No se observan problemas
set.seed(123)
X11()
plot(simulateResiduals(fittedModel = modeloP2))

# Problemas con el componente lineal
set.seed(123)
X11()
plot(simulateResiduals(fittedModel = modeloNB1))

# No se observan problemas
set.seed(123)
X11()
plot(simulateResiduals(fittedModel = modeloNB2))


# Los modelos con las covariables Ageprime y Ageprime^2 no tienen problemas
# con los supuestos


# Calculamos el AIC y BIC para todos los modelos
AIC(modeloP1)
AIC(modeloP2) # menor AIC
AIC(modeloNB1)
AIC(modeloNB2)

BIC(modeloP1)
BIC(modeloP2) # menor BIC
BIC(modeloNB1)
BIC(modeloNB2)

# En ambos criterios, el modelo Poisson con las covariables Ageprime y Ageprime^2
# es mejor


# Veamos si la incidencia de cancer es creciente con la edad

# Graficamos los datos y nuestra curva estimada
x <- seq(40, 74, by = 0.5)
y <- sapply(x, FUN  = function(x) {return(as.vector(exp(c(1, x, x^2) %*% modeloP2$coefficients)))})

ggplot() +
  geom_point(mapping = aes(x = Ageprima, y = Cases / Pop), data = datos) + 
  geom_line(mapping = aes(x = x, y = y)) +
  xlim(40, 74) +
  labs(x = "Edad")


# De acuerdo con el criterio de la primera derivada, si la incidencia
# es creciente con respecto a la edad entre 40 y 74 años, se debe cumplir que
# beta_1 + 2 * Edad * beta_2 >= 0
# entre las edades 40 y 74

# Realizamos las pruebas de hipótesis simultaneas
# H0: beta_1 + 2 * Edad * beta_2 <= 0
# H1: beta_1 + 2 * Edad * beta_2 > 0

age <- seq(from = 40, to = 74, by = 0.5)
n <- length(age)
K <- matrix(c(rep(0, n), rep(1, n), 2 * age), byrow = FALSE, ncol = 3)
m <- 0

summary(glht(modeloP2, linfct = K, rhs = m, alternative = "greater"))
summary(glht(modeloP2, linfct = K, rhs = m, alternative = "greater"), test = Ftest())
summary(glht(modeloP2, linfct = K, rhs = m, alternative = "greater"), test = Chisqtest())

# En ambos casos rechazamos H0. Se puede considerar que la función de incidencia
# es creciente con la edad en el intervalo de 40 y 76 años




# K <- matrix(c(rep(1, n), age, age^2), byrow = FALSE, ncol = 3)
# tmp <- glht(modeloP2, linfct = K)
# ICetatmp <- confint(tmp, level = 0.95)
# ICtmp <- as.data.frame(exp(ICetatmp$confint))
# ICtmp$Age <- age

# ggplot() +
#   geom_point(mapping = aes(x = Ageprima, y = Cases / Pop), data = datos) + 
#   geom_line(mapping = aes(x = Age, y = Estimate), data = ICtmp) +
#   geom_line(mapping = aes(x = Age, y = lwr), data = ICtmp) +
#   geom_line(mapping = aes(x = Age, y = upr), data = ICtmp) +
#   geom_line(mapping = aes(x = x, y = y), color = "red") +
#   xlim(40, 74) +
#   labs(x = "Edad")
