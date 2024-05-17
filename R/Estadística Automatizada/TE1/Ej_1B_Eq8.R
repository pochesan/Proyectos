####### 1. Regresion Lineal Multiple #######
#Se omiten acentos para que no haya problemas
#con la codificacion

#Limpieza de entorno
rm(list = ls(all.names = TRUE))
gc()
#Fijamos decimales
options(digits=4)

#Importamos librerias
library(multcomp) #Pruebas simultaneas
library(purrr)   # Para utilizar la función map(), map2(), pmap(), ...
library(car)     # Para utilizar la función boxTidwell() y ncvTest().
library(broom) #Test normalidad
library(nortest) #Test normalidad (J.B)
library(latex2exp) #Test aleatoriedad
library(purrr) #pmap function
library(dplyr) # Para el manejo de datos.
library(ggplot2) # Para gráficas.
library(GGally) # Graficas globales
#Cargamos datos
setwd("C:/Users/JOSUE/Documents/Est.Automatizada/TE1")
#Seleccionamos variables de interes y factorizamos
#Y = bpdiast; 
#X = (bmi, sex, age)
Data1B <- read.csv("Preg1B.csv") %>% 
  dplyr::select(bpdiast, bmi, sex, age) 

#La variable sex (1-hombre, 2-mujer) debe factorizarse
str(Data1B)
Data1B$sex = factor(Data1B$sex, levels = c(1,2), labels = c("hombre", "mujer"))

#i)-----------------------------------------------------

#Analisis descriptivo
head(Data1B)
summary(Data1B)

#Creamos tabla resumen de la variable dependiente
Data1B %>% group_by(sex) %>% 
  summarise(Observaciones = n(),
            Media = round(mean(bpdiast),2),
            Mediana = round(median(bpdiast),2),
            Varianza = round(var(bpdiast),2))


#Grafica Global
x11()
ggpairs(data=Data1B, title="Datos", aes(colour = sex))

#Grafica por variable
#Dispersiones con respecto a bpdiast
X11()
ggplot(Data1B, aes(x=bmi, y=bpdiast, color=sex, shape = sex)) +
  geom_point() +
  labs(title = "bpdiast v.s bmi",
       x = "bmi",
       y = "bpdiast") +
  theme_bw()
X11()
ggplot(Data1B, aes(x=age, y=bpdiast, color=sex)) +
  geom_point() +
  labs(title = "bpdiast v.s age",
       x = "age",
       y = "bpdiast") +
  theme_bw()

#Boxplot's con respecto a sex
X11()
variables <- names(Data1B)[sapply(Data1B, is.numeric)] # Obtiene los nombres de las variables numéricas
par(mfrow=c(1,3)); par(mar=c(5,5,2,8))
#iteramos por cada variable numerica y graficamos boxplot
for (var in variables) {
  boxplot(as.formula(paste(var, "~ sex")), data = Data1B, col = "white", outline=FALSE)
  stripchart(as.formula(paste(var, "~ sex")), data = Data1B,
             method = "jitter",
             pch = 19,
             col = 2:4,
             vertical = TRUE,
             add = TRUE)
}

#Revisamos cual es la referencia
levels(Data1B$sex) #La referencia es hombre

#Ajuste del modelo RLM sin interacciones
#Modelamos 
# E(bpdiast; bmi, age, sex)  = b0 + b1*bmi + b2*sex + b3*age
Ajuste1 <- lm(formula = bpdiast~.,
              data = Data1B)

#Prueba de significancia de la regresion
summary(Ajuste1)
#Notamos que p-value es casi cero, por lo que se rechaza que
#los coeficientes en conjunto sean cero, es decir, al menos
#una variable nos ayuda a modelar la Esperanza de bpdiast

#Ademas, cada covariable (sexo, edad y bmi) aporta informacion suficiente dado
#la interaccion de las otras variables; segun se puede ver
#en las pruebas t (individuales)

#Rectas del modelo
#E(Y;bmi, sex=hombre, age)=  b0 + b1*bmi + b3 age= b0 + b1*bmi + b3*age
#E(Y;bmi, sex=mujer, age)=  b0 + b1*bmi + b2 + b3*age = (b0 + b2) + b1*bmi + b3*age

#Notamos que ambas rectas poseen la misma pendiente, pero diferente ordenada al origen 
#Donde la recta para mujeres estará, en promedio, por debajo de la de los hombres
#Pues el coeficiente b2 es negativo

#Lo anterior se puede apreciar de manera visual
#para una edad dada, digamos 30
b0 <- Ajuste1[["coefficients"]][["(Intercept)"]]
b1 <- Ajuste1[["coefficients"]][["bmi"]]
b2 <- Ajuste1[["coefficients"]][["sexmujer"]]
b3 <- Ajuste1[["coefficients"]][["age"]]

## Rectas de regresión
## E(y; sexo = hombre, edad 30) = b0 + b1 bmi + b3*30
RA <- function(x) {
  b0 + b1*x + b3*30
}
## E(y; sexo = mujer, edad 30) = (b0 + b2) + b1 bmi + b3*30
RB <- function(x) {
  (b0 + b2) + b1*x + b3*30
}

ggplot(Data1B, aes(x = bmi,
                   y = bpdiast,
                   color = sex,
                   shape = sex)) +
  geom_point(size = 1.5) +
  scale_color_manual(values=c("#56B4E9", "#E69F00")) +
  labs(title = "Presion Arterial v.s BMI a edad 30",
       x = "bmi",
       y = "bpdiast") +
  geom_function(fun = RA,
                colour = "#56B4E9") +
  geom_function(fun = RB,
                colour = "#E69F00") +
  theme_minimal()

#Revision Supuestos
X11()
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(Ajuste1, 1)   #linealidad - Observamos cierta evidencia en contra
plot(Ajuste1, 3)   #homocedasticidad - Observamos poca evidencia en contra
plot(Ajuste1, 2)   #normalidad - Observamos una irregularidad en la cola derecha
plot(Ajuste1, 5, cook.levels = c(4/(dim(Data1B)[1]-2), 0.5, 1.0))   #Se observan outlier's 

#Linealidad (Esperamos no rechazar H0)
X11()
residualPlots(Ajuste1)
#Notamos que la variable age rechaza H0
#Es decir, encontramos evidencia en contra de 
#la linealidad de age con respecto a bpdiast
#Ademas el modelo ajustado, en general, falla el supuesto 
#de linealidad con una significancia del 5%

###Habra que transformar variables###

#Homocedasticidad
#H0: la varianza es constante 
ncvTest(Ajuste1) #No se rechaza H0, no hay evidencia en contra de la homocedasticidad
ncvTest(Ajuste1,~sex) #No se rechaza H0
ncvTest(Ajuste1,~age) #No se rechaza H0
ncvTest(Ajuste1,~bmi) #No se rechaza H0
lmtest::bptest(Ajuste1) #No se rechaza H0
#No encontramos evidencia en contra del supuesto de varianza constante

#Normalidad 
#Se basa en los residuales estandarizados o estudentizados
#H0: los datos provienen de una distribucion normal
StuAjuste1 = augment(Ajuste1)

shapiro.test(StuAjuste1$.std.resid) #Rechazamos H0
nortest::lillie.test(StuAjuste1$.std.resid) #Rechazamos H0
tseries::jarque.bera.test(StuAjuste1$.std.resid) #Rechazamos H0
#Encontramos evidencia en contra del supuesto de normalidad
#Sin embargo, no es un problema grave a considerar, aunque
#se puede arreglar con un modelo ponderado

#Conclusion, las covariables elegidas aportan informacion significativa
#al modelado de la esperanza de bpdiast, sin embargo, el modelo
#"Ajuste1" falla en cumplir los supuestos de linealidad y
#normalidad, por lo que consideramos necesario escoger otro
#modelo. Ante esto, no es necesario tranformar la variable
#dependentiente "bpdiast", solo las covariables

#Guardamos AIC para comparacion
AIC1 = AIC(Ajuste1)

#ii)-----------------------------------------------------
#Primero obtendremos un modelo a través de la aplicacion de BoxTidwell
#Luego obtendremos un modelo optimizado y compararemos
#sus AIC's

boxTidwell(bpdiast ~ bmi + age, data=Data1B) 
#Observamos que se rechaza que el exponente de
#la transformacion de age sea 1, por lo que 
#es prudente usar la transformacion indicada de
#-.8. Por otro lado, no se rechaza que la transformacion
#de la variable bmi sea 1.
#Esto cumple con lo determinado en el analisis del supuesto de
#linealidad

#Nota: Problema numerico al agregar sex, el problema comenzo a suceder 
#tras actualizar paquete car. Sin embargo, la no inclusion de sex no 
#afecto a los resultados de la funcion

#Ajustamos nuevo modelo
Ajuste2 = lm(formula = bpdiast ~ bmi + sex + I(age^-1), data = Data1B)
#Prueba de significancia de la regresion
#No se rechaza H0, es decir, al menos una covariable
#aporta informacion para modelar E(bpdiast)

#Guardamos AIC
AIC2 = AIC(Ajuste2)

#Verificacion de Supuestos
X11()
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(Ajuste2, 1)   #linealidad - Observamos cierta evidencia en contra
plot(Ajuste2, 3)   #homocedasticidad - Observamos poca evidencia en contra
plot(Ajuste2, 2)   #normalidad - Observamos una irregularidad en la cola derecha
plot(Ajuste2, 5, cook.levels = c(4/(dim(Data1B)[1]-2), 0.5, 1.0))   #Se observan outlier's 

#Eliminamos observaciones influyentes para ver efecto en el modelo
Ajuste2b = lm(formula = bpdiast ~ bmi + sex + I(age^-1), data = Data1B[-c(102,394, 253),])
summary(Ajuste2b) #los coeficientes son altamente alterados, por lo que se decide dejarlos

#Linealidad (Esperamos no rechazar H0)
X11()
residualPlots(Ajuste2) #No encontramos evidencia en contra del supuesto
#de linealidad

#Homocedasticidad
ncvTest(Ajuste2) #No se rechaza H0
ncvTest(Ajuste2,~sex) #No se rechaza H0
ncvTest(Ajuste2,~age) #No se rechaza H0
ncvTest(Ajuste2,~bmi) #No se rechaza H0
lmtest::bptest(Ajuste2) #No se rechaza H0
#No encontramos evidencia en contra de la homocedasticidad

#Normalidad
StuAjuste2 = augment(Ajuste2)
shapiro.test(StuAjuste2$.std.resid) #Rechazamos H0
nortest::lillie.test(StuAjuste2$.std.resid) #Rechazamos H0
tseries::jarque.bera.test(StuAjuste2$.std.resid) #Rechazamos H0
#Hay evidencia en contra de la normalidad, sin embargo 
#se podria dejar pasar este supuesto

# Aleatoriedad (H0: Datos Aleatorios)
X11()
par(mar=c(4, 5, 3, 1))
plot(1:length(StuAjuste2$.std.resid), StuAjuste2$.std.resid, xlab = TeX("$i$"), ylab=TeX("$e_s$"))
#No se observa algun patron en los residuales
acf(StuAjuste2$.std.resid) #Autocorrelograma de los errores
#No se observa patron alguno, lo que hace pausible la aleatoriedad
lawstat::runs.test(StuAjuste2$.std.resid) #No se rechaza H0
randtests::runs.test(StuAjuste2$.std.resid) #No se rechaza H0

#No hay evidencia en contra de la aleatoriedad, con esto en mente, podriamos decir
#que el modelo cumple todos los supuestos salvo normalidad.


##Finalmente, consideramos un modelo optimizado por minimo AIC
##donde aplicaremos una transformacion Box-Cox para 
##cumplir con el supuesto de normalidad

#Selección de valores de lambda para mejor modelo 
#que incluye la transformación xij^(lambda) segun la 
#metrica AIC

#Observamos parametro de transformacion Box-Cox
summary(powerTransform(Ajuste1)) 
#No se rechaza que el valor 0 (transformacion ln())
#sea un parametro adecuado para los datos, por lo que
#optamos por elegirlo

#Definimos intervalo de busqueda para el modelo a optimizar
lambda <- seq(from = -2, to = 2, by = 0.1)

#Malla de valores de lambda para las 2 covariables
Lambda = expand.grid(lambda, lambda)

#Funcion que ajusta un modelo RLM a traves de los distintos
#valores de lambda y bajo una transformacion ln() de Y = bpdiast
Regresion <- function(lambda, lambda1, return_type = "AIC") {
  Ajuste <- lm(formula = log(bpdiast) ~ I(bmi^lambda) + sex + I(age^lambda1), data = Data1B)
  if(return_type == "AIC") {
    return(AIC(Ajuste))
  } else {
    return(c(lambda, lambda1))
  }
}

#Obtenemos lista de valores
AIC <- pmap_dbl(Lambda, ~Regresion(..1, ..2))
Hiper <- pmap(Lambda, ~Regresion(..1, ..2, return_type = "hyper"))

#Obtenemos los valores del mejor modelo
#con AIC minimo
AIC_min <- AIC %>% unlist() %>% 
  which.min()
AIC[[AIC_min]]
Hiper[[AIC_min]]

#Ajustamos el modelo 3
Ajuste3 = lm(formula = log(bpdiast) ~ I(bmi^1.7) + sex + I(age^-0.8), data = Data1B)

#Prueba de significancia de la regresion
summary(Ajuste3) #No se rechaza H0, es decir, al menos una covariable
#aporta informacion para modelar E(ln(bpdiast))

#Guardamos AIC
AIC3 = AIC(Ajuste3)

#Verificacion de Supuestos
X11()
par(mfrow = c(2,2), mgp = c(2,0.7,0), mar = c(3,3,1.5,1))
plot(Ajuste3, 1)   #linealidad - Observamos buen comportamiento de la linealidad
plot(Ajuste3, 3)   #homocedasticidad - Observamos los residuales bien dispersos
plot(Ajuste3, 2)   #normalidad - Observamos normalidad
plot(Ajuste3, 5, cook.levels = c(4/(dim(Data1B)[1]-2), 0.5, 1.0))   #Se observan outlier's 

#Linealidad (Esperamos no rechazar H0)
X11()
residualPlots(Ajuste3) #No encontramos evidencia en contra del supuesto
#de linealidad a un nivel de significancia del 5%

#Homocedasticidad
ncvTest(Ajuste3) #No se rechaza H0
ncvTest(Ajuste3,~sex) #No se rechaza H0
ncvTest(Ajuste3,~age) #No se rechaza H0
ncvTest(Ajuste3,~bmi) #No se rechaza H0
lmtest::bptest(Ajuste3) #No se rechaza H0
#No encontramos evidencia en contra de la homocedasticidad

#Normalidad
StuAjuste3 = augment(Ajuste3)
shapiro.test(StuAjuste3$.std.resid) #No se rechaza H0
nortest::lillie.test(StuAjuste3$.std.resid) #No se rechaza H0
tseries::jarque.bera.test(StuAjuste3$.std.resid) #No se rechaza H0
#No hay evidencia en contra del supuesto de normalidad

# Aleatoriedad (H0: Datos Aleatorios)
X11()
par(mar=c(4, 5, 3, 1))
plot(1:length(StuAjuste3$.std.resid), StuAjuste3$.std.resid, xlab = TeX("$i$"), ylab=TeX("$e_s$"))
#No se observa algun patron en los residuales
acf(StuAjuste3$.std.resid)
#No se observa patron alguno en el acf, lo que hace pausible la aleatoriedad
lawstat::runs.test(StuAjuste3$.std.resid) #No se rechaza H0
randtests::runs.test(StuAjuste3$.std.resid) #No se rechaza H0

#No hay evidencia en contra de la aleatoriedad, con esto en mente, podriamos decir
#que el modelo cumple todos los supuestos

#Eliminamos observaciones influyentes para ver efecto en el modelo
Ajuste3b = lm(formula = log(bpdiast) ~ I(bmi^1.7) + sex + I(age^-0.8), data = Data1B[-c(102,394, 343,253),])
summary(Ajuste3b) #los coeficientes son alterados
coef(Ajuste3)
StuAjuste3b = augment(Ajuste3b)
acf(StuAjuste3b$.std.resid)
#Se observa un patron estacional en el acf, por lo que
#quitar los outliers redujo nuestra confiabilidad con respecto
#a la aleatoriedad de los datos
#Concluimos no quitar los oulier's


#Checamos el AIC para compararlo con el modelo con solo transformacion
#Box-Tiwell

#Convertimos el AIC a escala normal
AIC3 = loglikY=sum( log(dnorm(log(Data1B$bpdiast), Ajuste3$fitted.values, sigma(Ajuste3))*(1/Data1B$bpdiast))) 
AIC3 = -2*(loglikY)+2*(3+2) #p=3, y se estima parametro de dispersion
#Notamos que posee el menor AIC de entre los 3 modelos planteados
#y como es el unico de todos los modelos en cumplir todos los supuestos
#obtamos por usar este
c(AIC1, AIC2, AIC3)
#iii)-----------------------------------------------------

#¿Se puede indicar que para una persona de cierta edad y sexo, tener un índice de masa corporal alto
#se asocia con una alta presión arterial diastólica?

#Para esto es necesaria hacer una prueba de hipotesis t de una sola direccion que
#nos permita indicar si el coeficiente asociado a la variable bmi es positivo (B1 > 0)
#lo que indicaria una correlacion positiva entre bpdiast y bmi, especialmente
#para valores altos

K <- matrix(c(0,1,0,0), ncol=4, nrow=1, byrow=TRUE)
m <- c(0)
summary(glht(Ajuste3, linfct=K, rhs=m, alternative = "greater"))
#Se rechaza H0, por lo que hay evidencia de que B1>0
#Es decir, para una persona de cierta edad y sexo, tener un índice de masa corporal alto
#se asocia con una alta presión arterial diastólica

#iv)-----------------------------------------------------
#Grafica resumen

#Presente una gráfica resumen con
#la estimación puntual asociada a la relación entre bpdiast y bmi. Para esto considere sólo tres posibles
#edades: 30, 50 y 64, así como la diferenciación entre mujeres y hombres.

summary(Ajuste3)

#Guardamos coeficientes
b0 <- Ajuste3[["coefficients"]][["(Intercept)"]]
b1 <- Ajuste3[["coefficients"]][["I(bmi^1.7)"]]
b2 <- Ajuste3[["coefficients"]][["sexmujer"]]
b3 <- Ajuste3[["coefficients"]][["I(age^-0.8)"]]

## Rectas de regresión (Modelamos la mediana)
## E(ln(y); bmi^1.7, sexo = hombre, edad x^-.8) = b0 + b1*bmi^1.7 + b3*x^-.8
RA <- function(x, age) {
  exp(b0 + b1*(x^1.7) + b3*(age^-0.8))
}
## E(ln(y), bmi^1.7, sexo = mujer, edad x^-.8) = (b0 + b2) + b1*bmi^1.7 + b3*x^-0.8
RB <- function(x, age) {
  exp((b0 + b2) + b1*(x^1.7) + b3*(age^-0.8))
}

#Generamos los valores estimados para cada combinacion
#entre edad y sexo
bmi_df <- data.frame(bmi = Data1B$bmi)
for (sex in c("Hombre", "Mujer")) {
  for (age in c(30, 50, 64)) {
    col_name <- paste(sex, age, sep = "_")
    if (sex == "Hombre") {
      bmi_df[col_name] <- RA(Data1B$bmi, age)
    } else {
      bmi_df[col_name] <- RB(Data1B$bmi, age)
    }
  }
}

#Extendemos a lo largo el dataframe para graficar
asas <- tidyr::gather(bmi_df, key = measure, value = regression, 
                      colnames(bmi_df)[-1])

#Graficamos dispersion y los 3 pares de rectas  ajustadas
#Cambiamos nombre de columna
colnames(Data1B)[colnames(Data1B) == "sex"] <- "Sexo"
p = ggplot(Data1B, aes(x = bmi,
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