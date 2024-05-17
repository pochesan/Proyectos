######## Ejercicio 2 ##########
# Poder predictivo (clasificación supervisada) 
#Limpiamos entorno
rm(list = ls(all.names = TRUE))
gc()

## Datos
library(mlbench)

## Manejo y limpieza de datos
library(dplyr)
library(tidyr)
library(plyr)
library(tibble)
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

# Remuestro
library(rsample)
library(caret)

#Calculo en paralelo
library(purrr)
library(furrr)
library(future)

#PCA
library(factoextra)

#Multiple plots en 1 arrange (ggplot)
library(patchwork)

#Librerias para prediccion
library(ranger)
library(e1071)
library(class)
#### Carga de Datos ####

#Datos corregidos del National Institute of 
#Diabetes and Digestive and Kidney Diseases
data(PimaIndiansDiabetes2)
#El dataset contiene 768 observaciones, 8 variables
#independientes y la variable dependiente binaria

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
str(Datos) #Detectamos NA's
summary(Datos) #Todas las covariables son numericas
               #La variable diabetes es factor
#### Preprocesamiento ####
#Calculamos NA's
sum(apply(t(!is.na(Datos)),2, function(x) all(x))) #Se conservan 392 observaciones
length(na.omit(Datos)$pregnant) #Confirmamos
#Eliminamos NA's
DatosL = na.omit(Datos)
rownames(DatosL)  = NULL #Reseeteamos Index
#Cambiamos labels del Factor
levels(DatosL$diabetes) <- c("No","Yes")

#Resumen de variables
summary(DatosL)
str(DatosL)

#### Visualizacion ####

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



#Obtenemos componentes principales
PCA_M0V1 <- prcomp(scale(DatosL[,-9], scale = TRUE), 
                   scale = FALSE)
#Graficamos los primeros 3 PCA's y agrupamos segun la condicion de diabetes
p1 <- fviz_pca_biplot(PCA_M0V1, axes = c(1,2),
                      habillage = DatosL$diabetes,
                      label="var", title =  "PC1 vs PC2") + theme_bw()
p2 <- fviz_pca_biplot(PCA_M0V1, axes = c(2,3),
                      habillage = DatosL$diabetes,
                      label="var", title =  "PC2 vs PC3") + theme_bw()
p3 <- fviz_pca_biplot(PCA_M0V1, axes = c(1,3),
                      habillage = DatosL$diabetes,
                      label="var", title =  "PC1 vs PC3") + theme_bw()
p1/(p2+p3)

#Calculamos correlaciones para los 3 principales componentes
cbind(PCA_M0V1$x[,1:8], scale(DatosL[,-9], scale = TRUE)) %>% 
  cor() %>% 
  round(3) %>% 
  data.frame() %>% 
  dplyr::select(c("PC1", "PC2", "PC3")) %>% 
  slice(9:n()) %>% 
  rownames_to_column(var = "Covariable") 

#Observamos que, para el componente principal 1, a mayores valores de este,
#principalmente los valores de glucosa, edad, insulina, mass, triceps y presion 
#aumentan positivamente, y observamos que esto crea una particion de los datos;
#tanto en el primer como en el 3 grafico, entre aquellos que poseen diabetes y 
#los que no. Siendo que los que si presentan diabetes, poseen mayores valores de 
#glucosa, edad, insulina,... tal como es de esperarse, en tanto que aquellos que no 
#presentan diabetes se encuentran en los valores negativos del componente principal 1,
# es decir, presentan menores niveles de las variables ya mencionadas.
#Para el componente principal 2, los datos se entremezclan más, y no hay una clara
#interpretacion, y finalmente, con el componente principal 3, podemos observar 
#que las covariables mas representativas son glucosa e insulina con correlacion negativa,
#asi que, a menores valores de este componente, mayor sera el valor de glucosa e insulina,
#lo cual se corresponde con que los pacientes que no presentan diabetes se ubiquen en torno
#a la parte positiva de dicho componente, lo que diria que este grupo posee menores
#valores de insulina y glucosa, sin embargo no es una conclusion generalizada debido a la
#dispersion de los datos para las personas con diabetes sobre este componente.

#### Funciones Aux ####
#Funcion que particiona los datos dado un input de indices
SplitTT = function(x) {
  
  #Obtenemos training y testing
  Train= training(x)
  Test =  testing(x)
  
  Particion = list(Train = Train, 
                   Test = Test)
  return(Particion)
}

#Funcion que dados los valores de una matrix de confusion
#regresa las 3 diferentes medidades de error para evaluar el
#poder de prediccion del modelo
ErrorMeasure = function(MC.test) {
  #Calculamos Errores de Prediccion
  #Clase 1 (No Diabetes)- Especificidad
  Y1 = MC.test[1,1]/sum(MC.test[,1])
  #Clase 2 (Si Diabetes)- Sensibilidad
  Y2 = MC.test[2,2]/sum(MC.test[,2])
  #TCC (Precision)
  Global = sum(diag(MC.test))/sum(MC.test)
  
  Errores <- data.frame(Y1, Y2, Global)
  return(Errores)
  
}

#Funcion que dado un metodo de prediccion seleccionado
#y el numero de nucleos elegidos, te regresa un dataframe
#con el poder de prediccion promedio estimado
Resultados = function(Metodo, workers){

  #Activamos calculo en paralelo
  plan(strategy = multisession, 
       workers = workers)
  
  Aux1 = map(.x = Splits, 
             .f = ~ SplitTT(.x)) %>%
    transpose() %>% 
    future_pmap(.f = get(Metodo), .progress = TRUE, .options = furrr_options(seed = TRUE)) %>%
    transpose() %>% 
    pmap(.f = ~ ErrorMeasure(.x)) %>% 
    ldply(data.frame) %>% 
    mutate_if(is.numeric, ~.*100) %>%
    mutate_if(is.numeric, round, 3) %>%
    add_column(Metodo = Metodo, 
               .before  = "Y1")
  
  Aux2 = Aux1 %>% 
    summarise(Y1 = mean(Y1),
              Y2 = mean(Y2),
              Global = mean(Global))%>%
    mutate_if(is.numeric, round, 3) %>%
    add_column(Metodo = Metodo, 
               .before  = "Y1")
  return(list(Global = Aux2))
}

MasUsadas = function(Metodo, workers){
  
  #Activamos calculo en paralelo
  plan(strategy = multisession, 
       workers = workers)
  
  Aux1 = map(.x = Splits, 
             .f = ~ SplitTT(.x)) %>%
    transpose() %>% 
    future_pmap(.f = get(Metodo), .progress = TRUE, .options = furrr_options(seed = TRUE)) %>%
    transpose() %>% unlist() %>% table() %>% as.data.frame()
  
  return(Aux1)
}
### Ajustes ####
#Usaremos:
# Repeated holdout method
# B=50, train (80%) y test (20%)
# Calculo de la tasa de clasificacion correcta global como
# medida de poder predictivo

#Modelos a ajustar:
# 1. Reg Logistica (Solo efectos principales)
# 2. Reg Logistica (Efectos principales, interacciones y terminos cuadrados)
# 3. Reg Logistica (Efectos principales + seleccion por pasos con criterio BIC)
# 4. Reg Logistica (Efectos principales, interacciones y terminos cuadrados 
#                   + seleccion por pasos con criterio BIC)
# 5. Reg Logistica (Efectos principales, interacciones y terminos cuadrados 
#                   + seleccion usando Lasso con K-CV y Error Rate para tunear lambda)
# 6. Reg liga probit (Efectos principales, interacciones + seleccion step por BIC)
# 7. Metodo Naive
# 8. LDA continuo  (considerando variables binarias)
# 9. QDA continuo, (considerando variables binarias)
# 10. K vecinos mas cercanos
# 11. Random Forest (200 arboles y tuneo de parametro mtry)

#Ajustamos semilla
set.seed(1122)
B = 50

#Creamos particion con caret para el Holdout Method ya estratificada segun "diabetes"
Partition = createDataPartition(DatosL$diabetes, p = 0.80, list = FALSE, times = B)

#Creamos lista de splits a partir de la Particion dada por caret
Splits <- map(seq_len(ncol(Partition)), ~ make_splits(list(
  analysis = Partition[, .x],
  assessment = as.integer(rownames(DatosL[(-Partition[, .x]), ]))
), DatosL))

#### Metodos ####
# 1. Reg Logistica (Solo efectos principales)
Modelo1 <- function(Train, Test) {
  
  #Ajustamos modelo
  logit <- glm(formula = diabetes ~., 
               family = binomial(link = "logit"), 
               data = Train)
  
  #Predecimos valores y clasificamos por regla de
  #maxima probabilidad (corte en 0.5)
  PredTest <- (predict(object = logit, 
                        newdata = Test, 
                        type = "response") > .5) %>%
    #Se clasifica en los que si tienen diabetes
    ifelse(levels(DatosL$diabetes)[2], 
    #Se clasifican en los que no tienen diabetes
           levels(DatosL$diabetes)[1])
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 2. Reg Logistica (Efectos principales, interacciones y terminos cuadrados)
Modelo2 <- function(Train, Test) {
  
  #Definimos formula del modelo
  form <- formula(paste('diabetes ~ . ^2 + ', 
                        paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))
  
  #Ajustamos modelo
  logit <- glm(formula = form, 
               family = binomial(link = "logit"), 
               data = Train)
  
  #Predecimos valores y clasificamos por regla de
  #maxima probabilidad (corte en 0.5)
  PredTest <- (predict(object = logit, 
                       newdata = Test, 
                       type = "response") > .5) %>%
    #Se clasifica en los que si tienen diabetes
    ifelse(levels(DatosL$diabetes)[2], 
           #Se clasifican en los que no tienen diabetes
           levels(DatosL$diabetes)[1])
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 3. Reg Logistica (Efectos principales + seleccion por pasos con criterio BIC)
Modelo3 <- function(Train, Test) {

  #Modelo Nulo.
  AjusteNulo <- glm(formula = diabetes ~ 1, 
                    family =  binomial(link = "logit"), 
                    data = Train)
  
  #Modelo Saturado.
  AjusteSaturado <- glm(formula = diabetes ~., 
                        family =  binomial(link = "logit"), 
                        data = Train)
  
  #Aplicamos seleccion via step forward
  logit_forward <- step(object = AjusteNulo,
                        scope = list(lower = AjusteNulo, 
                                     upper = AjusteSaturado),
                        trace = FALSE, 
                        direction = "forward", 
                        k = log(dim(Train)[1]))
  
  #Predecimos valores y clasificamos por regla de
  #maxima probabilidad (corte en 0.5)
  PredTest <- (predict(object = logit_forward, 
                       newdata = Test, 
                       type = "response") > .5) %>%
    #Se clasifica en los que si tienen diabetes
    ifelse(levels(DatosL$diabetes)[2], 
           #Se clasifican en los que no tienen diabetes
           levels(DatosL$diabetes)[1])
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 4. Reg Logistica (Efectos principales, interacciones y terminos cuadrados 
#                   + seleccion por pasos con criterio BIC)
Modelo4 <- function(Train, Test) {
  
  #Definimos formula del modelo con E.P y terminos cuadraticos
  form <- formula(paste('diabetes ~ . ^2 + ', 
                        paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))
  
  #Modelo Nulo.
  AjusteNulo <- glm(formula = diabetes ~ 1, 
                    family =  binomial(link = "logit"), 
                    data = Train)
  
  #Modelo Saturado.
  AjusteSaturadoCuad <- glm(formula = form, 
                        family =  binomial(link = "logit"), 
                        data = Train)
  
  #Aplicamos seleccion via step forward
  logit_forward <- step(object = AjusteNulo,
                        scope = list(lower = AjusteNulo, 
                                     upper = AjusteSaturadoCuad),
                        trace = FALSE, 
                        direction = "forward", 
                        k = log(dim(Train)[1]))
  
  #Predecimos valores y clasificamos por regla de
  #maxima probabilidad (corte en 0.5)
  PredTest <- (predict(object = logit_forward, 
                       newdata = Test, 
                       type = "response") > .5) %>%
    #Se clasifica en los que si tienen diabetes
    ifelse(levels(DatosL$diabetes)[2], 
           #Se clasifican en los que no tienen diabetes
           levels(DatosL$diabetes)[1])
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}


# 5. Reg Logistica (Efectos principales, interacciones y terminos cuadrados 
#                   + seleccion usando Lasso con K-CV y Error Rate para tunear lambda)
Modelo5 <- function(Train, Test) {
  
  #Definimos formula del modelo con E.P y terminos cuadraticos
  form <- formula(paste('diabetes ~ . ^2 + ', 
                        paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))
  
  #Definimos matrices diseño
  XTrain <- model.matrix(form, data = Train)[,-1]
  YTrain <- Train$diabetes
  
  XTest <- model.matrix(form, data = Test)[,-1]
  
  #Semilla
  set.seed(1122)
  
  #Tuneamos la lambda con K-CV, 5 bloques y evaluamos sobre 100 lambdas
  #La metrica usada es tasa de clasificacion global erronea
  #Usamos estimadores lasso (Relax = False)
  lasso.tun = cv.glmnet(x = XTrain, y = YTrain,
                        nfolds = 5,
                        type.measure = "class",
                        gamma = 0,
                        relax = FALSE,
                        family = "binomial",
                        nlambda = 100)
  
  #Predecimos valores y clasificamos por regla de
  #maxima probabilidad (corte en 0.5)
  #Usamos lambda minima segun tasa de calif global erronea
  PredTest <- (predict(object = lasso.tun, 
                       newx = XTest, 
                       type = "response",
                       s = "lambda.min") > .5) %>%
    #Se clasifica en los que si tienen diabetes
    ifelse(levels(DatosL$diabetes)[2], 
           #Se clasifican en los que no tienen diabetes
           levels(DatosL$diabetes)[1])
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 6. Reg con liga probit (Efectos principales, interacciones y terminos cuadraticos 
#                         + seleccion step por BIC)
Modelo6 <- function(Train, Test) {
  
  #Definimos formula del modelo con E.P y terminos cuadraticos
  form <- formula(paste('diabetes ~ . ^2 + ', 
                        paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))
  
  #Modelo Nulo.
  AjusteNulo <- glm(formula = diabetes ~ 1, 
                    family =  binomial(link = "probit"), 
                    data = Train)
  
  #Modelo Saturado.
  AjusteSaturadoCuad <- glm(formula = form, 
                            family =  binomial(link = "probit"), 
                            data = Train)
  
  #Aplicamos seleccion via step forward
  logit_forward <- step(object = AjusteNulo,
                        scope = list(lower = AjusteNulo, 
                                     upper = AjusteSaturadoCuad),
                        trace = FALSE, 
                        direction = "forward", 
                        k = log(dim(Train)[1]))
  
  #Predecimos valores y clasificamos por regla de
  #maxima probabilidad (corte en 0.5)
  PredTest <- (predict(object = logit_forward, 
                       newdata = Test, 
                       type = "response") > .5) %>%
    #Se clasifica en los que si tienen diabetes
    ifelse(levels(DatosL$diabetes)[2], 
           #Se clasifican en los que no tienen diabetes
           levels(DatosL$diabetes)[1])
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 7. Metodo Naive
#Considera distribuciones normales para variables continuas
Modelo7 <- function(Train, Test) {
  
  Naive <- naiveBayes(formula = diabetes~., 
                   data = Train)
  
  #Se utiliza la regla de asignacion de maxima probabilidad
  #con punto de corte >0.5
  PredTest <- predict(object = Naive,
                      newdata = Test)
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 8. LDA continuo
Modelo8 <- function(Train, Test) {
  
  LDA <- MASS::lda(formula = diabetes~., 
                   data = Train)
  
  #Se utiliza la regla de asignacion de maxima probabilidad
  #con punto de corte >0.5
  PredTest <- predict(object = LDA,
                      newdata = Test)$class
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 9. QDA continuo
Modelo9 <- function(Train, Test) {
  
  QDA <- MASS::qda(formula = diabetes~., 
                   data = Train)
  
  #Se utiliza la regla de asignacion de maxima probabilidad
  #con punto de corte >0.5
  PredTest <- predict(object = QDA,
                      newdata = Test)$class
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 10. K vecinos mas cercanos (Variables estandarizadas)
Modelo10 <- function(Train, Test) {
  
  #Semilla
  set.seed(1122)
  
  #Definimos matrices diseño
  XTrain <- scale(model.matrix(diabetes ~., data = Train)[,-1], scale = TRUE)
  YTrain <- Train$diabetes
  
  XTest <- scale(model.matrix(diabetes ~., data = Test)[,-1], scale = TRUE)
  
  #Tuneamos el valor de K con K-CV, 5 bloques y evaluamos sobre 20 K's
  #La metrica usada es tasa de clasificacion global erronea
  knn.cross <- tune.knn(x = XTrain, y = YTrain, 
                        k = 1:20,
                        tunecontrol = tune.control(sampling = "cross"), 
                        cross=5)
  
  #Predecimos valores y clasificamos por regla de
  #maxima probabilidad (corte en 0.5)
  #Usamos K que minimiza tasa de calif global erronea
  PredTest <- knn(train = XTrain, test = XTest, YTrain, 
                   k = knn.cross$best.parameters[[1]], 
                   use.all = TRUE)
  
  #Obtenemos matrix de confusion
  MC.test <- table(PredTest, Test$diabetes)
  
  return(list(MC.test))
}

# 11. Random Forest (200 arboles y tuneo de parametro mtry)
Modelo11 <- function(Train, Test) {
  
  #Creamos malla de valores
  malla_hyper <- expand.grid(
    mtry       = seq(1,8,1),
    node_size  = c(1,10,15))
  
  #Creamos nueva columna para el error out of bag (tuneo)
  malla_hyper$OOBerr <- NA
  
  #Definimos Semilla
  set.seed(1122)
  
  #Ciclo for de seleccion de arbol por reduccion de impureza ginit  
  for(i in 1:nrow(malla_hyper)) {
    rf <- ranger(
      formula        = diabetes ~.,
      data           = Train,
      num.trees      = 200,
      mtry           = malla_hyper$mtry[i],
      min.node.size  = malla_hyper$node_size[i],
      importance = 'impurity')
    malla_hyper$OOBerr[i] <- rf$prediction.error
  }
  
  #Mejor conjunto de hiperparametros
  position <- which.min(malla_hyper$OOBerr) 
  
  #Random Forest tuneado
  RF.Tune <- ranger(diabetes ~.,
                    data = Train,
                    num.trees = 200,
                    min.node.size = malla_hyper$node_size[position], 
                    mtry = malla_hyper$mtry[position],
                    importance = 'impurity', 
                    probability = FALSE)
  
  PredTest <- predict(object = RF.Tune,
                      data = Test)
  
  MC.test <- table(PredTest$predictions, Test$diabetes)
  
  return(list(MC.test))
  
}

#### Ejecucion ####

#Mostramos numero de threads disponibles
parallelly::availableCores()

#Corremos los 11 modelos
M1 <- Resultados(Metodo = "Modelo1",
                 workers = 2)
M2 <- Resultados(Metodo = "Modelo2",
                 workers = 2)
M3 <- Resultados(Metodo = "Modelo3",
                 workers = 2)
M4 <- Resultados(Metodo = "Modelo4",
                 workers = 2)
M5 <- Resultados(Metodo = "Modelo5",
                 workers = 8)
M6 <- Resultados(Metodo = "Modelo6",
                 workers = 2)
M7 <- Resultados(Metodo = "Modelo7",
                 workers = 2)
M8 <- Resultados(Metodo = "Modelo8",
                 workers = 8)
M9 <- Resultados(Metodo = "Modelo9",
                 workers = 8)
M10 <- Resultados(Metodo = "Modelo10",
                 workers = 8)
M11 <- Resultados(Metodo = "Modelo11",
                  workers = 8)
#### Resultados ####

Errores <- rbind(M1[["Global"]], 
                 M2[["Global"]],
                 M3[["Global"]], 
                 M4[["Global"]], 
                 M5[["Global"]],
                 M6[["Global"]],
                 M7[["Global"]],
                 M8[["Global"]],
                 M9[["Global"]],
                 M10[["Global"]],
                 M11[["Global"]])
tuneo_values = c("-", "-", "-", "-", "lambda", "-", "-", "-", "-", "K", "mtry, node_size")
Errores$Tuneo = tuneo_values 
Errores %>% dplyr::rename(Especificidad = Y1, Sensibilidad = Y2, TCC = Global) %>% arrange(-TCC)

#### Variables Mas Usadas ####
#Los metodos hacen una seleccion de variables y se pueden obtener los datos son el 3,4,5 y 6
# 3. Reg Logistica (Efectos principales + seleccion por pasos con criterio BIC)
variables3 <- function(Train, Test) {
  
  #Modelo Nulo.
  AjusteNulo <- glm(formula = diabetes ~ 1, 
                    family =  binomial(link = "logit"), 
                    data = Train)
  
  #Modelo Saturado.
  AjusteSaturado <- glm(formula = diabetes ~., 
                        family =  binomial(link = "logit"), 
                        data = Train)
  
  #Aplicamos seleccion via step forward
  logit_forward <- step(object = AjusteNulo,
                        scope = list(lower = AjusteNulo, 
                                     upper = AjusteSaturado),
                        trace = FALSE, 
                        direction = "forward", 
                        k = log(dim(Train)[1]))
  
  return(gsub("aux", "", names(coef(logit_forward))[-1]))
}


# 4. Reg Logistica (Efectos principales, interacciones y terminos cuadrados 
#                   + seleccion por pasos con criterio BIC)
variables4 <- function(Train, Test) {
  
  #Definimos formula del modelo con E.P y terminos cuadraticos
  form <- formula(paste('diabetes ~ . ^2 + ', 
                        paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))
  
  #Modelo Nulo.
  AjusteNulo <- glm(formula = diabetes ~ 1, 
                    family =  binomial(link = "logit"), 
                    data = Train)
  
  #Modelo Saturado.
  AjusteSaturadoCuad <- glm(formula = form, 
                            family =  binomial(link = "logit"), 
                            data = Train)
  
  #Aplicamos seleccion via step forward
  logit_forward <- step(object = AjusteNulo,
                        scope = list(lower = AjusteNulo, 
                                     upper = AjusteSaturadoCuad),
                        trace = FALSE, 
                        direction = "forward", 
                        k = log(dim(Train)[1]))
  
  return(gsub("aux", "", names(coef(logit_forward))[-1]))
}


# 5. Reg Logistica (Efectos principales, interacciones y terminos cuadrados 
#                   + seleccion usando Lasso con K-CV y Error Rate para tunear lambda)
variables5 <- function(Train, Test) {
  
  #Definimos formula del modelo con E.P y terminos cuadraticos
  form <- formula(paste('diabetes ~ . ^2 + ', 
                        paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))
  
  #Definimos matrices diseño
  XTrain <- model.matrix(form, data = Train)[,-1]
  YTrain <- Train$diabetes
  
  XTest <- model.matrix(form, data = Test)[,-1]
  
  #Semilla
  set.seed(1122)
  
  #Tuneamos la lambda con K-CV, 5 bloques y evaluamos sobre 100 lambdas
  #La metrica usada es tasa de clasificacion global erronea
  #Usamos estimadores lasso (Relax = False)
  lasso.tun = cv.glmnet(x = XTrain, y = YTrain,
                        nfolds = 5,
                        type.measure = "class",
                        gamma = 0,
                        relax = FALSE,
                        family = "binomial",
                        nlambda = 100)
  #Obtenemos nombres de las covariables diferentes de 0
  aux = coef(glmnet(x = XTrain, y = YTrain, family = binomial("logit"), lambda = lasso.tun$lambda.min))[,1]
  
  return(gsub("aux", "", names(aux[aux != 0])[-1]))
}

# 6. Reg con liga probit (Efectos principales, interacciones y terminos cuadraticos 
#                         + seleccion step por BIC)
variables6 <- function(Train, Test) {
  
  #Definimos formula del modelo con E.P y terminos cuadraticos
  form <- formula(paste('diabetes ~ . ^2 + ', 
                        paste(paste0('I(', colnames(DatosL)[-9], '^2)'), collapse = " + ")))
  
  #Modelo Nulo.
  AjusteNulo <- glm(formula = diabetes ~ 1, 
                    family =  binomial(link = "probit"), 
                    data = Train)
  
  #Modelo Saturado.
  AjusteSaturadoCuad <- glm(formula = form, 
                            family =  binomial(link = "probit"), 
                            data = Train)
  
  #Aplicamos seleccion via step forward
  logit_forward <- step(object = AjusteNulo,
                        scope = list(lower = AjusteNulo, 
                                     upper = AjusteSaturadoCuad),
                        trace = FALSE, 
                        direction = "forward", 
                        k = log(dim(Train)[1]))
  
  return(gsub("aux", "", names(coef(logit_forward))[-1]))
}

#Corremos los 11 modelos
B3 <- MasUsadas(Metodo = "variables3",
                 workers = 2)
B4 <- MasUsadas(Metodo = "variables4",
                 workers = 2)
B5 <- MasUsadas(Metodo = "variables5",
                 workers = 8)
B6 <- MasUsadas(Metodo = "variables6",
                 workers = 2)
#Agrupamos los conteos por cada metodo
Conteo <- rbind(B3, B4, B5, B6)
#Sumamos y ordenamos
Conteo %>% aggregate(Freq ~ ., FUN = sum) %>% arrange(-Freq) %>% slice(1:10)
