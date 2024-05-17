######## Ejercicio 3 ##########
#Limpiamos entorno
rm(list = ls(all.names = TRUE))
gc()

## Manejo y limpieza de datos
library(dplyr)
library(tidyr)
library(tidyverse)
library(tibble)

## Gráficas
library(ggplot2)
library(GGally)
library(ggpubr)
library(corrplot)
library(RColorBrewer)

#Componentes Principales y Conglomerados
library(factoextra)
library(psych)

setwd("~/Est.Automatizada/TE2")
# Cargamos los datos
#Estos datos corresponden a una encuesta que intenta analizar
#la personalidad de un grupo de 228 alumnos de licenciatura de 
#una universidad de Estados Unidos.
Datos <- read.csv("Dat3Ex.csv")[c("V1", "V2", "V4", "V6", "V9", "V12", "V14", 
                                  "V16", "V17", "V26", "V27", "V29", "V31", "V34", "V37")]
summary(Datos) #Las escalas de los datos son iguales
str(Datos) #Asumimos primero que las variables son continuas
#Observamos que el dataset contiene
#228 observaciones, 15 variables 
#independientes "continuas"

#Xi (Continuas) 
# 1-Disagree strongly
# 2-Disagree a little
# 3-Neither agree nor disagree
# 4-Agree a little
# 5-Agree strongly

#V1-Is talkative
#V2-Tends to find fault with others
#V4-Is depressed, blue
#V6-Is reserved
#V9-Is relaxed,handles stress well
#V12-Starts quarrels with others
#V14-Can be tense
#V16-Generates a lot of enthusiasm
#V17-Has a forgiving nature
#V26-Has an assertive personality
#V27-Can be cold and aloof
#V29-Can be moody
#V31-Is considerate and kind to almost everyone
#V34-Remains calm in tense situations
#V37-Is sometimes rude to others

#Funcion para transformar a factor (no jala el apply ni sapply)
transformar_a_factor <- function(df) {
  for (col in names(df)) {
    df[[col]] <- as.factor(df[[col]])
  }
  return(df)
}
DatosFac <- transformar_a_factor(Datos)
str(DatosFac)

#Gráficas
#Relacion general entre las variables
#Aplicamos pivot para expandir a lo largo
Datos_long = Datos %>%           
  pivot_longer(colnames(Datos)) %>% 
  as.data.frame()

#Graficamos todas las densidades
ggp2 <- ggplot(Datos_long, aes(x = value)) +
  geom_density() + 
  facet_wrap(~ name, scales = "free")
#Notamos una mayor ponderancia en contestar la opcion 4 (ligeramente de acuerdo)
#sobre todo en las preguntas de naturaleza positiva salvo algunas negativas como 
#V29 y V14
#Donde hay mayor ponderancia en contestar la opcion 1 es con preguntas de indole
#negativa

#Analizamos correlaciones con grafico de calor
#Usamos el coeficiente de correlacion de Spearman(con rangos)
#para una mayor significancia de las correlaciones
Datos %>% cor(method = "spearman") %>% 
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

#Observamos algunas correlaciones significativas donde,
#por ejemplo,la correlacion positiva mas fuerte la presenta
#la pregunta V9 con la pregunta V34 (0.63), lo que podriamos interpretar como
#que hay cierto sesgo a que, si la persona esta de acuerdo con ser una persona
#relajada tiende a ser calmada en situaciones tensas.
#Observamos que solo existe una correlacion negativa <= -0.5, dada entre
#las preguntas V9 y V14, que podriamos intepretar como que, si una
#persona esta de acuerdo con ser relajada, tendra cierta tendencia a no
#estar de acuerdo con ser una persona tensa.

head(Datos)

# Definir función para generar la tabla de conteo
generarTablaConteo <- function(data, variable) {
  conteo <- data %>%
    pull(variable) %>%
    table() %>%
    as.data.frame() %>%
    rename(Orden = ".", Frecuencia = Freq)
  
  tabla_conteo <- tibble::tibble(Pregunta = variable, conteo)
  
  return(tabla_conteo)
}

# Generar tabla de conteo y moda para cada variable segun la categoria ordenada
tabla_final <- map_df(names(Datos), ~generarTablaConteo(Datos, .x))
reorganizado <- tabla_final %>%
  group_by(Pregunta) %>%
  mutate(Moda = Orden[which.max(Frecuencia)]) %>%
  spread(Orden, Frecuencia)
reorganizado

#kable(M&Sdf, booktabs = TRUE) %>%
#  add_header_above(c("Diabetes"= 2," "= 1)) %>% 
#  add_header_above(c("Mean(SD)" = 2," " = 1)) %>%
#  kable_styling(bootstrap_options = "basic", full_width =T,font_size = 15)

#### i #### 
#Componentes Principales (Escala original y logaritmica)

##Original
#Obtenemos componentes principales
R.CP=prcomp(Datos, scale = FALSE)
#Mas detalles de los componentes
print(summary(R.CP), digits=3) #Se alcanza el 70% en el componente PC5 (comienza a converger la varianza explicada)

#Grafico de varianza explicada
fviz_screeplot(R.CP, addlabels = TRUE)
#Grafico tipo biplot
X11()
fviz_pca_var(R.CP, 
             col.var = "contrib", 
             axes = c(1,2)) + theme_bw()

#Calculamos correlaciones
cbind(R.CP$x[,1:8], scale(Datos, scale = FALSE)) %>% 
  cor() %>% 
  round(3) %>% 
  data.frame() %>% 
  select(c("PC1", "PC2", "PC3", "PC4","PC5")) %>% 
  slice(9:n()) %>% 
  rownames_to_column(var = "Covariable") 

#Contribuciones de las variables a cada dimension
fviz_contrib(R.CP, choice = "var", axes = 1)
fviz_contrib(R.CP, choice = "var", axes = 2)
fviz_contrib(R.CP, choice = "var", axes = 3)
fviz_contrib(R.CP, choice = "var", axes = 4)

##Log
#Obtenemos componentes principales (tra)
R.CPlog=prcomp(log(Datos), scale = FALSE)
#Mas detalles de los componentes  
print(summary(R.CPlog), digits=3) #Se alcanza el 70% en el componente PC5 (comienza a converger la varianza explicada)

#Grafico de varianza explicada
fviz_screeplot(R.CPlog, addlabels = TRUE)

#Grafico tipo biplot
X11()
fviz_pca_var(R.CPlog, 
             col.var = "contrib", 
             axes = c(1,2)) + theme_bw()

#Contribuciones de las variables a cada dimension
fviz_contrib(R.CPlog, choice = "var", axes = 1)
fviz_contrib(R.CPlog, choice = "var", axes = 2)
fviz_contrib(R.CPlog, choice = "var", axes = 3)
fviz_contrib(R.CPlog, choice = "var", axes = 4)
  
#Calculamos correlaciones
cbind(R.CPlog$x[,1:8], log(Datos)) %>% 
  cor() %>% 
  round(3) %>% 
  data.frame() %>% 
  select(c("PC1", "PC2", "PC3", "PC4","PC5")) %>% 
  slice(9:n()) %>% 
  rownames_to_column(var = "Covariable")

#### ii ####
#Analisis Factorial Exploratorio

#Escala Original
#Determinamos posible numero de factores
X11() 
set.seed(1123)
#La funcion falla usando fa= "fa" y metodo minres
parallelmin <- fa.parallel(Datos, fa = "both", n.iter=100)#Recomienda usar 3 factores
parallelml <- fa.parallel(Datos, fa="fa", n.iter=100, fm = 'ml')#Recomienda usar 3 factores

#Calculamos factores
R.EFA <- fa(r = Datos,
                nfactors = 3,
                rotate = "oblimin")
#Diagrama
fa.diagram(R.EFA, cut = 0.5)
#Interpretacion rapida
print(R.EFA, cut = .5, digits=2, sort=TRUE)
summary(R.EFA) #La prueba de hipotesis indica que quizas sean necesarios mas factores

#Probamos con 5 factores
#Calculamos factores
R.EFA5 <- fa(r = Datos,
            nfactors = 5,
            rotate = "oblimin")
#Diagrama
fa.diagram(R.EFA5, cut = 0.5)
#Interpretacion rapida
print(R.EFA5, cut = .5, digits=2, sort=TRUE)
summary(R.EFA5) #La prueba de hipotesis no se rechaza 

#Escala log
parallelmin.log <- fa.parallel(log(Datos), fa = "fa", n.iter=100)#Recomienda usar 3 factores
parallelml.log <- fa.parallel(log(Datos), fa="fa", n.iter=100, fm = 'ml')#Recomienda usar 3 factores

#Calculamos factores
R.EFA.log <- fa(log(Datos),
                nfactors = 3, 
                rotate = "oblimin") 
#Diagrama
fa.diagram(R.EFA.log, cut = 0.5)
#Interpretacion rapida
print(R.EFA.log, cut = .5, digits=2, sort=TRUE)
summary(R.EFA.log) #La prueba de hipotesis indica que quizas sean necesarios mas factores

#Calculamos factores con 5 factores
R.EFA.log5 <- fa(log(Datos),
                nfactors = 5, 
                rotate = "oblimin") 
#Diagrama
fa.diagram(R.EFA.log5, cut = 0.5)
#Interpretacion rapida
print(R.EFA.log5, cut = .5, digits=2, sort=TRUE) #Es el de mejores metricas
summary(R.EFA.log5) 

#### iii ####
####  i) ####
#a) 
##Original
#Obtenemos componentes principales, usamos 
#Correlacion para variables ordinales
R.CPpoly = principal(Datos, cor = "poly", covar = TRUE, nfactor = p, rotate = "none", scores=TRUE)

#Mas detalles de los componentes
print(R.CPpoly$Structure, digits=3) #Se alcanza el 80% en el componente PC7
                                    #Se alcanza el 73% en el componente PC5

#Grafico tipo biplot
X11()
biplot(R.CPpoly, choose = c(1,2))

#Obtenemos correlaciones (solo 5 componentes)
R.CPpoly$Structure[,1:5]
#Posible candidato

#R.CPpoly.log = principal(scale(Datos, scale = TRUE)[,1:p], 
#                   cor = "poly", covar = TRUE, nfactor = p, rotate = "none", scores=TRUE)
#Bajo transformaciones log o escalado, la funcion principal con cor = "poly"
#falla en obtener los componentes principales

#b)
#Funcion que obtiene correlaciones para funcion principal()
#De este modo obtenemos las correlaciones sin que hayan
#valores fuera de [-1,1] (ejemplo revisar R.CPvar$Structure)
correCP = function(CP, n, df){
  cbind(CP$scores[,1:n], df) %>% 
    cor() %>% 
    round(3) %>% 
    data.frame() %>% 
    select(colnames(CP$scores)[1:n]) %>% 
    slice(n+1:n()) %>% 
    rownames_to_column(var = "Covariable") 
}

##Original
#Obtenemos componentes principales, usamos rotaciones
R.CPvar = principal(Datos, cor = "cov", covar = TRUE, nfactor = p, rotate = "varimax", scores=TRUE)
R.CPvar$loadings #Acumula 80% hasta componente RC12 (11 dimensiones)
#Correlaciones
correCP(R.CPvar,11, scale(Datos, scale = FALSE))
X11()
biplot(R.CPvar, choose = c(1,2))
#Obtenemos componentes principales, usamos rotaciones
R.CPclust = principal(Datos, cor = "cov", covar = TRUE, nfactor = p, rotate = "cluster", scores=TRUE)
R.CPclust$loadings #Acumula 80% hasta componente RC5 (7 dimensiones)
#Correlaciones
correCP(R.CPclust,7, scale(Datos, scale = FALSE))
X11()
biplot(R.CPclust, choose = c(1,2))
#Obtenemos componentes principales, usamos rotaciones
R.CPoblim = principal(Datos, cor = "cov", covar = TRUE, nfactor = p, rotate = "oblimin", scores=TRUE)
R.CPoblim$loadings #Acumula 73% hasta componente TC5 (5 dimensiones)
#Correlaciones
correCP(R.CPoblim,5, scale(Datos, scale = FALSE))
X11()
biplot(R.CPoblim, choose = c(1,2))
#Posible candidato
#Obtenemos componentes principales, usamos rotaciones
R.CPbiquar = principal(Datos, cor = "cov", covar = TRUE, nfactor = p, rotate = "biquartimin", scores=TRUE)
R.CPbiquar$loadings #Acumula 70% hasta componente RC13 (5 dimensiones) #install.packages('GPArotation')
#Correlaciones
correCP(R.CPbiquar,5, scale(Datos, scale = FALSE))
X11()
biplot(R.CPbiquar, choose = c(1,2))

##Log
#Obtenemos componentes principales, usamos rotaciones
R.CPvar.log = principal(log(Datos), cor = "cov", covar = TRUE, nfactor = p, rotate = "varimax", scores=TRUE)
R.CPvar.log$loadings #No llega a acumular ni el 20%

#Obtenemos componentes principales, usamos rotaciones
R.CPclust.log = principal(log(Datos), cor = "cov", covar = TRUE, nfactor = p, rotate = "cluster", scores=TRUE)
R.CPclust.log$loadings #No llega a acumular ni el 20%

#Obtenemos componentes principales, usamos rotaciones
R.CPoblim.log = principal(log(Datos), cor = "cov", covar = TRUE, nfactor = p, rotate = "oblimin", scores=TRUE)
R.CPoblim.log$loadings #No llega a acumular ni el 20%

#Obtenemos componentes principales, usamos rotaciones
R.CPbiquar.log = principal(log(Datos), cor = "cov", covar = TRUE, nfactor = p, rotate = "biquartimin", scores=TRUE)
R.CPbiquar.log$loadings #Solo acumula hasta el 20%

####  ii) ####
#Analisis Factorial Exploratorio
#a)
#Ajustamos correlacion para variables ordinales
poly_cor = polychoric(Datos)
rho = poly_cor$rho
X11()
parallelmin <- fa.parallel(rho, fa = "fa", n.iter=100)#Recomienda usar 3 factores
X11()
parallelml <- fa.parallel(rho, fa = "fa", n.iter=100, fm = 'ml')#Recomienda usar 3 factores 

R.EFA.poly = fa(Datos, nfactor=3, cor="poly", rotate = "oblimin")
R.EFA.poly$loadings #Explica 50% varianza
#Diagrama
fa.diagram(R.EFA.poly, cut = 0.5) #Posible Mejor candidato 
#Interpretacion rapida
print(R.EFA.poly, cut = .5, digits=2, sort=TRUE)
summary(R.EFA.poly) #La prueba de hipotesis indica que quizas sean necesarios mas factores

#Estimamos con ml
R.EFA.polyml = fa(Datos, nfactor=3, cor="poly", rotate = "oblimin", fm = 'ml')
R.EFA.polyml$loadings #Explica 50.1% varianza
fa.diagram(R.EFA.polyml, cut = 0.5) #Mayores correlaciones en ciertas variables
#Interpretacion rapida
print(R.EFA.polyml, cut = .5, digits=2, sort=TRUE)
summary(R.EFA.polyml) #La prueba de hipotesis indica que quizas sean necesarios mas factores

#5 factores
R.EFA.poly5 = fa(Datos, nfactor=5, cor="poly", rotate = "oblimin")
R.EFA.poly5$loadings #Explica 50% varianza
#Diagrama
fa.diagram(R.EFA.poly5, cut = 0.5) #Algunas variables se quedan sin explicar
#Interpretacion rapida
print(R.EFA.poly5, cut = .5, digits=2, sort=TRUE)
summary(R.EFA.poly5) #La prueba de hipotesis indica que quizas sean necesarios mas factores

#b) 
#ROTACIONES
#Escala Original
#Calculamos factores
R.EFAvar <- fa(r = Datos,
                nfactors = 3,
                rotate = "varimax")
#Diagrama de correlaciones
fa.diagram(R.EFAvar, cut = 0.5)
R.EFAvar$loadings #Explica 45.5% de varianza

#Calculamos factores
R.EFAbiqu<- fa(r = Datos,
                nfactors = 3,
                rotate = "biquartimin")
#Diagrama de correlaciones
fa.diagram(R.EFAbiqu, cut = 0.5)
R.EFAbiqu$loadings #Explica 45.5% de varianza

#Calculamos factores
R.EFAclus <- fa(r = Datos,
                nfactors = 3,
                rotate = "cluster")
#Diagrama de correlaciones
fa.diagram(R.EFAclus, cut = 0.5)
R.EFAclus$loadings #Explica 44.4% de varianza

#Escala Log
#Calculamos factores
R.EFAobli.log = fa(r = log(Datos),
                nfactors = 3,
                rotate = "oblimin")
#Diagrama de correlaciones
fa.diagram(R.EFAobli.log, cut = 0.5)
R.EFAobli.log$loadings #Explica 42.2% de varianza

#Calculamos factores
R.EFAvar.log = fa(r = log(Datos),
               nfactors = 3,
               rotate = "varimax")
#Diagrama de correlaciones
fa.diagram(R.EFAvar.log, cut = 0.5)
R.EFAvar.log$loadings #Explica 44.4% de varianza

#Calculamos factores
R.EFAbiqu.log = fa(r = log(Datos),
               nfactors = 3,
               rotate = "biquartimin")
#Diagrama de correlaciones
fa.diagram(R.EFAbiqu.log, cut = 0.5)
R.EFAbiqu.log$loadings #Explica 44.4% de varianza

#Calculamos factores
R.EFAclus.log = fa(r = log(Datos),
                nfactors = 3,
                rotate = "cluster")
#Diagrama de correlaciones
fa.diagram(R.EFAclus.log, cut = 0.5)
R.EFAclus.log$loadings #Explica 43% de varianza

