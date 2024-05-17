
library(GGally)
library(tidyverse)
library(NbClust)
library(psych)
library(factoextra)

# Cargamos los datos y quitamos filas con NAs
datos <- read.csv("Dat4ExB.csv") %>%
  na.omit() %>%
  select(!X)

# Hacemos una copia de los datos estandarizados con media 0 y varianza 1
datosP <- scale(datos, scale = TRUE) %>% as.data.frame()


#### i. k-means con datos en escala original y estandarizados ####
set.seed(123)

## Datos en escala original ##

# Usando el indice silhouette, observamos el posible valor optimo de k: valor optimo k = 2
X11()
figS <- fviz_nbclust(datos, FUNcluster = kmeans, method = c("silhouette"), k.max = 8, nstart = 20)
figS
figS$data

# Realizamos el metodo kmeans con 2 clusters
k.means <- kmeans(x = datos, centers = 2, nstart = 25)
table(k.means$cluster)

datosc2 <- datos
datosc2$k2 <- factor(k.means$cluster)

# Realizamos un grafico por cluster
X11()
ggpairs(data = datosc2, title = "Datos", aes(colour = k2))

# Obtenemos algunas estadisticas por cluster
describeBy(datosc2 ~ k2, mat = TRUE)

# Grafico de componentes principales
pca <- principal(datos, nfactor = 2, rotate = "none",scores = TRUE)
pca
X11()
biplot(pca, group = datosc2$k2, pch = c(0,21)[datosc2$k2])


## Datos estandarizados ##
set.seed(123)

# Usando el indice silhouette, observamos el posible valor optimo de k: valor optimo k = 2
X11()
figS=fviz_nbclust(datosP, FUNcluster = kmeans, method = c("silhouette"), k.max = 8, nstart = 20)
figS
figS$dataP

# Realizamos el metodo kmeans con 2 clusters
k.meansP <- kmeans(x = datosP, centers = 2, nstart = 25)
table(k.meansP$cluster)

datosc2P <- datosP
datosc2P$k2 <- factor(k.meansP$cluster)

# Realizamos un grafico por cluster
X11()
ggpairs(data = datosc2P, title = "Datos", aes(colour = k2))

# Obtenemos algunas estadisticas por cluster
describeBy(datosc2P ~ k2, mat = TRUE)

# Grafico de componentes principales
pcaP <- principal(datosP, nfactor = 2, rotate = "none", scores=TRUE)
pcaP
X11()
biplot(pcaP, group = datosc2P$k2, pch = c(0,21)[datosc2P$k2])



#### ii. conglomerados jerarquicos con datos en escala original y estandarizados #### 
set.seed(123)

## Datos en escala original ##

# Realizamos el método de conglomerados probando distintas
# disimilitudes entre observaciones y clusters

# Con ayuda del indice silhouette obtenemos un punto de partida
# para considerar los distintos números de clusters

# Camberra
dis_datos <- dist(x = datos, method = "canberra")

NbClust(diss = dis_datos, distance = NULL, min.nc = 2,
                  max.nc = 5, method = "complete", index = "silhouette")$Best.nc

NbClust(diss = dis_datos, distance = NULL, min.nc = 2,
                  max.nc = 5, method = "ward.D", index = "silhouette")$Best.nc


# Euclidian
dis_datos2 <- dist(x = datos, method = "euclidian")

NbClust(diss = dis_datos2, distance = NULL, min.nc = 2,
        max.nc = 5, method = "complete", index = "silhouette")$Best.nc

NbClust(diss = dis_datos2, distance = NULL, min.nc = 2,
        max.nc = 5, method = "ward.D", index = "silhouette")$Best.nc



# Camberra, complete
clust.jer1 <- hclust(dis_datos, method = "complete")

datosv1 = datos
datosv1$c2 = cutree(clust.jer1, k = 3)

pca <- principal(datos, nfactor = 2, rotate = "none", scores = TRUE)
pca
X11()
biplot(pca, group = datosv1$c2, pch = c(2, 5, 10)[datosv1$c2])


# Camberra, ward.D
clust.jer2 <- hclust(dis_datos, method="ward.D")

datosv2 = datos
datosv2$c2 = cutree(clust.jer2, k = 2)

pca <- principal(datos, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca, group = datosv2$c2, pch = c(0, 5)[datosv2$c2])


# Euclidian, complete
clust.jer3 <- hclust(dis_datos2, method="complete")

datosv3 = datos
datosv3$c2 = cutree(clust.jer3, k = 2)

pca <- principal(datos, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca, group = datosv3$c2, pch = c(0, 5)[datosv3$c2])


# Euclidian, ward.D
clust.jer4 <- hclust(dis_datos2, method="ward.D")

datosv4 = datos
datosv4$c2 = cutree(clust.jer4, k = 2)

pca <- principal(datos, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca, group = datosv4$c2, pch = c(0, 5)[datosv4$c2])


## Datos estandarizados ##

# Realizamos el método de conglomerados probando distintas
# disimilitudes entre observaciones y clusters

# Con ayuda del indice silhouette obtenemos un punto de partida
# para considerar los distintos números de clusters

# Camberra
dis_datosP <- dist(x = datosP, method = "canberra")

NbClust(diss = dis_datosP, distance = NULL, min.nc = 2,
        max.nc = 5, method = "complete", index = "silhouette")$Best.nc

NbClust(diss = dis_datosP, distance = NULL, min.nc = 2,
        max.nc = 5, method = "ward.D", index = "silhouette")$Best.nc


# Euclidian
dis_datos2P <- dist(x = datosP, method = "euclidian")

NbClust(diss = dis_datos2P, distance = NULL, min.nc = 2,
        max.nc = 5, method = "complete", index = "silhouette")$Best.nc

NbClust(diss = dis_datos2P, distance = NULL, min.nc = 2,
        max.nc = 5, method = "ward.D", index = "silhouette")$Best.nc



# Camberra, compelte
clust.jer1P <- hclust(dis_datosP, method = "complete")

datosv1P = datosP
datosv1P$c3 = cutree(clust.jer1P, k = 3)

pca <- principal(datosP, nfactor = 2, rotate = "none", scores = TRUE)
pca
X11()
biplot(pca, group = datosv1P$c3, pch = c(0, 5)[datosv1P$c3])


# Camberra, ward.D
clust.jer2P <- hclust(dis_datosP, method="ward.D")

datosv2P = datos
datosv2P$c2 = cutree(clust.jer2P, k = 2)

pca <- principal(datosP, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca, group = datosv2P$c2, pch = c(0, 5)[datosv2P$c2])


# Euclidian, complete # este
clust.jer3P <- hclust(dis_datos2P, method="complete")

datosv3P = datos
datosv3P$c2 = cutree(clust.jer3P, k = 2)

pca <- principal(datosP, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca, group = datosv3P$c2, pch = c(0, 5)[datosv3P$c2])

describeBy(datosv3P ~ c2, mat = TRUE)


# Euclidian, ward.D
clust.jer4P <- hclust(dis_datos2P, method="ward.D")

datosv4P = datos
datosv4P$c2 = cutree(clust.jer4P, k = 2)

pca <- principal(datosP, nfactor = 2, rotate = "none",scores=TRUE)
pca
X11()
biplot(pca, group = datosv4P$c2, pch = c(0, 5)[datosv4P$c2])



#### iii. Modificaciones con componentes principales ####

## Datos originales ##
R.CP <- prcomp(datos, scale = FALSE)

X11()
fviz_pca_var(R.CP, col.var = "contrib")

pcam <- principal(cov(datos), cor="cov", covar = TRUE, nfactor = 2, rotate = "none",scores=TRUE)
pcam

# PC1: correlación positiva con todos los componentes (>0.70)
# PC2: correlación negativa con e16, e22

datosCP <- R.CP$x[, c(1, 2)]

dis_datosCP1 <- dist(x = datosCP, method = "euclidian")
clust.jerCP1 <- hclust(dis_datosCP1, method="complete")

datosv1CP = as.data.frame(datosCP)
datosv1CP$c2 = cutree(clust.jerCP1, k = 2)

pca <- principal(datosCP, nfactor = 2, rotate = "none", scores=TRUE)
pca
X11()
biplot(pca, group = datosv1CP$c2, pch = c(0, 5)[datosv1CP$c2])

describeBy(datosv1CP$c2 ~ c2, mat = TRUE)


## Datos estandarizados ##

R.CPP <- prcomp(datosP, scale = FALSE)

X11()
fviz_pca_var(R.CPP, col.var = "contrib")

pcam <- principal(cov(datosP), cor="cov", covar = TRUE, nfactor = 2, rotate = "none",scores=TRUE)
pcam

# PC1: correlación positiva con todos los componentes (>0.60)
# PC2: correlacion negativa con e1, e2, correlacion positiva con e16, e22
#       e1 (arrive on time), e2 (journey occur as booked)
#       e16 (food and beverages), e22 (entertainment, service, information resources)

datosCPP <- R.CPP$x[, c(1, 2)]

dis_datosCP2 <- dist(x = datosCPP, method = "euclidian")
clust.jerCP2 <- hclust(dis_datosCP2, method="complete")

datosv2CP = as.data.frame(datosCPP)
datosv2CP$c2 = cutree(clust.jerCP2, k = 2)

pca <- principal(datosCPP, nfactor = 2, rotate = "none", scores=TRUE)
pca
X11()
biplot(pca, group = datosv2CP$c2, pch = c(0, 5)[datosv2CP$c2])

describeBy(datosv2CP$c2 ~ c2, mat = TRUE)


# El grupo 1 tiene en promedio mayores mayores valores (positivos) que el grupo 2 
# (negativos) en el CP1 
# En cuanto al CP2, el grupo 1 tiene en promedio valores negativos pero cercanos a 0
# mientras que el grupo 2 tiene en promedio valores postivos

# El grupo 1 son personas que tienen una consideración mayor al promedio, sobre que
# todas las caracteristicas son cruciales en el servicio de la aerolinea
# El grupo 2 son personas que, con respecto al promedio, no consideran cruciales
# los servicios generales, pero sí aquellos servicios que estan fuera del negocio de
# la aerolinea, como la comida, bebida y entretenimiento que ofrece.
