
library(rgdal) # Se cargan los mapas

departamentos <- readOGR(dsn = 'Mapas', layer = 'DepartamentosVeredas')

names(departamentos)

data <- departamentos
IDs <- departamentos$DPTO_CCDGO
names(data)

## Función: Convertir km a grados
km2d <- function(km){
out <- (km/1.852)/60
return(out)
}

km2d(100) ## 100 km

## Función: Convertir grados a km
d2km <- function(d){
out <- d*60*1.852
return(out)
}

d2km(1) ## 1 grado

library(spdep)

## Vecindades sin establecer la distancia
W_cont <- poly2nb(data, queen=T)
W_cont_mat <- nb2listw(W_cont, style="W", zero.policy=TRUE)

## Vecindades estableciendo distancia (100 km)
W_cont_s <- poly2nb(data, queen=T, snap=km2d(100))
W_cont_s_mat <- nb2listw(W_cont_s, style="W", zero.policy=TRUE)

map_crd <- coordinates(departamentos) # Se extraen las coordenadas

par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_cont_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("Vecindad Directa")

plot(data,border="grey")
plot(W_cont_s_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("Vecindad + 100 km")

library(readr)
capitales <- read_delim("capitales.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
head(capitales)

capitales<-as.data.frame(capitales)
capitales$DPTO_CCDGO<-factor(capitales$DPTO_CCDGO)
str(capitales)

data <- as.data.frame(departamentos)
merged <- merge(data, capitales, by.x="DPTO_CCDGO", by.y="DPTO_CCDGO", all.x=T, all.y=F)
head(merged)

merged <- merged[order(merged$DPTO_CCDGO),]
map2<-departamentos
rownames(merged) <- map2$DPTO_CCDGO
rownames(map2@data) <- map2$DPTO_CCDGO

library(dplyr)

map2@data <- left_join(map2@data, merged)

head(map2@data)

map_crd2 <- cbind(map2$LONGITUD, map2$LATITUD) # Coordenadas de las Capitales
head(map_crd2)

data <- map2
IDs <- map2$DPTO_CCDGO
names(data)

library(spdep)

#########
## k = 1
#########

## Centroides
W_knn1 <- knn2nb(knearneigh(map_crd, k=1), row.names=IDs)
W_knn1_mat <- nb2listw(W_knn1)

## Capitales
W_knn1_2 <- knn2nb(knearneigh(map_crd2, k=1), row.names=IDs)
W_knn1_mat_2 <- nb2listw(W_knn1_2)


par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_knn1_mat,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("k=1 (Centroides)")

plot(data,border="grey")
plot(W_knn1_mat_2,coords=map_crd2,pch=19, cex=0.1, col="blue", add=T)
title("k=1 (Capitales)")

#########
## k = 5
#########

## Centroides
W_knn1.5 <- knn2nb(knearneigh(map_crd, k=5), row.names=IDs)
W_knn1_mat.5 <- nb2listw(W_knn1.5)

## Capitales
W_knn1_2.5 <- knn2nb(knearneigh(map_crd2, k=5), row.names=IDs)
W_knn1_mat_2.5 <- nb2listw(W_knn1_2.5)

par(mfrow=c(1,2),mar=c(0,0,1,0))

plot(data,border="grey")
plot(W_knn1_mat.5,coords=map_crd,pch=19, cex=0.1, col="blue", add=T)
title("k=5 (Centroides)")

plot(data,border="grey")
plot(W_knn1_mat_2.5,coords=map_crd2,pch=19, cex=0.1, col="blue", add=T)
title("k=5 (Capitales)")

load('Total_nacional(csv)/Cultivos.RData')

Cultivos$P_S6P46<-factor(Cultivos$P_S6P46) # Esto ya se discutió
cultivo1<-Cultivos[Cultivos$P_S6P46=='00159201001',]

cultivo1$P_DEPTO<-factor(cultivo1$P_DEPTO)
cultivo1$P_MUNIC<-factor(cultivo1$P_MUNIC)
cultivo1$COD_VEREDA<-factor(cultivo1$COD_VEREDA)

areas<-aggregate(AREA_COSECHADA~P_DEPTO, FUN = sum, data = cultivo1)

head(areas)

areas$DPTO_CCDGO<-areas$P_DEPTO
areas$P_DEPTO<-NULL
head(areas)

data@data <- left_join(data@data, areas)

head(data@data)

data$AREA_COSECHADA[is.na(data$AREA_COSECHADA)] <- 0

summary(data@data)

# Normal
moran.test(data$AREA_COSECHADA, listw=W_cont_mat, zero.policy=T)

# 100 Km
moran.test(data$AREA_COSECHADA, listw=W_cont_s_mat, zero.policy=T)

# Centroides
moran.test(data$AREA_COSECHADA, listw=W_knn1_mat, zero.policy=T)

# Capitales
moran.test(data$AREA_COSECHADA, listw=W_knn1_mat_2, zero.policy=T)

# centroides k=5
moran.test(data$AREA_COSECHADA, listw=W_knn1_mat.5, zero.policy=T)

# Capitales k=5
moran.test(data$AREA_COSECHADA, listw=W_knn1_mat_2.5, zero.policy=T)

lm1 <- localmoran(data$AREA_COSECHADA, listw=W_cont_mat, zero.policy=T)
data$lm1 <- abs(lm1[,4]) ## Extract z-scores

lm.palette <- colorRampPalette(c("white","orange", "red"), space = "rgb")

spplot(data, zcol="lm1", col.regions=lm.palette(20), main="Local Moran's I (|z| scores)", pretty=T, col="transparent",
      par.settings = list(panel.background=list(col="lightblue")))

lm2 <- localmoran(data$AREA_COSECHADA, listw=W_knn1_mat_2.5, zero.policy=T)
data$lm2 <- abs(lm2[,4]) ## Extract z-scores

spplot(data, zcol="lm2", col.regions=lm.palette(20), main="Local Moran's I (|z| scores)", pretty=T,col="transparent",
      par.settings = list(panel.background=list(col="lightblue")))

data@data
