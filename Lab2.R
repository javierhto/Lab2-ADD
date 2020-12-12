# C√≥digo para Laboratorio 1: "Clustering" - para el curso de An?lisis de Datos
# Diciembre 2020 - Univerisad Santiago de Chile
# Autor                         Rut             Correo

# Sebasti√°n Salazar Vivanco    22.805.902-1    sebastian.salazar.v@usach.cl
# Javier Valerio Morales        19.683.108-8    javier.valerio@usach.cl

# Librerias a utilizar
library("dplyr")
library("tidyr")
library("tidyverse")
library("ggpubr")
library("cluster")
library("factoextra")
library("VIM")
library("clusterSim")

# a) Preliminares

# Se obtienen los datos en formato ancho desde archivo "Congressional Voting 
# Records Data Set" obtenidos de: https://archive.ics.uci.edu/ Donados el 
# 27 de Abril de 1987.

# Las variables que presenta el data set son:
# 1. Class Name: 2 (democrat, republican)
# 2. handicapped-infants: 2 (y,n)
# 3. water-project-cost-sharing: 2 (y,n)
# 4. adoption-of-the-budget-resolution: 2 (y,n)
# 5. physician-fee-freeze: 2 (y,n)
# 6. el-salvador-aid: 2 (y,n)
# 7. religious-groups-in-schools: 2 (y,n)
# 8. anti-satellite-test-ban: 2 (y,n)
# 9. aid-to-nicaraguan-contras: 2 (y,n)
# 10. mx-missile: 2 (y,n)
# 11. immigration: 2 (y,n)
# 12. synfuels-corporation-cutback: 2 (y,n)
# 13. education-spending: 2 (y,n)
# 14. superfund-right-to-sue: 2 (y,n)
# 15. crime: 2 (y,n)
# 16. duty-free-exports: 2 (y,n)
# 17. export-administration-act-south-africa: 2 (y,n)

# b) Obtencion de los datos

col.names <- c("class.name", "handicapped.infants", "water.project.cost.sharing",
               "adoption.of.the.budget.resolution", "physician.fee.freeze", "el.salvador.aid",
               "religiosus.groups.in.schools","anti.satellite.test.ban", "aid.to.nicaraguan.contras",
               "mx.missile", "immigration", "synfuels.corporation.cutback", "educacion.spending",
               "superfund.right.to.sue", "crime", "duty.free.exports", "export.administration.act.south.africa")
observation <- c(1:435)

#dir <- "~/Documentos/Usach/2-2020/Analisis de Datos/Lab1/"
dir <- "C:/Develop/Lab2-ADD"
basename <- "votes.DATA"
file <- file.path(dir, basename)
raw.data <- read.csv(file, header = FALSE, col.names = col.names, na.strings = "?")
raw.data <- cbind(observation, raw.data)

#  Convirtiendo los datos a factores
raw.data$class.name <- factor(raw.data$class.name, 
                              labels = c("democrat","republican"))

raw.data$handicapped.infants <- factor(raw.data$handicapped.infants, 
                                       labels = c("n","y"))

raw.data$water.project.cost.sharing <- factor(raw.data$water.project.cost.sharing, 
                                              labels = c("n","y"))

raw.data$adoption.of.the.budget.resolution <- factor(raw.data$adoption.of.the.budget.resolution, 
                                                     labels = c("n","y"))

raw.data$physician.fee.freeze <- factor(raw.data$physician.fee.freeze, 
                                        labels = c("n","y"))

raw.data$el.salvador.aid <- factor(raw.data$el.salvador.aid, 
                                   labels = c("n","y"))

raw.data$religiosus.groups.in.schools <- factor(raw.data$religiosus.groups.in.schools, 
                                                labels = c("n","y"))

raw.data$anti.satellite.test.ban <- factor(raw.data$anti.satellite.test.ban, 
                                           labels = c("n","y"))

raw.data$aid.to.nicaraguan.contras <- factor(raw.data$aid.to.nicaraguan.contras, 
                                             labels = c("n","y"))

raw.data$mx.missile <- factor(raw.data$mx.missile, 
                              labels = c("n","y"))

raw.data$immigration <- factor(raw.data$immigration, 
                               labels = c("n","y"))

raw.data$synfuels.corporation.cutback <- factor(raw.data$synfuels.corporation.cutback, 
                                                labels = c("n","y"))

raw.data$educacion.spending <- factor(raw.data$educacion.spending, 
                                      labels = c("n","y"))

raw.data$superfund.right.to.sue <- factor(raw.data$superfund.right.to.sue, 
                                          labels = c("n","y"))

raw.data$crime <- factor(raw.data$crime, 
                         labels = c("n","y"))

raw.data$duty.free.exports <- factor(raw.data$duty.free.exports, 
                                     labels = c("n","y"))

raw.data$export.administration.act.south.africa <- factor(raw.data$export.administration.act.south.africa, 
                                                          labels = c("n","y"))

# Convertimos los datos a datos numÈricos
data <- data.frame(
  class.name = factor(raw.data$class.name, 
                      labels = c("democrat","republican")),
  
  handicapped.infants = (factor(raw.data$handicapped.infants, 
                               labels = c("n","y")) == "y")*1,
  
  water.project.cost.sharing = (factor(raw.data$water.project.cost.sharing, 
                                labels = c("n","y")) == "y")*1,
  
  adoption.of.the.budget.resolution = (factor(raw.data$adoption.of.the.budget.resolution, 
                                              labels = c("n","y")) == "y")*1,
  
  physician.fee.freeze = (factor(raw.data$physician.fee.freeze, 
                                 labels = c("n","y")) == "y")*1,
  
  el.salvador.aid = (factor(raw.data$el.salvador.aid, 
                            labels = c("n","y")) == "y")*1,
  
  religiosus.groups.in.schools = (factor(raw.data$religiosus.groups.in.schools, 
                                         labels = c("n","y")) == "y")*1,
  
  anti.satellite.test.ban = (factor(raw.data$anti.satellite.test.ban, 
                                    labels = c("n","y")) == "y")*1,
  
  aid.to.nicaraguan.contras = (factor(raw.data$aid.to.nicaraguan.contras, 
                                    labels = c("n","y")) == "y")*1,
  
  mx.missile = (factor(raw.data$mx.missile, 
                       labels = c("n","y")) == "y")*1,
  
  immigration = (factor(raw.data$immigration, 
                        labels = c("n","y")) == "y")*1,
  
  synfuels.corporation.cutback = (factor(raw.data$synfuels.corporation.cutback, 
                                  labels = c("n","y")) == "y")*1,
  
  educacion.spending = (factor(raw.data$educacion.spending, 
                               labels = c("n","y")) == "y")*1,
  
  superfund.right.to.sue = (factor(raw.data$superfund.right.to.sue, 
                                   labels = c("n","y")) == "y")*1,
  
  crime = (factor(raw.data$crime, 
                  labels = c("n","y")) == "y")*1,
  
  duty.free.exports = (factor(raw.data$duty.free.exports, 
                              labels = c("n","y")) == "y")*1,
  
  export.administration.act.south.africa = (factor(raw.data$export.administration.act.south.africa, 
                                                   labels = c("n","y")) == "y")*1

)

# Manejo de los datos perdidos

lost.data.1 <- sapply(data, function(x) sum(is.na(x)))
lost.data.2 <- aggr(data)

# Nos indica que hay m˙ltiples observaciones que cuentan con pÈrdida de datos, 
# es por esto que solo se usar·n las columnas que contengan menos datos perdidos
# immigration(7), adoption.of.the.budget.resolution(11), physician.fee.freeze(11), 
# religiosus.groups.in.schools(11) y handicapped.infants(12)

# Borramos las columnas que tenemos datos erroneos
data1 <- data[c(1,2,4,5,7,11)]

# Borramos las filas que tienen datos perdidos.
data1 <- data1[!is.na(data1$handicapped.infants),]
data1 <- data1[!is.na(data1$adoption.of.the.budget.resolution),]
data1 <- data1[!is.na(data1$physician.fee.freeze),]
data1 <- data1[!is.na(data1$religiosus.groups.in.schools),]
data1 <- data1[!is.na(data1$immigration),]

lost.data.3 <- sapply(data1, function(x) sum(is.na(x)))
lost.data.4 <- aggr(data1)

# °De esta forma no tenemos datos faltantes!

# Tomamos la misma muestra con igual n˙mero de republicanos y demÛcratas
i <- which(data1[["class.name"]] == "republican")
j <- which(data1[["class.name"]] == "democrat")

set.seed(13)
N <- 435*0.6/2
i <- sample(i, N)
j <- sample(j, N)
k <- sample(-i, 435-N)
l <- sample(-j, 435-N)

train.data <- data1[c(i, j), ]
test.data <- data1[c(k, l), ]

# Analisis de los datos
# Total: 
train.observations <- count(train.data)$n
test.observations <- count(test.data)$n

# Ahora que tenemos los datos, procederemos a normalizarlos.
observations.scaled = scale(data[c(2:17)])

# Revisamos el n˙mero de clusters
wcss <- c()
for(i in 1:20){
  wcss[i] <- sum(kmeans(train.data[2:6], i)$withinss)
}

p1 <- ggplot() + geom_point(aes(x = 1:20, y = wcss), color = 'blue') + 
      geom_line(aes(x = 1:20, y = wcss), color = 'blue') + 
      ggtitle("MÈtodo del Codo") + 
      xlab('Cantidad de Centroides k') + 
      ylab('WCSS')

p2 <- fviz_nbclust(x = train.data[, 2:6], FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(data1, method = "euclidean"), nstart = 50)

# Podemos apreciar que existen diversos n˙meros de clusters que estabilizan los 
# valores de wcss, (MÈtodo del codo) por tanto usaremos 2, 5, 7

# Calculamos las distancias entre los datos, usaremos la distancia binaria, la
# distancia Sokal-Michener y Daisy

# Binaria
binary.dist <- dist(train.data[, 2:6], method = "binary")
hc.1 <- hclust(binary.dist)
plot(hc.1)
p3 <- fviz_dist(binary.dist)

# Sokal-Michener
SM.dist <- dist.SM(train.data[, 2:6])
hc.2 <- hclust(SM.dist)
plot(hc.2)
p4 <- fviz_dist(SM.dist)

# Daisy
daisy.dist = daisy(train.data[, 2:6])
hc.3 <- hclust(daisy.dist)
plot(hc.3)
p5 <- fviz_dist(daisy.dist)


# Si miramos los datos agrupados, se agurpar·n de forma b·sica
clusters.2 <- kmeans(train.data[,2:6], 2)
clusters.5 <- kmeans(train.data[,2:6], 5)
clusters.7 <- kmeans(train.data[,2:6], 7)

train.data$cluster.2 <- as.factor(clusters.2$cluster)
train.data$cluster.5 <- as.factor(clusters.5$cluster)
train.data$cluster.7 <- as.factor(clusters.7$cluster)

p6 <- fviz_cluster(clusters.2, data = train.data[,2:6], palette = "jco", ggtheme = theme_minimal())
p7 <- fviz_cluster(clusters.5, data = train.data[,2:6], palette = "jco", ggtheme = theme_minimal())
p8 <- fviz_cluster(clusters.7, data = train.data[,2:6], palette = "jco", ggtheme = theme_minimal())

# Clusters usando distancia Daisy
dis.matrix.daisy = as.matrix(daisy.dist)
km.daisy = kmeans(dis.matrix.daisy, 3)
p9 <- fviz_cluster(km.daisy, data = train.data[, 2:6], palette = "jco", ggtheme = theme_minimal())


# Finalmente usamos PAM
pam.data.2 <- pam(train.data[, 2:6], 2)
pam.data.5 <- pam(train.data[, 2:6], 5)
pam.data.7 <- pam(train.data[, 2:6], 7)

p10 <- fviz_cluster(pam.data.2)
p11 <- fviz_cluster(pam.data.5)
p12 <- fviz_cluster(pam.data.7)


