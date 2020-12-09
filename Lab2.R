# Código para Laboratorio 1: "Clustering" - para el curso de An?lisis de Datos
# Diciembre 2020 - Univerisad Santiago de Chile
# Autor                         Rut             Correo

# Sebastián Salazar Vivanco    22.805.902-1    sebastian.salazar.v@usach.cl
# Javier Valerio Morales        19.683.108-8    javier.valerio@usach.cl

# Librerias a utilizar
library("dplyr")
library("tidyr")
library("tidyverse")
library("ggpubr")
library("cluster")
library("factoextra")
library("dummies")
library("kableExtra")

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

# Convertimos los datos a datos num�ricos
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


# Analisis de los datos
# Total: 
total.observations <- count(raw.data)$n

# Ahora que tenemos los datos, procederemos a normalizarlos.
observations.scaled = scale(data[c(2:17)])

# calculamos las distancias
dist.eucl = dist(data, method = "euclidean")

fviz_dist(dist.eucl)

fviz_nbclust(data, kmeans, method = "wss")
