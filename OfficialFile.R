library(caret)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
library(dplyr)
library(gridExtra)
library(pdp) #pima dataset
library(corrplot)
library(palmerpenguins)
library(plotly)
library(car)
library(GGally)
library(MASS)
library(modeest)
library(NbClust)
library(cluster)
library(parameters)
library(stats)
library(factoextra)

#Semilla para que nuestro análisis sea reproducible
set.seed(123)

#Lectura de datos en csv
orig_df<-read_csv("datos/data.csv", show_col_types=FALSE)
#Eliminamos los datos con valor 'unknown' en la variable gender.
df_no_unknown<-orig_df[orig_df$gender !=0,]
#Eliminamos las variables target que no vamos a usar. Comprobamos el procedimiento: 
df<-subset(df_no_unknown, select=-c(C_api,C_man)); df

#Obtenemos la dimensión de la matriz con los que trabajaremos de ahora en adelante
dim(df)
n<- dim(df)[1]  # Así nombraremos las filas
p <- dim(df)[2] # Así nombraremos las columnas

n
p

#Obtengo los índices para el 60% de los datos totales
indices_60 <- createDataPartition(df$gender, times = 1, p = 0.6)

#Se utiliza el 60% de los datos para entrenar el modelo
train_Data <- df[indices_60$Resample1,] # Selecciono todas las columnas

dim(train_Data)

#El signo menos (-) indica "no". Selecciono los datos que no he seleccionado antes: el 40% restante
data_test_val <- df[-indices_60$Resample1,] # Datos para dividir en prueba y validación

#Obtengo los índices para el 20% de los datos totales (50% de data_test_val)
indices_40 <- createDataPartition(data_test_val$gender, times = 1, p = 0.5)

#El 20% de los datos se utiliza para probar el modelo.
test_Data <- data_test_val[indices_40$Resample1,] # Selecciono todas las columnas

#Selecciono los datos que no he seleccionado antes: el 50% restante (20% del total)
val_Data <- data_test_val[-indices_40$Resample1,] # Selecciono todas las columnas
dim(val_Data)

#Verifico si las dimensiones coinciden. Objetivo: 3145 observaciones.
dim(train_Data) + dim(test_Data) + dim(val_Data)

train_Data$gender<-as.factor(train_Data$gender)
train_Data$E_NEds <-as.factor(train_Data$E_NEds)
train_Data$E_Bpag <-as.factor(train_Data$E_Bpag)
train_Data$weightIJ<-as.factor(train_Data$weightIJ)
train_Data$NIJ<-as.factor(train_Data$NIJ)

histogram = function(x_var, title= 'Histograma', x_label, y_label = "Frecuencia") {
  ggplot(train_Data, aes(x = {{x_var}})) +
    geom_histogram(bins = 40, color = 'black', fill = 'white') +
    geom_vline(aes(xintercept = mean({{x_var}})), color = "blue", linetype = "dashed", size = 1) +
    labs(title = title,
         x = x_label,
         y = y_label)
}
pdf = function(x_var, title='PDF', x_label, y_label = "Densidad") {
  ggplot(train_Data, aes(x = {{x_var}})) +
    geom_density() +
    labs(title = title,
         x = x_label,
         y = y_label)
}

boxplot = function(x_var, title = "Boxplot", x_label) {
  ggplot(train_Data, aes({{x_var}})) +
    geom_boxplot() +
    labs(title = title,
         x = x_label)
}

# Veamos la proporción de hombres y mujeres.
prop.table(table(train_Data$gender))

# Representamos un diagrama de barras.
ggplot(data = train_Data, aes(x = gender, fill = gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = "none") +
  ylab("Frecuencia relativa") +
  xlab("Género")

summary(train_Data$E_NEds)

summary(train_Data$E_Bpag)

#Resumen de weightIJ.
summary(train_Data$weightIJ)

#Resumen de NIJ
summary(train_Data$NIJ)

ggplot(data=train_Data,aes(x=E_NEds,fill=E_NEds)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="none") +
  ylab("Frecuencia relativa") +
  xlab("Variable respuesta: Grupo")

ggplot(data=train_Data,aes(x=E_Bpag,fill=E_Bpag)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="none") +
  ylab("Frecuencia relativa") +
  xlab("Variable respuesta: Grupo")

ggplot(data=train_Data,aes(x=weightIJ,fill=weightIJ)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels = NULL) + 
  theme(legend.position="none") +
  ylab("Frecuencia relativa") +
  xlab("Variable respuesta: Grupo")

ggplot(data=train_Data,aes(x=weightIJ,fill=weightIJ)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels = NULL) + 
  theme(legend.position="none") +
  ylab("Frecuencia relativa") +
  xlab("Variable respuesta: Grupo")

ano_inicial<-as.numeric(substr(train_Data$firstDay,start = 1, stop = 4 ))
train_Data$ano_inicial<-ano_inicial

summary(ano_inicial)

histogram(ano_inicial,title = "Año incial",x_label = "Año")

pdf(ano_inicial, x_label="Año")

boxplot(ano_inicial,title = "Año inicial", x_label = "Año")

mes_inicial<-as.numeric(substr(train_Data$firstDay,start = 5, stop = 6 ))
train_Data$mes_inicial<-mes_inicial
#Resumen del mes inicial.
summary(mes_inicial)

#Mes inicial - Histograma
histogram(mes_inicial, x_label = "Mes",title = "Mes inicial")

#Boxplot - Mes inicial
boxplot(mes_inicial,x_label = "Mes inicial")

#Obtenemos el día.
dia_inicial<-as.numeric(substr(train_Data$firstDay,start = 7, stop = 8 ))
train_Data$dia_inicial<-dia_inicial
#Resumen del dato.
summary(dia_inicial)

#Dia incial - Histograma
histogram(dia_inicial, x_label = "Día",title = "Día inicial")

#Día inicial - Boxplot
boxplot(dia_inicial, x_label = "Dia inicial")

ano_final<-as.numeric(substr(train_Data$lastDay,start = 1, stop = 4 ))
train_Data$ano_final<-ano_final
#Resumen del dato.
summary(ano_final)

#HAño final - Histograma
histogram(ano_final,x_label = "Año final")

#Año final - Boxplot
boxplot(ano_final, x_label = "Año")

#Obtenemos el mes final.
mes_final<-as.numeric(substr(train_Data$lastDay,start = 5, stop = 6 ))
train_Data$mes_inicial<-mes_final
#Resumen del dato.
summary(mes_final)

#Mes final - Histograma
histogram(mes_final,x_label = "Mes final")

#Mes final - Boxplot
boxplot(mes_final, x_label = "Mes final")

#Sacamos el día.
dia_final<-as.numeric(substr(train_Data$lastDay,start = 7, stop = 8 ))
train_Data$dia_final<-dia_final
#Resumen del dato
summary(dia_final)

histogram(dia_final, x_label = "Ultimo dia")

#Dia final - Boxplot
boxplot(dia_final, x_label= "Dia")

#Resumen de los datos
summary(train_Data$NEds)

#Veamos el histograma.
histogram(NEds,x_label = "Cantidad de ediciones", y_label = "Frecuencia",title = "Número de ediciones" )

#NEds - pdf
pdf(NEds, x_label = "Cantidad ediciones")

#log(NEds) - Boxplot
boxplot(log(NEds),title = "Número de ediciones (log)", x_label = "Frecuencia")

# Resumen de la variable NDays en train_Data.
summary(train_Data$NDays)

# Veamos la forma de su distribución con un histograma. La línea azul representa la media.
NDays_hist<-histogram(NDays, x_label= "Último día-Primer día +1"); NDays_hist

# Veamos su función de densidad de probabilidad.
NDays_pdf<- pdf(NDays, x_label='Último día-Primer día +1'); NDays_pdf

# Boxplot.
bp_NDays<-boxplot(NDays, x_label='Último día-Primer día +1'); bp_NDays

# Resumen de la variable NActDays en train_Data. 
summary(train_Data$NActDays)

#NActDAys - Estimación unimodal
mlv(train_Data$NActDays, method = "meanshift")

# Resumen de la variable NPages en train_Data. 
summary(train_Data$NPages)

# Resumen de la variable NPcreated en train_Data. 
summary(train_Data$NPcreated)

par(mfrow = c(1, 3))

# NActDays - Veamos su función de densidad de probabilidad.
NActDays_pdf<- pdf(NActDays, x_label='Días con ediciones'); NActDays_pdf

# NPages - Veamos su función de densidad de probabilidad.
NPages_pdf<-pdf(NPages, x_label='Páginas diferentes editadas'); NPages_pdf

# NPcreated - Veamos su función de densidad de probabilidad.
NPcreated_pdf<-pdf(NPcreated, x_label='Páginas creadas'); NPcreated_pdf

# NActDays - Diagrama de caja.
bp_NActDays<-boxplot(NActDays, x_label='Días con ediciones'); bp_NActDays

# NPages - Diagrama de caja.
bp_NPages<-boxplot(NPages, x_label='Páginas diferentes editadas'); bp_NPages

# NPcreated - Diagrama de caja.
bp_NPcreated<-boxplot(NPcreated, x_label='Páginas creadas'); bp_NPcreated

# NActDays - Transformación de datos (box cox).
boxcox(lm(train_Data$NActDays ~ 1))

new_NActDays<- log10(train_Data$NActDays+0.5)
#Representamos el histograma con los datos transformados
histogram(new_NActDays, x_label='Días con ediciones')

new_NPages<- log10(train_Data$NPages+0.5)
#Representamos el histograma con los datos transformados
histogram(new_NPages, x_label='Páginas diferentes editadas')

# Resumen de la variable pagesWomen en train_Data.
summary(train_Data$pagesWomen)

# Veamos la forma de su distribución con un histograma 
pagesWomen_hist<-histogram(pagesWomen, x_label='Ediciones en pág. Mujer'); pagesWomen_hist

# Veamos su función de densidad de probabilidad.
pagesWomen_pdf<-pdf(pagesWomen, x_label='Ediciones en pág. Mujer'); pagesWomen_pdf

# Diagrama de caja.
bp_pagesWomen<-boxplot(pagesWomen, x_label='Ediciones en pág. Mujer'); bp_pagesWomen

#Binarización de pagesWomen
train_Data$pagesWomen_bin<-ifelse(train_Data$pagesWomen>0,1,0)
table(train_Data$pagesWomen_bin)

#Histograma de la variable.
ggplot(data=train_Data,aes(x=factor(pagesWomen_bin),fill=pagesWomen_bin)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="none") +
  ylab("Frecuencia relativa") +
  xlab("Variable respuesta: Grupo")

# Resumen de la variable wikiprojWomen en train_Data.
summary(train_Data$wikiprojWomen)

#Histograma - wikiprojWomen
histogram(wikiprojWomen, x_label = "Ediciones")

#wikiprojWomen - PDF
pdf(wikiprojWomen, x_label = "Ediciones")

#wikiprojWomen - Boxplot
boxplot(wikiprojWomen, x_label = "Ediciones")

#Binzarizamos variable
train_Data$wikiprojWomen_bin<-ifelse(train_Data$wikiprojWomen>0,1,0)

#Resumen de la nueva variable.
table(train_Data$wikiprojWomen_bin)

#Histograma de la variable.
ggplot(data=train_Data,aes(x=factor(wikiprojWomen_bin),fill=wikiprojWomen_bin)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="none") +
  ylab("Frecuencia relativa") +
  xlab("Variable respuesta: Grupo")

#Resumen de la variable ns_user.
summary(train_Data$ns_user)

#ns_user - Histograma
histogram(ns_user, x_label = "Número de ediciones")

#ns_user - PDF
pdf(ns_user,  x_label = "Número de ediciones")

#ns_user - Boxplot
boxplot(ns_user, x_label = "Número ediciones")

boxcox(lm(train_Data$ns_user ~ 1))

new_ns_user<-log10(train_Data$ns_user)
#Histograma con los datos transformados.
histogram(new_ns_user,x_label = "Número ediciones")

#Resumen de ns_wikipedia.
summary(train_Data$ns_wikipedia)

#ns_wikipedia - Histograma
histogram(ns_wikipedia, x_label = "Número de ediciones")

#ns_wikipedia - PDF
pdf(ns_wikipedia, x_label = "Número de ediciones")

new_ns_wikipedia<-log10(train_Data$ns_wikipedia +0.5)
#Histograma con los datos transformados.
histogram(new_ns_wikipedia,x_label = "Número ediciones")

#Resumen de la variable
summary(train_Data$ns_talk)

#ns_talk - Histograma
histogram(ns_talk, x_label = "Número de ediciones")

#ns_talk - PDF
pdf(ns_talk,x_label = "Número de ediciones")

#ns_talk - Boxplot
boxplot(ns_talk, x_label = "Número de ediciones")

# ns_talk - Transformación de datos (box cox).
boxcox(lm((train_Data$ns_talk)+0.5 ~ 1))

new_ns_talk<-log10(train_Data$ns_talk + 0.5)
#Histograma con los datos transformados.
histogram(new_ns_talk,x_label = "Número ediciones")

#Resumen de ns_userTalk.
summary(train_Data$ns_userTalk)

#ns_userTalk - Histograma
histogram(ns_userTalk,x_label = "Número de ediciones")

#ns_userTalk - PDF
pdf(ns_userTalk,x_label = "Número de ediciones")

#ns_userTalk - Boxplot
boxplot(ns_userTalk, x_label = "Número de ediciones")

boxcox(lm((train_Data$ns_userTalk + 0.5) ~ 1))

new_ns_userTalk<-log10(train_Data$ns_userTalk +0.5)
#Histograma con los datos transformados.
histogram(new_ns_userTalk,x_label = "Número ediciones")

#Resumen de ns_content.
summary(train_Data$ns_content)

#ns_content - Histograma
histogram(ns_content, x_label = "Número de ediciones")

#ns_content - PDF
pdf(ns_content, x_label = "Número de ediciones")

#ns_content - Boxplot
boxplot(ns_content,x_label = "Número de ediciones")

# ns_content - Transformación de datos (box cox).
boxcox(lm(train_Data$ns_content + 0.5 ~ 1)) 


new_ns_content<- log10(train_Data$ns_content +0.5)
#Representamos el histograma con los datos transformados
histogram(new_ns_content, x_label='Número de ediciones')

# Transformación de variables
new_NActDays<- log10(train_Data$NActDays+0.5)
new_NPages<- log10(train_Data$NPages)
new_ns_talk<- log10(train_Data$ns_talk+0.5)
new_ns_userTalk<- log10(train_Data$ns_userTalk+0.5)
new_ns_content<- log10(train_Data$ns_content+0.5)
new_ns_user<- log10(train_Data$ns_user)
new_ns_wikipedia<- log10(train_Data$ns_wikipedia+0.5)

# Eliminamos las variables que no podemos introducir al PCA
NEds<-train_Data$NEds
NPcreated<-train_Data$NPcreated
NDays<-train_Data$NDays
delete_PCA <- subset(train_Data, select=-c(gender,E_NEds, E_Bpag, firstDay, lastDay, NIJ,weightIJ,NActDays,NPages,ns_talk,ns_userTalk,ns_content,ns_user, ns_wikipedia,wikiprojWomen,pagesWomen,ano_inicial,mes_inicial,dia_inicial,ano_final,mes_final,dia_final))

# Añadimos variables a introducir en el PCA
train_PCA<- cbind(delete_PCA,new_NActDays, new_NPages,new_ns_talk, new_ns_userTalk, 
                  new_ns_content,new_ns_user,new_ns_wikipedia,NEds,NPcreated,NDays)
dim(train_PCA)

PCA_std <- prcomp(train_PCA, scale=T); PCA_std

summary(PCA_std)

plot(PCA_std)

plot(PCA_std$x[,1], PCA_std$x[,2], col=train_Data$gender)

plot(train_PCA$new_NActDays, train_PCA$new_NPages, col=train_Data$gender)

#Eliminamos las variables que no podemos usar
delete_Kmeans <- subset(train_Data, select=-c(gender,E_NEds, E_Bpag, firstDay, lastDay, 
                                              NIJ, weightIJ,NActDays,NPages,ns_talk,ns_userTalk,
                                              ns_content,ns_user, ns_wikipedia,wikiprojWomen,pagesWomen,ano_inicial,ano_final,mes_inicial,dia_inicial,dia_final,mes_final))

# Subconjunto de datos
train_Kmeans<- cbind(delete_PCA,new_NActDays, new_NPages,new_ns_talk, new_ns_userTalk, 
                     new_ns_content,new_ns_user,new_ns_wikipedia,NEds,NPcreated,NDays)

#Estandarizamos las variables
train_Kmeans_std <- scale(train_Kmeans)

# Método del codo
fviz_nbclust(train_Kmeans_std, kmeans, method = "wss")

# 2 centroides
k2 <- kmeans(train_Kmeans_std, centers = 2, nstart = 25)

k2plot <- fviz_cluster(k2, data = train_Kmeans_std); k2plot

k3 <- kmeans(train_Kmeans_std, centers = 3, nstart = 25)

k3plot <- fviz_cluster(k3, data = train_Kmeans_std); k3plot

# 7 centroides
k7 <- kmeans(train_Kmeans_std, centers = 7, nstart = 25)

k7plot <- fviz_cluster(k7, data = train_Kmeans_std); k7plot

# Método de la silueta
fviz_nbclust(train_Kmeans_std, kmeans, method = "silhouette")

# Usamos k2 según la anterior gráfica
sil <- silhouette(k2$cluster, dist(train_Kmeans_std))

#Dividimos los datos
fviz_silhouette(sil)

gap_stat <- clusGap(train_Kmeans_std, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

# Usamos la distancia euclidea
nb <- NbClust(train_Kmeans_std, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

# Matriz de disimilaridades
d <- dist(train_Kmeans_std, method = "euclidean")

# Clustering jerárquico usando enlace completo
hc1 <- hclust(d, method = "complete" )

# Dendrograma
plot(hc1, cex = 0.6, hang = -1,labels = FALSE)

# Método de Ward
hc2 <- hclust(d, method = "ward.D2" )

# Cortamos para 2 clústeres
sub_grp2 <- cutree(hc2, k = 2)
plot(hc2, cex = 0.6,labels = FALSE)
rect.hclust(hc2, k = 2, border = 2:5)

#Número de observaciones en cada cluster.
table(sub_grp2)

fviz_cluster(list(data=train_Kmeans_std,cluster=sub_grp2))

# Método de Ward
hc3 <- hclust(d, method = "ward.D2" )

# Cortamos para 3 clústeres
sub_grp3 <- cutree(hc3, k = 3)
plot(hc3, cex = 0.6,labels = FALSE)
rect.hclust(hc3, k = 3, border = 2:5)

#Número de observaciones en cada cluster.
table(sub_grp3)

fviz_cluster(list(data=train_Kmeans_std,cluster=sub_grp3))

hc4 <- agnes(train_Kmeans_std, method = "ward" )

# AGNES: Agglomerative Nesting
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrograma de AGNES",labels = FALSE) 
rect.hclust(hc4, k = 2, border = 2:5)

# Evaluación de métodos

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Función para calcular el coeficiente de agrupamiento
ac <- function(x) {
  agnes(train_Kmeans_std, method = x)$ac
}

map_dbl(m, ac)

# Clustering jerárquico divisivo
hc3 <- diana(train_Kmeans_std)

# Coeficiente de división; cantidad de estructura de agrupación encontrada
hc3$dc


# DIANA: divisive analysis.
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram de DIANA",labels = FALSE)