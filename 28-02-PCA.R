#Si los data no salen normales aplicamos la raíz cuadrada o el logaritmo

#No podemos hacer binaria una categoría y tener 200 de un tipo y 8000 del otro
#Tienen que estar balanceados, es inútil tenerlo así

############################### LIBRARIES #################################################
library(caret)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(dplyr)
library(pdp) #pima dataset
library(corrplot)
library(palmerpenguins)
library(plotly)
library(car)
library(GGally)
library(MASS)
library(modeest)
#install.packages("modeest")

############################### MAIN CODE #################################################
wd<- getwd()
orig_df<-read_csv("/Users/pablopardo/Desktop/R_Studio/AA/Practica_AA1/datos/data.csv", show_col_types=FALSE)
#We eliminate the data with 'unknown' values in the variable gender.
df_no_unknown<-orig_df[orig_df$gender !=0,]
#We remove the other two target variables.
df<-subset(df_no_unknown, select=-c(C_api,C_man)); df

#We should have 19 variables now.
dim(df)

#Division of data
set.seed(123)

#ARGUMENTS OF 'createdataPartition' FUNCTION {
  #times: Nº of partitions
  #p: Proportion of the total that has the sample obtained 

  #RETURN: data indices (no data values)
#}

#I get the indices for the 60% of the total data
indices_60<-createDataPartition(df$gender, times =1, p=0.6)

# 60% of data is used to train the model
train_Data <- df[indices_60$Resample1,] #I select all columns
dim(train_Data)

#The minus sign (-) indicates "no". I select the data I haven't selected before: the 40% left
data_test_val<-df[-indices_60$Resample1,] #data to split into test and validation
dim(data_test_val)

#I get the indices for the 20% of the total data (50% of data_test_val)
indices_40<- createDataPartition(data_test_val$gender, times= 1, p=0.5)

#20% of the data is used to test the model.
test_Data <- data_test_val[indices_40$Resample1,] #I select all columns
dim(test_Data)

#I select the data I haven't selected before: the 50% left (20% of the total)
val_Data<- data_test_val[-indices_40$Resample1,]#I select all columns
dim(val_Data)

#Check if dimensions match. Objective 3145 obs (it does match)
dim(train_Data)+dim(test_Data)+dim(val_Data)


######################  EDA  ##########################

#See one row of data
train_Data[1,]

#We tell R the variables,  is a factor
train_Data$gender<-as.factor(train_Data$gender)
train_Data$E_NEds <-as.factor(train_Data$E_NEds)
train_Data$E_Bpag <-as.factor(train_Data$E_Bpag)

#Lets see that all our variables are numeric, except the gender factor.
str(train_Data)

#FUNCTIONS USED IN THE EDA:

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


#Complete analysis of variables.

#--- TARGET: gender. 1: Male; 2: Female.

#Lets see the proportion of male and female.
prop.table(table(train_Data$gender))

#Lets represent the data with ggplot2.

#Bar plot
ggplot(data=train_Data,aes(x=gender,fill=gender)) +
  geom_bar(aes(y=(..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  theme(legend.position="none") +
  ylab("Frecuencia relativa") +
  xlab("Género")


#--- NDays: Número de días totales.

#Lets see the range of this variable.
summary(train_Data$NDays) #We can appreciate the median is similar to the mean.

#EN ESTE CASO NO SE APLICA LO SIGUIENTE PORQUE NO MEJORA LA DISTRIBUCION. SE HA PROBADO LOG Y SQRT.
#Si la variable es estrictamente positiva, no tiene un límite superior para sus valores, 
#y su rango abarca dos o más órdenes de magnitud (potencias de 10), entonces la transformación
#logarítmica suele ser útil. A la inversa, cuando la variable tiene un rango de valores 
#pequeño (menor de un orden de magnitud), el logaritmo o cualquier otra transformación simple
#no ayudará mucho.

#Moda
mlv(train_Data$NDays, method = "meanshift")

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#Its simmetry it's not bad, but it doesn't seem data comes from a normal distribution
NDays_hist<-histogram(NDays, x_label= "Último día-Primer día +1"); NDays_hist

#Lets see its probability density function
NDays_pdf<- pdf(NDays, x_label='Último día-Primer día +1'); NDays_pdf

#Lets do a shapiro test to contrast if the data comes from a normal distribution
NDays_shtest<-shapiro.test(train_Data$NDays); NDays_shtest
#The p-value confirms at 100% that data isn't normal

#Box plot
bp_NDays<-boxplot(NDays, x_label='Último día-Primer día +1'); bp_NDays
#NO OUTLIERS


#--- NActDays:Número de días con ediciones. 

#Lets see the range of this variable.
summary(train_Data$NActDays) #We can appreciate the median is not similar to the mean.

mlv(train_Data$NActDays, method = "meanshift")


#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it.

NActDays_hist<-histogram(NActDays, x_label='Días con ediciones'); NActDays_hist

#Lets see its probability density function
NActDays_pdf<- pdf(NActDays, x_label='Días con ediciones'); NActDays_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_NActDays<-boxplot(NActDays, x_label='Días con ediciones'); bp_NActDays
#HUGE PERCENTAGE OF OUTLIERS as we have a long right tail.

#Data transformation with box cox.

boxcox(lm(train_Data$NActDays ~ 1)) #We see it's better to use lambda=0 --> log(x)
new_NActDays<- log10(train_Data$NActDays)
histogram(new_NActDays, x_label='Días con ediciones')


#--- NPages: Número de páginas diferentes editadas.

#Lets see the range of this variable.
summary(train_Data$NPages) #We can appreciate the median is not similar to the mean.

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it, as it happens with NActDays

NPages_hist<-histogram(NPages, x_label='Páginas diferentes editadas'); NPages_hist

#Lets see its probability density function
NPages_pdf<-pdf(NPages, x_label='Páginas diferentes editadas'); NPages_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_NPages<-boxplot(NPages, x_label='Páginas diferentes editadas'); bp_NPages
#HUGE PERCENTAGE OF OUTLIERS as we have a long right tail.

#Data transformation with box cox.

boxcox(lm(train_Data$NPages ~ 1)) #We see it's better to use lambda=0
new_NPages<- log10(train_Data$NPages)
histogram(new_NPages, x_label='Páginas diferentes editadas')


#--- NPcreated: Número de páginas creadas.

#Lets see the range of this variable.
summary(train_Data$NPcreated) #We can appreciate the median is not similar to the mean.

#NPcreated is a semi-sparse variable, as huge percent of values are 0
train_Data$NPcreated

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it, as it happens with NActDays or NPages

NPcreated_hist<-histogram(NPcreated, x_label='Páginas creadas'); NPcreated_hist

#Lets see its probability density function
NPcreated_pdf<-pdf(NPcreated, x_label='Páginas creadas'); NPcreated_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_NPcreated<-boxplot(NPcreated, x_label='Páginas creadas'); bp_NPcreated
#HUGE PERCENTAGE OF OUTLIERS as we have a long right tail

#Data transformation with box cox. CHECK!!

#boxcox(lm(train_Data$NPcreated ~ 1)) A BIT SPARSE VARIABLE. DOESNT WORK
#new_NPcreated<- log10(train_Data$NPcreated)
#histogram(new_NPcreated)


#--- pagesWomen: Número de ediciones en páginas relacionadas con la mujer.

#Lets see the range of this variable.
summary(train_Data$pagesWomen) #We can appreciate the median is far away from the mean.

#PagesWomen is an sparse variable, as almost all values are 0
train_Data$pagesWomen

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it, as it happens with NActDays

pagesWomen_hist<-histogram(pagesWomen, x_label='Ediciones en pág. Mujer'); pagesWomen_hist

#Lets see its probability density function
pagesWomen_pdf<-pdf(pagesWomen, x_label='Ediciones en pág. Mujer'); pagesWomen_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_pagesWomen<-boxplot(pagesWomen, x_label='Ediciones en pág. Mujer'); bp_pagesWomen
#HUGE PERCENTAGE OF OUTLIERS due to the long right tail

#Data transformation with box cox.

#boxcox(lm(train_Data$pagesWomen ~ 1)) A BIT SPARSE VARIABLE. DOESNT WORK
#new_NPages<- log10(train_Data$NPages)
#histogram(new_NPages)



############################### PCA ###############################

#pca= A*X
#prcomp me devuelve las airquality
#si ponemos  $x obtenemos el pca como tal

# Interpreto el pca en función de los coeficientes de las a.

#---------------------------------------------------------------
# Para realizar nuestro PCA solo seleccionamos las variables continuas o discretas con muchos valores
# También eliminamos variables redundantes, que categorizan otras más generales: E_Neds, E_Bpag, NIJ (estas tres en el fondo explican N_Eds)
# También eliminamos las fechas y tiempos, expresados con un número completo

train_PCA <- subset(train_Data, select=-c(gender,E_NEds, E_Bpag, firstDay, lastDay, NIJ)); df

#Ahora trabajamos con 14 Variables
dim(train_PCA)

#Realizamos el PCA con los datos correctos
PCA<-prcomp(train_PCA); PCA
summary(PCA)
plot(PCA)

#NOTA: Las standard deviations son los autovalores de la matriz de correlaciones, y representan la variabilidad en 
#cada componente. A mayor valor, más relevante es la variable correspondiente a efectos de visualización.

#Observamos que las componentes con mayor variabilidad son PC1, PC2, PC3, PC4. A partir de PC4, la std baja.

#No se muestran las últimas 4 componentes puesto que presentan una despreciable std.

#Específicamente en PC1, antes de estandarizar los datos, las variables más influyentes parecen ser NEds y ns_content, puesto que toman
#valores muy altos en comparación con el resto de variables. 


# Estandarizamos los datos

PCA_std <- prcomp(train_PCA, scale=T); PCA_std
summary(PCA_std)
plot(PCA_std)

# Con las 3 primeras componentes recogemos el 68% de la variabilidad. Con 4 recogeríamos un 76%. 
# PC1 asigna pesos positivos a todas las variables.
# PC2 ordena positivamente por el numero de ediciones en proyectos wiki de mujeres y las páginas de con ediciones de mujeres
# PC3 ordena negativamente por el peso del estrato
# PC4 ordena positivamente por el número de días totales, ponderando en sentido contrario el peso del estrato.


# Seleccionamos las 4 primeras componentes. VER SI HAY QUE METER AL PCA LAS COMPONENTES MODIFICADAS
plot(PCA_std$x[,1:4])

