#Si los data no salen normales aplicamos la raíz cuadrada o el logaritmo

#No podemos hacer binaria una categoría y tener 200 de un tipo y 8000 del otro
#Tienen que estar balanceados, es inútil tenerlo así

############################### LIBRARIES #################################################
library(caret)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
#install.packages("viridis")
############################### MAIN CODE #################################################
orig_df<-read_csv("datos/data.csv", show_col_types=FALSE)
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
test_data <- data_test_val[indices_40$Resample1,] #I select all columns
dim(test_data)

#I select the data I haven't selected before: the 50% left (20% of the total)
val_data<- data_test_val[-indices_40$Resample1,]#I select all columns
dim(val_data)

#Check if dimensions match. Objective 3145 obs (it does match)
dim(train_Data)+dim(test_data)+dim(val_data)


######################  EDA  ##########################

#See one row of data
train_Data[1,]

#We tell R the variable gender is a factor
train_Data$gender<-as.factor(train_Data$gender)

#Lets see that all our variables are numeric, except the gender factor.
str(train_Data)

#FUNCTIONS USED IN THE EDA:

histogram=function(x_var){ #VER COMO PONERLE TITULO AL EJE X PARA DIFERENCIAR DE QUE GRAFICO ES CADA VAR
  
  ggplot(train_Data, aes(x=x_var)) + 
    geom_histogram(bins=40, color='black', fill='white')+ 
    geom_vline(aes(xintercept=mean(x_var)),color="blue", linetype="dashed", size=1)
}

pdf=function(x_var){ #VER COMO PONERLE TITULO AL EJE X PARA DIFERENCIAR DE QUE GRAFICO ES CADA VAR
  
  ggplot(train_Data, aes(x = x_var)) +
    geom_density() +
    ggtitle('PDF')
}

boxplot= function(x_var){ #VER COMO PONERLE TITULO AL EJE X PARA DIFERENCIAR DE QUE GRAFICO ES CADA VAR
  
  ggplot(train_Data,
         aes(x_var)) +
    geom_boxplot()
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


#Let see the form of its distribution with an histogram (blue line: mean of variable)
#Its simmetry it's not bad, but it doesn't seem data comes from a normal distribution
NDays_hist<-histogram(train_Data$NDays); NDays_hist

#Lets see its probability density function
NDays_pdf<- pdf(train_Data$NDays); NDays_pdf

#Lets do a shapiro test to contrast if the data comes from a normal distribution
NDays_shtest<-shapiro.test(train_Data$NDays); NDays_shtest
#The p-value confirms at 100% that data isn't normal

#Box plot
bp_NDays<-boxplot(train_Data$NDays); bp_NDays
#No outliers


#--- NActDays:Número de días con ediciones. NOT FINISHED!!!

#Lets see the range of this variable.
summary(train_Data$NActDays) #We can appreciate the median is not similar to the mean.

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it.

NActDays_hist<-histogram(train_Data$NActDays); NActDays_hist

#Lets see its probability density function
NActDays_pdf<- pdf(train_Data$NActDays); NActDays_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_NActDays<-boxplot(train_Data$NActDays); bp_NActDays

#HUGE PERCENTAGE OF OUTLIERS. We'll have to see how we manage that


#--- NPages: Número de páginas diferentes editadas.

#Lets see the range of this variable.
summary(train_Data$NPages) #We can appreciate the median is not similar to the mean.

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it, as it happens with NActDays

NPages_hist<-histogram(train_Data$NPages); NPages_hist

#Lets see its probability density function
NPages_pdf<-pdf(train_Data$NPages); NPages_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_NPages<-boxplot(train_Data$NPages); bp_NPages

#HUGE PERCENTAGE OF OUTLIERS. We'll have to see how we manage that


#--- NPcreated: Número de páginas creadas.

#Lets see the range of this variable.
summary(train_Data$NPcreated) #We can appreciate the median is not similar to the mean.

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it, as it happens with NActDays or NPages

NPcreated_hist<-histogram(train_Data$NPcreated); NPcreated_hist

#Lets see its probability density function
NPcreated_pdf<-pdf(train_Data$NPcreated); NPcreated_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_NPcreated<-boxplot(train_Data$NPcreated); bp_NPcreated

#HUGE PERCENTAGE OF OUTLIERS. We'll have to see how we manage that


#--- pagesWomen: Número de ediciones en páginas relacionadas con la mujer.

#Lets see the range of this variable.
summary(train_Data$pagesWomen) #We can appreciate the median is far away from the mean.

#PagesWomen is an sparse variable, as almost all values are 0
train_Data$pagesWomen

#Let see the form of its distribution with an histogram (blue line: mean of variable)
#We'll have to probably transform this data to be able to work with it, as it happens with NActDays

pagesWomen_hist<-histogram(train_Data$pagesWomen); pagesWomen_hist

#Lets see its probability density function
pagesWomen_pdf<-pdf(train_Data$pagesWomen); pagesWomen_pdf

#We don't even need to do a shapiro test to contrast if the data comes from a normal distribution.
#DATA DOESN'T COME FROM A NORMAL DISTRIBUTION

#Boxplot
bp_pagesWomen<-boxplot(train_Data$pagesWomen); bp_pagesWomen

#HUGE PERCENTAGE OF OUTLIERS. We'll have to see how we manage that.

# ------ Sophia Section ---- #

histogram_log=function(x_var){ #VER COMO PONERLE TITULO AL EJE X PARA DIFERENCIAR DE QUE GRAFICO ES CADA VAR
  
  ggplot(train_Data, aes(x=x_var)) + 
    geom_histogram(bins=10, color='black', fill='white')+ 
    geom_vline(aes(xintercept=mean(x_var)),color="blue", linetype="dashed", size=1)
}

histogram_ten_bins=function(x_var){ #VER COMO PONERLE TITULO AL EJE X PARA DIFERENCIAR DE QUE GRAFICO ES CADA VAR
  
  ggplot(train_Data, aes(x=x_var)) + 
    geom_histogram(bins=10, color='black', fill='white')+ 
    geom_vline(aes(xintercept=mean(x_var)),color="blue", linetype="dashed", size=1)
}

# histogram of wiki projects, log is relatively centered
wikiProjWomenHist <- histogram_log(log(train_Data$wikiprojWomen)); wikiProjWomenHist

# extremely right skewed boxplot, the majority of values at 0 with large disparities
bpWomen <- boxplot(train_Data$wikiprojWomen); bpWomen

# creates a table showing each data point (and how they are very skewed)
table(train_Data$wikiprojWomen)

# ----- NS User ---#

# histogram for train data and wiki projects for women (many zero values here)
trainDataHist <- histogram_log(log(train_Data$ns_user)); trainDataHist

# creates a table showing each data point for ns users; 
table(train_Data$ns_user)

# extremely right skewed boxplot, the majority of values at 0 with large disparities
bpUser <- boxplot(train_Data$ns_user); bpUser

# --- NS Wikipedia ---#

#large variations among data, right skewed
wikiHist <- histogram_log(log(train_Data$ns_wikipedia)); wikiHist

# huge variety of values between 0 and 2989
table(train_Data$ns_wikipedia)

# extremely right skewed boxplot, tons of outliers
bpWiki <- boxplot(train_Data$ns_wikipedia); bpWiki

# --- NS Talk ---#

#large variations among data, right skewed
nsHist <- histogram_log(log(train_Data$ns_talk)); nsHist

# huge variety of values between 0 and 4788
table(train_Data$ns_talk)

# boxplot very right skewed
nsTalkBp <- boxplot(train_Data$ns_talk); nsTalkBp

# --- NS User Talk --- #

#large variations among data, right skewed
nsUserTalk <- histogram_log(log(train_Data$ns_userTalk)); nsUserTalk

# huge variety of values between 0 and about 9000 with an outlier at 12004
table(train_Data$ns_userTalk)

#right skewed with tons of outliers
userTalk <- boxplot(train_Data$ns_userTalk); userTalk

# --- ns content --- #

#large variations among data, right skewed
nsContent <- histogram_log(log(train_Data$ns_content)); nsContent

# huge variety of values between 0 and 2989
table(train_Data$ns_content)

content <- boxplot(train_Data$ns_content); content

#--- IJ -- #

# a nice relatively normal loking graph, centered around < 1.0
weightIJHist <- histogram_ten_bins(train_Data$weightIJ); weightIJHist

# values from 0 to 2
table(train_Data$weightIJ)

ij <- boxplot(train_Data$weightIJ); ij

#--- NIJ -- #

# a nice relatively normal loking graph, centered around < 1000
NIJHist <- histogram_ten_bins(train_Data$NIJ); NIJHist

# values from 297 to 1595, relatively regular in between
table(train_Data$NIJ)

nij <- boxplot(train_Data$NIJ); nij



