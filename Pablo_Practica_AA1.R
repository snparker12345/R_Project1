#Si los data no salen normales aplicamos la raíz cuadrada o el logaritmo
#Hay que fijarnos en las variables target: gender, C_api, C_man

#No podemos hacer binaria una categoría y tener 200 de un tipo y 8000 del otro
#Tienen que estar balanceados, es inútil tenerlo así

#Si mi variable a predecir es continua y no binaria --> Hay que pasarla a binaria
# En principio nosotros tenemos 3 clases.

#################################################################################

data<-read.csv("/Users/pablopardo/Desktop/R_Studio/AA/Practica_AA1/datos/data.csv")
dim(data)

#Division of data
library(caret)
set.seed(123)

#ARGUMENTS OF 'createDataPartition' FUNCTION {
  #times: Nº of partitions
  #p: Proportion of the total that has the sample obtained 

  #RETURN: Data indices (no data values)
#}


#I get the indices for the 60% of the total data
indices_60<-createDataPartition(data$C_api, times =1, p=0.6)

# 60% of data is used to train the model
train_data <- data[indices_60$Resample1,] #I select all columns
dim(train_data)

#The minus sign (-) indicates "no". I select the data I haven't selected before: the 40% left
data_test_val<-data[-indices_60$Resample1,] #Data to split into test and validation
dim(data_test_val)

#I get the indices for the 20% of the total data (50% of data_test_val)
indices_40<- createDataPartition(data_test_val$C_api, times= 1, p=0.5)

#20% of the data is used to test the model.
test_data <- data_test_val[indices_40$Resample1,] #I select all columns
dim(test_data)

#I select the data I haven't selected before: the 50% left (20% of the total)
val_data<- data_test_val[-indices_40$Resample1,]#I select all columns
dim(val_data)

#Check if dimensions match. Objective 4746 obs (it does match)
dim(train_data)+dim(test_data)+dim(val_data)










