#Si los data no salen normales aplicamos la raíz cuadrada o el logaritmo
#Hay que fijarnos en las variables target: gender, gender, C_man

#No podemos hacer binaria una categoría y tener 200 de un tipo y 8000 del otro
#Tienen que estar balanceados, es inútil tenerlo así

#Si mi variable a predecir es continua y no binaria --> Hay que pasarla a binaria
# En principio nosotros tenemos 3 clases.

#################################################################################
# installing caret
install.packages("caret", dependencies = TRUE)

# find name of wd
wdname <- getwd()

# everyone starts from the same directory, practica
wdname <- paste(wdname, "/Practica_AA1", sep='')

# sets wd to the name we have found
setwd(wdname)

# reads the data file
prevdf<-read.csv("datos/data.csv")

# removes the lines with an unknown gender
dfwithgender <- prevdf[prevdf$gender != 0,]

# removes C api and C man columns
df <- subset(dfwithgender, select = -c(C_api,C_man) )

# finds the distribution of the dataframe
dim(df)

#Division of data
library(caret)
set.seed(123)

#ARGUMENTS OF 'createDataPartition' FUNCTION {
  #times: Nº of partitions
  #p: Proportion of the total that has the sample obtained 

  #RETURN: Data indices (no data values)
#}


#I get the indices for the 60% of the total data
indices_60<-createDataPartition(df$gender, times =1, p=0.6)

# 60% of data is used to train the model
train_data <- df[indices_60$Resample1,] #I select all columns
dim(train_data)

#The minus sign (-) indicates "no". I select the data I haven't selected before: the 40% left
data_test_val<-df[-indices_60$Resample1,] #Data to split into test and validation
dim(data_test_val)

#I get the indices for the 20% of the total data (50% of data_test_val)
indices_40<- createDataPartition(data_test_val$gender, times= 1, p=0.5)

#20% of the data is used to test the model.
test_data <- data_test_val[indices_40$Resample1,] #I select all columns
dim(test_data)

#I select the data I haven't selected before: the 50% left (20% of the total)
val_data<- data_test_val[-indices_40$Resample1,]#I select all columns
dim(val_data)

#Check if dimensions match. Objective 4746 obs (it does match)
dim(train_data)+dim(test_data)+dim(val_data)

newdf <- train_data$wikiprojWomen

table(newdf)

# histogram for train data and wiki projects for women (many zero values here)
hist((train_data$wikiprojWomen)^(1/6), 10)

# creates a table showing each data point (and how they are very skewed)
table(train_data$wikiprojWomen)

# histogram for train data and wiki projects for women (many zero values here)
hist(sqrt(train_data$ns_user), 10)

# creates a table showing each data point for ns users; 
table(train_data$ns_user)

# this histogram has quickly dropping numbers from left to right, right skewed
hist(sqrt(train_data$ns_user), 50)

# creates a table showing each data point for ns users, there are very many points
table(train_data$ns_user)

# this histogram has quickly dropping numbers from left to right, right skewed
hist(sqrt(train_data$ns_user), 50)

# creates a table showing each data point for ns users, there are very many points
table(train_data$ns_user)

#ns_wikipedia, ns_talk, ns_userTalk, ns_content, weightIJ, nIJ

#large variations among data, right skewed
hist((train_data$ns_wikipedia)^(1/3), 20)

# huge variety of values between 0 and 2989
table(train_data$ns_wikipedia)

#large variations among data, right skewed
hist((train_data$ns_talk)^(1/3), 20)

# huge variety of values between 0 and 4788
table(train_data$ns_talk)

#large variations among data, right skewed
hist((train_data$ns_talk)^(1/3), 20)

# huge variety of values between 0 and 4788
table(train_data$ns_talk)

#large variations among data, right skewed
hist(sqrt(train_data$ns_userTalk), 200)

# huge variety of values between 0 and about 9000 with an outlier at 12004
table(train_data$ns_userTalk)


#large variations among data, right skewed
hist(sqrt(train_data$ns_content), 200)

# huge variety of values between 0 and 2989
table(train_data$ns_content)

# a nice relatively normal loking graph, centered around < 1.0
hist(train_data$weightIJ, 10)

# values from 0 to 2
table(train_data$weightIJ)

# a nice relatively normal loking graph, centered around < 1000
hist(train_data$NIJ, 10)

# values from 297 to 1595, relatively regular in between
table(train_data$NIJ)

