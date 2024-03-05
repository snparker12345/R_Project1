library(caret)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
library(cluster)    # algoritmos de clustering 
library(factoextra)
############################### MAIN CODE #################################################
orig_df<-read_csv("Practica_AA1/datos/data.csv", show_col_types=FALSE)
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

# create new df
head(train_Data)

df <- train_Data

# eliminamos valores faltantes
df <- na.omit(df)

# escalado de todas las variables
df <- scale(df)

head(df)

distance <- get_dist(df)

# create graph of gradient
gradient_graph <- fviz_dist(distance, gradient = list(low = "white", mid = "black", high = "blue")); gradient_graph

# create k means cluster with 2 centers
k2 <- kmeans(df, centers = 2, nstart = 25)
kmeansnum <- str(k2); kmeansnum
k2

# create cluster with 8 centers based on ideal cluster number plot
fviz_cluster(k2, data = df)

set.seed(123)

# find ideal number of clusters 
fviz_nbclust(df, kmeans, method = "wss")

k8 <- kmeans(df, centers = 8, nstart = 25); k8

# plot with ideal number of clusters
k8clus <- fviz_cluster(k8, data = df); k8clus

# find ideal number of clusters with the silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")

library(cluster)

# create new silhouette clusters
sil <- silhouette(k2$cluster, dist(df))

fviz_silhouette(sil)

set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)

print(gap_stat, method = "firstmax")

# this finds that the ideal number of clusters is 1
fviz_gap_stat(gap_stat)

k1 <- kmeans(df, centers = 1, nstart = 25)

fviz_cluster(k1, data = df)
