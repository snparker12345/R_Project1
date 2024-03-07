library(caret)
library(ggplot2)
library(readr)
library(tidyverse)
library(viridis)
library(cluster)    # algoritmos de clustering 
library(factoextra)
library(parameters)
library("NbClust")
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

delete_Kmeans <- subset(train_Data, select=-c(gender,E_NEds, E_Bpag, firstDay, lastDay, 
                                              NIJ, weightIJ,NActDays,NPages,ns_talk,ns_userTalk,
                                              ns_content,ns_user, ns_wikipedia))
new_NActDays<- log10(train_Data$NActDays+0.5)
new_NPages<- log10(train_Data$NPages)
new_ns_talk<- log10(train_Data$ns_talk+0.5)
new_ns_userTalk<- log10(train_Data$ns_userTalk+0.5)
new_ns_content<- log10(train_Data$ns_content+0.5)
new_ns_user<- log10(train_Data$ns_user)
new_ns_wikipedia<- log10(train_Data$ns_wikipedia+0.5)

# Subconjunto de datos
train_Kmeans<- cbind(delete_Kmeans,new_NActDays, new_NPages,new_ns_talk, new_ns_userTalk, 
                     new_ns_content,new_ns_user,new_ns_wikipedia)

#Estandarizamos las variables
train_Kmeans_std <- scale(train_Kmeans)

fviz_nbclust(train_Kmeans_std, kmeans, method = "wss")

k2 <- kmeans(train_Kmeans_std, centers = 2, nstart = 25)

k2plot <- fviz_cluster(k2, data = train_Kmeans_std); k2plot

# 3 centroides
k3 <- kmeans(train_Kmeans_std, centers = 3, nstart = 25)

k3plot <- fviz_cluster(k3, data = train_Kmeans_std); k3plot

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

nb <- NbClust(train_Kmeans_std, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans");  nb

#NO CONVERGE
n_clust <- n_clusters(as.data.frame(train_Kmeans_std), package = c("easystats", "NbClust", "mclust"), standardize = FALSE); n_clust

summary(n_clust)

# DEBERIA SALIR QUE 3 CLUSTERES SON LO OPTIMO
plot(n_clust)

