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
library(randomForest)
library(DALEX)

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

##### ==== NEW ACTIVE ML CODE 

#partition differently for this case

df$wikiprojWomen_bin<-ifelse(df$wikiprojWomen>0,1,0)

df$NActDays <-as.factor(df$NActDays)
df$NDays <-as.factor(df$NDays)
df$wikiprojWomen_bin <-as.factor(df$wikiprojWomen_bin)
df$wikiprojWomen <-as.factor(df$wikiprojWomen)
df$gender <-as.factor(df$gender)
df$NPages <-as.factor(df$NPages)

df_subset <- dplyr::select(df, wikiprojWomen, NDays, gender)

df_x = df[, 1:18]
df_y = df[, 19]

trellis.par.set(theme = col.whitebg(), warn = FALSE)
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=df_x, y=df_y, plot="density", scales=scales)

ggplot(df_subset, aes(x = wikiprojWomen, y = NDays, 
                        shape = gender, color = gender)) + 
  geom_point() +
  ggtitle("Two Features in gender: NPages vs. Ndays")


glfit <- glm(gender ~ ., data = df_subset, family = binomial(logit))

# Calculate the slope and intercept for the logistic regression line
slope <- coef(glfit)[2] / (-coef(glfit)[3])
intercept <- coef(glfit)[1] / (-coef(glfit)[3]) 

# Store the slope and intercept for later comparison
truth_slope <- slope
truth_intercept <- intercept

# Split the dataset into training and testing sets
df_subset$gender <- droplevels(df_subset$gender)
levels(df_subset$gender) <- c(0, 1)
smp_size <- floor(0.8 * nrow(df_subset))
set.seed(123)
train_ind <- sample(seq_len(nrow(df_subset)), size = smp_size)
train <- df_subset[train_ind, ]
test <- df_subset[-train_ind, ]

# Randomly select initial labeled samples and create the pool (unlabeled)
init_ind <- sample(seq_len(nrow(train)), size = 10)
train_init <- train[init_ind, ]
train_pool <- train[-init_ind, ]

# Initialize the logistic regression model with the initial labeled samples
glfit_init <- glm(gender ~ ., data = train_init, family = binomial(logit))

# Calculate the slope and intercept for the initial model
slope_init <- coef(glfit_init)[2] / (-coef(glfit_init)[3])
intercept_init <- coef(glfit_init)[1] / (-coef(glfit_init)[3]) 

# Plot the initial state of the active learning process
ggplot() + 
  geom_point(data = train, aes(x = NDays, y = wikiprojWomen)) +
  geom_abline(slope = slope_init, intercept = intercept_init) + 
  geom_point(data = train_init, aes(x = NDays, y = wikiprojWomen, 
                                    shape = gender, color = gender)) + 
  ggtitle("Active Learning: iteration 0 (initial state)") + 
  geom_abline(slope = truth_slope, intercept = truth_intercept, 
              linetype = "dashed", color = "purple")

# Initialize iteration counter for active learning process
iter <- 1

# Loop for active learning process (uncertainty sampling)
while (iter <= 30) {
  # Calculate the Euclidean distance between a point and a line defined by two other points
  dist2d <- function(a, b, c) {
    v1 <- b - c
    v2 <- a - b
    m <- cbind(v1, v2)
    d <- abs(det(m)) / sqrt(sum(v1 * v1))
    
    return(d)
  }
  
  # Return two points on a line defined by slope and intercept
  return_two_points_in_a_line <- function(slope, intercept){
    x1 <- 5
    x2 <- 6
    y1 <- slope * x1 + intercept
    y2 <- slope * x2 + intercept
    # plot(c(x1, x2), c(y1, y2), xlim = c(3, 7))
    point1 <- c(x1, y1)
    point2 <- c(x2, y2)
    
    return(list(point1 = point1, point2 = point2))
  }
  
  # Calculate the distance of each point in the pool to the logistic regression line
  train_pool$dist <- NA
  for (i in 1:nrow(train_pool)) {
    point <- c(train_pool$NDays[i], train_pool$wikiprojWomen[i])
    train_pool$dist[i] <- dist2d(point, two_points$point1, two_points$point2)
  }
  
  # Select the point in the pool with the minimum distance to the logistic regression line
  query_point_index <- which.min(train_pool$dist)
  query_point <- train_pool[query_point_index, c(1, 2, 3)]
  
  # Remove the selected point from the pool and add it to the labeled set
  train_pool <- train_pool[-query_point_index,]
  train_init_add <- rbind(train_init, query_point)
  
  # Fit a logistic regression model with the updated labeled set
  glfit_init_add <- glm(gender ~ ., data = train_init_add, family = binomial(logit))
  
  # Calculate the slope and intercept for the updated model
  slope_init_add <- coef(glfit_init_add)[2] / (-coef(glfit_init_add)[3])
  intercept_init_add <- coef(glfit_init_add)[1] / (-coef(glfit_init_add)[3])
  
  # Plot the updated state of the active learning process (iteration 1)
  ggplot() + 
    geom_point(data = train, aes(x = NDays, y = wikiprojWomen)) +
    geom_abline(slope = slope_init_add, intercept = intercept_init_add) + 
    geom_point(data = train_init_add, aes(x = NDays, y = wikiprojWomen, 
                                          shape = gender, color = gender)) +
    geom_point(data = query_point, aes(x = NDays, y = wikiprojWomen), 
               size = 7, shape = 23, color = "blue") +
    ggtitle(paste0("Active Learning (Uncertainty): iteration ", iter)) + 
    geom_abline(slope = truth_slope, intercept = truth_intercept, 
                linetype = "dashed", color = "purple")
  
  # Evaluate the model's performance using a confusion matrix
  predicted_prob <- predict(glfit_init, test, type = "response")
  predicted_classes <- ifelse(predicted_prob > 0.5, 1, 0)
  table(test$gender, predicted_classes, dnn = c("Obs", "Pred"))
  
  # Increment iteration counter
  iter <- iter + 1
}

