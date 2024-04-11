
# BAGGING

bagging_model <- bagging(deposit ~., data=df,nbagg=100,
                         control = rpart.control(minsplit = 2, cp = 0,maxdepth = 20))
bagging_model

# sobre la partición de entrenamiento
prediction <- predict(bagging_model, df, type = 'class')
prediction_prob <- predict(bagging_model, df, type = 'prob')
cf <- confusionMatrix(prediction, as.factor(df$deposit),positive="yes")
print(cf)

#------------------------------------------------------------------------

# RANDOM FOREST

library(randomForest)
library(caret)

rf <- randomForest(as.factor(deposit)~., data=df, importance=TRUE,proximity=TRUE) 
print(rf)
plot(rf)

# sobre la partición de prueba
df.test <- bank.test %>%
  mutate(fmarital=as.factor(marital))%>%
  select(age,job,housing,marital=fmarital,education,duration,poutcome,balance,deposit)
df.test$deposit=as.factor(df.test$deposit)
prediction.rf <- predict(rf, df.test,type="prob")[,2]
clase.pred.rf=ifelse(prediction.rf>0.5,"yes","no")
cf <- confusionMatrix(as.factor(clase.pred.rf), as.factor(df.test$deposit),positive="yes")
print(cf)

#Podemos averiguar la importancia de las variables en el modelo.
varImp(rf)
varImpPlot(rf)

#obtener una visualización del funcionamiento del Bosque con la función 
MDSplot(rf,as.factor(df$deposit),k=3)

#------------------------------------------------------------------------

# BOOSTING

#XGBoost
library(purrr)
library(dplyr)
library(caret)
library(xgboost)

#convertimos los datos a numéricos
df.train <- map_df(df, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

datos <- list()
datos$train <- df.train

df.test <- map_df(df.test, function(columna) {
  columna %>% 
    as.factor() %>% 
    as.numeric %>% 
    { . - 1 }
})

datos$test <- df.test


# Convertimos los datos al formato Dmatrix
datos$train_mat  <- 
  datos$train %>% 
  select(-deposit) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = datos$train$deposit)
datos$test_mat  <- 
  datos$test %>% 
  select(-deposit) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = datos$test$deposit)

# Entrenamiento del modelo
datos$modelo_01 <- xgboost(data = datos$train_mat,  
                           objective = "binary:logistic", #clasificación binaria
                           nround = 10, # número máximo de iteraciones boosting 
                           max_depth=2, # número de nodos de bifurcación de los árboles de de decisión usados en el entrenamiento
                           eta =0.3, # La tasa de aprendizaje del modelo
                           nthread =2) #  El número de hilos computacionales que serán usados en el proceso de entrenamiento. 


# Interpretamos nuestro modelo
datos$modelo_01


# Visualización del conjunto de árboles como una única unidad 
xgb.plot.multi.trees(model = datos$modelo_01)


# convertimos log odds a probabilidades
odds_to_probs <- function(odds){
  return(exp(odds)/ (1 + exp(odds)))
}
# ejemplo 
odds_to_probs(-1.2496)

# importancia sobre cada característica
importance_matrix <- xgb.importance(model = datos$modelo_01)

xgb.plot.importance(importance_matrix)


# Predicciones en la muestra de prueba
datos$predict_01 <- predict(datos$modelo_01, datos$test_mat)
cbind(datos$predict_01 > 0.5, datos$test$deposit) %>% 
  data.frame() %>% 
  table() %>% 
  confusionMatrix(positive="1")
