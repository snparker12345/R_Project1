library(naivebayes)
library(dplyr)
library(caret)
library(pROC)

######## NAIVE
model <- naive_bayes(as.factor(deposit) ~ ., data = df.train, usekernel = T) 
probabilities <- predict(model, df.test[,-9],type="prob")[,2]
classes <- as.numeric(probabilities>0.5)
confusionMatrix(table(classes, datos$test$deposit),positive="1")


######## COMPARACION DE MODELOS

par(pty = "s") # square
roc(df.test$deposit, probabilities,plot=TRUE, legacy.axes = TRUE,
    percent = TRUE, xlab = "Porcentaje Falsos positivos",
    ylab = "Porcentaje verdaderos postivios", col = "#377eb8", lwd = 2,
    print.auc = TRUE,legend=TRUE,  brier.in.legend =TRUE)


roc(df.test$deposit, predicciones, percent=TRUE, col="#4daf4a",lwd= 2,
    print.auc =TRUE, add=TRUE,print.auc.y = 40,plot=TRUE,legend=TRUE)

roc(df.test$deposit, prediction.dt, percent=TRUE, col="goldenrod",lwd= 2,
    print.auc =TRUE, add=TRUE,print.auc.y = 30,plot=TRUE,legend=TRUE)

roc(df.test$deposit, prediction.rf, percent=TRUE, col="salmon",lwd= 2,
    print.auc =TRUE, add=TRUE,print.auc.y = 20,plot=TRUE,legend=TRUE)

roc(df.test$deposit, prediction.knn, percent=TRUE, col="#977eb8",lwd= 2,
    print.auc =TRUE, add=TRUE,print.auc.y = 10,plot=TRUE,legend=TRUE)

#prediction <- predict(tune.fit, df.test, type = 'prob')

legend("bottom",
       legend=c("XGBOOST", "LOG.REG.", "DT", "RF","KNN"),
       col=c("#377eb8", "#4daf4a", "goldenrod","salmon","#977eb8"),
       lwd=2, cex =.5, xpd = TRUE, horiz = TRUE)
