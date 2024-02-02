#Si los datos no salen normales aplicamos la raíz cuadrada o el logaritmo
#Hay que fijarnos en las variables target: gender, C_api, C_man

#No podemos hacer binaria una categoría y tener 200 de un tipo y 8000 del otro
#Tienen que estar balanceados, es inútil tenerlo así

#################################################################################


#Si mi variable a predecir es continua y no binaria --> Hay que pasarla a binaria
# En principio nosotros tenemos 3 clases.

setwd("C:/Users/snpar/ML")
getwd()
data<-read.csv("data.csv")
summary(data)
datafr <- data.frame(data)
filteredfr <- datafr[data$C_api != 'unknown',]

#Partir los datos: test, train, validation.  ACUERDATE DE PONER LA SEMILLA

set.seed(123)
dim(data)
# i'm not totally sure how to do this part bc I had to leave.
# i am confused on how to have 60 20 20 instead of 60 20.
sample <- sample(c(TRUE, FALSE), nrow(penguins), replace=TRUE, prob=c(0.6,0.2))
train  <- penguins[sample, ]
test   <- penguins[!sample, ]







