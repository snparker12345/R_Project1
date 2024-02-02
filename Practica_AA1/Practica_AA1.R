#Si los datos no salen normales aplicamos la raíz cuadrada o el logaritmo
#Hay que fijarnos en las variables target: gender, C_api, C_man

#No podemos hacer binaria una categoría y tener 200 de un tipo y 8000 del otro
#Tienen que estar balanceados, es inútil tenerlo así

#################################################################################


#Si mi variable a predecir es continua y no binaria --> Hay que pasarla a binaria
# En principio nosotros tenemos 3 clases.
library('caret')

#I import the data from the window at the bottom right, as with read.csv as sophie had, it didnt work for me

summary(data)
filteredfr <- data[data$C_api != 'unknown',]

#Partir los datos: test, train, validation.  ACUERDATE DE PONER LA SEMILLA

set.seed(123)
dim(data)

spec = c(train = .6, test = .2, validate = .2)

g = sample(cut(
  seq(nrow(filteredfr)), 
  nrow(filteredfr)*cumsum(c(0,spec)),
  labels = names(spec)
))

res = split(data, g)
addmargins(prop.table(table(g)))

train<-res$train
test<-res$test
validate<-res$validate










