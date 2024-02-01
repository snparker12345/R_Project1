#Si los datos no salen normales aplicamos la raíz cuadrada o el logaritmo
#Hay que fijarnos en las variables target: gender, C_api, C_man

#No podemos hacer binaria una categoría y tener 200 de un tipo y 8000 del otro
#Tienen que estar balanceados, es inútil tenerlo así

#################################################################################


#Si mi variable a predecir es continua y no binaria --> Hay que pasarla a binaria
# En principio nosotros tenemos 3 clases.

data<-read.csv("/Users/pablopardo/Desktop/R_Studio/AA/Practica_AA1/datos/data.csv")

#Partir los datos: test, train, validation.  ACUERDATE DE PONER LA SEMILLA

dim(data)




