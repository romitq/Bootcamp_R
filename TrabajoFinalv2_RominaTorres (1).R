#******************************************************************************#
#                      Leer datos de excel a R                                 #      
#******************************************************************************#
# IMPORTAR DATASET (csv)
#================================
#install.packages("readr")
library(readr)

# Paso II: buscar la ruta del archivo de csv
file.choose()
RUTACSV="C:\\Users\\Romina Torres\\Desktop\\Final bootcamp R\\games.csv"
# Importar datos
data=read.csv(RUTACSV)

# mirar datos
head(data)

# names() muestra los nombres de las variables de un dataframe
names(data)

#==============================================#
# 1. Estructura de datos                       #
#==============================================#

#1. Crear vectores con los títulos de la dataset

juegos_data <- c("app_id", "title", "date_release")
sistema_operativo <-c("win", "mac", "linux")
precios <-c("price_final", "price_original", "discount")
popularidad <-c("rating", "positive_ratio", "user_reviews", "steam_deck")


#Ver la estructura del df. Visualizaremos los nombres y el tipo de las variables
str(data)


#2. Crear vector numérico con el precio final de los videojuegos
#data[, 10]

vector_precioFinal<- c(data[, 10])
vector_precioFinal

#3. Crear una condición lógica de precios bajos (ejemplo precios menores a 10.99)
precio_menor<-ifelse(vector_precioFinal < 10.99, print(vector_precioFinal),0)
precio_menor

#Lo convertimos en vector
precios_bajos<-c(precio_menor)
precios_bajos

#4. Sumar 5 al vector creado
precios_bajos + 5

#5. Dividir la puntuación entre 2
precios_bajos/2

#6. Calcular la media, moda, max, min de los datos de tipo numérico (Verficar con
#la función Class)

str(data)
summary(data)

#Tipos de datos
sapply(data, class)

#-- media, max, min
summary(data$positive_ratio)
summary(data$user_reviews)
summary(data$price_final)
summary(data$price_original)
summary(data$discount)


#-- moda
mode <- function(x){
  return(as.numeric(names(which.max(table(x)))))
}

mode(data$app_id)
mode(data$user_reviews)
mode(data$positive_ratio)
mode(data$price_final)
mode(data$price_original)
mode(data$discount)

head(data)

#7. Crear un dataFrame de 13 col con la base de datos y guardar en una nueva
#variable

#Separamos las variables de tipo caracter y las variables numerícas
#-- Variables de tipo caracter --#
title<- c(data[, 2])
date_release<- c(data[, 3])
wind<- c(data[, 4])
iMac<- c(data[, 5])
linux_so<- c(data[, 6])
rating_tv<- c(data[, 7])
steam<-c(data[, 13])

#Construimos la matriz
matriz_1<-(cbind(titulo,fecha,wind,iMac,linux_so,rating_tv,steam))
head(matriz_1)
class(matriz_1)


app_id_so<-c(data[, 1])
ratio<-c(data[, 8])
reviews<-c(data[, 9])
prec_final<-c(data[, 10])
prec_original<-c(data[, 11])
descuento<-c(data[, 12])

matriz_2<-(cbind(app_id_so,ratio,reviews,prec_final, prec_original, descuento))
head(matriz_2)

data2<-(cbind(matriz_1, matriz_2))
head(data2)
names(data2)

#8 Agregar filas y columnas a la matriz (Sugerencia una columna de "1" y una fila
#de datos de un juego de tu preferencia

#agregar columna
data3<-cbind(data2,1)
head(data3)

#agregar fila (se ubicará en la ultima fila..)
data4<-rbind(data3, "Mario Bros")
head(data4)

#11. Eliminar filas y columnas de la matriz
# Eliminar la ultima columna
data3 <-data3[,-14]

# Eliminar la ultima fila
data4<-data4[-46069,]

#12. Seleccionar los elementos de la matriz
data2[1,2] #Selecciona el elemento que está en la primera y en la segunda fila
data2[5:7,1:4] #selecciona los elementos de las filas 5,6,7 y las columnas 1,2,3,4.
data2[94,10:12] #selecciona el elemento de la fila num 94 y las columnas 10, 11 y 12.
data2[6:8,] #selecciona todos los elementos del a fila 6, 7 y 8.

#13. Convertir la matriz en data.frame y asignar nombres a las columnas
datos_game<-data.frame(id=app_id_so, titulo=title, fecha=date_release, windows=wind, Mac=iMac, linux=linux_so, rating=rating_tv,
                  ratio_positivo=ratio, usuarios = reviews, precioFinal=prec_final, precio_Original=prec_original,
                  descuent=descuento, steamDeack=steam)
head(datos_game)

#Nombres de las variables (columnas)
names(datos_game)

#14. Acceder a los datos del dataframe
datos_game$titulo
datos_game$precio_Original

datos_game[2:5, 1:3]
datos_game[101:112,7:10]
datos_game[1,"titulo"]

#15. Cambiar nombre de dataframe

names(datos_game) <- c("ID", "TITULO","FECHA", "WINDOWS", "MAC", "LINUX", "RATING", "RATIO_POSITIVO",
                       "USUARIOS", "PRECIO_FINAL", "PRECIO_ORIGINAL", "DESCUENTO", "STEAM")
head(datos_game)

#16. Seleccionar un elemento del dataframe
datos_game[5,"PRECIO_FINAL"]

#==============================================#
# 2. ---  Importar Dato                        #
#==============================================#

#1. Importar Datos desde Excel y Ordenar los datos con la función order(), de
#preferencia para la variable Price_final.

#IMPORTANTE: LOS DATOS FUERON IMPORTADOS EN LA PREGUNTA 1

head(data)
summary(data$price_final)

v<-data[,"price_final"]
order(v)

#2. Mostrar el dataframe ordenado de manera ascendente y descendente

# funcion order (menor a mayor) por la variable PRICE_FINAL
orden_ascendente <- order(data$price_final,
                           decreasing = FALSE)
head(data[orden_ascendente, ])
     

# funcion order (mayor a menor) por la variable PRICE_FINAL
orden_descendente <- order(data$price_final,
                           decreasing = TRUE)
head(data[orden_descendente, ])

#3. Calcular el resumen estadístico de los datos con la función que corresponde

summary(data)

#4. 4. Realizar las graficas


attach(data)   # acceder directamente a las variables (columnas)


#Histograma para la variable positive_Ratio
hist(data$positive_ratio)

hist(positive_ratio,
     breaks = 20, 
     main = "Ratio de retroalimentaciones positivas", 
     xlab = "Ratio", ylab = "Frecuencia",  
     col = "turquoise4", 
     border = "white" 
)

#vamos a representar con la frecuencia relativa en vez de la absoluta
#==============================================
hist(positive_ratio,
     breaks = 20, 
     main = "Ratio de retroalimentaciones positivas", 
     freq = FALSE,                  # frecuencias relativas (en tanto por uno)
     xlab = "Ratio",                                         
     ylab = "Frecuencia relativa (en tanto por uno)",  
     xlim = c(0,50), 
     ylim =c (0,0.10),          
     col = "turquoise4",
     border = "white" 
)

#boxplot
#=========
boxplot(price_final, main = "Precio Final", ylab = "Precio en dolares $", col = "pink", border = "purple")

#Diagrama de barras
frecuencias <- table(rating)
frecuencias
barplot(frecuencias, col = heat.colors(6))


#==============================================#
# 3. ---  PROGRAMACION ---                     #
#==============================================#
#1. Implementar una función para la multiplicación de dos vectores(xy) y probar
#con valores

#Obteniendo los vectores:
prec_final<-c(data[, 10])
prec_original<-c(data[, 11])

#Funcion
multiplicacion <- function(x, y) {
  resultado <- x * y
  return(resultado)
}
multiplicacion(x=prec_final, y=prec_original)


#2. Implementar una función que muestre el resultado de la ecuación de Bhaskara y
#probar con valores.


variables <- function(a,b,c){
  if (b^2 - 4*a*c >= 0) {
    raiz1 <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
    raiz2 <- (-b - sqrt(b^2 - 4*a*c))/(2*a)
  }
  else {
    raiz1 <- (-b + sqrt(as.complex(b^2 - 4*a*c)))/(2*a)
    raiz2 <- (-b - sqrt(as.complex(b^2 - 4*a*c)))/(2*a)
  }
  c(raiz1, raiz2)
}

#ejemplos:
variables(a=1, b=8, c=3)
variables(a=3, b=4, c=8)

#3.Se quiere conocer la media muestral de n observaciones obtenidas
#independientemente de una distribución normal con media = 0 y varianza =1.

# 3.1 Realizar una simulación, luego calcular las estadísticas descriptivas
# aplicando la función que corresponde y graficar.

#sintaxis de rnorm
#rnorm(n,        # Número de observaciones a ser generadas
#      mean = 0, # Número o vector representando la/s media/s
#      sd = 1)   # Número o vector representando la/s desviación/es típica/s


set.seed(1)
muestra = rnorm(n=100, mean=0, sd=1)
muestra[1:10]

x<-seq(-10,10, length=200)

#grafico:
hist(muestra, main = "n = 100",
     xlab = "", prob = TRUE)
lines(x, dnorm(x), col = "red", lwd = 2)

#media
mean(muestra)

#mediana
median(muestra)

#Convirtir a dataframe
muestra_df <-data.frame(muestra)

#Grafico de Histograma
library(ggplot2)
ggplot(muestra_df, aes(x=muestra)) + 
  geom_histogram()