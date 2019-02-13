PARTE 1: CARGA DE ARCHIVOS

install.packages("XML")
install.packages("xml2")
install.packages("methods")

#para exportar a XML
install.packages("rio")
rio::install_formats()
install.packages("reshape")

library(XML)
library(methods)
library(xml2)
library(reshape)
library(rio)
library(dplyr)

#Carga de Tabla Hortalizas 2011-2017

H1 <- "../RAIF_Horticolas_Muestreos_2011_2017.xml"

xmldoc <- xmlParse(H1)
rootnode <- xmlRoot(xmldoc)

data_H1 <- xmlSApply(rootnode, function(x) xmlSApply(x, xmlValue))
catalogo_H1 <- data.frame(t(data_H1), row.names = NULL)

# Carga de tabla Hortalizas 2017-2018

H2 <- "../Datos Horticolas/RAIF_Horticolas_Muestreos_2017_2018.xml"

xmldoc2 <- xmlParse(H2)
rootnode2 <- xmlRoot(xmldoc2)

data_H2 <- xmlSApply(rootnode2, function(y) xmlSApply(y, xmlValue))
catalogo_H2 <- data.frame(t(data_H2), row.names = NULL)


#Unión de tablas Hortalizas, H_complete contiene Hortalizas 2011 al 2018

hortalizas<- rbind(catalogo_H1, catalogo_H2)
export(hortalizas,"hortalizas.xml")

class(hortalizas$VALOR)

View(hortalizas)

summary(hortalizas)

#Dejar con las clases que corresponden a las variables

hortalizas$PROVINCIA<- as.character(hortalizas$PROVINCIA)
hortalizas$MUNICIPIO<- as.character(hortalizas$MUNICIPIO)
hortalizas$PARCELA<- as.factor(hortalizas$PARCELA)
hortalizas$CULTIVO<- as.character(hortalizas$CULTIVO)
hortalizas$VARIABLE<- as.factor(hortalizas$VARIABLE)
hortalizas$FECHA<- as.Date(hortalizas$FECHA)
hortalizas$VALOR<- as.numeric(levels(hortalizas$VALOR))[hortalizas$VALOR]

#Cargar tabla de clima

clima<- read.csv("Clima_2016_2018.csv", header = TRUE, sep = ",")

#Dejar con las clases que corresponden a las variables

clima$fecha<- as.Date(clima$fecha)
clima$cod_est<- as.character(clima$cod_est)
clima$X<- as.factor(clima$X)

#renombrar columnas de dataframe hortalizas

colnames(hortalizas)<- c("provincia", "municipio", "parcela", "cultivo", "fecha", "variable", "valor")

#Carga de estaciones climatológicas

estaciones<- xmlToDataFrame("Identificacion_EstacionesClima.xml")

#revisión de tablas

View(clima)
head(hortalizas)
head(estaciones)
View(estaciones)

#PARTE 2: REVISIÖN Y LIMPIEZA

#renombrar columnas de dataframe estaciones

colnames(estaciones)<- c("cod_est", "municipio", "provincia", "municipio_cod", "latitud", "longitud", "altitud")

#Realizar el cruce de tabla de clima con estaciones

clima_estacion<- merge(clima,estaciones, by = "cod_est")

#Eliminar variables

clima_estacion$X<- NULL
clima_estacion$municipio_cod<- NULL

#Revisión de tabla unión clima-estaciones

head(clima_estacion)
View(clima_estacion)

#Creación base final con hortalizas y clima

basetotal<- merge( x = hortalizas, y = clima_estacion, by = c("fecha","municipio"))
View(basetotal)

#Eliminar variables repetidas

basetotal$provincia.y<- NULL

#PARTE 3: ANÁLISIS DE VARIABLES

#Grafica temperatura media por cultivo según fecha del año

library(ggplot2)
ggplot(basetotal, aes(x = fecha, y = t_med, col = cultivo)) +
  geom_point() + geom_smooth(alpha = 0.2)

#Revisión clases de las variables de la tabla final
str(basetotal)

#Resumen de las variables, min, mediana, promedio, máx, etc.
summary(basetotal)