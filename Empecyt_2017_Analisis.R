remove(list = ls())

#Encuesta EMPECYT 2017
setwd("~/GitHub/SciData/Estadistica-Analisis-Datos-RStudio-Python/Enpecyt_2017/")
dir() #para ver que objetos hay en la carpeta

library(foreign) #para poder leer bases de datos tipo .dbf
library(tidyverse) #contiene varias paqueterías para ciencia de datos 

cb1 <- read.dbf("enpecyt2017_cb1.dbf") 
view(cb1)
names(cb1)

head(cb1$S3P1) #ver los

#primeras 6 datos de la educación del encuestado
table(cb1$S3P1)

head(cb1$FAC) #factor de expansión de cada individuo. La suma de FAC es el total de la población. ¿Cuanta gente representa ese individuo?

nivel_estudios <- tapply(cb1$FAC, cb1$S3P1,sum) #sumas mediante clasifiaciones. La suma de FAC clasificados por nivel de estudios
names(nivel_estudios) <- c("Grado", "Absoluto") #cambiar el nombre a las columnas

niveles_estudios <- c("Ninguno","Preescolar", "Primaria", "Secundaria", "Preparatoria", "Normal", "TSU", "Licenciatura", "Especialidad", "Maestría", "Doctorado")


nivel_estudios$relativo <- nivel_estudios$Absoluto/sum(nivel_estudios$Absoluto) #crear una columna con un porcentaje

nivel_estudios <- data.frame(niveles_estudios, nivel_estudios)
view(nivel_estudios)

#Grafico de Barras
x11()
ggplot(data = nivel_estudios)+
  geom_col(mapping = aes(x=Grado ,y=relativo, fill=Grado))

# Grafico de pastel
etiquetas_nivel <- paste(nivel_estudios$Grado, round(100*nivel_estudios$relativo,2),"%", sep=" ")
pie(nivel_estudios$relativo, etiquetas_nivel)

cb1$S3P1 #los grados de todas las personas

cb1$grupo <- 0 #nueva columna llena de 0
cb1[cb1$S3P1 %in% c(0),]$grupo <- 0 #fijate qye valores son 0 'Ninguno' y dame la tabla con ellos
cb1[cb1$S3P1 %in% c(1,2,3),]$grupo <- 1
cb1[cb1$S3P1 %in% c(4),]$grupo <- 2
cb1[cb1$S3P1 %in% c(5,6,7),]$grupo <- 3
cb1[cb1$S3P1 %in% c(8,9,10),]$grupo <- 4

head(cb1$grupo) #ya los agrupamos en 4 grupos

# Lo mismo pero con nuevas categorias

grupo_estudios <- tapply(cb1$FAC, cb1$grupo, sum) 
grupos_estudios <- c("Ninguno","Básico", "Medio Superior", "Superior", "Posgrado")
grupo_estudios <- data.frame(grupos_estudios, grupo_estudios)
names(grupo_estudios) <- c("Grado", "Absoluto")

grupo_estudios$relativo <- grupo_estudios$Absoluto/sum(grupo_estudios$Absoluto)

view(grupo_estudios)

# Grafico de pastel
etiquetas_grupo <- paste(grupo_estudios$Grado, round(100*grupo_estudios$relativo,2),"%", sep=" ")
pie(grupo_estudios$relativo, etiquetas_grupo)






