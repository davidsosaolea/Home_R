## ____________________________________
##
## Nombre del script: Linear_Regression.R
##
## Propósito del script: 
##
## Prerrequisitos: ninguno
##               
##  
## Parámetros: 
##            
##
## Resultados: 
##                  
##
## Autor: BIDDSA
##
## Fecha de creación: 2023-08-06
##
## Derechos de autor (c) BIDDSA, 2023
## Correo electrónico: <email>@BIDDSA.com
## _______________________________________________________________________________________________________________________________
##> Notas: Este script es una exploración de datos de listados de alquileres, con enfoque en la preparación de los datos para análisis de regresión lineal.
##> Primero carga el conjunto de datos y combina datos de todos los códigos postales en un solo dataframe.
##> El script maneja los valores faltantes omitiéndolos. Los valores atípicos se eliminan utilizando tres métodos diferentes:
##> - desviación estándar
##> - Rango Intercuartil (IQR)
##> - Desviación Absoluta Mediana (MAD)
##> El script verifica la linealidad y normalidad de los residuos creando gráficos de dispersión, gráficos de residuos y gráficos QQ.
##> Se aplican transformaciones de datos para lograr linealidad y se calculan los coeficientes de correlación.
##> Finalmente, los resultados se almacenan para ser compartidos.
## _______________________________________________________________________________________________________________________________
##
## HISTORIA DE MODIFICACIONES
##
## 06AGO2023    person0
## Cambios:     Versión inicial.
##
## 06AGO2023    person1
## Cambios:     - Cambios xxx
##
## 07AGO2023    person2
## Cambios:     - Eliminado xxx
##              - Inicio xxx
##
## 08AGO2023    person3
## Cambios:     - Los errores se redirigen a xxxx
##                
##
##________________________________________________________________________
# update.packages(ask = FALSE) # Esto actualizará todos tus paquetes a las versiones más recientes.
# .rs.restartR() 
# Cargar librerías requeridas
library(ggplot2)   # visualización de datos
library(caret)     # tareas de aprendizaje automático
library(tidyverse) # manipulación y visualización de datos
library(naniar)    # para entender los valores faltantes
library(purrr)     # para iteraciones
library(rlang)     # ayuda a convertir un string en un símbolo
library(lubridate) # ayuda a manejar datos de fechas
library(gridExtra) # ayuda con la visualización
library(ggpubr)    # mejora las visualizaciones de gráficos de ggplot2
library(patchwork) # ayuda a combinar varios gráficos de ggplot2 en un gráfico compuesto
library(broom)     # ayuda con el ordenamiento de objetos de modelos
library(Metrics)
library(plotly)

setwd("R_class/Analysis_&_Manipulation_De_Datos_En_Espanol/")

#Cargar los datos de listados de alquileres
rent_data <- read_rds(file = "01_Informacion_Data/Rental_Listing_Data/Rental_Listings.RDS")

# Combinar datos de todos los codigos postales en un unico dataframe
# El argumento id se utiliza para crear una nueva columna que registra el origen de cada fila.

rent_data_unlist <- rent_data |>
    bind_rows(.id = "source")

# Preparar los Datos de los modelos
# Filtrar los datos para incluir solo los códigos postales especificados para el análisis #c("48237", "48069","48067", "48017", "48072", "48220","48030","48221", "48203")

model_data <- rent_data_unlist |>
    mutate (dates = ymd_hms (lastSeen),
            year = format(dates, "%Y")) |>
    select(year, zipCode, price, bathrooms, bedrooms, squareFootage) |>
    filter (zipCode %in% c("48237", "48069", "48067", "48017", "48072", "48220", "48030", "48221", "48203"))

gg_miss_var(model_data)
glimpse (model_data)

#> Manejar valores faltantes
#> La regresion lineal no puede manajar valores faltantes

model_data <- model_data |> na.omit()


#> Manejar Valores atípicos
#> 1. Eliminar valores atípicos con SD:

# calcular estadisticas

mean_price <- mean(model_data$price, na.rm = T) 
sd_price <- sd(model_data$price, na.rm = T)

outliers <- model_data$price < (mean_price - 3 * sd_price) | model_data$price > (mean_price + 3*  sd_price)
model1_no_outliers_sd_method <- model_data[!outliers, ]
model1_no_outliers_sd_method |> glimpse()

#> 2. Eliminar valores atipicos con el metodo IQR
#> Este metodo utiliza el Rango Intercuartil (IQR)
#> Cualquier Valor que caiga por debajo de Q1 1.5 IQR o por encima de Q3 + 1.5 IQR se puede considerar un valor atipico

Q1 <- quantile(model_data$price, .25)
Q3 <- quantile (model_data$price, .75)
IQR <- Q3 - Q1

outliers <- model_data$price < (Q1 - 1.5 * IQR) | model_data$price > (Q3 + 1.5 * IQR)

model2_no_outliers_IQR_method <- model_data[!outliers, ]
glimpse (model2_no_outliers_IQR_method)

#3. Usar método Robustos----

#> Métodos robustos como el de median absolute deviation (MAD)
#> se puede usar cuando se tienen datos que no siguen una distribución normal.

# calcular MAD

MAD <- mad (model_data$price, constant = 1)

# valores atípicos

outliers <- abs(model_data$price - median(model_data$price))/MAD > 2
model3_no_outliers_rubust_method <- model_data[!outliers, ]
model3_no_outliers_rubust_method |> glimpse()

# Comprbacion de linealidad 

variables <- list("bathrooms", "bedrooms", "squareFootage", "zipCode")
# Definir una función para crear un gráfico de dispersión
#!! sym(variable) para evaluar la variable como símbolo
# el operador !! se utiliza par evaluar este símbolo dentro de la función aes()

create_scatter_plot_fun <- function(variables){
    ggplot(model1_no_outliers_sd_method, aes(!!sym(variables), price)) +
        geom_point() +
        labs(title = paste("Grafico de dispersion de precio vs", variables),
             x=variables,
             y="Precio")
}

plots <- map(variables, create_scatter_plot_fun)

grid.arrange(plots [[1]], plots [[2]], plots[[3]], plots[[4]], ncol = 2)
















