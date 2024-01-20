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

#setwd("R_class/Analysis_&_Manipulation_De_Datos_En_Espanol/")

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

outliers <- model_data$price < (mean_price - 3 * sd_price) | model_data$price > (mean_price + 3 *  sd_price)
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


#2. Gráficos de Residuos

data_sets <- list("model1_no_outliers_sd_method"      = model1_no_outliers_sd_method,
                  "model2_no_outliers_IQR_method"     = model2_no_outliers_IQR_method, 
                  "model3_no_outliers_rubust_method"  = model3_no_outliers_rubust_method)

# Definir la formula para los modelos

formula <- price ~ bathrooms + bedrooms + squareFootage + zipCode

# Ajustar un modelo a cada conjunto de datos y almacenar los modelos en una lista 

models <- map(data_sets, ~ lm(formula, data = .x))

create_residuals_plot_fun <- function(model, model_name) {
    ggplot() +
        geom_point(aes (x = model$fitted.values, y = model$residuals)) +
        
        labs(
            title = paste("Residuos vs Valores Ajustados para", model_name), 
            x = "Valores Ajustados",
            y = "Residuos"
            )
}

plots  <-  map2(models, names (models), create_residuals_plot_fun)
grid.arrange (plots [[1]], plots[[2]], plots[[3]], ncol = 2)


# Normal Q - Q

models <-  list("model1_no_outliers_sd_method"      = lm( price ~ bathrooms + bedrooms + squareFootage + zipCode, data = model1_no_outliers_sd_method),
                "model2_no_outliers_IQR_method"     = lm( price ~ bathrooms + bedrooms + squareFootage + zipCode, data = model2_no_outliers_IQR_method), 
                "model3_no_outliers_rubust_method"  = lm( price ~ bathrooms + bedrooms + squareFootage + zipCode, data = model3_no_outliers_rubust_method))


create_qq_plot <- function(model_name, model){
    residuals  <- model$residuals
    qqplot <- ggqqplot(residuals) +
        ggtitle(paste("Grafico Q-Q para", model_name))
    return(qqplot)
    
}

plots  <-  map2 (names (models), models, create_qq_plot)
combined_plot <- wrap_plots (plots, ncol = 2)
combined_plot


# 3. coeficiente de Correlación:
#> El coeficiente de correlación mide la fuerza y dirección de la relación lineal entre 2 variables.
#> -1 a 1.
#> si el valor es cercano a @ incidida que no hay relación lineal

cor(model1_no_outliers_sd_method$price, model1_no_outliers_sd_method$bathrooms, use = "complete.obs")
cor(model1_no_outliers_sd_method$price, model1_no_outliers_sd_method$bedrooms, use = "complete.obs")
cor(model1_no_outliers_sd_method$price, model1_no_outliers_sd_method$squareFootage, use = "complete.obs")


#> Transformaciones de Datos.
#> Si un predictor (variable independiente) tiene una relación no lineal con la respuesta (variable dependiente), 
#> podríamos considerar transformar el predicter, la respuesta, o ambos para lograr linealidad.

m1_log <- model1_no_outliers_sd_method |>
    mutate (squareFootage = log(squareFootage))

m1_sqrt <- model1_no_outliers_sd_method |>
    mutate (squareFootage = sqrt(squareFootage))

m1_sq2 <-  model1_no_outliers_sd_method |>
    mutate (squareFootage = (squareFootage)^2)

models <- list("model1 no outliers sd method" = lm(price ~ bathrooms + bedrooms + squareFootage + zipCode, data = model1_no_outliers_sd_method),
               "ml log" = lm(price ~ bathrooms + bedrooms + squareFootage + zipCode, data = m1_log),
               "m1 sqrt"= lm(price ~ bathrooms + bedrooms + squareFootage + zipCode, data = m1_sqrt),
               "m1_5q2" = lm(price ~ bathrooms + bedrooms + squareFootage + zipCode, data = m1_sq2))

create_residuals_plot <- function(model_name, model) {
    ggplot() +
        geom_point(aes (x = model$fitted.values, y = model$residuals)) +
        labs (
            title = paste("Residuos vs Valores Ajustados para", model_name), x = "Valores Ajustados",
            y= "Residuos"
        )
}

plots <- map2(names (models), models, create_residuals_plot)
grid.arrange(plots [[1]], plots[[2]], plots[[3]], plots[[4]], ncol = 2)


#> Convertir a Factores
#> E.g. el color de un coche (rojo, azul, verde etc..)

map(data_sets, str)

mutate_data_to_fct <- function(df) {
    df <- df |> mutate_at(c("zipCode", "year"), as.factor) 
    return(df)
}
data_sets <- map(data_sets, mutate_data_to_fct)




# > Partición de Datos

split_data_fun <- function(df, var1, var2) {
    
    levels1 <- unique (df[[var1]])
    levels2 <- unique (df[[var2]])
    
    training_indices <- c()
    testing_indices  <- c()
    
    for (level1 in levels1) {
        for (level2 in levels2) {
            
            rows <- which(df[[var1]] == level1 & df[[var2]] == level2)
            
            if (length(rows) < 2 ) {
                # Si solo hay una instancia, añadirla a los datos de entrenamiento
                
                training_indices <- c(training_indices, rows)
                
            } else {
                
                partition        <- createDataPartition(y = rows, p = 0.70, list = FALSE)
                training_indices <- c(training_indices, rows [partition])
                testing_indices  <- c(testing_indices, rows[-partition])
            }
            
        }
    }
    list(
        training = df[training_indices, ],
        testing = df[testing_indices, ]
        )
}

split_data_frames <- map(data_sets, ~ split_data_fun(.x, "year", "zipCode" ))


model <- lm(price ~ year
            + bathrooms
            + bedrooms
            + squareFootage * zipCode, data = split_data_frames$model1_no_outliers_sd_method$training) 

model2 <- lm(price ~ year
                 + bathrooms
                 + bedrooms
                 + squareFootage * zipCode, data = split_data_frames$model2_no_outliers_IQR_method$training)

model3 <- lm(price ~ year
                 + bathrooms
                 + bedrooms
                 + squareFootage * zipCode, data = split_data_frames$model3_no_outliers_rubust_method$training)

## obtener resultados

summary_model  <- summary(model)
summary_model2 <- summary(model2)
summary_model3 <- summary(model3)

glance_model  <- broom::glance(model)
glance_model2 <- broom::glance(model2)
glance_model3 <- broom::glance(model3)

# Obtener los intervalos de confianza
confint_model  <- confint(model, level = 0.95)
confint_model2 <- confint(model2, level = 0.95)
confint_model3 <- confint(model3, level = 0.95)

# Gráficos de Predicción

m1 <- split_data_frames$model1_no_outliers_sd_method$testing
m2 <- split_data_frames$model2_no_outliers_IQR_method$testing 
m3 <- split_data_frames$model3_no_outliers_rubust_method$testing

# Crear una lista para los modelos
models <- list(model = model, model2 = model2, model3 = model3)

# Crear una una lista para el testing data 

datasets <- list(m1 = m1, m2 = m2, m3 = m3)

# Función para añadir predicciones a los datos

add_preditions <- function(dataset, model) {
    dataset$predicted <- predict(object = model, newdata = dataset)
    dataset
}

datasets <- mapply(add_preditions, datasets, models, SIMPLIFY = FALSE)

# Función para crear 3 gráficos

create_parity_plot <- function(dataset, name){
    
    ggplot(dataset, aes (x = price, y = predicted)) +
        geom_point() +
        geom_abline(color = "red", linetype = "dashed") +
        labs(
            x = "Precio Observado",
            y = "Precio Predicho",
            title = paste("Grafico de Paridad: Precio Observado vs Predicho (",name,")") +
            theme_minimal()
            )
}

plots <- imap(datasets, ~ create_parity_plot(.x, .y))

grid.arrange(grobs = plots, ncol = 3)
