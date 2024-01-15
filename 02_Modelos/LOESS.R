## ____________________________________
##
## Nombre del script: LOESS.r
##
## Propósito del script: Evaluar el uso de LOESS.
##
## Requisitos previos:
## 
## 
##
## Parámetros de la función:
##
## 
## Resultados:
##
## Códigos de retorno: NA
##
## Autor: BIDDSA
## Fecha de creación: 2023-09-11
## Derechos de autor: (c) BIDDSA, 2023
## Correo electrónico: <email>@BIDDSA.com
##________________________________________________________________________
##> Notas:
##> 
##> 
##________________________________________________________________________
## HISTORIA DE MODIFICACIÓN
## 06AGO2023 persona0
## Cambios: Versión inicial.
##
## 06AGO2023 persona1
## Cambios: - Cambios xxx
##
## 07AGO2023 persona2
## Cambios: - Se eliminó xxx
##
##________________________________________________________________________

# install.packages()
# Carga de bibliotecas ----
library(tidyverse)  # Paquete para manipulación y visualización de datos
library(lubridate)  # Paquete para manipulación de fechas
library(stringr)    # Paquete para manipulación de cadenas de texto
library(fs)         # Paquete para operaciones con el sistema de archivos
library(scales)     # Paquete para escalas personalizadas en gráficos

# Carga de la biblioteca ggplot2 si no está cargada
library(ggplot2)

# Establecer una semilla para reproducibilidad
set.seed(13)

# Generar datos de ejemplo ----
x <- seq(1, 200, length.out = 200)  # Secuencia de valores para x
y <- sin(x/10) + rnorm(200, 0, 0.5)  # Función seno con ruido aleatorio

# Aplicar suavizado LOESS ----
smoothed_data <- lowess(x, y)

# Graficar los datos originales y suavizados ----
ggplot(mapping = aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_line(aes(y = smoothed_data$y), color = "red") +
    ggtitle("LOESS")


# Generar tress modelos con diferentes spans
# Entre mas bajo el span mas alta la posibilidad de overfitting
# Cross validation para encontrar el mejor span

model1 <- loess(y ~ x, span = 0.2)
model2 <- loess(y ~ x, span = 0.5)
model3 <- loess(y ~ x, span = 0.8)

# Grafico de comparacion de modelos ----

FP <- ggplot(mapping = aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_line(aes (y = fitted (model1)), color = "red")+
    geom_line(aes (y = fitted (model2)), color = "black")+
    geom_line(aes (y = fitted (model3)), color = "yellow")+
    ggtitle("LOESS")

library(plotly)
FP |> ggplotly()
##################
#################
# Carga de bibliotecas ----
library(tidyverse)
library(lubridate)
library(stringr)
library(fs)
library(scales)
library(ggplot2)

# Establecer una semilla para reproducibilidad
set.seed(13)

# Generar datos de ejemplo ----
x <- seq(1, 200, length.out = 200)
y <- sin(x/10) + rnorm(200, 0, 0.5)

# Definir función de suavizado LOESS con span variable
loess_smooth <- function(x, y, span_value) {
    loess_model <- loess(y ~ x, span = span_value, data = data.frame(x = x, y = y))
    smoothed_data <- predict(loess_model, data.frame(x = x))
    return(smoothed_data)
}

# Definir función de validación cruzada para LOESS
loess_cv <- function(x, y, span_value) {
    folds <- 5  # Número de pliegues en la validación cruzada
    indices <- sample(1:length(x), length(x), replace = FALSE)
    mse_values <- numeric(folds)
    
    for (i in 1:folds) {
        # Dividir los datos en conjunto de entrenamiento y prueba
        test_indices <- indices[((i-1)*(length(x)/folds) + 1):(i*(length(x)/folds))]
        train_indices <- setdiff(indices, test_indices)
        
        # Ajustar modelo LOESS con el conjunto de entrenamiento
        loess_model <- loess(y[train_indices] ~ x[train_indices], span = span_value, data = data.frame(x = x, y = y))
        
        # Predecir en el conjunto de prueba y calcular MSE
        y_pred <- predict(loess_model, newdata = data.frame(x = x[test_indices]))
        mse_values[i] <- mean((y_pred - y[test_indices])^2)
    }
    
    # Devolver el promedio de los MSE de todos los pliegues
    return(mean(mse_values))
}

# Rango de valores de span a probar
span_values <- seq(0.1, 1, by = 0.1)

# Calcular el MSE promedio para cada valor de span usando validación cruzada
mse_values <- sapply(span_values, function(span) loess_cv(x, y, span))

# Encontrar el valor de span que minimiza el MSE
best_span <- span_values[which.min(mse_values)]

# Aplicar suavizado LOESS con el mejor span
smoothed_data <- loess_smooth(x, y, best_span)

# Graficar los datos originales y suavizados con el mejor span
P1 <- ggplot(mapping = aes(x = x, y = y)) +
    geom_point(color = "blue") +
    geom_line(aes(y = smoothed_data), color = "red") +
    ggtitle(paste("LOESS con el mejor span =", round(best_span, 2)))

print(P1)



########################
##########################





















