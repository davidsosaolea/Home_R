## ____________________________________
##
## Nombre del script: Preparacion_de_datos.r
##
## Propósito del script: Preparacion de datos 
##
## Requisitos previos:
## *  funciones_script_de_importacion_de_datos.R
## 
##
## Parámetros de la función:
## * funciones_script_de_importacion_de_datos:
##   * (dataSource)
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
# Cargar las bibliotecas necesarias
library(readxl)
library(tidyverse)
library(naniar)
library(lubridate)
library(stringr)
library(fs)
library(scales)
library(FinancialMath)
library(magrittr)

# Para importar conjuntos de datos como "Employment", "Zillow", "Population" y "LandArea" 
source("06_funciones_y_interacion_iteration/funciones_de_importacion.R")

# Importar datos de Zillow
zillow <- data_import(dataSource = "Zillow")

# Filtrar datos para el estado de Michigan y área metropolitana específica
zillow_MI <- zillow |> filter(StateName == "MI" & Metro == "Detroit-Warren-Dearborn, MI")

# Resumen de valores faltantes en el conjunto de datos filtrado
miss_val_z <- zillow_MI |> miss_case_summary()

# Agregar una columna 'id' como identificador único de fila
zillow_MI <- zillow_MI |>
    mutate(id = 1:nrow(zillow_MI)) |> 
    select(id, everything())

# Realizar un left join con el resumen de valores faltantes
zillow_MI <- left_join(zillow_MI,
                       miss_val_z, by = c("id" = "case"))

# Filtrar filas con ningún valor faltante
zillow_MI <- zillow_MI |> filter(n_miss == 0)

# Resumen de valores faltantes después del filtrado
zillow_MI |> miss_case_summary()

# Transformar el formato del conjunto de datos utilizando gather() y agregar una columna City_zip
# Cargar la biblioteca necesaria
library(tidyverse)
library(lubridate)

# Crear un tibble a partir del dataframe zillow_MI
ZMI_data_pre <- zillow_MI |>
    
    # Convertir el dataframe a formato tibble
    tibble() |>
    
    # Reorganizar los datos en formato largo (tidy data)
    gather(key = "Date", value = "Z_Home_Value_Index", X2000.01.31:X2023.08.31) |>
    
    # Crear una nueva columna concatenando City y RegionName
    mutate(City_zip = str_glue("{City}_{RegionName}")) |>
    
    # Seleccionar las columnas relevantes
    select(Date, City_zip, Z_Home_Value_Index, RegionName) |>
    
    # Procesar la columna Date: eliminar "X", agregar "00:00:00" y convertir a formato de fecha y hora
    mutate(Date_chr = Date |> str_replace_all(pattern =  "X", replacement = "")) |>
    mutate(Date_chr = Date_chr |> str_c("00:00:00")) |>
    mutate(Date_chr = Date_chr |> ymd_hms(tz = "UTC")) |>
    
    # Crear una nueva columna Year a partir de la columna Date_chr
    mutate(Year = Date_chr |> year()) |>
    
    # Seleccionar las columnas relevantes para el análisis de series temporales
    select(Year, City_zip, Z_Home_Value_Index, RegionName) |>
    
    # Agrupar los datos por Ciudad-ZIP, Año y Región
    group_by(City_zip, Year, RegionName) |>
    
    # Calcular el valor medio del índice de valor de la vivienda para cada grupo
    summarise(Z_Home_Value_Index = Z_Home_Value_Index |> mean()) |>
    
    # Desagrupar el dataframe
    ungroup() |>
    
    # Crear una nueva columna (ZHVI_lag1) con el valor de Z_Home_Value_Index retrasado en un periodo
    mutate(ZHVI_lag1 = lag(Z_Home_Value_Index, n = 1)) |>
    
    # Ajustar el valor de ZHVI_lag1 para el año 2000
    mutate(ZHVI_lag1 = case_when (
        Year == 2000 ~ Z_Home_Value_Index,
        TRUE ~ ZHVI_lag1
    )) |>
    
    # Calcular la diferencia entre Z_Home_Value_Index y su valor retrasado
    mutate(ZHVI_diff = Z_Home_Value_Index - ZHVI_lag1) |>
    
    # Calcular la tasa de crecimiento interanual (Year-over-Year) en forma de porcentaje
    mutate(ZHVI_PC_YoY = ZHVI_diff/ZHVI_lag1 ) |>
    
    # Formatear la tasa de crecimiento interanual como cadena de texto con formato de porcentaje
    mutate(ZHVI_PC_YoY_chr = ZHVI_PC_YoY |> scales::percent (accuracy = 0.01))

# Cálculo de AAR
AAR <- ZMI_data_pre |>
    group_by(City_zip) |>                       # Agrupa los datos por City_zip
    summarise(AAR = ZHVI_PC_YoY |> sum()) |>    # Suma los valores de ZHVI_PC_YoY para calcular AAR
    ungroup() |>                                # Elimina la agrupación
    mutate(AAR = AAR/23) |>                     # Divide AAR por 23
    mutate(AAR_chr = AAR |> scales::percent())  # Convierte AAR a porcentaje y almacénalo en AAR_chr

# Cálculo de CAGR
CAGR <- ZMI_data_pre |>
    filter(Year %in% c(2000, 2023)) |>          # Filtra los datos para los años 2000 y 2023
    group_by(City_zip) |>                       # Agrupa los datos por City_zip
    mutate(Beginning_Value = Z_Home_Value_Index[Year == 2000],
           Ending_Value = Z_Home_Value_Index[Year == 2023]) |> # Crea Beginning_Value y Ending_Value
    ungroup() |>                                # Elimina la agrupación
    select(City_zip, Beginning_Value, Ending_Value) |> # Selecciona columnas relevantes
    distinct()

# Calcula CAGR
CAGR <- CAGR |>                                 # Utiliza el marco de datos CAGR creado anteriormente
    mutate(Number_of_Years = 2023 - 2000,
           CAGR = ((Ending_Value / Beginning_Value)^(1/ Number_of_Years)) - 1) # Calcula CAGR

# # Inicializa el monto principal, CAGR y el número de años
# principal <- 155053
# CAGR <- 0.01918387
# Years <- 23
# principal_chr1 <- principal |> scales::dollar()
# 
# # Imprime el monto principal inicial
# print(str_glue("Year: {0} Amount: {principal_chr1}"))
# 
# # Bucle para calcular e imprimir valores futuros basados en CAGR
# for (i in 1: Years) {
#     principal <- principal * (1 + CAGR)
#     principal_chr <- principal |> scales::dollar() 
#     print(str_glue("Year: {i} Amount: {principal_chr}")) # Agrega 2000 al índice para representar el año correctamente
# }

## ROI AAR CAGR

crime_rate <- data_import("Crime_rate")
demographis <- data_import("Demographis")

# MD

ZMI_data_pre <- ZMI_data_pre|> mutate(RegionName = RegionName |> as.character())
demographis <- demographis |> mutate(zip = zip |> as.character())

MD <- ZMI_data_pre |> 
    left_join(AAR,  by ="City_zip") |>
    left_join(CAGR, by = "City_zip") |>
    left_join(crime_rate, by = c("RegionName" = "Zipcode")) |>
    left_join(demographis, by = c("RegionName" = "zip"))

MD |> miss_case_summary()
MD |> gg_miss_var()

# check <- MD |> filter(is.na(`Overall Crime Grade`) == TRUE)
MD <- MD |> filter(!is.na(`Overall Crime Grade`) == TRUE)

# MD |> na.omit()
# test <- MD
# test[1,2] <- NA
# test |> na.omit()

# Filter
check <- MD |> filter(CAGR > 0.04 & Z_Home_Value_Index < 300000 & familyPercentPoverty < 12)

# Employment

Employment <- data_import("Employment")
