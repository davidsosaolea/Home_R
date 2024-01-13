## ____________________________________
##
## Nombre del script: Importacion_de_Datos_por_medio_de_IPs_con_Rapid_API.R
##
## Propósito del script: Importando datos a Rstudio desde Rapid API (Interfaz de programación de aplicaciones)
##
## Requisitos previos:
## *  funciones_script_de_importacion_de_datos.R
## 
##
## Parámetros de la función:
## * funciones_script_de_importacion_de_datos:
##   * (dataSource)
##
## * data_import_IP:
##   * url
##   * API_Key
##   * API_Host
##
## Resultados: Indice_de_crimen_por_código_postal
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

# Cargar las bibliotecas necesarias
library(httr)
library(jsonlite)
library(tidyverse)
library(fs)
library(rvest)
library(xml2)
library(readxl)
library(dplyr)
getwd()
setwd("R_class/Analysis_&_Manipulation_De_Datos_En_Espanol")
# Para importar conjuntos de datos como "Employment", "Zillow", "Population" y "LandArea." 
source("06_funciones_y_interacion_iteration/funciones_de_importacion.R")
#source("06_funciones_y_interacion_iteration/funciones_de_importacion.R")

# Uso de la función para cargar el conjunto de datos "Zillow"
Zillow <- data_import(dataSource = "Zillow")

# Filtrado y selección de códigos postales (zip codes) en el conjunto de datos Zillow
zip_codes <- Zillow %>%
    filter(StateName == "MI" & Metro == "Detroit-Warren-Dearborn, MI") %>%
    distinct(RegionName) %>%
    pull(RegionName)

# Lectura de datos de crimen desde un archivo RDS
crime_data <- readRDS("01_Informacion_Data/Crime_Rates_by_ZipCodes/crime_rating_by_zip.R")
crime_zip <- crime_data %>% distinct(Zipcode) %>% pull(Zipcode) %>% as.character()

# Convertir los códigos postales a caracteres
DWD_MI_Zip <- as.character(zip_codes)

# Comprobar si los conjuntos de códigos postales son idénticos
identical(DWD_MI_Zip, crime_zip)

# Mostrar los códigos postales que faltan en cada conjunto
DWD_MI_Zip[!DWD_MI_Zip %in% crime_zip]
crime_zip[!crime_zip %in% DWD_MI_Zip]

# Encontrar códigos postales que faltan en ambos conjuntos
missing_zip_codesDWD_MI <- setdiff(crime_zip, DWD_MI_Zip)
missing_zip_codescrime_zip <- setdiff(DWD_MI_Zip, crime_zip)

# Configurar la URL y claves de la API
url <- "https://us-zip-code-to-income.p.rapidapi.com/"
API_Key <- "b2f499316emsh3019cd6d5c60270p190dc5jsn9e46ae620e51"
API_Host <- "us-zip-code-to-income.p.rapidapi.com"
request_delay <- 2 # Definir el retardo entre las solicitudes (en segundos)

# Definir una función para importar datos utilizando la API
data_import_IP <- function(url, API_Key, API_Host, request_delay) {
    
    data_df <- tibble()
    
    for (i in seq_along(missing_zip_codescrime_zip)) {
        
        query_params <- list(zip = missing_zip_codescrime_zip[i])
        
        # Realizar la solicitud GET a la API
        response <- GET(url, add_headers("X-RapidAPI-Key" =  API_Key, "X-RapidAPI-Host" = API_Host), query = query_params)
        
        # Verificar el estado de la respuesta
        stop_for_status(response)
        
        # Convertir la respuesta JSON a un marco de datos
        cont_response <- fromJSON(rawToChar(response$content))
        
        # Concatenar el marco de datos resultante
        data_df <- bind_rows(data_df, as_tibble(cont_response))
        
        # Imprimir información sobre el código postal procesado
        print(str_glue("Zip Code: {missing_zip_codescrime_zip[i]}"))
        
        # Retardo entre las solicitudes para evitar límites de la API
        Sys.sleep(request_delay)
    }
    
    # Nombrar las columnas del marco de datos resultante
    colnames(data_df) <- c("success", "zip", "House Hold Median Income", "household MeanIncome", "familyMedianIncome", "familyMeanIncome", "numHouseholds", "nonFamilyHousehold MedianIncome", "nonFamilyHouseholdMeanIncome", "familyPercentPoverty")
    
    return(data_df)
}

# Llamar a la función de importación de datos utilizando la API
result_df <- data_import_IP(url, API_Key, API_Host, request_delay)

