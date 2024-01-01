## ____________________________________
##
## Nombre del script: script_de_importacion_de_datos.R
##
## Propósito del script: Importar datos a Rstudio
##
## Requisitos previos:
## * Los archivos de datos de Excel deben estar cerrados para poder leerlos en Rstudio
## * Los archivos de datos de Excel deben estar en la ubicación/ruta proporcionada en la función
##
## Parámetros de la función:
## * Fuente de datos (dataSource)
##
## Códigos de retorno:
## 0 - Éxito: 
## 1 - Error: 
## 2 - Error: 
## 3 - Error: 
##
## Autor: BIDDSA
## Fecha de creación: 2023-09-11
## Derechos de autor: (c) BIDDSA, 2023
## Correo electrónico: <email>@BIDDSA.com
##________________________________________________________________________
##> Notas:
##> data_import importa datos de diferentes fuentes como
##> Employment, Zillow, Population y LandArea. 
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

# Paquetes
library(readxl)
library(tidyverse)
library(lubridate)
library(fs)
library(purrr)

data_import <- function(dataSource) {
    data <- switch(dataSource,
                   "Employment" = read_excel(
                       path = "01_Informacion_Data/Bureau_Labor_of_Statistics_Data/Detroit_Warren_Dearborn.xlsx",
                       sheet = "BLS Data Series",
                       skip = 10
                   ),
                   "Zillow" = read.csv(
                       file = "01_Informacion_Data/Zillow/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
                   ),
                   "Land_Area" = read_excel(
                       path = "01_Informacion_Data/Land_Area/LandArea.xlsx"
                   ),
                   "Crime_rate" = read_rds(
                      "01_Informacion_Data/Crime_Rates_by_ZipCodes/crime_rating_by_zip.R"
                   ),
                   "Demographis" = read_rds(
                      "01_Informacion_Data/Crime_Rates_by_ZipCodes/Zip_Income_and_Poerverty.R"
                       
                   ),
                   "pop_data" = readRDS(file.path(my_dir, "pop_data.rds")),
                   stop("Dataset name not recognized.")

                   
    )
    
    return(data)
}



