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
## * Descomprimir datos (unzipData)
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

# Employment ----
# https://www.bls.gov/eag/

# Cargar datos de empleo desde el archivo Excel
Employment <- read_excel(
    path = "01_Informacion_Data/Bureau_Labor_of_Statistics_Data/Detroit_Warren_Dearborn.xlsx",
    sheet = "BLS Data Series",
    skip = 10
)

# Zillow ----
# https://www.zillow.com/research/data/

# Cargar datos de Zillow desde el archivo CSV
Zillow <- read.csv(file = "01_Informacion_Data/Zillow/Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv")

# Land Area ----
# Formato .lpk
# https://www.arcgis.com/home/item.html?id=8d2012a2016e484dafaac0451f9aea24#!

# Cargar datos de área de tierra desde el archivo Excel
Land_Area <- read_excel(path = "01_Informacion_Data/Land_Area/LandArea.xlsx")

# Population ----
## Archivos Zip----
# https://data.census.gov/table?t=Populations+and+People&g=040XX00US26$8600000&y=2020&tid=ACSDP5Y2020.DP05

# Configuración del directorio y archivos ZIP
my_dir <- "01_Informacion_Data/US_Census_Bureau/"
zip_files <- list.files(path = my_dir, pattern = "*.zip", full.names = TRUE)

# Descomprimir archivos ZIP
unzip_files <- map(zip_files, ~unzip(.x, exdir = my_dir))

# Obtener archivos de datos en formato CSV
data_files <- list.files(path = my_dir, pattern = "*Data.csv", full.names = TRUE)

# Leer archivos CSV y almacenar en una lista
pop_data <- map(.x = data_files, .f = read.csv, skip = 1, header = TRUE)

# Agregar la columna 'Year' a cada conjunto de datos
for (i in 1:length(pop_data)) {
    pop_data[[i]]$Year <- 2010 + i
}

# Combinar la lista de conjuntos de datos en un marco de datos
pop_data <- map_dfr(.x = pop_data, .f = pluck)

# Seleccionar columnas relevantes
pop_data <- pop_data %>% 
    select(
        Geographic.Area.Name, 
        Estimate..SEX.AND.AGE..Total.population, 
        Year
    )

# Obtener la lista de archivos .csv y .txt en el directorio
files_to_remove <- list.files(path = my_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)

# Verificar si hay archivos para eliminar
if (length(files_to_remove) > 0) {
    # Eliminar los archivos
    file.remove(files_to_remove)
    cat("Archivos .csv y .txt eliminados correctamente en el directorio:", my_dir)
} else {
    cat("No hay archivos .csv y .txt para eliminar en el directorio:", my_dir)
}

# Guardar en archivo XLSX
xlsx_file <- file.path(my_dir, "pop_data.xlsx")
write_excel_csv(pop_data, xlsx_file)

# Guardar en archivo RDS
rds_file <- file.path(my_dir, "pop_data.rds")
saveRDS(pop_data, rds_file)

# Cargar el archivo RDS en un objeto llamado pop_data
pop_data <- readRDS(file.path(my_dir, "pop_data.rds"))

