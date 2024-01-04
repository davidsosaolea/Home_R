## ____________________________________
##
## Nombre del script: Zillow_PC_Function.R
##
## Propósito del script: Data Manipulation
##
## Requisitos previos:
##   * funciones_script_de_importacion_de_datos.R
##
## Parámetros de la función:
##   * Y1
##   * Y2
##   * show_rows
## 
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

# Paquetes 
library(readxl)
library(tidyverse) 
library(lubridate)
library(fs)

getTopCitiesByCAGR <- function (Y1, Y2, show_rows){
    source("06_funciones_y_interacion_iteration/funciones_de_importacion.R")

# Importacion de datos        
    ZMI_Data_Prep <- data_import("ZMI_data_pre")
    
# Data prep
    
   CAGR <- ZMI_Data_Prep |>
        filter (Year %in% c(Y1, Y2)) |>
        group_by(City_zip) |>
        mutate (Beginning_Value = Z_Home_Value_Index[Year == Y1],
                Ending_Value = Z_Home_Value_Index[Year == Y2]) |>
        ungroup() |>
        select (City_zip, Beginning_Value, Ending_Value) |>
        mutate(timeperiod = str_c(Y1, Y2, sep = "-")) |>
        distinct()
   
# calculando CAGR
# CAGR se establece para ser calculado en base a Y1 y Y2
   CAGR <- CAGR |>
       mutate (Number_of_Years = Y2 - Y1,
               CAGR = ((Ending_Value / Beginning_Value)^(1/ Number_of_Years)) - 1)|>
       arrange (desc(CAGR))

# Top Cities
   if(show_rows == "all") {
       
       show_rows <- CAGR |> nrow()
       CAGR <- CAGR[1:show_rows,]
       return(CAGR)
   }
   
   n <- CAGR |> nrow()
   if (is.numeric(show_rows) == TRUE & show_rows < n+1) {
       CAGR <- CAGR [1: show_rows,]
       return(CAGR)
       
   }else
       print(str_glue("Ingrese 'all' o un valor numerico entre 0 y {n}"))
}


# getTopCitiesByCAGR(2000, 2019, 170)

