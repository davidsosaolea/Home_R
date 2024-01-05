## ____________________________________
##
## Nombre del script: EcoGraphHomeUnemploy.r
##
## Propósito del script: Visualization de Datos 
##
## Requisitos previos:
## *  Zillow_PC_Function.R
## 
##
## Parámetros de la función:
##   * Y1
##   * Y2
##   * show_rows:
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
# Bibliotecas ----
library(readxl)
library(tidyverse)
library(naniar)
library(lubridate)
library(stringr)
library(fs)
library(scales)
library(ggplot2)
library(plotly)
library(tidyquant)
library(ggrepel)


source("06_funciones_y_interacion_iteration/Zillow_PC_Function.R")
source("06_funciones_y_interacion_iteration/funciones_de_importacion.R")

CAGR_Data <- getTopCitiesByCAGR(2000,2022,30)
ZMI_Data_Prep <- data_import("ZMI_data_pre")

ZillowTrends <- CAGR_Data |> left_join(ZMI_Data_Prep, by = "City_zip")

ZillowTrends |> gg_miss_var()

ZillowTrends <- ZillowTrends |>
    select (City_zip, Year, Z_Home_Value_Index, ZHVI_PC_YoY, ZHVI_PC_YoY_chr, timeperiod, Number_of_Years, CAGR)

ave_rate <- ZillowTrends |>
    pull (CAGR) |>
    mean() |>
    scales::percent (accuracy = 0.01)

min_y <- ZillowTrends |>
    pull (Year) |>
    min()

max_y <- ZillowTrends |>
    pull (Year) |>
    max()

# Grafico/Plot
g <- ZillowTrends |> ggplot(aes (x = Year, y = ZHVI_PC_YoY, color = City_zip)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.50) +
    #facet_wrap(~ City_zip, scales = "free_y" ) +
    labs(
        title = "Home Prices Appreciation Year over Year",
        subtitle = str_glue("Percentage change YoY from {min_y} to {max_y}"),
        x = "YEAR",
        y = "Percentage Change (%)",
        caption = str_glue("From {min_y} to {max_y}, the average CAGR has been {ave_rate} \n for these top 30 ZipCodes in the Metro Detroit Area")
    )
g |> ggplotly()
