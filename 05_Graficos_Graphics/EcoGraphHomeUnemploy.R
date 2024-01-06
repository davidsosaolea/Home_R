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

ZillowTrends <- ZillowTrends |>
    mutate(tool_tip = str_glue("CityZip: {City_zip},
                               %Change: {ZHVI_PC_YoY_chr},
                               Year: {Year}"))
# Plot 1
# Grafico/Plot
g <- ZillowTrends |> ggplot(aes (x = Year, y = ZHVI_PC_YoY, color = City_zip, text = tool_tip, group = City_zip )) +
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
g |> ggplotly(tooltip = "tool_tip") 



########################

## plot alternativo
# # Ordena los datos por Year para asegurar que las líneas estén conectadas en orden temporal
# ZillowTrends <- ZillowTrends |> arrange(Year)
# 
# # Grafico/Plot
# g <- ZillowTrends |> ggplot(aes(x = Year, y = ZHVI_PC_YoY, color = City_zip, text = str_glue("CityZip: {City_zip}, %Change: {ZHVI_PC_YoY_chr}, Year: {Year}"))) +
#     geom_line(aes(group = City_zip), linewidth = 1) +
#     geom_point(size = 2, alpha = 0.5) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.50) +
#     labs(
#         title = "Home Prices Appreciation Year over Year",
#         subtitle = str_glue("Percentage change YoY from {min_y} to {max_y}"),
#         x = "YEAR",
#         y = "Percentage Change (%)",
#         caption = str_glue("From {min_y} to {max_y}, the average CAGR has been {ave_rate} \n for these top 30 ZipCodes in the Metro Detroit Area")
#     )
# 
# # Utiliza ggplotly con tooltip
# g |> ggplotly()

#Plot 2

g <- ZillowTrends |> ggplot(aes (x = Year, y = ZHVI_PC_YoY, color = City_zip, text = tool_tip, group = City_zip )) +
    geom_line(linewidth = 1) +
    geom_point(size = 2, alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.50) +
    facet_wrap(~ City_zip, scales = "free_y" ) +
    labs(
        title = "Home Prices Appreciation Year over Year",
        subtitle = str_glue("Percentage change YoY from {min_y} to {max_y}"),
        x = "YEAR",
        y = "Percentage Change (%)",
        caption = str_glue("From {min_y} to {max_y}, the average CAGR has been {ave_rate} \n for these top 30 ZipCodes in the Metro Detroit Area")
    )

g |> ggplotly(tooltip = "tool_tip") 

# Plot 3 
ZMI_Data_Prep
Employment_C <- data_import("Employment_C")

avg_zhvi_pc_by_year <- ZMI_Data_Prep |>
    group_by(Year) |> 
    summarise("Home Value Index" = ZHVI_PC_YoY |> mean())

homev_vs_Job_growth <- Employment_C |>
    left_join(avg_zhvi_pc_by_year, by = "Year") |>
    select(Year, PC_YoY, "Home Value Index") |>
    mutate("Employment in Metro Detroit" = PC_YoY) |>
    gather(key = "Legend", value = "Percentage_Change", "Employment in Metro Detroit", "Home Value Index")

homev_vs_Job_growth <- homev_vs_Job_growth |>
    mutate (PC_YoY_chr = Percentage_Change |> scales::percent (accuracy = 0.01)) |>
    mutate (Percentage_Change = (Percentage_Change*100)) |>
    mutate(label_text = str_glue("Legend:{Legend},
                                 Year:{Year},
                                 Percentage:{PC_YoY_chr}"))


num_of_zip <- ZMI_Data_Prep |> select (City_zip) |> unique() |> nrow()

p3 <- homev_vs_Job_growth |>
    drop_na(Percentage_Change) |>
    ggplot(aes (x = Year, y = Percentage_Change, linetype = Legend, colour = Legend, label = label_text)) +
    
    geom_line(linewidth = 1) +
    geom_point(aes(text = label_text), size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray0", size = 0.50)

#####
# Si prefieres que el dataframe resultante ya no esté agrupado, puedes usar unmutate
df <- ZMI_Data_Prep %>%
    group_by(Year) %>%
    mutate(cambio2 = mean(ZHVI_PC_YoY, na.rm = TRUE)) %>%
    ungroup()


