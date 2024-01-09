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
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray0", size = 0.50)+

    labs(
        title = "Home Prices vs Employment",
        subtitle = "Percentage Change YoY between Home Prices & Employment",
        y = "Percetage Change (%)",
        caption = str_glue("Data Sources are from 'Zillow' & 'Bureau of Labor Statistics'.
        Employment data is from the area of Detroit-Warren-Dearborn Employment. 
        The Home Value Index is the mean of the change of {num_of_zip} ZipCodes in the Detroit-Warren-Dearborn area'"
    ))+
    theme_light()

p3 |> ggplotly(tooltip = "text")


## Themes

p3

RColorBrewer::display.brewer.all()

p3 + scale_colour_brewer(palette = "Set1")
p3 + scale_colour_brewer(palette = "Set2")
p3 + scale_colour_brewer (palette = "Pastel1")
p3 + scale_colour_brewer (palette = "Dark2")

p3 + theme_dark() + scale_colour_manual(values = c("blue","gold"))
p3 + theme (
    
    plot.title    = element_text(color="dark blue", size = 12, face = "bold"),
    plot.subtitle = element_text(color="dark blue", size = 10, face = "italic"),
    plot.caption  = element_text(color="dark blue", size = 8, face = "bold.italic")
)    
    
p3 + lims(x = c(2001, 2022))


# Plot4
## LOESS Regression
## (LOESS) Local Polynomial Regression

homev_vs_Job_growth_II <- Employment_C |>
    left_join(avg_zhvi_pc_by_year, by = "Year") |>
    select(Year, PC_YoY, "Home Value Index") |>
    mutate (Employment = PC_YoY) |>
    mutate(Home_Value_Index = `Home Value Index`) |>
    filter (Year > 1999)
##########################
# Establecer una semilla para reproducibilidad
set.seed(123)

# Ajustar un modelo loess con un parámetro de suavidad (span) personalizado
mod <- loess(Home_Value_Index ~ Employment, data = homev_vs_Job_growth_II)

# Crear un conjunto de datos de la cuadrícula para las predicciones
grid <- tibble(Employment = seq(min(homev_vs_Job_growth_II$Employment), max(homev_vs_Job_growth_II$Employment), length = 50))
grid$Home_Value_Index <- predict(mod, newdata = grid)

# Calcular residuos estandarizados
std_resid <- resid(mod) / mod$s

# Identificar outliers
outlier <- filter(homev_vs_Job_growth_II, abs(std_resid) > 1)

# Crear el gráfico
p4 <- ggplot(homev_vs_Job_growth_II, aes(Employment, Home_Value_Index)) +
    geom_point() +
    geom_line(data = grid, colour = "blue", size = 1.5) + 
    ggrepel::geom_text_repel(data = outlier, aes(label = Year), color = "red") +
    labs(
        title = "LOESS Regression",
        y = "Home Value Index",
        x = "Employment",
        caption = "LOESS regression, sometimes called local regression, is a method that uses local fitting to fit a regression model to a dataset"
    ) +
    theme_minimal()  # Puedes cambiar a theme_light() u otros temas según tu preferencia

# Añadir anotaciones al gráfico
p4 + annotate(
    geom = "curve",
    x = -0.10,
    y = 0.09,
    xend = -0.106,
    yend = 0.055,
    curvature = 0.3,
    arrow = arrow(length = unit(2, "mm"))
) +
    annotate(geom = "text", x = -0.10, y = 0.09, label = "Global Pandemic", hjust = "left")


#######################
mod <- loess(Home_Value_Index ~ Employment, data = homev_vs_Job_growth_II)
grid <- tibble (Employment = seq(min(homev_vs_Job_growth_II$Employment), max(homev_vs_Job_growth_II$Employment), length = 50))
grid$Home_Value_Index <- predict(mod, newdata = grid)

std_resid <- resid (mod)/mod$s
outlier   <- filter (homev_vs_Job_growth_II, abs(std_resid) > 1)
outlier

p4 <- ggplot(homev_vs_Job_growth_II, aes(Employment, Home_Value_Index)) +
    geom_point() +
    geom_line(data = grid, colour = "blue", size = 1.5) + 
    ggrepel::geom_text_repel(data = outlier, aes (label = Year)) +
    
    labs (
        title = "LOESS Regression",
        y = "Avg % Change Home Value Index",
        x = "Avg % Change Employment",
        caption = "LOESS regression, sometimes called local regression, 
        is a method that uses local fitting to fit a regression model to a dataset"
        )

homev_vs_Job_growth_II |> tail()

p4 + annotate(geom = "curve",
              x = -0.10,
              y = 0.09,
              xend = -0.106,
              yend = 0.055,
              curvature = 0.3, arrow = arrow (length = unit(2, "mm"))) +
    annotate(geom = "text", x = -0.10, y = 0.09, label = "Global Pandemic", hjust = "left")



## LOESS & ggplotly
homev_vs_Job_growth_II <- homev_vs_Job_growth_II |>
    mutate (Home_Value_Index_ch = Home_Value_Index |> scales::percent (accuracy = 0.01)) |>
    mutate (Employment_ch = PC_YoY |> scales::percent (accuracy = 0.01)) |>
    mutate (label_text  = str_glue("Year: {Year},
                                   Home Value Index: {Home_Value_Index_ch},
                                   Employment: {Employment_ch}"))

p4 <- ggplot(homev_vs_Job_growth_II, aes(Employment, Home_Value_Index)) +
    geom_point(aes(text = label_text), size = 2) +
    geom_line(data = grid, colour = "blue", size = 1.5) + 
    ggrepel::geom_text_repel(data = outlier, aes (label = Year)) +
    
    labs (
        title = "LOESS Regression",
        y = "Avg % Change Home Value Index",
        x = "Avg % Change Employment",
        caption = "LOESS regression, sometimes called local regression, 
        is a method that uses local fitting to fit a regression model to a dataset"
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) 

p4 |>
    ggplotly(tooltip = "text")


###Plot5
# geombart

homev_vs_Job_growth_III <- Employment_C |>
    left_join(avg_zhvi_pc_by_year, by = "Year") |>
    select(Year, PC_YoY, "Home Value Index") |>
    mutate (Employment = PC_YoY) |>
    mutate(Home_Value_Index = `Home Value Index`)

homev_vs_Job_growth_III <- homev_vs_Job_growth_III |>
    mutate (Home_Value_Index_PC = `Home Value Index` |> scales:: percent (accuracy = 0.01)) |> 
    mutate (Employment_PC       =  PC_YoY |> scales::percent (accuracy = 0.01)) |>
    mutate (label_text1         = str_glue("Employment: (Employment_PC}")) |>
    mutate (label_text2         = str_glue("Home Value Index: {Home_Value_Index_PC}")) |>
    mutate(label_text3          = str_glue("E: {Employment_PC}%")) |>
    mutate(label_text4          = str_glue("H: {Home_Value_Index_PC}%"))

miny <- homev_vs_Job_growth_III |>
    pull (Year) |> min()

max_y <- homev_vs_Job_growth_III |>
    pull (Year) |> max()

p5 <- ggplot(homev_vs_Job_growth_III) +
    geom_bar(aes (Year, Employment, text = label_text1),
             stat     = "identity",
             position = "dodge",
             fill     = "cadetblue1",
             color    = "black") +
    
    geom_bar(aes (Year, Home_Value_Index, text = label_text2),
             stat     = "identity",
             position = "dodge",
             fill     = "navy",
             color    = "black",
             alpha    =  0.3)+
    scale_y_continuous (labels = scales::percent)+
    
    labs (
    title = "Home Prices vs Employment",
    caption = str_glue("Data sources: Zillow & Bureau Labor of Statistics
                       Timeframe: {min_y} {max_y}")
    )+
    geom_label(aes (Year, Employment, label = label_text3),
           hjust = "left",
           vjust = "top",
           size = 3) +
    geom_label(aes (Year, Home_Value_Index, label = label_text4),
               hjust = "right",
               vjust = "bottom",
               size = 3)

p5 |> 
    ggplotly (tooltip = "text") |>
    layout (title = list(text = paste('Home Prices vs Employment',
                                      '<br>',
                                      '<sup>',
                                      '<b>','Data sources:',
                                      '</b>', "(Zillow) & (Bereau Labor of Statistics)",
                                      '</sup>')))



# Plot6
## 80/20
# Preparing the data
CAGR_Data_zip <- CAGR_Data |>
    mutate (zip = str_extract (City_zip, pattern = "\\d{5}")) |>
    mutate (zip = zip |> as.integer())

demographis <- data_import(dataSource = "Demographis")

demographis_CAGR <-  CAGR_Data_zip |> left_join(demographis, by="zip")

demographis_CAGR |> glimpse()


demographis_CAGR  <- demographis_CAGR |>
    mutate(label_text = str_glue("House Hold Median Income: {House_Hold_Median_Income |> scales::dollar()}"))

p6 <- ggplot(demographis_CAGR, aes(x = reorder (City_zip, -House_Hold_Median_Income))) +
    geom_bar(aes(y = House_Hold_Median_Income, text = label_text),
             stat = "identity",
             color = "black", fill = "skyblue") +
    scale_y_continuous (name = "Median Income", labels = scales::dollar) +
    labs (
        x = "ZipCode"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

p6 |>
    ggplotly(tooltip = "label_text") |>
    layout(title = list(text = paste0('<br>',
                                 'House Hold Median Income',
                                 '</br>',
                                 '<br>',
                                 '<sup>',
                                 '<b>',
                                 'Note:',
                                 '</b>', 'These ZipCodes have the higher CAGR in Metro Detroit',
                                 '</sup>')))
##plot7
Y1 <-  2010
Y2 <-  2023
CAGR_Data2 <- getTopCitiesByCAGR (Y1 = Y1, Y2 = Y2, show_rows = 30)

CAGR_Data2 <- CAGR_Data2 |>
    mutate(CumulativeCAGR = cumsum (CAGR)) |>
    mutate (CumulativePercent = CumulativeCAGR / sum (CAGR)) |>
    mutate(CAGR_chr = CAGR |> scales:: percent (accuracy = 0.01)) |>
    mutate(CAGR_chr = str_glue("CAGR: {CAGR_chr} 
                               Data from {Y1} to {Y2}")) |>
    
    mutate (Label = str_glue("Location: {City_zip},
                             CumulativePercent: {CumulativePercent |> scales::percent()}"))

pareto_chart <- ggplot(CAGR_Data2, aes (x = reorder (City_zip, ~ CAGR), y= CAGR)) +
    geom_bar(aes (text = CAGR_chr),
             stat = "identity",
             fill = "skyblue") +
    
    geom_line(aes (y = CumulativePercent, group = 1, text = Label), colour = "red", size = 1) + 
    geom_point(aes(y = CumulativePercent, group = 1, text = Label), colour = "red", size = 2) +
    scale_y_continuous (name = "CAGR (%)",
                        sec.axis = sec_axis(~ . , name = "Cumulative %"),
                        labels = scales::percent) +
    
    labs(
        x = "ZipCode",
        title = "Pareto Chart of CAGR by ZipCode"
        )+
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))


ggplotly(pareto_chart, tooltip = "text")

#########
library(dplyr)
library(ggplot2)
library(ggplotly)
library(scales)

Y1 <- 2010
Y2 <- 2023
CAGR_Data2 <- getTopCitiesByCAGR(Y1 = Y1, Y2 = Y2, show_rows = 30)

CAGR_Data2 <- CAGR_Data2 |>
    mutate(
        CumulativeCAGR = cumsum(CAGR),
        CumulativePercent = CumulativeCAGR / sum(CAGR),
        CAGR_chr = CAGR |> scales::percent(accuracy = 0.01),
        CombinedLabel = str_glue("CAGR: {CAGR_chr}, Data from {Y1} to {Y2}, Location: {City_zip}, CumulativePercent: {CumulativePercent |> scales::percent()}")
    )

# Truncar las etiquetas a una longitud fija (ajusta según sea necesario)
max_label_length <- 200
CAGR_Data2$CombinedLabel <- substr(CAGR_Data2$CombinedLabel, 1, max_label_length)

pareto_chart <- ggplot(CAGR_Data2, aes(x = reorder(City_zip, ~CAGR), y = CAGR)) +
    geom_bar(fill = "skyblue") +
    geom_line(aes(y = CumulativePercent), color = "red", size = 1) +
    geom_point(aes(y = CumulativePercent), color = "red", size = 2) +
    scale_y_continuous(
        name = "CAGR (%)",
        sec.axis = sec_axis(~., name = "Cumulative %"),
        labels = scales::percent
    ) +
    labs(
        x = "ZipCode",
        title = "Pareto Chart of CAGR by ZipCode"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90))

# Anotar con etiquetas
annotations <- CAGR_Data2 %>%
    group_by(City_zip) %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    mutate(Label = str_glue("CAGR: {CAGR_chr}\nData from {Y1} to {Y2}\nLocation: {City_zip}\nCumulativePercent: {CumulativePercent |> scales::percent()}"))

pareto_chart <- pareto_chart +
    annotate("text", x = annotations$City_zip, y = annotations$CAGR, label = annotations$Label, vjust = -1, hjust = 0, color = "black", size = 3)

ggplotly(pareto_chart, tooltip = "text")

