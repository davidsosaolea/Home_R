library(tidyverse)

# ROI ----
# Define the ROI function
calculate_ROI <- function(ending_value, starting_value) {
    
    ROI    <- (ending_value - starting_value) / starting_value
    ROI_ch <- ROI |> scales::percent(accuracy = 0.01)
        
    print(str_glue("The ROI is: {ROI_ch}"))

}

## Escenario 1 ----
# Digamos que compraste una casa por $100,000 en el ano 2000 y en el ano 2001 la vendes por 110000
calculate_ROI(ending_value = 110000, starting_value = 100000)

## Escenario 2 ----
# Digamos que compraste una casa por $110,000 en el ano 2000 y en el ano 2006 la vendes por 121000
calculate_ROI(ending_value = 121000, starting_value = 110000)


# ROI Anualizado 2 ----
# Define the Annualized ROI function
calculate_annualized_ROI <- function(ROI, years_held) {
    
    annualized_ROI    <- ROI / years_held
    annualized_ROI_ch <- annualized_ROI  |> scales::percent(accuracy = 0.01)
    print(paste("The Annualized ROI is:", annualized_ROI_ch))
}

## Escenario 1 ----
calculate_annualized_ROI(ROI = 0.10, years_held =  1)
## Escenario 2 ----
calculate_annualized_ROI(ROI = 0.10, years_held = 6)

# AAR ----

# Scenario 1
scenario_1 <- tibble(
    YEAR = 0:3,
    ROI = c(NA, 10, 10, 10),
    BALANCE = c(100000, 110000, 121000, 133100)  %>% scales::dollar()
)

# Scenario 2
scenario_2 <- tibble(
    YEAR = 0:3,
    ROI = c(NA, 15, 10, 5),
    BALANCE = c(100000, 115000, 126500, 132825)  %>% scales::dollar()
)

# Scenario 3
scenario_3 <- tibble(
    YEAR = 0:3,
    ROI = c(NA, 20, 10, 0),
    BALANCE = c(100000, 120000, 132000, 132000)  %>% scales::dollar()
)


scenario_4 <- tibble(
    YEAR = 0:3,
    ROI = c(NA, 30, 0, 0),
    BALANCE = c(100000, 130000, 130000, 130000) %>% scales::dollar()
)

scenario_1
scenario_2
scenario_3
scenario_4

AAR_f <- function(data) {
    
    data |> 
        mutate(AAR = mean(ROI , na.rm = TRUE)) |> 
        mutate(AAR = AAR %>% scales::comma(accuracy = 0.01, suffix =  "%"))
    
}

AAR_f(data = scenario_1)
AAR_f(data = scenario_2) 
AAR_f(data = scenario_3) 
AAR_f(data = scenario_4) 

# CAGR/TCAC ----
# Tasa de crecimiento anual compuesta
# Define the CAGR function
calculate_CAGR <- function(data, ending_value, starting_value, n) {
    
    data |>
        mutate(CAGR = ((ending_value / starting_value)^(1/n)) - 1) |>
        mutate(CAGR = CAGR |> scales::percent(accuracy = 0.01))
    
}

calculate_CAGR(data = scenario_1 ,ending_value = 133100, starting_value = 100000, n = 3)
calculate_CAGR(data = scenario_2 ,ending_value = 132825, starting_value = 100000, n = 3)
calculate_CAGR(data = scenario_3 ,ending_value = 132000, starting_value = 100000, n = 3)
calculate_CAGR(data = scenario_4 ,ending_value = 130000, starting_value = 100000, n = 3)



    



