# Analysis & Manipulation De Datos En Espanol ----
# Instalacion de paquete (Packages) ----

# CRAN Packages ----
# https://cran.r-project.org/ (Website)

# Sistema de archivos
"fs"   # Para manejar el sistema de archivos

# Importar 
"readxl"      # lee archivos de excel                  
"writexl"     # guarda los datos en archivos de excel  
"odbc"        # para conectarse a base de datos        
"RSQLite"     # para conectarse a base de datos        

# Para analizar transformar y visualizar los Datos     
"naniar"      # ayuda a manejar los valores faltantes  
"lubridate"   # para trabjar con fechas y tiempo       
"tidyquant"   # para el formato de las graficas y mas  
"tidyverse"   # Instala las siguientes paquetes (Packages) dplyr, ggplot2, tibble, tidyr, readr, purrr, stringr, forcats

# Modelos 
"tidymodels"  # Instala las siguientes paquetes (Packages) broom, infer, recipes, rsample, & yardstick
"umap"        # para visualización de clústeres

# Otros  
"devtools"    # para paquetes (Packages) que no estan en la pagina de CRAN  
"rmarkdown"   # Para documentación en html, pdf, word, etc ..               
"kableExtra"  # HTML tablas

pkgs_cran <- c("fs","readxl", "writexl", "odbc", "RSQLite", "naniar", "tidyverse",
               "lubridate", "tidyquant", "tidymodels", "umap", "devtools", "rmarkdown", "kableExtra")


install.packages("fs")        # Para instalar una libraria a la vez 
install.packages(pkgs_cran)   # Para instalar todas las librarias  




