# Importing Data
source('Scripts/Import Data.R')
source("Scripts/Graphics.R")

## Visual Graphical Analysis

# Importing Data and Assigning Names
data <- getDataFolder("Data")   # Importing data and storing it in a variable
data_names <- setNames("Data")  # Setting the names of the datasets

# Data Transformation
data_by_month <- ordTableMonth(data) # Transforming data by month

# ........................................................
#     Plotting Each Dataset and Exporting the Plots
# ........................................................
plots <- list()
for (i in seq_along(data_by_month)) {
  plot1 <- getGraphMonth(data_by_month[[i]], data_names[i])
  plots[[i]] <- plot1
  print(plot1) # Print each plot
}

# ........................................................
#                    Exporting the Plots
# ........................................................

#exportPlots(plots, "histograms") # Uncomment if you want to export the plots

##################### End of Plotting ####################

# ........................................................
#                    Export data to excel
# ........................................................

library(openxlsx)

for (i in 1:length(data)) {
  data[[i]]$total <- rowSums(data[[i]][, 2:13], na.rm = TRUE)
  data[[i]]$mean <- rowMeans(data[[i]][, 2:13], na.rm = TRUE) # This comment if you don't have the mean
}

wb <- createWorkbook()

# Iteramos a través de los dataframes en la lista y los escribimos en hojas separadas del archivo Excel
for (i in 1:length(data)) {
  addWorksheet(wb, sheetName = data_names[i])  # Creamos una nueva hoja en el archivo Excel
  writeData(wb, sheet = i, x = data[[i]], startCol = 1, startRow = 1)  # Escribimos el dataframe en la hoja
}

# Guardamos el archivo Excel
saveWorkbook(wb, file = "dataframes.xlsx", overwrite = TRUE)

# ........................................................
#         Leer los datos de excel corregido
# ........................................................

library(readxl)
# Especifica la ruta de tu archivo Excel
archivo_excel <- "Excel/data he-4.xlsx"

# Leer el archivo Excel
nombres_hojas <- excel_sheets(archivo_excel)

# Crear una lista para almacenar los datos de cada hoja
datos_lista <- list()

# Leer cada hoja y almacenar los datos en la lista
for (hoja in nombres_hojas) {
  datos_lista[[hoja]] <- read_excel(archivo_excel, sheet = hoja)
}

# Llamar a la función con datos_lista como entrada
datos_lista_final <- limpiar_datos_lista(datos_lista)

# Ordenar los datos por mes
datos_list_by_month <- ordTableMonth(datos_lista_final)

# ........................................................
#     Plotting Each Dataset and Exporting the Plots
# ........................................................
plots <- list()
for (i in seq_along(datos_list_by_month)) {
  plot1 <- getGraphMonth(datos_list_by_month[[i]], data_names[i])
  plots[[i]] <- plot1
  print(plot1) # Print each plot
}

# ........................................................
#                    Exporting the Plots
# ........................................................

#exportPlots(plots, "histograms-corregidos")
