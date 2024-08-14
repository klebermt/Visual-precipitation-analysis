source('Scripts/tabla.R')

# Function to import data from a folder
getDataFolder <- function(path) {
  folder_path <- path # Path of the folder {url}
  
  # Use list.files() to retrieve the list of files in the folder
  files_in_folder <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Import all data files and structure them appropriately
  data <- setTables(files_in_folder)
  
  return(data)
}

# Function to import names from files in a folder
setNames <- function(name) {
  # Define the path of the folder
  folder_path <- name
  
  # Use list.files() to retrieve the list of files in the folder
  files_in_folder <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Create an empty array to store the names of the DataFrames
  dataframe_names <- c()
  
  # Iterate over each file in the list
  for (file in files_in_folder) {
    # Extract the name of the file
    dataframe_name <- sub(".csv$", "", basename(file))
    
    # Append the name to the array
    dataframe_names <- c(dataframe_names, dataframe_name)
  }
  
  return(dataframe_names)
}

# Function to modify table for visualization
ordTableMonth <- function(data) {
  # Iterate over each data frame in the list
  for (dataframe_name in names(data)) {
    # Print the name of the current data frame
    print(dataframe_name)
    
    # Reshape the dataframe to long format
    Sys.setlocale("LC_TIME", "en_US.UTF-8")
    data_long <- data[[dataframe_name]] %>%
      pivot_longer(cols = -c(year), names_to = "month", values_to = "pp") %>%
      mutate(month = as.Date(paste(month, " 01, ", year, sep = ""), format = "%B %d, %Y"))
    
    # Update the dataframe with the reshaped data
    data[[dataframe_name]] <- data_long
  }
  
  return(data)
}

# Function to export plots to a folder
exportPlots <- function(plots, folder_path) {
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
  }
  
  total_plots <- length(plots)
  progress_bar <- txtProgressBar(min = 0, max = total_plots, style = 3)
  
  for (i in seq_along(plots)) {
    file_name <- paste0(folder_path, "/plot_", i, ".png")
    ggsave(file_name, plots[[i]], width = 10, height = 6, units = "in", dpi = 300)
    setTxtProgressBar(progress_bar, i)
  }
  
  close(progress_bar)
  cat("Plots exported successfully to", folder_path, "\n")
}

# Función para limpiar las listas de excel
limpiar_datos_lista <- function(datos_lista) {
  # Crear una función para quitar la primera y última columna de un dataframe
  quitar_primera_ultima_columna <- function(df) {
    return(df[, -c(1, ncol(df))]) # Selecciona todas las columnas excepto la primera y la última
  }
  
  # Define una función para eliminar la letra "E" al final de los números
  eliminar_letra_E <- function(valores) {
    # Utiliza sub() para eliminar "E" al final de cada valor
    return(sub("E$", "", valores))
  }
  
  # Define una función para dividir los valores por 100, excepto los de la columna 'YEAR'
  dividir_valores_por_100 <- function(df) {
    # Itera sobre todas las columnas del data frame excepto 'YEAR'
    for (col in names(df)[-1]) {
      # Divide los valores de la columna por 100
      df[[col]] <- df[[col]] / 100
    }
    return(df)
  }
  
  # Definir los nombres de los meses en inglés
  nombres_meses <- c("year","January", "February", "March", "April", "May", "June",
                     "July", "August", "September", "October", "November", "December")
  
  # Definir una función para renombrar las columnas
  renombrar_columnas <- function(df) {
    # Renombrar las columnas, comenzando desde la segunda columna (X1)
    colnames(df) <- nombres_meses
    return(df)
  }
  
  # Aplicar todas las transformaciones a la lista de datos
  datos_lista_final <- lapply(datos_lista, function(df) {
    df <- quitar_primera_ultima_columna(df)
    df <- as.data.frame(lapply(df, eliminar_letra_E), stringsAsFactors = FALSE)
    df[-1] <- lapply(df[-1], as.numeric)
    df <- dividir_valores_por_100(df)
    df <- renombrar_columnas(df)
    return(df)
  })
  
  return(datos_lista_final)
}

# Complete data by mean
complete_by_mean <- function(data) {
  for (i in seq_along(data)) {
    data[[i]] <- data[[i]] %>%
      mutate_at(vars(January:December), ~replace_na(., mean(., na.rm = TRUE)))
  }
  return(data)
}