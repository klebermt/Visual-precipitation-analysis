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
ordTableMonth <- function(data, name) {
  # Import data ------------------------------
  folder_path <- name # Define the path of the folder {url}
  
  # Use list.files() to retrieve the list of files in the folder
  files_in_folder <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Iterate over each file in the folder
  for (file in files_in_folder) {
    # Extract the filename without extension
    dataframe_name <- sub(".csv$", "", basename(file))
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

