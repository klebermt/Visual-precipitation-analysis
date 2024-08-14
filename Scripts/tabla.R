# Libraries that we need
library(dplyr)
library(tidyr)

# Function to process the table and structure it appropriately
setTable <- function(data) {
  # Assign meaningful names to the columns
  colnames(data) <- c("year", "month", "day", "precipitation", "max_temp", "min_temp")
  
  # Clean data, replacing negative precipitation values with NA
  data$precipitation[data$precipitation < 0] <- NA
  
  # Aggregate data by year and month, calculating total precipitation
  precipitation_by_month <- data %>%
    group_by(year, month) %>%
    summarise(total_precipitation = sum(precipitation))
  
  # Create a dataframe with all combinations of year and month
  all_months <- expand.grid(year = unique(precipitation_by_month$year), month = 1:12)
  
  # Left join with precipitation_by_month
  precipitation_by_month <- left_join(all_months, precipitation_by_month, by = c("year", "month"))
  
  # Rename months in the DataFrame to their respective names (January, ..., December)
  precipitation_by_month <- precipitation_by_month %>%
    pivot_wider(names_from = month, values_from = total_precipitation) %>%
    rename(January = `1`, February = `2`, March = `3`, April = `4`, May = `5`, June = `6`,
           July = `7`, August = `8`, September = `9`, October = `10`, November = `11`, December = `12`)
  
  # Reorder the columns for better readability
  precipitation_by_month <- precipitation_by_month %>%
    select(year, January, February, March, April, May, June, July, August, September, October, November, December)
  
  return(precipitation_by_month)
}

# Function to manipulate multiple tables
setTables <- function(files_in_folder) {
  # Create a list to store dataframes
  dataframe_list <- list()
  
  # Iterate over the list of files and create dataframes
  for (file in files_in_folder) {
    # Extract the filename without extension
    file_name <- sub(".csv$", "", basename(file))
    
    # Load the CSV file into a dataframe with the same name
    dataframe_list[[file_name]] <- read.csv(file, header = FALSE, sep = ' ')
    
    # Give proper column names to the dataframe
    dataframe_list[[file_name]] <- setTable(dataframe_list[[file_name]])
  }
  
  return(dataframe_list)
}
