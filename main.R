# Importing Data
source('Scripts/Import Data.R')
source("Scripts/Graphics.R")

## Visual Graphical Analysis

# Importing Data and Assigning Names
data <- getDataFolder("Data")   # Importing data and storing it in a variable
data_names <- setNames("Data")  # Setting the names of the datasets

# Data Transformation
data_by_month <- ordTableMonth(data, "Data") # Transforming data by month

# Plotting Each Dataset and Exporting the Plots
plots <- list()
for (i in seq_along(data_by_month)) {
  plot1 <- getGraphMonth(data_by_month[[i]], data_names[i])
  plots[[i]] <- plot1
  print(plot1) # Print each plot
}

# Exporting the Plots
exportPlots(plots, "histograms")
