# libraries that we need
library(ggplot2)

# Plot histogram
getGraphMonth <- function(data, name) {
  ggplot(data, aes(x = month, y = pp)) +
    geom_line(aes(color = "Precipitation"), na.rm = TRUE, show.legend = FALSE) +  # Add a line plot for precipitation
    geom_point(size = 0.5, color = "#7CB5EC") +  # Add points with smaller size and same color as the line
    geom_smooth(method = "lm", formula = y ~ x, color = "#ff0000", se = FALSE) +  # Add linear regression line
    labs(title = paste("Histograma de precipitación (mm) - Estación", name),
         x = paste("Periodo (", min(data$year), "-", max(data$year), ")"), 
         y = "Precipitación (mm)",
         caption = "Fuente: SENAMHI") +  # Add a caption with the source
    scale_color_manual(values = c("Precipitation" = "#7CB5EC")) +  # Set color for precipitation
    theme_bw()  +  # Use classic theme
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)  # Adjust plot title
    )
}