
plot_inputs <- function(df, exclude_cols = c("id"), plots_per_row = 2, output_dir = ".") {
  
  # Get all columns except excluded (case insensitive check)
  all_cols <- setdiff(names(df), exclude_cols)
  
  if (length(all_cols) == 0) {
    return("<html><body><p>No columns found for distributions.</p></body></html>")
  }
  
  # Separate numeric and non-numeric
  numeric_cols <- all_cols[sapply(df[all_cols], is.numeric)]
  non_numeric_cols <- all_cols[!sapply(df[all_cols], is.numeric)]
  
  # Combine for plotting order: numeric first, then non-numeric
  # (Matching the logic of separating types then appending)
  plot_cols <- c(numeric_cols, non_numeric_cols)
  plot_types <- list()
  
  for (col in numeric_cols) plot_types[[col]] <- 'hist'
  for (col in non_numeric_cols) plot_types[[col]] <- 'pie'
  
  num_plots <- length(plot_cols)
  if (num_plots == 0) {
    return("<html><body><p>No suitable columns found for distributions.</p></body></html>")
  }
  
  # Calculate number of rows needed
  num_rows <- ceiling(num_plots / plots_per_row)
  
  # Start HTML
  html <- "
    <html>
    <head>
        <style>
            table { width: 100%; border-collapse: collapse; }
            td { text-align: center; padding: 10px; }
            img { max-width: 100%; height: auto; }
        </style>
    </head>
    <body>
        <table>
    "
  
  plot_index <- 1
  
  for (row in 1:num_rows) {
    html <- paste0(html, "<tr>")
    
    for (col_idx in 1:plots_per_row) {
      if (plot_index <= num_plots) {
        column <- plot_cols[plot_index]
        plot_type <- plot_types[[column]]
        
        # Create plot object using ggplot2
        p <- NULL
        
        if (plot_type == 'hist') {
          p <- ggplot(df, aes(x = .data[[column]])) +
            geom_histogram(bins = 20, fill = "steelblue", color = "white") +
            labs(title = paste("Distribution of", column, "(Histogram)"),
                 x = column, y = "Frequency") +
            theme_minimal()
          
        } else if (plot_type == 'pie') {
          # Prepare data for pie chart (calculate counts and percentages)
          pie_data <- as.data.frame(table(Var = df[[column]]))
          pie_data$Pct <- round(pie_data$Freq / sum(pie_data$Freq) * 100, 1)
          
          # Logic for legend placement if too many categories
          use_legend <- length(unique(df[[column]])) <= 10
          
          p <- ggplot(pie_data, aes(x = "", y = Freq, fill = Var)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            geom_text(aes(label = paste0(Pct, "%")), 
                      position = position_stack(vjust = 0.5), 
                      size = 3, check_overlap = TRUE) +
            labs(title = paste("Distribution of", column, "(Pie Chart - %)"),
                 x = "", y = "") +
            theme_void() + # Removes axes and grids
            theme(plot.title = element_text(hjust = 0.5))
          
          # Handle legend visibility based on category count
          if (!use_legend) {
            # If too many categories, hide legend to prevent crowding (or move it)
            p <- p + theme(legend.position = "none") 
          }
        }
        
        # Save to temp file to mimic BytesIO behavior
        tmp_file <- tempfile(fileext = ".png")
        ggsave(tmp_file, plot = p, width = 6, height = 4, dpi = 100)
        
        # Encode to Base64
        image_base64 <- base64encode(tmp_file)
        
        # Cleanup temp file
        unlink(tmp_file)
        
        # Add to HTML
        html <- paste0(html, sprintf('<td><img src="data:image/png;base64,%s" alt="%s distribution"></td>', image_base64, column))
        
        plot_index <- plot_index + 1
      } else {
        html <- paste0(html, "<td></td>") # Empty cell
      }
    }
    html <- paste0(html, "</tr>")
  }
  
  html <- paste0(html, "
        </table>
    </body>
    </html>
    ")
  
  # Write to file
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(html, paste0(output_dir, "/input_distributions.html"))
  message("File 'distributions.html' has been created.")
}