library(dplyr)
library(desplot)

generate_positions <- function(n_cols, n_rows) {
  expand.grid(col = 1:n_cols, row = 1:n_rows) %>%
    sample_frac()
}

generate_positions_by_block <- function(n_rows, n_cols) {
  expand.grid(row = 1:n_rows, col = 1:n_cols)
}

field_layout <- function(data, n_col, n_row, design) {
  data$rep <- as.factor(data$rep)
  data$trat <- as.factor(data$trat)
  sketch <- list()
  
  if (design == "DIC" || design == "CRD") {
    positions <- generate_positions(n_col, n_row)
    data <- data %>%
      mutate(col = positions$col, row = positions$row)
    
    plot <- desplot(trat ~ col * row,
                    data = data,
                    text = trat,
                    main = "Field Layout - CRD",
                    show.key = FALSE,
                    cex = 0.8,
                    show.legend = NA,
                    col.regions = colorRampPalette(c("#d7191c", 
                                                     "#fdae61", 
                                                     "#ffffbf", 
                                                     "#a6d96a",
                                                     "#1a9641"))(nlevels(data$trat)))
  }
  
  else if (design == "DBC" || design == "RCBD") {
    colnames(data)[colnames(data) == "bloco"] <- "rep"
    data <- arrange(data, rep)
    
    n_trat <- nlevels(data$trat)
    n_rep <- nlevels(data$rep)
    n_plots <- n_trat * n_rep
    
    if ((n_row * n_col) %% n_rep != 0)
      stop("Number of rows and columns must result in equal-sized blocks.")
    
    plots_per_block <- n_plots / n_rep
    block_cols <- n_col / n_rep
    block_rows <- n_row
    
    positions_list <- vector("list", n_rep)
    for (i in 1:n_rep) {
      block_pos <- generate_positions_by_block(block_rows, block_cols)
      block_pos$col <- block_pos$col + (i - 1) * block_cols
      positions_list[[i]] <- block_pos[sample(nrow(block_pos)), ]
    }
    
    all_positions <- do.call(rbind, positions_list)
    data <- data[order(data$rep), ]
    data$col <- all_positions$col
    data$row <- all_positions$row
    
    plot <- desplot(trat ~ col + row | rep,
                    data = data,
                    text = trat,
                    cex = 0.7,
                    shorten = "no",
                    show.legend =F,
                    out1 = rep,
                    main = "Field Layout - RCBD",
                    col.regions = colorRampPalette(c("#d7191c", 
                                                     "#fdae61", 
                                                     "#ffffbf",
                                                     "#a6d96a", 
                                                     "#1a9641"))(nlevels(data$trat)))
  }
  sketch$plot <- plot
  sketch$data <- data
  return(sketch)
}