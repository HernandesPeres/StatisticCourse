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
  sketch <- list()
  
  if (design == "DIC" || design == "CRD") {
    data$rep <- as.factor(data$rep)
    data$trat <- as.factor(data$trat)
    positions <- generate_positions(n_col, n_row)
    data <- data %>%
      mutate(col = positions$col, row = positions$row)
    
    plot <- desplot(trat ~ col * row,
                    data = data,
                    text = trat,
                    main = "Field Layout - CRD",
                    show.key = FALSE,
                    cex = 0.8,
                    col.regions = colorRampPalette(c("#d7191c", 
                                                     "#fdae61", 
                                                     "#ffffbf", 
                                                     "#a6d96a",
                                                     "#1a9641"))(nlevels(data$trat)))
  }
  
  else if (design == "DBC" || design == "RCBD") {
    data$bloco <- as.factor(data$bloco)
    data$trat <- as.factor(data$trat)
    data <- arrange(data, bloco)
    
    n_trat <- nlevels(data$trat)
    n_bloco <- nlevels(data$bloco)
    n_plots <- n_trat * n_bloco
    
    if ((n_row * n_col) %% n_bloco != 0)
      stop("Number of rows and columns must result in equal-sized blocks.")
    
    plots_per_block <- n_plots / n_bloco
    block_cols <- n_col / n_bloco
    block_rows <- n_row
    
    positions_list <- vector("list", n_bloco)
    for (i in 1:n_bloco) {
      block_pos <- generate_positions_by_block(block_rows, block_cols)
      block_pos$col <- block_pos$col + (i - 1) * block_cols
      positions_list[[i]] <- block_pos[sample(nrow(block_pos)), ]
    }
    
    all_positions <- do.call(rbind, positions_list)
    data <- data[order(data$bloco), ]
    data$col <- all_positions$col
    data$row <- all_positions$row
    
    plot <- desplot(trat ~ col + row | bloco,
                    data = data,
                    text = trat,
                    cex = 0.7,
                    shorten = "no",
                    out1 = bloco,
                    show.key = F,
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