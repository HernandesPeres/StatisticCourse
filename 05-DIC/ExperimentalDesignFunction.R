library(dplyr)
library(desplot)

generate_positions <- function(n_cols, n_rows) {
  expand.grid(
    col = seq_len(n_cols),
    row = seq_len(n_rows)
  ) %>%
    slice_sample(prop = 1)
}

field_layout <- function(data, n_col, n_row, design) {
  
  sketch <- list()
  
  # ============================================================
  # DIC / CRD
  # ============================================================
  
  if (design %in% c("DIC", "CRD")) {
    
    data <- data %>%
      mutate(
        rep  = as.factor(rep),
        trat = as.factor(trat)
      )
    
    # Número total de parcelas
    n_plots <- nrow(data)
    
    if (n_col * n_row != n_plots) {
      stop("n_col * n_row must be equal to the number of plots.")
    }
    
    # Randomização completa
    positions <- expand.grid(
      col = seq_len(n_col),
      row = seq_len(n_row)
    ) %>%
      slice_sample(prop = 1)
    
    data <- data %>%
      mutate(
        col = positions$col,
        row = positions$row
      )
    
    plot <- desplot(
      trat ~ col + row,
      data = data,
      text = trat,
      main = "Field Layout - CRD",
      show.key = FALSE,
      cex = 0.8,
      col.regions = colorRampPalette(
        c("#d7191c",
          "#fdae61",
          "#ffffbf",
          "#a6d96a",
          "#1a9641")
      )(nlevels(data$trat))
    )
  }
  
  # ============================================================
  # DBC / RCBD
  # ============================================================
  
  else if (design %in% c("DBC", "RCBD")) {
    
    data <- data %>%
      mutate(
        bloco = as.factor(bloco),
        trat  = as.factor(trat)
      )
    
    n_trat  <- nlevels(data$trat)
    n_bloco <- nlevels(data$bloco)
    n_plots <- nrow(data)
    
    # ------------------------------------------------------------
    # Verificações
    # ------------------------------------------------------------
    
    if (n_plots != n_trat * n_bloco) {
      stop(
        "In a RCBD, the number of plots must equal ",
        "number of treatments × number of blocks."
      )
    }
    
    # Cada tratamento deve aparecer exatamente uma vez
    # dentro de cada bloco
    check <- table(data$bloco, data$trat)
    
    if (any(check != 1)) {
      stop(
        "Invalid RCBD: each treatment must occur exactly once ",
        "within each block."
      )
    }
    
    # Número de parcelas por bloco
    plots_per_block <- n_trat
    
    # ------------------------------------------------------------
    # Verificar se o grid comporta todos os blocos
    # ------------------------------------------------------------
    
    if (n_col * n_row != n_plots) {
      stop("n_col * n_row must equal the total number of plots.")
    }
    
    # ------------------------------------------------------------
    # Definir posição espacial dos blocos
    #
    # Aqui os blocos são organizados em linhas.
    # Cada bloco ocupa uma faixa horizontal.
    # ------------------------------------------------------------
    
    if (n_col %% n_bloco != 0) {
      stop(
        "For this layout, n_col must be divisible by the ",
        "number of blocks."
      )
    }
    
    block_cols <- n_col / n_bloco
    
    if (block_cols * n_row != n_trat) {
      stop(
        "The spatial dimensions are incompatible with the ",
        "number of treatments per block."
      )
    }
    
    # ------------------------------------------------------------
    # Randomização INDEPENDENTE dentro de cada bloco
    # ------------------------------------------------------------
    
    positions_list <- vector("list", n_bloco)
    
    block_levels <- levels(data$bloco)
    
    for (i in seq_along(block_levels)) {
      
      block <- block_levels[i]
      
      # Posições pertencentes ao bloco
      block_positions <- expand.grid(
        col = seq(
          from = (i - 1) * block_cols + 1,
          to   = i * block_cols
        ),
        row = seq_len(n_row)
      )
      
      # Randomização das parcelas dentro do bloco
      block_positions <- block_positions %>%
        slice_sample(prop = 1)
      
      block_positions$bloco <- block
      
      positions_list[[i]] <- block_positions
    }
    
    all_positions <- bind_rows(positions_list)
    
    # ------------------------------------------------------------
    # Randomizar tratamentos dentro de cada bloco
    # ------------------------------------------------------------
    
    randomized_data <- data %>%
      group_by(bloco) %>%
      slice_sample(prop = 1) %>%
      ungroup()
    
    # Garantir que a ordem dos blocos seja a mesma
    randomized_data <- randomized_data %>%
      mutate(bloco = factor(bloco, levels = block_levels)) %>%
      arrange(bloco)
    
    all_positions <- all_positions %>%
      mutate(bloco = factor(bloco, levels = block_levels)) %>%
      arrange(bloco)
    
    # ------------------------------------------------------------
    # Associar cada tratamento randomizado a uma posição
    # ------------------------------------------------------------
    
    randomized_data$col <- all_positions$col
    randomized_data$row <- all_positions$row
    
    data <- randomized_data
    
    # ------------------------------------------------------------
    # Croqui
    # ------------------------------------------------------------
    
    plot <- desplot(
      trat ~ col + row,
      data = data,
      text = trat,
      out1 = bloco,
      main = "Field Layout - RCBD",
      show.key = FALSE,
      cex = 0.7,
      shorten = "no",
      col.regions = colorRampPalette(
        c("#d7191c",
          "#fdae61",
          "#ffffbf",
          "#a6d96a",
          "#1a9641")
      )(nlevels(data$trat))
    )
  }
  
  else {
    stop("design must be 'CRD', 'DIC', 'RCBD' or 'DBC'.")
  }
  
  # ------------------------------------------------------------
  # Output
  # ------------------------------------------------------------
  
  sketch$plot <- plot
  sketch$data <- data
  
  return(sketch)
}


df = data.frame(trat = rep(c(1:36),2),
                bloco = rep(1:2, each = 36))

field_layout(df, n_col = 12, n_row = 6, design = "RCBD")
