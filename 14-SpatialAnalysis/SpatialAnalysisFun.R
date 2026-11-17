FillGrid = function(dat, verbose = TRUE) {
  
  dat = dat %>%
    dplyr::mutate(col = as.integer(as.character(col)),
                  row = as.integer(as.character(row)),
                  clone = as.character(clone),
                  rep = as.character(rep))
  
  grid = expand.grid(col = seq(min(dat$col, na.rm = TRUE), max(dat$col, na.rm = TRUE)),
    row = seq(min(dat$row, na.rm = TRUE), max(dat$row, na.rm = TRUE)))
  
  falta = dplyr::anti_join(grid, dplyr::distinct(dat, col, row),
                            by = c("col", "row"))
  
  dat$filler = FALSE
  if (nrow(falta) == 0) {
    if (verbose) cat('Grid Complet\n')
    return(dat)}
  if (verbose) cat('Imputing', nrow(falta), 'position', nrow(grid), '\n')
  
  freq = dat %>%
    dplyr::filter(!is.na(clone)) %>%
    dplyr::count(clone, name = "n") %>%
    dplyr::arrange(n, clone)
  
  clones_imp = rep(freq$clone, length.out = nrow(falta))
  
  falta = falta[sample(nrow(falta)), , drop = FALSE]
  falta$clone = clones_imp
  
  rep_col = dat %>%
    dplyr::filter(!is.na(rep)) %>%
    dplyr::count(col, rep, name = "n") %>%
    dplyr::group_by(col) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::select(col, rep_c = rep)
  
  rep_glob = names(sort(table(dat$rep), decreasing = TRUE))[1]
  
  falta = falta %>%
    dplyr::left_join(rep_col, by = "col") %>%
    dplyr::mutate(rep = ifelse(is.na(rep_c), rep_glob, rep_c)) %>%
    dplyr::select(-rep_c)
  
  novo = dat[rep(1L, nrow(falta)), ]
  rownames(novo) = NULL
  
  traits = intersect(c("TY", "MY", "SG", "check"), names(novo))
  novo[traits] = NA
  
  novo$col = falta$col
  novo$row = falta$row
  novo$clone = falta$clone
  novo$rep = falta$rep
  novo$filler = TRUE
  
  dplyr::bind_rows(dat, novo) %>% dplyr::arrange(col, row)
}



icREML <- function(fm, scale=1) {
  if(!is.list(fm)) stop(" Models need to be in a list\n")
  if(is.null(names(fm))) namesfm <- paste("fm", 1:length(fm))
  else namesfm <- names(fm)
  require(asreml)
  asreml.options(Cfixed = TRUE, gammaPar=FALSE)
  fm <- lapply(fm, function(el) {
    if(is.null(el$Cfixed)) {
      out <- update(el, maxit=1) }
    else out <- el
    out})
  logl <- lapply(fm, function(el) el$loglik)
  summ <- lapply(fm, function(el) summary(el, coef=TRUE)$coef.fixed)
  which.X0 <- lapply(summ, function(el) !is.na(el[, "z.ratio"]))
  p.0 <- lapply(which.X0, function(el) sum(el))
  Cfixed <- lapply(fm, function(el) el$Cfixed)
  logdet <- lapply(1:length(fm), function(el, Cfixed, which.X0, scale) {
    log(prod(svd(as.matrix(scale*Cfixed[[el]][which.X0[[el]], which.X0[[el]]]))$d))
  }, Cfixed, which.X0, scale)
  vparam <- lapply(fm, function(el) summary(el)$varcomp)
  q.0 <- lapply(vparam, function(el) sum(!(el$bound == "F" | el$bound == "B")))
  b.0 <- lapply(vparam, function(el) sum(el$bound == "F" | el$bound == "B"))
  logl <- lapply(1:length(fm), function(el, logl, logdet, p.0) {
    logl[[el]] - logdet[[el]]/2}, logl, logdet,p.0)
  aic <- unlist(lapply(1:length(fm), function(el, logl, p.0, q.0) {
    -2*logl[[el]] + 2*(p.0[[el]] + q.0[[el]])}, logl, p.0, q.0))
  bic <- unlist(lapply(1:length(fm), function(el, logl, p.0, q.0, fm) {
    -2*logl[[el]] + log(fm[[el]]$nedf+p.0[[el]])*(p.0[[el]] + q.0[[el]])},
    logl, p.0, q.0, fm))
  results <- data.frame(model=namesfm, loglik = unlist(logl), p=unlist(p.0),
                        q=unlist(q.0), b = unlist(b.0), AIC = aic, BIC = bic, logdet=unlist(logdet))
  row.names(results) <- 1:dim(results)[1]
  invisible(results)
}


asreml.options(trace = FALSE)

H2_cullis <- function(model, geno_term, vc_name = NULL, subsample = NULL) {
  vc <- summary(model)$varcomp
  
  if (is.null(vc_name)) {
    hit <- grep(paste0("^", geno_term, "$|^vm\\(", geno_term), rownames(vc), value = TRUE)
    if (length(hit) == 0) {
      hit <- grep(geno_term, rownames(vc), value = TRUE)
      hit <- hit[!grepl(":", hit)]        # descarta interações
    }
    if (length(hit) == 0)
      stop("componente de '", geno_term, "' não encontrado. varcomp tem: ",
           paste(rownames(vc), collapse = " | "))
    vc_name <- hit[1]
  }
  
  vg <- vc[vc_name, "component"]
  if (is.na(vg) || vg <= 0) stop("vg inválido (", vg, ") em '", vc_name, "'")
  
  pred <- predict(model, classify = geno_term, sed = TRUE)
  if (is.null(pred$sed)) stop("predict não devolveu matriz sed")
  
  sed <- pred$sed
  if (!is.null(subsample) && nrow(sed) > subsample) {
    idx <- sample(nrow(sed), subsample)
    sed <- sed[idx, idx]
  }
  
  d    <- sed[upper.tri(sed)]
  nNA  <- sum(is.na(d))
  if (nNA == length(d)) stop("todas as SEDs são NA")
  
  vdBLUP <- mean(d^2, na.rm = TRUE)
  
  data.frame(vc_used = vc_name, vg = vg, vdBLUP_avg = vdBLUP,
             n_NA_sed = nNA, H2_Cullis = 1 - vdBLUP / (2 * vg),
             erro = NA_character_, stringsAsFactors = FALSE)
}


