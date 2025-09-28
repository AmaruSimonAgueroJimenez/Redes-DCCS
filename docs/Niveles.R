# Muestra niveles (valores únicos) de columnas character y factor
# ---------------------------------------------------------------
# df            : data.frame / tibble
# max_levels    : máximo de niveles a imprimir por variable (por defecto 50)
# show_counts   : si TRUE, imprime también n y % por nivel
# include_na    : si TRUE, incluye NA como un nivel más
# sort_levels   : si TRUE, ordena por nivel (o por frecuencia si show_counts=TRUE)
#
# Valor de retorno (invisible): lista nombrada con los niveles (o tabla de frecuencias)

show_char_levels <- function(df,
                             max_levels  = 50L,
                             show_counts = FALSE,
                             include_na  = FALSE,
                             sort_levels = TRUE) {
  if (!is.data.frame(df)) stop("df debe ser un data.frame/tibble.")
  
  is_char_or_fac <- vapply(df, function(x) is.character(x) || is.factor(x), logical(1))
  cols <- names(df)[is_char_or_fac]
  
  if (length(cols) == 0L) {
    message("No hay variables de tipo character o factor en el data.frame.")
    return(invisible(NULL))
  }
  
  out <- vector("list", length(cols))
  names(out) <- cols
  
  for (nm in cols) {
    x <- df[[nm]]
    den <- if (include_na) length(x) else sum(!is.na(x))
    is_fac <- is.factor(x)
    
    cat("\n", strrep("-", 60), "\n", sep = "")
    if (show_counts) {
      # Para factores, table() incluye niveles no usados con conteo 0
      tab <- table(x, useNA = if (include_na) "ifany" else "no")
      if (sort_levels) tab <- sort(tab, decreasing = TRUE)
      n_levels <- length(tab)
      cat("$", nm, " — ", length(x), " obs., ", n_levels, " niveles\n", sep = "")
      
      n_show <- min(n_levels, max_levels)
      df_tab <- data.frame(
        nivel = names(tab)[seq_len(n_show)],
        n     = as.integer(tab[seq_len(n_show)]),
        pct   = round(100 * as.integer(tab[seq_len(n_show)]) / den, 1),
        stringsAsFactors = FALSE
      )
      print(df_tab, row.names = FALSE)
      if (n_levels > n_show) {
        cat("… ", n_levels - n_show, " niveles no mostrados (ajusta max_levels para ver más)\n", sep = "")
      }
      out[[nm]] <- tab
    } else {
      if (is_fac) {
        # Para factores mostramos TODOS los niveles declarados (aunque no aparezcan)
        lev <- levels(x)
        # Añade NA como "nivel" si se pide y hay NA presentes
        if (include_na && any(is.na(x))) lev <- c(lev, NA)
        if (sort_levels) lev <- sort(lev, na.last = TRUE)
      } else {
        # Para character, mostramos valores únicos observados
        vec <- if (include_na) x else x[!is.na(x)]
        lev <- unique(vec)
        if (sort_levels) lev <- sort(lev, na.last = TRUE)
      }
      n_levels <- length(lev)
      cat("$", nm, " — ", length(x), " obs., ", n_levels, " niveles\n", sep = "")
      
      n_show <- min(n_levels, max_levels)
      to_show <- lev[seq_len(n_show)]
      cat(paste0("  - ", to_show), sep = "\n")
      if (n_levels > n_show) {
        cat("… ", n_levels - n_show, " niveles no mostrados (ajusta max_levels para ver más)\n", sep = "")
      }
      out[[nm]] <- lev
    }
  }
  
  invisible(out)
}

targets <- c("sustancia_principal_2","sustancia_principal_3")

datos_limpios <- datos_limpios %>%
  mutate(across(all_of(targets), \(x) {
    x_chr <- as.character(x)
    to_na <- grepl("^(?i)\\s*sin\\s+consumo\\s*$", x_chr, perl = TRUE) |
             grepl("^(?i)\\s*sin\\s+sustancia\\s+principal(\\s*\\)?\\s*)?$", x_chr, perl = TRUE)
    x_chr[to_na] <- NA_character_
    factor(x_chr)   # mantiene como factor y elimina niveles marcados
  }))

