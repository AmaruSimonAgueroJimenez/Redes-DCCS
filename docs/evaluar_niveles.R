# Vector de variables
vars <- c(
  "diagnostico_trs_psiquiatrico_dsm_iv",
  "diagnostico_trs_psiquiatrico_sub_dsm_iv",
  "x2_diagnostico_trs_psiquiatrico_dsm_iv",
  "x3_diagnostico_trs_psiquiatrico_dsm_iv",
  "diagnostico_trs_psiquiatrico_cie_10",
  "diagnostico_trs_psiquiatrico_sub_cie_10",
  "x2_diagnostico_trs_psiquiatrico_cie_10",
  "x3_diagnostico_trs_psiquiatrico_cie_10",
  "sustancia_principal",
  "otras_sustancias_no1",
  "otras_sustancias_no2",
  "otras_sustancias_no3"
)

# 1) Lista con niveles/valores únicos por variable (incluye indicador de NA si existen)
get_levels_list <- function(df, vars) {
  faltantes <- setdiff(vars, names(df))
  if (length(faltantes) > 0) {
    warning("Estas variables no están en el data frame: ", paste(faltantes, collapse = ", "))
    vars <- intersect(vars, names(df))
  }
  out <- lapply(vars, function(v) {
    x <- df[[v]]
    # Si es character, no forzamos a factor para no alterar el df original
    levs <- if (is.factor(x)) levels(x) else sort(unique(x))
    # Agrega indicador si hay NA
    if (any(is.na(x))) levs <- c(levs, "(NA)")
    levs
  })
  names(out) <- vars
  out
}

levels_list <- get_levels_list(data, vars)

# Mostrar en consola, variable por variable
for (v in names(levels_list)) {
  cat("\n$", v, ":\n", sep = "")
  print(levels_list[[v]])
}
