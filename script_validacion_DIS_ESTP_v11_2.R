###############################################################################
## VALIDACIÓN PSICOMÉTRICA DE LA ESCALA DIS-ESTP — COHORTES 2024, 2025 Y 2026
## Script v11.2 · Manual técnico de uso e interpretación
##
## ALCANCE
##   Evidencia de validez de la escala (calidad de respuesta, descriptivos de
##   ítems, estructura interna, confiabilidad e invarianza) y construcción de
##   perfiles: latentes (LPA) y descriptivos (cortes anclados en la escala).
##   No incluye retención, beneficios ni modelos predictivos.
##
## SUPUESTO
##   Las tres cohortes respondieron la misma versión de 19 ítems. La versión
##   revisada 2027 (15 ítems) NO se valida aquí: requiere datos propios.
##
## DISEÑO (cohortes independientes; evita usar la misma muestra para
## descubrir y confirmar la estructura)
##   2024 = derivación   -> AFE
##   2025 = confirmación -> AFC
##   2026 = replicación  -> AFC + invarianza por subgrupos
##
## SALIDA
##   Un único libro Excel con una hoja por tabla, listo para el manual.
###############################################################################


# =============================================================================
# 0. CONFIGURACIÓN
# =============================================================================

install.packages("BifactorIndicesCalculator")

library(readxl)
library(dplyr)
library(tidyr)
library(psych)
library(GPArotation)   # psych lo requiere para la rotación oblimin
library(lavaan)
library(semTools)
library(openxlsx)
library(mclust)       # perfiles latentes (LPA)
library(BifactorIndicesCalculator)

# EDITAR: carpeta de trabajo y nombres de archivo
ruta_base <- "C:/Users/proyecto_2025_001/Desktop/Javiera Ortiz/FCU"
ruta_result <- "C:/Users/proyecto_2025_001/Desktop/Javiera Ortiz/RESULTADOS/RESULTADOS_V11"
archivo_2024 <- file.path(ruta_base, "FCU_2024_investigación.xlsx")
archivo_2025 <- file.path(ruta_base, "FCU_2025_investigación.xlsx")
archivo_2026 <- file.path(ruta_base, "FCU_2026_investigación.xlsx")
archivo_resultados <- file.path(ruta_result, "RESULTADOS_VALIDACION_DIS_ESTP_v11_2.xlsx")

semilla <- 20260923
set.seed(semilla)

# Ítems de la escala aplicada 2024-2026 y estructura del manual (14 ítems)
items_19 <- sprintf("DIS_ES_%02d", 1:19)
items_redaccion_negativa <- sprintf("DIS_ES_%02d", c(3, 5, 7, 10, 13, 17, 18))
items_compromiso <- sprintf("DIS_ES_%02d", c(4, 6, 9, 11, 12, 14, 15, 19))
items_vulnerabilidad <- sprintf("DIS_ES_%02d", c(3, 5, 10, 13, 17, 18))
items_14 <- c(items_compromiso, items_vulnerabilidad)

# Los ítems NO se invierten. Invertir un ítem no cambia el ajuste de ningún
# modelo, y mantener la dirección original deja la dimensión Vulnerabilidad
# en el sentido del manual (mayor puntaje = mayor dificultad percibida).
# Consecuencia: en los modelos con factor general (M1, M3) los ítems de
# redacción negativa tendrán cargas negativas. Es esperable, no es un error.

min_items_respondidos <- 15  # mínimo de ítems respondidos para evaluar straight-lining
min_casos_grupo <- 200       # tamaño mínimo por grupo en pruebas de invarianza
cohorte_subgrupos <- "2026"  # cohorte donde se prueba invarianza por subgrupos


# =============================================================================
# 1. CARGA DE BASES (una por cohorte, sólo variables necesarias)
# =============================================================================

# Para revisar los nombres reales sin cargar la base completa
# (ejecutar a mano, una vez, y luego editar los vectores de abajo):
names(read_excel(archivo_2024, n_max = 0))
names(read_excel(archivo_2025, sheet = 2, n_max = 0))
names(read_excel(archivo_2026, n_max = 0))


## 1.1 FCU 2024 (dos versiones complementarias) ------------------------------
# FCU_BDD_2024.xlsx:           base completa con escala DIS_ES.
# FCU_2024_investigación.xlsx: base depurada por SIES.

fcu <- openxlsx::read.xlsx(
  file.path(ruta_base, "FCU_BDD_2024.xlsx"),
  colNames = TRUE, na.strings = NA, check.names = TRUE, sep.names = ".")

fcu_sies <- openxlsx::read.xlsx(
  file.path(ruta_base, "FCU_2024_investigación.xlsx"),
  colNames = TRUE, na.strings = NA, check.names = TRUE, sep.names = ".")

##  a. Llave común 'run' 
fcu      <- dplyr::rename(fcu,      run = RUN)
fcu_sies <- dplyr::rename(fcu_sies, run = N_SIES)

## b. Forzar 'run' a character SOLO SI CORRESPONDE (evita errores de join) 
class(fcu$RUN)
class(fcu_sies$N_SIES)
#fcu$run      <- as.character(fcu$run)
#fcu_sies$run <- as.character(fcu_sies$run)

## c. Selección de aporte no redundante de FCU original 
# De FCU original tomamos sólo lo que FCU_SIES NO tiene:
#   - Crítico: los 19 ítems de la escala DIS_ES.
#   - Cualquier otra variable de FCU no incorporada al match.
vars_solo_fcu <- setdiff(names(fcu), names(fcu_sies))

## d. Verificación crítica: la escala DIS_ES debe estar en FCU original
items_escala_originales <- c(
  paste0("DIS_ES_", 1:19),         # nomenclatura sin ceros: DIS_ES_1..9
  sprintf("DIS_ES_%02d", 1:19)     # nomenclatura con ceros: DIS_ES_01..09
)
items_presentes <- intersect(items_escala_originales, vars_solo_fcu)

if (length(items_presentes) < 19) {
  warning("Se esperaban 19 ítems DIS_ES en FCU original; se detectaron ",
          length(items_presentes), ".")
} else {
  cat("✓ Los 19 ítems de la escala DIS_ES están en FCU original.\n")
}

cat("\n--- Aporte de FCU original (variables NO presentes en FCU_SIES) ---\n")
cat("Total:", length(vars_solo_fcu), "variables\n")
cat("De las cuales", length(items_presentes), "son ítems DIS_ES.\n")

fcu_aporte <- fcu %>%
  dplyr::select(run, dplyr::all_of(vars_solo_fcu)) %>%
  dplyr::distinct(run, .keep_all = TRUE)

## e. Ensamblar base 2024                           <--

fcu24 <- fcu_sies %>%
  dplyr::left_join(fcu_aporte, by = "run")



## e. EDITAR: a la izquierda, el nombre estándar (NO cambiar) ----------
# a la derecha, el nombre real de la columna en el Excel de esa cohorte.

names(fcu24)
names(read_excel(archivo_2025, sheet = 2, n_max = 0))
names(read_excel(archivo_2026, n_max = 0))


variables_2024 <- c(
  id           = "MM_N_DOCUMENTO",                    
  sexo         = "SEXO.COLUMNA.A.UTILIZAR",
  tipo_ies     = "MC_TIPO_INST_1",
  edad         = "EDAD.COLUMNA.A.UTILIZAR",
  modalidad_em = "MOD_EM"
)
variables_2025 <- c(
  id           = "MM_N_DOCUMENTO...19",
  sexo         = "SEXO",
  tipo_ies     = "MC_TIPO_INST_1",
  edad         = "Edad",
  modalidad_em = "MOD_EM"
)

variables_2026 <- c(
  id           = "mm_n_documento",
  sexo         = "sexo",
  tipo_ies     = "mc_tipo_inst_1",
  edad         = "Edad", 
  modalidad_em = "mod_em"
)

# f. Vars contexto (base 2024)
# NO CAMBIAR: sólo se usan sus nombres estándar (izquierda), que son
# iguales en los tres vectores, para el control de carga.

variables_contexto <- variables_2024

## g. Estandarizar items Escala 01-19

estandarizar_item <- function(nombre_columna) {
  sprintf("DIS_ES_%02d", as.integer(sub("DIS_ES_", "", nombre_columna, ignore.case = TRUE)))
}

## h. Carga Bases -----------------
# col_types = "text" evita que readxl adivine mal el tipo de una columna con
# muchos vacíos iniciales (y la convierta en NA). Se pasa a número en la sección 2.

fcu_2024 <- fcu24 |>
  select(any_of(variables_2024), matches("^DIS_ES_\\d+$")) |>
  rename_with(estandarizar_item, matches("^DIS_ES_\\d+$"))

fcu_2025 <- read_excel(archivo_2025, sheet = 2, col_types = "text") |>
  filter(MC_TIPO_INST_1 != "" & !is.na(MC_TIPO_INST_1)) |>
  select(any_of(variables_2025), matches("^DIS_ES_\\d+$")) |>
  rename_with(estandarizar_item, matches("^DIS_ES_\\d+$"))

fcu_2026 <- read_excel(archivo_2026, col_types = "text") |>
  select(any_of(variables_2026), matches("^DIS_ES_\\d+$")) |>
  rename_with(estandarizar_item, matches("^DIS_ES_\\d+$"))

#Esta línea se actualiza  más adelante:
bases_cohorte <- list("2024" = fcu_2024, "2025" = fcu_2025, "2026" = fcu_2026)

# Control de carga: filas, ítems y variables de contexto presentes
control_carga <- tibble(
  cohorte = names(bases_cohorte),
  filas = sapply(bases_cohorte, nrow),
  items_faltantes = sapply(bases_cohorte, \(base) paste(setdiff(items_19, names(base)), collapse = ", ")),
  contexto_faltante = sapply(bases_cohorte, \(base) paste(setdiff(names(variables_contexto), names(base)), collapse = ", "))
)
print(control_carga)

if (any(control_carga$items_faltantes != "")) {
  stop("Faltan ítems DIS-ESTP en alguna cohorte. Revise control_carga.")
}
if (!all(sapply(bases_cohorte, \(base) "id" %in% names(base)))) {
  stop("Falta la columna identificadora en alguna cohorte. Revise el vector de variables.")
}
if (any(control_carga$contexto_faltante != "")) {
  warning("Hay variables de contexto no encontradas (quedarán como NA). Revise control_carga.")
}


## i. Unión bases 2024-2025-2026 -------------

# A partir del error de rbind, identifiqué class de fcu2024$ edad y fcu2025$edad.
fcu_2024 <- fcu_2024 %>% mutate(edad = as.character(edad)) 

comparacion <- data.frame(columna = names(fcu_2024), 
                          tipo_2024 = sapply(fcu_2024,class), 
                          tipo_2025 = sapply(fcu_2025,class),
                          tipo_2026 = sapply(fcu_2026,class))
#Se identifica en la comparacion el tipo de columnas problematicas y se transforma todo 2024 a character.
fcu_2024 <- fcu_2024 %>% mutate(across(everything(),as.character))

bases_cohorte <- list("2024" = fcu_2024, "2025" = fcu_2025, "2026" = fcu_2026)

# Join bases: 
fcu <- bind_rows(bases_cohorte, .id = "cohorte")

# Variables de contexto ausentes en todas las cohortes quedan como NA
for (variable_contexto in names(variables_contexto)) {
  if (!variable_contexto %in% names(fcu)) fcu[[variable_contexto]] <- NA_character_
}

# =============================================================================
# 2. RECODIFICACIÓN (una sola vez, para las tres cohortes)
# =============================================================================

# Auditoría antes de convertir: respuestas no vacías fuera de 1-5 o no numéricas
valores_no_validos <- fcu |>
  pivot_longer(all_of(items_19), names_to = "item", values_to = "respuesta") |>
  filter(!is.na(respuesta), respuesta != "", !respuesta %in% as.character(1:5)) |>
  dplyr::count(cohorte, item, respuesta, name = "casos")

# IDs duplicados dentro de cada cohorte (se conserva el primer registro)
n_bruto_cohorte <- dplyr::count(fcu, cohorte, name = "n_bruto")

fcu <- fcu |>
  mutate(id = toupper(gsub("[[:space:].-]", "", id))) |>
  filter(is.na(id) | !duplicated(paste(cohorte, id)))

fcu <- fcu |>
  mutate(
    # Ítems: número entero 1-5; cualquier otro valor pasa a NA
    across(all_of(items_19), \(respuesta) suppressWarnings(as.numeric(respuesta))),
    across(all_of(items_19), \(respuesta) if_else(respuesta %in% 1:5, respuesta, NA_real_)),

    # VERIFICAR códigos con la hoja "03_Recodificacion" antes de interpretar
    grupo_sexo = case_when(
      toupper(sexo) %in% c("1", "H", "h") ~ "Hombre",
      toupper(sexo) %in% c("2", "M", "m") ~ "Mujer",
      toupper(sexo) %in% c("X", "")  ~ NA 
    ),
    grupo_tipo_ies = case_when(
      grepl("CFT|FORMACI", toupper(tipo_ies)) ~ "CFT",
      grepl("^IP$|INSTITUTO", toupper(tipo_ies)) ~ "IP"
    ),
    edad_numero = suppressWarnings(as.numeric(edad)),
    grupo_edad = case_when(
      edad_numero >= 15 & edad_numero < 30 ~ "Menor de 30",
      edad_numero >= 30 & edad_numero <= 80 ~ "30 o más"
    ),
    # MOD_EM: 1 = HC regular, 2 = HC EPJA, 3 = TP regular, 4 = TP EPJA
    modalidad_numero = suppressWarnings(as.numeric(modalidad_em)),
    grupo_rama_em = case_when(
      modalidad_numero %in% c(1, 2) ~ "HC",
      modalidad_numero %in% c(3, 4) ~ "TP"
    ),
    grupo_epja = case_when(
      modalidad_numero %in% c(1, 3) ~ "Regular",
      modalidad_numero %in% c(2, 4) ~ "EPJA"
    )
  )

# Control de recodificación: valor original frente a valor recodificado
pares_recodificacion <- c(sexo = "grupo_sexo", tipo_ies = "grupo_tipo_ies", modalidad_em = "grupo_rama_em", grupo_epja = "grupo_epja")

control_recodificacion <- bind_rows(lapply(names(pares_recodificacion), \(variable_original) {
  fcu |>
    dplyr::count(cohorte,
          valor_original = .data[[variable_original]],
          valor_recodificado = .data[[pares_recodificacion[[variable_original]]]],
          name = "casos") |>
    mutate(variable = variable_original, .before = 1)
}))

# Identificar id vacíos (sin RUN).

table(is.na(fcu$id))

## j. Base FCU analítica (sin NA en RUN-id) -----------------------

fcu <- fcu %>% filter(!is.na(fcu$id))

# =============================================================================
# 3. CALIDAD DE RESPUESTA Y MUESTRAS ANALÍTICAS
# =============================================================================

# Straight-lining = misma respuesta en todos los ítems (DE = 0). 

fcu <- fcu |>
  mutate(
    n_respondidos = rowSums(!is.na(pick(all_of(items_19)))),
    de_respuestas = apply(pick(all_of(items_19)), 1, sd, na.rm = TRUE),
    straight_lining = (n_respondidos >= min_items_respondidos & de_respuestas == 0) %in% TRUE,
    completo_19 = rowSums(is.na(pick(all_of(items_19)))) == 0,
    completo_14 = rowSums(is.na(pick(all_of(items_14)))) == 0
  )

# Datos faltantes: eliminación por lista (listwise). Con más de 95% de casos
# completos es defendible (Schafer, 1999); además FIML no es compatible con WLSMV.
fcu_afc <- fcu |> filter(!straight_lining, completo_14)

flujo_muestral <- fcu |>
  group_by(cohorte) |>
  summarise(
    n_sin_duplicados = n(),
    n_straight_lining = sum(straight_lining),
    incompletos_14_items = sum(!straight_lining & !completo_14),
    n_analitico_afc = sum(!straight_lining & completo_14),
    n_analitico_afe_19 = sum(!straight_lining & completo_19)
  ) |>
  left_join(n_bruto_cohorte, by = "cohorte") |>
  mutate(
    duplicados = n_bruto - n_sin_duplicados,
    pct_straight_lining = round(100 * n_straight_lining / n_sin_duplicados, 2),
    pct_incompletos = round(100 * incompletos_14_items / n_sin_duplicados, 2),
    pct_analitico = round(100 * n_analitico_afc / n_bruto, 2)
  ) |>
  relocate(n_bruto, duplicados, .after = cohorte)
print(flujo_muestral)

faltantes_items <- fcu |>
  filter(!straight_lining) |>
  group_by(cohorte) |>
  summarise(across(all_of(items_19), \(respuesta) round(100 * mean(is.na(respuesta)), 2))) |>
  pivot_longer(-cohorte, names_to = "item", values_to = "pct_faltante")


# =============================================================================
# 4. DESCRIPTIVOS DE ÍTEMS POR COHORTE
# =============================================================================

descriptivos_items <- fcu |>
  filter(!straight_lining) |>
  pivot_longer(all_of(items_19), names_to = "item", values_to = "respuesta") |>
  filter(!is.na(respuesta)) |>
  group_by(cohorte, item) |>
  summarise(
    n = n(),
    media = round(mean(respuesta), 3),
    de = round(sd(respuesta), 3),
    asimetria = round(psych::skew(respuesta), 3),   # semTools también tiene skew()
    curtosis = round(psych::kurtosi(respuesta), 3),
    pct_1 = round(100 * mean(respuesta == 1), 1),
    pct_2 = round(100 * mean(respuesta == 2), 1),
    pct_3 = round(100 * mean(respuesta == 3), 1),
    pct_4 = round(100 * mean(respuesta == 4), 1),
    pct_5 = round(100 * mean(respuesta == 5), 1),
    .groups = "drop"
  ) |>
  mutate(
    redaccion = if_else(item %in% items_redaccion_negativa, "negativa", "positiva"),
    dimension_manual = case_when(
      item %in% items_compromiso ~ "Compromiso",
      item %in% items_vulnerabilidad ~ "Vulnerabilidad",
      .default = "Descartado"
    ),
    .after = item
  )


# =============================================================================
# 5. ANÁLISIS FACTORIAL EXPLORATORIO (cohorte 2024, derivación)
# =============================================================================

# 5.1 AFE con los 19 ítems: documenta de dónde sale la estructura ----------
datos_afe_19 <- fcu |>
  filter(cohorte == "2024", !straight_lining, completo_19) |>
  select(all_of(items_19))

# AFE y AFC usan dos estimadores distintos pero de la misma familia:
#   - AFE (psych::fa): se factoriza la matriz POLICÓRICA con "minres"
#     (residuos mínimos = mínimos cuadrados no ponderados, ULS). No se usa
#     máxima verosimilitud (ML) porque ML supone variables continuas con
#     normalidad multivariada, supuesto que una matriz policórica no cumple
#     (Lloret-Segura et al., 2014).
#   - AFC (lavaan): WLSMV, que también se basa en correlaciones policóricas
#     (mínimos cuadrados ponderados en diagonal, con corrección robusta).
policorica_19 <- polychoric(datos_afe_19)$rho   # ítems ordinales -> correlación policórica
kmo_19 <- KMO(policorica_19)
bartlett_19 <- cortest.bartlett(policorica_19, n = nrow(datos_afe_19))

paralelo_19 <- fa.parallel(policorica_19, n.obs = nrow(datos_afe_19), fa = "fa",
                           fm = "minres", n.iter = 100, plot = FALSE)

# VSS calcula el MAP de Velicer probando hasta 10 factores; los avisos de
# convergencia de GPFoblq en soluciones grandes son esperables.
map_19 <- VSS(policorica_19, n = 10, rotate = "oblimin", fm = "minres",
              n.obs = nrow(datos_afe_19), plot = FALSE)
factores_paralelo <- paralelo_19$nfact
factores_map <- which.min(map_19$map)

adecuacion_afe <- tibble(
  cohorte = "2024",
  n_casos = nrow(datos_afe_19),
  KMO = round(kmo_19$MSA, 3),
  bartlett_chi2 = round(bartlett_19$chisq, 1),
  bartlett_gl = bartlett_19$df,
  bartlett_p = bartlett_19$p.value,
  factores_analisis_paralelo = factores_paralelo,
  factores_MAP = factores_map
)
print(adecuacion_afe)

# Soluciones a comparar: 1, 2 y las sugeridas por análisis paralelo y MAP
soluciones_afe <- sort(unique(c(1, 2, factores_paralelo, factores_map)))
afe_19 <- lapply(soluciones_afe, \(numero_factores) {
  fa(policorica_19, nfactors = numero_factores, n.obs = nrow(datos_afe_19), fm = "minres",
     rotate = if (numero_factores == 1) "none" else "oblimin")
})
names(afe_19) <- paste0(soluciones_afe, "_factores")

ajuste_afe_19 <- bind_rows(lapply(names(afe_19), \(nombre_solucion) {
  resultado_afe <- afe_19[[nombre_solucion]]
  tibble(
    solucion = nombre_solucion,
    RMSR = round(resultado_afe$rms, 3),
    RMSEA = round(unname(resultado_afe$RMSEA[1]), 3),
    TLI = round(resultado_afe$TLI, 3),
    BIC = round(resultado_afe$BIC, 1),
    varianza_acumulada = round(100 * sum(resultado_afe$Vaccounted["Proportion Var", ]), 1)
  )
}))

cargas_afe_19 <- bind_rows(lapply(names(afe_19), \(nombre_solucion) {
  resultado_afe <- afe_19[[nombre_solucion]]
  data.frame(solucion = nombre_solucion, item = rownames(resultado_afe$loadings),
             round(unclass(resultado_afe$loadings), 3),
             comunalidad = round(resultado_afe$communality, 3), row.names = NULL)
}))

# 5.2 AFE de 2 factores con los 14 ítems del manual -------------------------
# La policórica se recalcula con los casos completos en estos 14 ítems.
datos_afe_14 <- fcu |>
  filter(cohorte == "2024", !straight_lining, completo_14) |>
  select(all_of(items_14))
policorica_14 <- polychoric(datos_afe_14)$rho
afe_14 <- fa(policorica_14, nfactors = 2, n.obs = nrow(datos_afe_14), fm = "minres", rotate = "oblimin")

cargas_afe_14 <- data.frame(
  item = rownames(afe_14$loadings),
  round(unclass(afe_14$loadings), 3),
  comunalidad = round(afe_14$communality, 3),
  row.names = NULL
)
varianza_afe_14 <- data.frame(indicador = rownames(afe_14$Vaccounted),
                              round(afe_14$Vaccounted, 3), row.names = NULL)
correlacion_afe_14 <- data.frame(factor = colnames(afe_14$loadings),
                                 round(afe_14$Phi, 3), row.names = NULL)


# =============================================================================
# 6. ANÁLISIS FACTORIAL CONFIRMATORIO: MODELOS RIVALES POR COHORTE
# =============================================================================

# Tres explicaciones rivales para los 14 ítems:
#   M1: un factor general
#   M2: dos factores correlacionados (estructura del manual)
#   M3: un factor general + factor de método de redacción negativa (ortogonal)

# Si M3 ajusta igual que M2 y las cargas de método son relevantes, parte del
# segundo factor puede deberse al fraseo negativo (Marsh, 1996; DiStefano & Motl, 2006).

modelos_rivales <- list(
  M1_unidimensional = paste("GENERAL =~", paste(items_14, collapse = " + ")),
  M2_dos_factores = paste(
    paste("COMPROMISO =~", paste(items_compromiso, collapse = " + ")),
    paste("VULNERABILIDAD =~", paste(items_vulnerabilidad, collapse = " + ")),
    sep = "\n"
  ),
  M3_general_metodo = paste(
    paste("GENERAL =~", paste(items_14, collapse = " + ")),
    paste("METODO =~", paste(items_vulnerabilidad, collapse = " + ")),
    "GENERAL ~~ 0*METODO",
    sep = "\n"
  )
)

# WLSMV es el estimador principal para ítems ordinales; MLR sólo como sensibilidad.
ajustar_afc <- function(modelo, datos, estimador = "WLSMV") {
  tryCatch(
    if (estimador == "WLSMV") {
      cfa(modelo, data = datos, estimator = "WLSMV", ordered = items_14,
          std.lv = TRUE, parameterization = "theta")
    } else {
      cfa(modelo, data = datos, estimator = "MLR", std.lv = TRUE)
    },
    error = \(error) { message("AFC no estimado: ", conditionMessage(error)); NULL }
  )
}

# Índices escalados (WLSMV) o robustos (MLR)
indices_ajuste <- function(ajuste) {
  if (is.null(ajuste)) {
    return(tibble(n = NA_integer_, chi2 = NA_real_, gl = NA_real_, CFI = NA_real_, TLI = NA_real_,
                  RMSEA = NA_real_, RMSEA_ic_inf = NA_real_, RMSEA_ic_sup = NA_real_, SRMR = NA_real_))
  }
  sufijo <- if (lavInspect(ajuste, "options")$estimator == "ML") ".robust" else ".scaled"
  medidas <- fitMeasures(ajuste)
  tibble(
    n = lavInspect(ajuste, "ntotal"),
    chi2 = round(medidas[["chisq.scaled"]], 1),
    gl = medidas[["df.scaled"]],
    CFI = round(medidas[[paste0("cfi", sufijo)]], 3),
    TLI = round(medidas[[paste0("tli", sufijo)]], 3),
    RMSEA = round(medidas[[paste0("rmsea", sufijo)]], 3),
    RMSEA_ic_inf = round(medidas[[paste0("rmsea.ci.lower", sufijo)]], 3),
    RMSEA_ic_sup = round(medidas[[paste0("rmsea.ci.upper", sufijo)]], 3),
    SRMR = round(medidas[["srmr"]], 3)
  )
}

rol_cohorte <- c("2024" = "derivación", "2025" = "confirmación", "2026" = "replicación")

ajustes_afc <- list()
for (cohorte_actual in names(rol_cohorte)) {
  datos_cohorte <- filter(fcu_afc, cohorte == cohorte_actual)
  ajustes_afc[[cohorte_actual]] <- lapply(modelos_rivales, ajustar_afc, datos = datos_cohorte)
}

ajuste_afc <- bind_rows(lapply(names(ajustes_afc), \(cohorte_actual) {
  bind_rows(lapply(names(modelos_rivales), \(nombre_modelo) {
    indices_ajuste(ajustes_afc[[cohorte_actual]][[nombre_modelo]]) |>
      mutate(cohorte = cohorte_actual, rol = rol_cohorte[[cohorte_actual]],
             modelo = nombre_modelo, estimador = "WLSMV", .before = 1)
  }))
}))
print(ajuste_afc)


# 6.1 Cargas estandarizadas y correlación entre factores del modelo M2 -------
# standardizedSolution() entrega todos los parámetros en una tabla; la columna
# "op" distingue cargas (=~), covarianzas (~~) y umbrales (|).

solucion_m2 <- lapply(ajustes_afc, \(ajustes_cohorte) {
  if (is.null(ajustes_cohorte$M2_dos_factores)) return(NULL)
  standardizedSolution(ajustes_cohorte$M2_dos_factores)
})
solucion_m2 <- Filter(Negate(is.null), solucion_m2)   # descarta cohortes sin convergencia

cargas_afc_m2 <- bind_rows(solucion_m2, .id = "cohorte") |>
  filter(op == "=~") |>
  transmute(cohorte, factor = lhs, item = rhs,
            carga = round(est.std, 3), error_estandar = round(se, 3))

correlacion_afc_m2 <- bind_rows(solucion_m2, .id = "cohorte") |>
  filter(op == "~~", lhs == "COMPROMISO", rhs == "VULNERABILIDAD") |>
  transmute(cohorte, correlacion = round(est.std, 3),
            ic_inf = round(ci.lower, 3), ic_sup = round(ci.upper, 3))

# Ver cargas est:
View(solucion_m2[["2024"]])

# 6.2 Peso del factor de método (M3) ----------------------------------------
# Compara, en los ítems de redacción negativa, la carga en el factor general
# con la carga en el factor de método. ECV = proporción de la varianza común
# explicada por el factor general (Rodriguez, Reise & Haviland, 2016).

metodo_m3 <- bind_rows(lapply(names(ajustes_afc), \(cohorte_actual) {
  ajuste_m3 <- ajustes_afc[[cohorte_actual]]$M3_general_metodo
  if (is.null(ajuste_m3)) return(NULL)
  cargas_m3 <- standardizedSolution(ajuste_m3) |> filter(op == "=~")
  cargas_general <- filter(cargas_m3, lhs == "GENERAL")
  cargas_metodo <- filter(cargas_m3, lhs == "METODO")
  cargas_general |>
    filter(rhs %in% items_vulnerabilidad) |>
    select(item = rhs, carga_general = est.std) |>
    left_join(select(cargas_metodo, item = rhs, carga_metodo = est.std), by = "item") |>
    mutate(
      cohorte = cohorte_actual,
      ECV_general = sum(cargas_general$est.std^2) /
        (sum(cargas_general$est.std^2) + sum(cargas_metodo$est.std^2)),
      .before = 1
    ) |>
    mutate(across(where(is.numeric), \(valor) round(valor, 3)))
}))

View(metodo_m3)

# 6.3 Unidimensionalidad esencial: ECV, PUC, OMEGA y OMEGA-H -------------------

# Funcion para obtener indicadores de m3
# General: factor general con 14 items
# Metodo: factor ortogonal de redacción negativa.

calcular_indices_m3 <- function(ajuste, items_14, items_vulnerabiliad) {
  if(is.null(ajuste))
    return(NULL)
    #Solucion completamente estandarizada
  sol <- standardizedSolution(ajuste)
    #1. Cargas del factor General
  cargas_g <- sol |> 
    filter(op == "=~",
           lhs == "GENERAL",
           rhs %in% items_14) |>
    select(item = rhs, lambda_g = est.std)
  #2. Cargas del factor  de Metodo
  cargas_m <- sol |> 
    filter(op == "=~",
           lhs == "METODO",
           rhs %in% items_vulnerabilidad) |>
    select(item = rhs, lambda_m = est.std)
  
  cargas <- cargas_g |> 
    left_join(cargas_m, by = "item") |>
    mutate(lambda_m = replace_na(lambda_m, 0))
  #3. ECV general (prop de varianza comun atribuible al factor General)
  ECV <- sum(cargas$lambda_g^2) / (sum(cargas$lambda_g^2) + sum(cargas$lambda_m^2))
  #4. PUC: pares de items cuya covarianza no esta contaminada por el factor de método.
  # Con 14 items, total pares = choose(14,2)
  # Pares contaminados = pares entre los 6 items que comparten el factor de metodo.
  n_total <- length(items_14)
  n_metodo <- length(items_vulnerabilidad)
    PUC <- 1 - choose(n_metodo,2) / choose(n_total, 2)
  #5. Omega Total: aproximado desde solución estandarizada
    # Varianza atribuible a todos los factores comunes frente a varianza total del puntaje compuesto.
    var_general <- sum(cargas$lambda_g)^2
    var_metodo <- sum(cargas$lambda_m)^2
    # Varianzas residuales de los items
    resid <- sol |>
      filter(op== "~~", lhs == rhs, lhs %in% items_14) |>
      summarise(residual = sum(est.std)) |> pull(residual)
    
    omega_total <- (var_general + var_metodo) / (var_general + var_metodo + resid)
    # 6.Omega jerárquico (hierarchical) Omega-H
    # prop. de la varianza del puntaje total atribuible solo al factor General.
    omega_h <- var_general / (var_general + var_metodo + resid)
    # Resultado
    tibble(
      ECV_general = ECV,
      PUC = PUC,
      omega_total = omega_total,
      omega_H = omega_h
    )
}

indices_unidim <- bind_rows(
  lapply(names(ajustes_afc),
         function(cohorte_actual) {
           ajuste_m3 <- ajustes_afc[[cohorte_actual]]$M3_general_metodo
           
           resultado <- calcular_indices_m3(
             ajuste = ajuste_m3,
             items_14 = items_14,
             items_vulnerabiliad = items_vulnerabilidad
           )
           
           if(is.null(resultado))
             return(NULL)
           
           resultado |>
             mutate(cohorte = cohorte_actual,
                    .before = 1)
         })
) |> mutate (across(where(is.numeric), ~ round(.x, 3)))

print(indices_unidim)
View(indices_unidim)

# 6.5 Cálculo Indices paquete Bifactor -------------------------------------------------------------

# Probar con datos 2024
ajustes_afc[["2024"]]$M3_general_metodo

m3_2024 <- ajustes_afc[["2024"]]$M3_general_metodo

class(m3_2024)
lavInspect(m3_2024,"converged")  # debería obtener "lavaan" TRUE

indices_2024 <- BifactorIndicesCalculator::bifactorIndices(m3_2024)
indices_2024

# Si resulta con 2024, automatizar otros años:
indices_bifactor <- lapply(names(ajustes_afc), function(cohorte_actual) {
  ajuste_m3 <- ajustes_afc[[cohorte_actual]]$M3_general_metodo
  if (is.null(ajuste_m3)) {
    return(NULL)
  }
  if (!lavInspect(ajuste_m3, "converged")) {
    message("M3 no convergió en cohorte ", cohorte_actual)
    return(NULL)
  }
  resultado <- BifactorIndicesCalculator::bifactorIndices(ajuste_m3)
  list(cohorte = cohorte_actual, indices = resultado)
})

names(indices_bifactor) <- names(ajustes_afc)

# Revisar:
indices_bifactor[["2024"]]$indices
indices_bifactor[["2025"]]$indices
indices_bifactor[["2026"]]$indices



# 6.5 Sensibilidad -------------------------------------------------------------

# (a) Mismos modelos con estimador MLR (trata los ítems como continuos).
# (b) Modelo M2 con WLSMV incluyendo los casos de straight-lining.

sensibilidad_afc <- bind_rows(lapply(names(rol_cohorte), \(cohorte_actual) {
  datos_cohorte <- filter(fcu_afc, cohorte == cohorte_actual)
  datos_con_straight <- filter(fcu, cohorte == cohorte_actual, completo_14)
  bind_rows(
    bind_rows(lapply(names(modelos_rivales), \(nombre_modelo) {
      indices_ajuste(ajustar_afc(modelos_rivales[[nombre_modelo]], datos_cohorte, "MLR")) |>
        mutate(modelo = nombre_modelo, estimador = "MLR", muestra = "sin straight-lining", .before = 1)
    })),
    indices_ajuste(ajustar_afc(modelos_rivales$M2_dos_factores, datos_con_straight)) |>
      mutate(modelo = "M2_dos_factores", estimador = "WLSMV", muestra = "con straight-lining", .before = 1)
  ) |>
    mutate(cohorte = cohorte_actual, .before = 1)
}))


# =============================================================================
# 7. CONFIABILIDAD POR DIMENSIÓN Y COHORTE
# =============================================================================

# - Omega categórico (Green & Yang, 2009): compRelSEM() con ord.scale = TRUE,
#   calculado desde el modelo M2 de cada cohorte. Es el índice principal.
# - Alfa de Cronbach: referencia habitual para lectores no especialistas.
#   Supone cargas iguales (tau-equivalencia); por eso no es el índice principal
#   (McNeish, 2018).
# No se informa confiabilidad total: el manual no calcula un puntaje global y
# las dos dimensiones tienen dirección opuesta.

dimensiones <- list(COMPROMISO = items_compromiso, VULNERABILIDAD = items_vulnerabilidad)

# 7.1 Alfa por dimensión y cohorte --------------------------
# se calcula una vez y se reutiliza abajo.
# check.keys = FALSE: dentro de cada dimensión todos los ítems van en la misma dirección.
alfas <- lapply(names(ajustes_afc), \(cohorte_actual) {
  datos_cohorte <- filter(fcu_afc, cohorte == cohorte_actual)
  lapply(dimensiones, \(items_dimension) {
    psych::alpha(select(datos_cohorte, all_of(items_dimension)), check.keys = FALSE)
  })
})
names(alfas) <- names(ajustes_afc)

confiabilidad <- bind_rows(lapply(names(ajustes_afc), \(cohorte_actual) {
  ajuste_m2 <- ajustes_afc[[cohorte_actual]]$M2_dos_factores
  omega_m2 <- if (is.null(ajuste_m2)) {
    message("Cohorte ", cohorte_actual, ": M2 no convergió; omega queda como NA.")
    c(COMPROMISO = NA_real_, VULNERABILIDAD = NA_real_)
  } else {
    compRelSEM(ajuste_m2, ord.scale = TRUE)
  }
  tibble(
    cohorte = cohorte_actual,
    dimension = names(dimensiones),
    n_items = lengths(dimensiones),
    n_casos = sum(fcu_afc$cohorte == cohorte_actual),
    omega = round(unname(omega_m2[names(dimensiones)]), 3),
    alfa = round(sapply(alfas[[cohorte_actual]], \(resultado) resultado$total$raw_alpha), 3)
  )
}))
print(confiabilidad)

# 7.2 Correlación ítem-test y alfa si se elimina el ítem------------------------
# Aporte de cada ítem a su dimensión: Documenta el criterio "contribución a la confiabilidad"
# que el manual declara en la depuración de ítems.
confiabilidad_items <- bind_rows(lapply(names(alfas), \(cohorte_actual) {
  bind_rows(lapply(names(dimensiones), \(dimension) {
    resultado <- alfas[[cohorte_actual]][[dimension]]
    tibble(
      cohorte = cohorte_actual,
      dimension = dimension,
      item = rownames(resultado$item.stats),
      correlacion_item_resto = round(resultado$item.stats$r.drop, 3),
      alfa_si_se_elimina = round(resultado$alpha.drop$raw_alpha, 3),
      alfa_dimension = round(resultado$total$raw_alpha, 3)
    )
  }))
}))

# 7.3 Confiabilidad ordinal con semTools ---------------------------------------
rel_2024 <- compRelSEM(ajustes_afc[["2024"]]$M3_general_metodo, ord.scale = T)
rel_2025 <- compRelSEM(ajustes_afc[["2025"]]$M3_general_metodo, ord.scale = T)
rel_2026 <- compRelSEM(ajustes_afc[["2026"]]$M3_general_metodo, ord.scale = T)


rel_2024
rel_2025
rel_2026

confiabilidad_ordinal_m3 <- tibble(cohorte = c("2024","2025","2026"),
                                   GENERAL = c(
                                     as.numeric(rel_2024)[1],
                                     as.numeric(rel_2025)[1],
                                     as.numeric(rel_2026)[1]),
                                   METODO = c(
                                     as.numeric(rel_2024)[2],
                                     as.numeric(rel_2025)[2],
                                     as.numeric(rel_2026)[2])) |>
  dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x,3)
  ))

print(confiabilidad_ordinal_m3)


# =============================================================================
# 8. INVARIANZA DE MEDICIÓN (modelo M2, ítems ordinales)
# =============================================================================

# Secuencia para ítems ordinales (Wu & Estabrook, 2016): configural ->
# umbrales -> umbrales + cargas -> escalar. El paso de umbrales suele cambiar
# poco el ajuste; su función es fijar la escala antes de restringir cargas.

# Criterio heurístico (Chen, 2007): ΔCFI >= -0,010 y ΔRMSEA <= 0,015.
# Son guías derivadas de estimación ML; con WLSMV se interpretan con cautela.

evaluar_invarianza <- function(datos, variable_grupo) {
  datos <- datos |>
    filter(!is.na(.data[[variable_grupo]])) |>
    group_by(across(all_of(variable_grupo))) |>
    filter(n() >= min_casos_grupo) |>
    ungroup()
  if (n_distinct(datos[[variable_grupo]]) < 2) return(NULL)

  restricciones <- list(
    configural = "",
    umbrales = "thresholds",
    umbrales_cargas = c("thresholds", "loadings"),
    escalar = c("thresholds", "loadings", "intercepts")
  )
  bind_rows(lapply(names(restricciones), \(nivel) {
    ajuste <- tryCatch(
      measEq.syntax(configural.model = modelos_rivales$M2_dos_factores, data = datos,
                    ordered = items_14, estimator = "WLSMV", parameterization = "theta",
                    ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                    group = variable_grupo, group.equal = restricciones[[nivel]],
                    return.fit = TRUE),
      error = \(error) { message("Invarianza ", nivel, " no estimada: ", conditionMessage(error)); NULL }
    )
    indices_ajuste(ajuste) |> mutate(nivel = nivel, .before = 1)
  })) |>
    mutate(
      grupo = variable_grupo,
      categorias = paste(sort(unique(datos[[variable_grupo]])), collapse = " / "),
      delta_CFI = round(CFI - lag(CFI), 3),
      delta_RMSEA = round(RMSEA - lag(RMSEA), 3),
      cumple_criterio = delta_CFI >= -0.010 & delta_RMSEA <= 0.015,
      .before = 1
    )
}

# 8.1 Entre cohortes: ¿la escala mide lo mismo en 2024, 2025 y 2026? ---------
invarianza_cohortes <- evaluar_invarianza(fcu_afc, "cohorte")
print(invarianza_cohortes)

# 8.2 Entre subgrupos, en la cohorte de replicación --------------------------
grupos_invarianza <- c("grupo_sexo", "grupo_tipo_ies", "grupo_edad",
                       "grupo_rama_em", "grupo_epja")
datos_subgrupos <- filter(fcu_afc, cohorte == cohorte_subgrupos)
invarianza_subgrupos <- bind_rows(lapply(grupos_invarianza, \(variable_grupo) {
  evaluar_invarianza(datos_subgrupos, variable_grupo)
}))
print(invarianza_subgrupos)


# =============================================================================
# 9. PUNTAJES POR DIMENSIÓN
# =============================================================================

# Promedio simple de los ítems de cada dimensión, en la métrica original 1-5.
# Vulnerabilidad NO se invierte: mayor puntaje = mayor dificultad percibida.

puntajes <- fcu_afc |>
  mutate(
    puntaje_compromiso = rowMeans(pick(all_of(items_compromiso))),
    puntaje_vulnerabilidad = rowMeans(pick(all_of(items_vulnerabilidad)))
  ) |>
  select(cohorte, id, puntaje_compromiso, puntaje_vulnerabilidad)

descriptivos_puntajes <- puntajes |>
  pivot_longer(starts_with("puntaje_"), names_to = "dimension", values_to = "puntaje") |>
  group_by(cohorte, dimension) |>
  summarise(
    n = n(),
    media = round(mean(puntaje), 3),
    de = round(sd(puntaje), 3),
    minimo = min(puntaje),
    p25 = quantile(puntaje, 0.25),
    mediana = median(puntaje),
    p75 = quantile(puntaje, 0.75),
    maximo = max(puntaje),
    asimetria = round(psych::skew(puntaje), 3),
    .groups = "drop"
  )
print(descriptivos_puntajes)


# =============================================================================
# 10. PERFILES LATENTES (LPA)
# =============================================================================

# LPA = mezcla de distribuciones normales sobre los dos puntajes (mclust).
# Se estima por separado en cada cohorte: si la misma solución (número,
# medias y tamaños de perfiles) aparece en 2024, 2025 y 2026, los perfiles
# son una regularidad y no un accidente de una muestra.
#
# Modelos de varianza (nomenclatura mclust):
#   EEI = LPA clásico: varianzas iguales entre perfiles, sin covarianzas.
#   VVI = varianzas distintas por perfil, sin covarianzas (sensibilidad).
#
# Cautela: Compromiso es asimétrico (concentrado en valores altos). Una mezcla
# de normales puede crear perfiles extra sólo para reproducir esa asimetría
# (Bauer & Curran, 2003). Por eso la decisión combina ajuste, tamaño,
# nitidez, replicación entre cohortes e interpretación.

rango_perfiles <- 1:6  # Probar con 9
modelos_lpa <- c("EEI", "VVI")
hacer_blrt <- FALSE   # TRUE activa bootstrap LRT (lento; con N grande casi siempre p < 0,05)

## Criterios de ajuste para una solución estimada.
# BIC y aBIC en convención habitual: MENOR es mejor. (mclust reporta el BIC
# con signo invertido, donde MAYOR es mejor; aquí se reconvierte.)

resumen_lpa <- function(ajuste) {
  n_casos <- ajuste$n
  numero_perfiles <- ajuste$G
  menos2ll <- -2 * ajuste$loglik
  probabilidades <- ajuste$z
  asignacion <- ajuste$classification
  entropia <- if (numero_perfiles == 1) NA_real_ else
    1 - sum(-probabilidades * log(pmax(probabilidades, 1e-12))) / (n_casos * log(numero_perfiles))
  # Probabilidad posterior promedio del perfil asignado (ideal >= 0,80)
  app_por_perfil <- sapply(seq_len(numero_perfiles), \(perfil) mean(probabilidades[asignacion == perfil, perfil]))
  tibble(
    modelo = ajuste$modelName,
    perfiles = numero_perfiles,
    n = n_casos,
    parametros = ajuste$df,
    log_verosimilitud = round(ajuste$loglik, 1),
    BIC = round(menos2ll + ajuste$df * log(n_casos), 1),
    aBIC = round(menos2ll + ajuste$df * log((n_casos + 2) / 24), 1),
    entropia = round(entropia, 3),
    app_minima = round(min(app_por_perfil), 3),
    pct_perfil_menor = round(100 * min(table(asignacion)) / n_casos, 1),
    n_perfil_menor = min(table(asignacion))
  )
}

ajustes_lpa <- list()
for (cohorte_actual in names(rol_cohorte)) {
  matriz_puntajes <- puntajes |>
    filter(cohorte == cohorte_actual) |>
    select(puntaje_compromiso, puntaje_vulnerabilidad)
  # Con decenas de miles de casos, la inicialización jerárquica de mclust sobre
  # toda la base es inviable: se inicializa con una submuestra aleatoria.
  set.seed(semilla)
  submuestra_inicio <- sample(nrow(matriz_puntajes), min(nrow(matriz_puntajes), 3000))
  ajustes_lpa[[cohorte_actual]] <- list()
  for (modelo_varianza in modelos_lpa) {
    for (numero_perfiles in rango_perfiles) {
      nombre_ajuste <- paste0(modelo_varianza, "_", numero_perfiles)
      ajustes_lpa[[cohorte_actual]][[nombre_ajuste]] <- tryCatch(
        Mclust(matriz_puntajes, G = numero_perfiles, modelNames = modelo_varianza,
               initialization = list(subset = submuestra_inicio), verbose = FALSE),
        error = \(error) NULL
      )
    }
  }
}

# Con 1 perfil mclust rotula el modelo como "XXI": la columna "solucion"
# conserva el modelo solicitado (EEI o VVI).
ajuste_lpa <- bind_rows(lapply(names(ajustes_lpa), \(cohorte_actual) {
  soluciones_estimadas <- Filter(Negate(is.null), ajustes_lpa[[cohorte_actual]])
  bind_rows(lapply(names(soluciones_estimadas), \(nombre_ajuste) {
    resumen_lpa(soluciones_estimadas[[nombre_ajuste]]) |> mutate(solucion = nombre_ajuste, .before = 1)
  })) |>
    mutate(cohorte = cohorte_actual, .before = 1)
}))
print(ajuste_lpa)

# Bootstrap LRT opcional (k perfiles vs. k-1), por cohorte y modelo EEI

if (hacer_blrt) {
  blrt_lpa <- bind_rows(lapply(names(rol_cohorte), \(cohorte_actual) {
    matriz_puntajes <- puntajes |>
      filter(cohorte == cohorte_actual) |>
      select(puntaje_compromiso, puntaje_vulnerabilidad)
    prueba <- mclustBootstrapLRT(matriz_puntajes, modelName = "EEI", maxG = max(rango_perfiles) - 1,
                                 nboot = 100, verbose = FALSE)
    tibble(cohorte = cohorte_actual, contraste = paste(seq_along(prueba$obs), "vs", seq_along(prueba$obs) + 1),
           LRT = round(prueba$obs, 1), p_bootstrap = prueba$p.value)
  }))
} else {
  blrt_lpa <- tibble(nota = "BLRT no ejecutado (hacer_blrt = FALSE)")
}

# ---------------------------------------------------------------------------
# EDITAR TRAS REVISAR LA HOJA "22_LPA_ajuste". La elección NO se automatiza.
# Criterios: BIC/aBIC que dejan de bajar sustantivamente, perfil menor >= 5%,
# entropía y APP mínima >= 0,80 (sólo nitidez, no para elegir k; Nylund-Gibson
# & Choi, 2018), misma solución en las tres cohortes y perfiles interpretables.
# ---------------------------------------------------------------------------
numero_perfiles_lpa <- 3   #probar con 3,4,5,6
modelo_varianza_lpa <- "EEI"
solucion_elegida <- paste0(modelo_varianza_lpa, "_", numero_perfiles_lpa)

# Parámetros de la solución elegida en cada cohorte. Los perfiles se ordenan
# por Compromiso (de mayor a menor) para poder comparar cohortes: las
# etiquetas numéricas de mclust son arbitrarias.
# Publicar estos parámetros permite a cualquier institución calcular la
# probabilidad de pertenencia de un estudiante nuevo en una planilla:
#   P(perfil k | x) ∝ proporción_k × Normal(x_compromiso; media_k, var_k)
#                                  × Normal(x_vulnerabilidad; media_k, var_k)
parametros_lpa <- bind_rows(lapply(names(ajustes_lpa), \(cohorte_actual) {
  ajuste <- ajustes_lpa[[cohorte_actual]][[solucion_elegida]]
  if (is.null(ajuste)) return(NULL)
  varianzas <- apply(ajuste$parameters$variance$sigma, 3, diag)
  tibble(
    cohorte = cohorte_actual,
    perfil_mclust = seq_len(ajuste$G),
    proporcion = round(ajuste$parameters$pro, 4),
    media_compromiso = round(ajuste$parameters$mean["puntaje_compromiso", ], 3),
    media_vulnerabilidad = round(ajuste$parameters$mean["puntaje_vulnerabilidad", ], 3),
    varianza_compromiso = round(varianzas[1, ], 4),
    varianza_vulnerabilidad = round(varianzas[2, ], 4)
  ) |>
    arrange(desc(media_compromiso), media_vulnerabilidad) |>
    mutate(perfil = paste0("P", row_number()), .after = cohorte)
}))
print(parametros_lpa)

# Asignación de cada estudiante al perfil de mayor probabilidad (regla modal)
asignacion_lpa <- bind_rows(lapply(names(ajustes_lpa), \(cohorte_actual) {
  ajuste <- ajustes_lpa[[cohorte_actual]][[solucion_elegida]]
  equivalencia <- filter(parametros_lpa, cohorte == cohorte_actual)
  puntajes |>
    filter(cohorte == cohorte_actual) |>
    mutate(
      perfil_lpa = equivalencia$perfil[match(ajuste$classification, equivalencia$perfil_mclust)],
      probabilidad_asignada = round(apply(ajuste$z, 1, max), 3)
    )
}))

clasificacion_lpa <- asignacion_lpa |>
  group_by(cohorte, perfil_lpa) |>
  summarise(
    n = n(),
    media_compromiso = round(mean(puntaje_compromiso), 3),
    media_vulnerabilidad = round(mean(puntaje_vulnerabilidad), 3),
    probabilidad_media = round(mean(probabilidad_asignada), 3),
    pct_probabilidad_bajo_070 = round(100 * mean(probabilidad_asignada < 0.70), 1),
    .groups = "drop_last"
  ) |>
  mutate(pct = round(100 * n / sum(n), 1), .after = n) |>
  ungroup()
print(clasificacion_lpa)


# =============================================================================
# 11. PERFILES DESCRIPTIVOS (cortes anclados en la escala de respuesta)
# =============================================================================

# Alternativa sin modelamiento. Los cortes NO dependen de la media de la
# muestra (no son normativos): se anclan en el significado de las categorías.
#   < 3      : en promedio, bajo "Ni de acuerdo ni en desacuerdo" (predomina desacuerdo)
#   3 a < 4  : en promedio, entre neutral y "De acuerdo"
#   >= 4     : en promedio, "De acuerdo" o "Muy de acuerdo"

# Ventaja: el mismo puntaje recibe la misma lectura en cualquier cohorte o
# institución. Límite: la proporción en cada tramo puede ser muy desigual.

cortes_descriptivos <- c(-Inf, 3, 4, Inf)
etiquetas_tramo <- c("bajo", "intermedio", "alto")

# Error estándar de medida (EEM = DE × raíz(1 − omega)) por dimensión y
# cohorte: identifica puntajes tan cerca de un corte que el tramo asignado es
# incierto (Harvill, 1991).
eem_dimension <- descriptivos_puntajes |>
  mutate(dimension = toupper(sub("puntaje_", "", dimension))) |>
  left_join(select(confiabilidad, cohorte, dimension, omega), by = c("cohorte", "dimension")) |>
  transmute(cohorte, dimension, eem = de * sqrt(1 - omega))

perfiles_descriptivos <- asignacion_lpa |>
  left_join(filter(eem_dimension, dimension == "COMPROMISO") |> select(cohorte, eem_compromiso = eem), by = "cohorte") |>
  left_join(filter(eem_dimension, dimension == "VULNERABILIDAD") |> select(cohorte, eem_vulnerabilidad = eem), by = "cohorte") |>
  mutate(
    tramo_compromiso = cut(puntaje_compromiso, cortes_descriptivos, labels = etiquetas_tramo, right = FALSE),
    tramo_vulnerabilidad = cut(puntaje_vulnerabilidad, cortes_descriptivos, labels = etiquetas_tramo, right = FALSE),
    perfil_descriptivo = paste0("Compromiso ", tramo_compromiso, " · Dificultad percibida ", tramo_vulnerabilidad),
    cerca_corte_compromiso = pmin(abs(puntaje_compromiso - 3), abs(puntaje_compromiso - 4)) < eem_compromiso,
    cerca_corte_vulnerabilidad = pmin(abs(puntaje_vulnerabilidad - 3), abs(puntaje_vulnerabilidad - 4)) < eem_vulnerabilidad
  )

distribucion_descriptiva <- perfiles_descriptivos |>
  dplyr::count(cohorte, tramo_compromiso, tramo_vulnerabilidad, perfil_descriptivo, name = "n") |>
  group_by(cohorte) |>
  mutate(pct = round(100 * n / sum(n), 1)) |>
  ungroup()

incertidumbre_cortes <- perfiles_descriptivos |>
  group_by(cohorte) |>
  summarise(
    eem_compromiso = round(first(eem_compromiso), 3),
    eem_vulnerabilidad = round(first(eem_vulnerabilidad), 3),
    pct_cerca_corte_compromiso = round(100 * mean(cerca_corte_compromiso), 1),
    pct_cerca_corte_vulnerabilidad = round(100 * mean(cerca_corte_vulnerabilidad), 1),
    pct_perfil_incierto = round(100 * mean(cerca_corte_compromiso | cerca_corte_vulnerabilidad), 1)
  )
print(distribucion_descriptiva)
print(incertidumbre_cortes)

# Concordancia entre ambos métodos: ¿qué perfil descriptivo tienen los
# estudiantes asignados a cada perfil latente?
concordancia_perfiles <- perfiles_descriptivos |>
  dplyr::count(cohorte, perfil_lpa, perfil_descriptivo, name = "n") |>
  group_by(cohorte, perfil_lpa) |>
  mutate(pct_dentro_perfil_lpa = round(100 * n / sum(n), 1)) |>
  ungroup()

print(concordancia_perfiles, n=Inf)

# Base individual (datos personales: guardar sólo en el computador autorizado)
saveRDS(select(perfiles_descriptivos, -id), file.path(ruta_base, "perfiles_DIS_ESTP_2024_2026.rds"))


# =============================================================================
# 12. EXPORTACIÓN
# =============================================================================

paquetes_usados <- c("readxl", "dplyr", "tidyr", "psych", "GPArotation", "lavaan", "semTools", "openxlsx", "mclust")
sesion_r <- tibble(
  elemento = c("R", paquetes_usados, "semilla", "fecha_ejecucion"),
  valor = c(R.version.string,
            sapply(paquetes_usados, \(paquete) as.character(packageVersion(paquete))),
            semilla, format(Sys.time(), "%Y-%m-%d %H:%M"))
)

write.xlsx(
  list(
    "01_Control_carga" = control_carga,
    "02_Valores_no_validos" = valores_no_validos,
    "03_Recodificacion" = control_recodificacion,
    "04_Flujo_muestral" = flujo_muestral,
    "05_Faltantes_items" = faltantes_items,
    "06_Descriptivos_items" = descriptivos_items,
    "07_AFE_adecuacion" = adecuacion_afe,
    "08_AFE19_ajuste" = ajuste_afe_19,
    "09_AFE19_cargas" = cargas_afe_19,
    "10_AFE14_cargas" = cargas_afe_14,
    "11_AFE14_varianza" = varianza_afe_14,
    "12_AFE14_correlacion" = correlacion_afe_14,
    "13_AFC_ajuste" = ajuste_afc,
    "14_AFC_cargas_M2" = cargas_afc_m2,
    "15_AFC_correlacion_M2" = correlacion_afc_m2,
    "16_AFC_metodo_M3" = metodo_m3,
    "17_AFC_sensibilidad" = sensibilidad_afc,
    "18a_Confiabilidad" = confiabilidad,
    "18b_Confiabilidad_items" = confiabilidad_items,
    "18c_Confiabilidad_ordinal" = confiabilidad_ordinal_m3,
    "19_Invarianza_cohortes" = invarianza_cohortes,
    "20_Invarianza_subgrupos" = invarianza_subgrupos,
    "21_Puntajes_descriptivos" = descriptivos_puntajes,
    "22_LPA_ajuste" = ajuste_lpa,
    "23_LPA_BLRT" = blrt_lpa,
    "24_LPA_parametros" = parametros_lpa,
    "25_LPA_clasificacion" = clasificacion_lpa,
    "26_Perfiles_descriptivos" = distribucion_descriptiva,
    "27_Incertidumbre_cortes" = incertidumbre_cortes,
    "28_Concordancia_perfiles" = concordancia_perfiles,
    "29_Sesion_R" = sesion_r
  ),
  file = archivo_resultados,
  overwrite = TRUE
)
message("Resultados guardados en: ", archivo_resultados)


###############################################################################
## CÓMO LEER LOS RESULTADOS PARA EL MANUAL
##
## Estructura interna (hojas 13, 15 y 16)
##   - Si M2 ajusta claramente mejor que M1 en 2025 y 2026, y la correlación
##     entre factores es menor a ~0,85, hay respaldo para dos dimensiones.
##   - Si M3 ajusta igual que M2, mirar la hoja 16:
##       * ECV general alto (>= ~0,70) y cargas de método bajas: la escala es
##         esencialmente unidimensional y el fraseo es un artefacto menor.
##       * Cargas de método altas: la segunda dimensión descansa sólo en ítems
##         negativos; no se puede separar contenido de redacción. Declararlo.
##   - La decisión la toma quien investiga con estas tablas; el script no
##     la automatiza.
##
## Invarianza (hojas 19 y 20)
##   - Escalar: permite comparar medias entre cohortes o subgrupos.
##   - Sólo métrica (umbrales + cargas): comparar relaciones, no medias.
##   - No invariante: reportar resultados por separado, sin compararlos.
##
## Referencias
##   Bauer & Curran (2003) Psych Methods 8(3); Chen (2007) SEM 14(3);
##   DiStefano & Motl (2006) SEM 13(3); Harvill (1991) EM:IP 10(2);
##   Green & Yang (2009) Psychometrika 74(1); Lloret-Segura et al. (2014)
##   Anales de Psicología 30(3); Marsh (1996) JPSP 70(4);
##   Nylund-Gibson & Choi (2018) Transl Issues Psychol Sci 4(4);
##   Rodriguez, Reise & Haviland (2016) Psych Methods 21(2);
##   Schafer (1999) Stat Methods Med Res 8(1); Wu & Estabrook (2016) Psychometrika 81(4).
###############################################################################
