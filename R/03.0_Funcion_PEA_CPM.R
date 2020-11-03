#' @title PEA CPM
#' @description La funcion permite calcular la PEA de una localidad.
#'
#' @param path_ageb_urbana Ruta donde se encuentra SHP de AGEB Urbana.
#' @param clave_geo Clave Geoestadística de la localidad.
#' @param path_caract_economicas Ruta donde se encuentra tabla DBF de caracteristicas economicas
#' @param pea_me_1smm PEA menor a 1 SMM.
#' @param r Tasa de crecimiento PEA.
#' @param t Año
#'
#' @details Esta funcion es parte del proceso de estudio de mercado. Permite calcular la
#' PEA CPM y segmentarla para su posterior analisis.
#'
#' @examples
#' # PEA CPM de la localidad Los Mochis, Sinaloa.
#' \dontrun{
#'
#' path_ageb_urbana <- "MAPAS SINCE-REPUBLICA/SINALOA/25/ageb_urb.shp"
#' path_caract_economicas <- "MAPAS SINCE-REPUBLICA/SINALOA/25/tablas/ageb_urb_caracteristicas_economicas.dbf"
#' clave_geo <- "250010001"
#' pea_me_1smm <- 0.0615    # Equivalente a 6.15%
#' r <- 0.9                 # Tasa de Crecimiento PEA
#' t <- 9                   # Ultimo digito del año (2019)
#'
#'
#' PEA_CPM_MOCHIS <- PEA_CPM(path_ageb_urbana = path_ageb_urbana,
#'                           path_caract_economicas = path_caract_economicas,
#'                           clave_geo = clave_geo,
#'                           pea_me_1smm = pea_me_1smm,
#'                           r = r,
#'                           t = t)
#' }
#'
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @import purrr
#' @importFrom sf read_sf
#' @importFrom foreign read.dbf
#' @importFrom readr write_excel_csv
#' @importFrom sf st_transform
#' @importFrom htmltools HTML
#' @export
PEA_CPM <- function(path_ageb_urbana,
                    clave_geo,
                    path_caract_economicas,
                    pea_me_1smm,
                    r,
                    t,
                    exportar = FALSE){

# Importando AGEB Urbana y Caracteristicas Economicas
  ageb_urbana <- read_sf(path_ageb_urbana) %>%
    dplyr::filter(str_detect(CVEGEO, str_c("^", clave_geo))) %>%
    dplyr::select(CVEGEO, POB1, POB19, POB21, POB23) %>%
    dplyr::left_join(.,
              read.dbf(file = path_caract_economicas) %>%
                select(CVEGEO, ECO1, ECO25, ECO28)) %>%
    mutate(across(where(is.numeric), ~ifelse(. %in% c(-6,-9), NA, .)))

# Calculando PEA CPM
  PEA_CPM <- ageb_urbana %>%
    mutate(CVEGEO           = str_pad(CVEGEO, width = 13, pad = 0),
           POB_12A18        = POB19 - POB23,
           PEA_REAL         = POB21 - ECO25,
           PEA_ME_1SMM      = pea_me_1smm,
           POB_18A60        = POB21 - POB23,
           FACTOR_PEA_1SMM  = ECO1 * PEA_ME_1SMM,
           POB_POTENCIAL    = PEA_REAL - FACTOR_PEA_1SMM - POB23,
           POB_POTENCIAL_TC = round(((r/100 + 1)^t) * POB_POTENCIAL),
           PEA_CPM          = round(POB_POTENCIAL_TC *(ECO1 / (ECO28 + ECO1))),
           CATEGORIA        = cut(PEA_CPM,
                                  breaks = c(0,
                                             quantile(PEA_CPM, 0.50, na.rm = T),
                                             quantile(PEA_CPM, 0.75, na.rm = T),
                                             Inf),
                                  labels = c("BAJO", "MEDIO", "ALTO")))

  if(exportar == TRUE){

    write_excel_csv(PEA_CPM, "PEA CPM.xlsx")

  }

# Transformando y etiquetando
  PEA_CPM <- PEA_CPM %>%
    dplyr::select(CVEGEO, PEA_CPM, CATEGORIA) %>%
    st_transform(., 4326) %>%
    mutate(etiqueta = str_c("Categoría: ","<strong>", CATEGORIA, "</strong>",
                          "<br/>","PEA CPM: ", PEA_CPM) %>%
             map(htmltools::HTML))

  return(PEA_CPM)

}
