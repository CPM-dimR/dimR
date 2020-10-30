#' @title Consultas DENUE Buffer
#' @description Función para realizar consultas en un radio de hasta 5000 metros.
#'
#' @param latitud latitud.
#' @param longitud longitud.
#' @param metros metros de radio. Minimo 250 - Maximo 5000
#' @param exportar TRUE para exportar la informacion a xlsx.
#' @param keyword Palabra clave. Ejemplo "banca-multiple"
#'
#' @details Esta funcion es parte del proceso de estudio de mercado. Permite
#' extraer todas las unidades economicas dentro de un radio de hasta 5000 metros.
#'
#' @examples
#' # Todos los cajeros automaticos en un radio de 2000 metros en Leon
#' \dontrun{
#' latitud <- 21.059960
#' longitud <- -101.574593
#' metros <- 2000
#' exportar <- FALSE
#' keyword <- "cajero-automatico"
#' atm <- denue_buffer(latitud, longitud, metros, exportar, keyword)
#' }
#'
#' @import stringr
#' @import dplyr
#' @import magrittr
#' @importFrom XML xmlToList
#' @importFrom XML xmlParse
#' @importFrom zoo as.yearmon
#' @importFrom zoo as.Date
#' @importFrom httr GET
#' @importFrom openxlsx write.xlsx
#' @importFrom lubridate year
#' @importFrom lubridate month
#' @importFrom lubridate today
#' @importFrom tidyr separate
#' @export
denue_buffer <- function(latitud, longitud,
                         metros = 250,
                         exportar = TRUE,
                         keyword = "todos",
                         token = "a3c33cba-ab0a-4439-8c01-a6cec4453f8f"){

   # Revisar que coordenadas esten en Mexico---
    .EstaEnMexico <- function(latitud, longitud){
      if(as.numeric(latitud) < 14.559507 | as.numeric(latitud) > 32.757120 |
         as.numeric(longitud) > -86.708301 | as.numeric(longitud) < -118.312155)
        {FALSE} else {TRUE}}

    if(.EstaEnMexico(latitud, longitud)){}else{stop("Las coordenadas no estan en México")}

    suppressWarnings({
  # Configurar consulta
    url <- "https://www.inegi.org.mx/app/api/denue/v1/consulta/Buscar/"
    coordenadas <- paste0(latitud, ",", longitud)
    consulta <- paste0(url, keyword, "/", coordenadas, "/", metros, "/", token)
    r = httr::GET(consulta)

  # Extraccion inicial
    invisible(
      utils::capture.output(
        s<-XML::xmlToList(XML::xmlParse(r, isHTML = T, encoding = "UTF-8"))
      ))
    l <- strsplit(x = as.character(s$body), split = "\",\"|}")
    l <- as.list(l[[1]])
    largo <- length(l)-1
    l <- l[1:largo]

  # limpia
    l_limpia <- gsub(pattern = "\"", replacement = "",l)
    l_limpia <- gsub(pattern = "{", replacement = "", l_limpia, perl=TRUE)

  # Revisar que sea divisible
    if(length(l_limpia)%%21==0){

    # dividir
      l_split <- split(x = l_limpia, f = 1:21)

    # Hacer en un data.frame
      LimpiarRapido <- function(pat, elemento)
      {
        exit <- substr(elemento, regexpr(pattern = pat, text = elemento)+1, stop = 4000)
        return(exit)
      }

      df <- data.frame(
        id = LimpiarRapido(":",l_split$'1'),
        nom_estab = LimpiarRapido(":",l_split$'2'),
        raz_social = LimpiarRapido(":",l_split$'3'),
        nombre_act = LimpiarRapido(":",l_split$'4'),
        per_ocu = LimpiarRapido(":",l_split$'5'),
        tipo_vial = LimpiarRapido(":",l_split$'6'),
        nom_vial = LimpiarRapido(":",l_split$'7'),
        num_ext = LimpiarRapido(":",l_split$'8'),
        num_int = LimpiarRapido(":",l_split$'9'),
        colonia = LimpiarRapido(":",l_split$'10'),
        cod_postal = LimpiarRapido(":",l_split$'11'),
        ubicación = LimpiarRapido(":",l_split$'12'),
        tel = LimpiarRapido(":",l_split$'13'),
        email = LimpiarRapido(":",l_split$'14'),
        website = LimpiarRapido(":",l_split$'15'),
        tipo = LimpiarRapido(":",l_split$'16'),
        longitud = LimpiarRapido(":",l_split$'17'),
        latitud = LimpiarRapido(":",l_split$'18'),
        CentroComercial = LimpiarRapido(":",l_split$'19'),
        TipoCComercial = LimpiarRapido(":",l_split$'20'),
        NumLocal = LimpiarRapido(":",l_split$'21'),
        stringsAsFactors = FALSE
        )

    # Manipulación de resultados
      resultados <- df %>%
        separate(ubicación, sep = ",", into = c("localidad", "municipio", "estado")) %>%
        mutate(nombre_act = if_else(str_detect(nom_estab, "CAJERO AUTOMÁTICO") == TRUE,
                                    "Cajero automático", nombre_act),
               categoría  = case_when(
                 nombre_act == "Exhibición de películas y otros materiales audiovisuales" ~ "Cines",
                 nombre_act == "Comercio al por menor en supermercados"                   ~ "Supermercados",
                 nombre_act == "Comercio al por menor en tiendas departamentales"         ~ "Tienda departamental",
                 TipoCComercial %in% c("CENTRAL DE ABASTO",
                                       "MERCADO PUBLICO")                                 ~ "Mercado",
                 TipoCComercial %in% c("PLAZUELA COMERCIAL",
                                       "PASAJE Y ANDADOR COMERCIAL",
                                       "OTRO TIPO DE CONJUNTO",
                                       "OTRO CONJUNTO COMERCIAL",
                                       "CENTRO O PLAZA COMERCIAL",
                                       "CENTRO Y PLAZA COMERCIAL")                        ~ "Plazas comerciales",
                 TRUE ~ " "),
               competencia = if_else(nombre_act %in% c("Banca múltiple", "Cajero automático",
                                                       "Casas de empeño", "Cajas de ahorro popular",
                                                       "Montepíos",
                                                       "Otras instituciones de intermediación crediticia y financiera no bursátil",
                                                       "Otras instituciones de ahorro y préstamo",
                                                       "Sociedades financieras de objeto limitado",
                                                       "Sociedades financieras de objeto múltiple",
                                                       "Uniones de crédito"), "Competencia", " "),
               unidades_econ = case_when(
                 per_ocu %in% c("0 a 5 personas", "6 a 10 personas")       ~ "Micro",
                 per_ocu %in% c("11 a 30 personas", "31 a 50 personas")    ~ "Pequeña",
                 per_ocu %in% c("51 a 100 personas", "101 a 250 personas") ~ "Mediana",
                 per_ocu == "251 y más personas"                           ~ "Grande",
                 TRUE ~ " "))

    # Exportando información en formato xlsx
      if(exportar == TRUE){
        excel <- resultados %>%
          write.xlsx(str_c(str_sub(year(today()), 3, 4),
                           str_pad(month(today()), width = 2, pad = 0),
                           "_Buffer DENUE ", resultados$id[1], "_",
                           str_to_title(resultados$localidad[1]), ".xlsx"))
        }

    } else {
    # No hay datos
      resultados <- data.frame()
      print(str_c("No hay datos. Consulta la url: '", consulta, "' para corroborar."))}

  # Fin del sumitwarmings
    })

  return(resultados)

}
