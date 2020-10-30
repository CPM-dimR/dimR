#' @title CSVs FROM ZIP
#' @description Descargar web zip desde R.
#'
#' @param url URL de donde se descargará la información.
#'
#' @details Esta funcion permite extraer web zips desde R.
#'
#' @examples
#' # Descargar datos abiertos de COVID México
#' \dontrun{
#' url <- "http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip"
#' datos.covid <- CSVs_From_ZIP(url)
#' }
#'
#' @importFrom readr read_csv
#' @export
CSVs_From_ZIP <- function(url){
  # crea un archivo temporal
    temp <- tempfile()

  # descarga el .zip en ese archivo temporal
    download.file(url,temp)

  # identifica el contenido del archivo
    file_list <- unzip(temp, list = TRUE)

  # extrae documento por documento
    lapply(file_list$Name,function(x) {
      suppressMessages(
        readr::read_csv(unz(temp,x))) -> hh
      return(hh)
    })->lista

  # se desconecta del archivo temporal
    unlink(temp)

  # devuelve la lista con los datasets
    return(lista)
}
