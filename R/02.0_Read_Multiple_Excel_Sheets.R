#' @title Leer Multiples Hojas de Excel.
#' @description Leer todas las hojas de varios documentos de excel.
#'
#' @param path Ruta donde se encuentran los archivos.
#'
#' @details Esta funcion permite leer todas las hojas de varios documentos de excel.
#'
#' @examples
#' # Leer todas las hojas de los archivos en una carpeta
#' \dontrun{
#' data_df <- dir_ls(regexp = "xlsx") %>%
#'  map_df(read_multiple_excel,
#'         .id = "city")
#' }
#'
#' @importFrom readr read_csv
#' @export
read_multiple_excel <- function(path) {
  path %>%
    excel_sheets() %>%
    set_names() %>%
    map_df(read_excel, path = path)
}
