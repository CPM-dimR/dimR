#' @title DENUE Estadísticas
#' @description Genera estadisticas de las unidades economicas en un radio de 250 a 5000 metros
#'
#' @param latitud_vector Vector de coordenada latitud.
#' @param localidad_vector Vector de coordenada longitud.
#' @param metros Metros del radio.
#' @param keyword Palabra clave.
#' @param exportar TRUE para exportar la informacion a xlsx.
#'
#' @details Esta funcion es parte del proceso de estudio de mercado. Permite extraer
#' estadisticas de las unidades economicas a partir de un par de coordenadas, en un radio
#' minimo de 250 metros y maximo de 5000 metros.
#'
#' @examples
#' # Estadisticas de unidades economicas en el centro de Oaxaca y Monterrey
#' \dontrun{
#' latitud_vector <- c(17.061857, 25.677952)
#' longitud_vector <- c(-96.724723, -100.313294)
#' metros <- 1500
#' exportar <- FALSE
#' ue <- denue_localidad(latitud_vector, longitud_vector, metros, exportar)
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
denue_stats <- function(latitud_vector, longitud_vector,
                        metros = 250, keyword = "todos",
                        exportar = FALSE, token){

  # Cuantas coordenadas a revisar
    n <- length(latitud_vector)
    if(length(latitud_vector) == length(longitud_vector)){}else{stop("Latitud and Longitud vectors must be of same length")}

  # Antes de correr, revisar datos de columnas
    if(class(latitud_vector)=="numeric"){}else{stop(print("Latitud vector must be numeric"))}
    if(class(longitud_vector)=="numeric"){}else{stop(print("Longitud vector must be numeric"))}

  # Inicial data.frame
    d <- data.frame(
      businesses = 1:n,
      unique_streets = 1:n,
      unique_activities = 1:n,
      are_fixed = 1:n,
      have_legalname = 1:n,
      have_phone = 1:n,
      have_website = 1:n,
      in_avenue = 1:n,
      estimated_employees = 1:n,
      employee_stdev  = 1:n,
      lat = 1:n,
      lon = 1:n
      )

  # Loop
    for (i in 1:n)
      {
      # Crear matriz por negocio
        m <- denue_buffer(latitud  = latitud_vector[i],
                          longitud = longitud_vector[i],
                          metros   = metros,
                          keyword  = keyword,
                          exportar = exportar)

        if(length(m)==26){

        # Add approximate number of employees
          for (j in 1:length(m[,1]))
          {
            m$employees[j]<-if(grepl(x = m["per_ocu"][j,],pattern = "^11")){as.numeric(20)} else {
              if(grepl(x = m["per_ocu"][j,],pattern = "^0")){as.numeric(3)} else {
                if(grepl(x = m["per_ocu"][j,],pattern = "^6")){as.numeric(8)} else {
                  if(grepl(x = m["per_ocu"][j,],pattern = "^31")){as.numeric(40)} else {
                    if(grepl(x = m["per_ocu"][j,],pattern = "^101")){as.numeric(220)} else {
                      if(grepl(x = m["per_ocu"][j,],pattern = "^51")){as.numeric(75)}  else {
                        #mas de 250
                        as.numeric(300)
                      } } } } } } }

        # Cantidad de negocios
          d[i,]$businesses <- if(is.null(length(m[,1]))){0} else {length(m[,1])}
        # Cantidad de calles diferentes
          d[i,]$unique_streets <- if(is.null(length(unique(m[,"nom_vial"])))){0} else {length(unique(m[,"nom_vial"]))}
        # Cantidad de actividades diferentes
          d[i,]$unique_activities <- if(is.null(length(unique(m[,"nombre_act"])))){0} else {length(unique(m[,"nombre_act"]))}
        # Negocios con telefono
          d[i,]$have_phone <- if(is.null(length(m[!grepl(pattern = "^$",m["tel"][,1]),][,1]))){0} else {length(m[!grepl(pattern = "^$",m["tel"][,1]),][,1])}
        # Negocios con sitios de internet - no WWW,
          d[i,]$have_website <- if(is.null(length(m[!grepl(pattern = "^$",m["website"][,1]),][,1]))) {0} else {length(m[!grepl(pattern = "^$",m["website"][,1]),][,1])}
        # Negocios con razon social (la que sea)
          d[i,]$have_legalname <- if(is.null(length(m[!grepl(pattern = "^$",m["raz_social"][,1]),][,1]))){0} else {length(m[!grepl(pattern = "^$",m["raz_social"][,1]),][,1])}
        # Negocios ubicados en avenida
          d[i,]$in_avenue <- if(is.null(length(subset(m,m$tipo_vial == "AVENIDA")[,1]))){0} else {length(subset(m,m$tipo_vial == "AVENIDA")[,1])}
        # Negocios fijos
          d[i,]$are_fixed <- if(is.null(length(subset(m,m$tipo == "Fijo")[,1]))){0} else {length(subset(m,m$tipo == "Fijo")[,1])}
        # Estimacion de empleados en base a columna de estrato
          d[i,]$estimated_employees <- if(is.null(sum(as.numeric((m[,"employees"]))))){0} else {sum(as.numeric((m[,"employees"])))}
        # Desviacion standar de empleados en comercios
          d[i,]$employee_stdev <- if(is.null(stats::sd(x = as.numeric((m[,"employees"]))))) {0} else {stats::sd(x = as.numeric((m[,"employees"])))}
        # Latitud
          d[i,]$lat <- latitud_vector[i]
        # Longitud
          d[i,]$lon <- longitud_vector[i]

        }
        else{
        # Cantidad de negocios
          d[i,]$businesses <- NA
        # Cantidad de calles diferentes
          d[i,]$unique_streets <- NA
        # Cantidad de actividades diferentes
          d[i,]$unique_activities <- NA
        # Negocios con telefono
          d[i,]$have_phone <- NA
        # Negocios con sitios de internet - no WWW,
          d[i,]$have_website <- NA
        # Negocios con razon social (la que sea)
          d[i,]$have_legalname <- NA
        # Negocios ubicados en avenida
          d[i,]$in_avenue <- NA
        # Negocios fijos
          d[i,]$are_fixed <- NA
        # Estimacion de empleados en base a columna de estrato
          d[i,]$estimated_employees <- NA
        # Desviacion standar de empleados en comercios
          d[i,]$employee_stdev <- NA
        # Latitud
          d[i,]$lat <- latitud_vector[i]
        # Longitud
          d[i,]$lon <- longitud_vector[i]
        }

        }

  # Calculos sobre data frame
    d$businesses_per_street <- d$businesses/d$unique_streets
    d$businesses_per_meter <- d$businesses/metros

    d$employees_per_business <- d$estimated_employees/d$businesses
    d$employees_per_meter   <- d$estimated_employees/metros


  # Exportar
    return(d)

}
