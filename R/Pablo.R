#'Data.Frame Filters
#'
#'
#'\code {filtrar_df_col} filtrado por media, mediana y desviacion tipica de columnas de un df. Obteniendo listas con los diferentes df solicitados.
#'
#'@param df Un objeto de la clase data.frame
#'@param nombre_columna nombre de la columna que se desea filtrar
#'@param eliminar_NA Parametro que controla la eliminacion o no de NA, existe la posibilidad de modificar si son NULL, -9999.99, etc.
#'@param filtro tipo de filtro que queremos aplicar. Existiendo las siguientes posibilidades: mayor_media, menor_media, mayor_mediana, menor_mediana, rango_normal y atipicos
#'@param x establecimiento del valor numerico por el que se multiplica a las deviacion tipica para el calculo de valores atipicos
#'@return lista con los df solicitados en filtro
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' mi.df <- data.frame(ID = 1:7, Valor = c(10, 12, NA, 15, 100, 9, NA))
#' solo_atipicos <- filtrar_df_col(mi.df, "Valor", filtro = "atipicos")
#'}
#'@import dplyr
#'@importFrom stats median
#'@importFrom stats sd
#'
filtrar_df_col <- function(df, nombre_columna, eliminar_NA = TRUE, filtro = "todos", x = 2) {
#Checks
  if (!inherits(df, "data.frame")) {
    stop("`df`:  este archivo debe ser un data.frame")
  }
  if (!nombre_columna %in% names(df)) {
    stop("`nombre_columna`: esta columna no existe en `df`")
  }
  if (!is.numeric(df[[nombre_columna]])) {
    stop("La columna elegida debe ser numerica")
  }
#NA Filter
  if(eliminar_NA) {
    df_NA <- df |>
    filter(!is.na(.data[[nombre_columna]]))
  } else {
    df_NA <- df
  }
#Statistics
  media <- mean(df_NA[[nombre_columna]],na.rm = TRUE)
  mediana <- median(df_NA[[nombre_columna]],na.rm = TRUE)
  desv_tipica <- sd(df_NA[[nombre_columna]],na.rm = TRUE)
#List creation
  if (filtro == "mayor_media") {
    return(df_NA |> filter(.data[[nombre_columna]] > media))
  } else if (filtro == "menor_media") {
    return(df_NA |> filter(.data[[nombre_columna]] < media))
  } else if (filtro == "mayor_mediana") {
    return(df_NA |> filter(.data[[nombre_columna]] > mediana))
  } else if (filtro == "menor_mediana") {
    return(df_NA |> filter(.data[[nombre_columna]] < mediana))
  } else if (filtro == "rango_normal") {
    return(df_NA |> filter(.data[[nombre_columna]] >= (media - desv_tipica) &
                             .data[[nombre_columna]] <= (media + desv_tipica)))
  } else if (filtro == "atipicos") {
    return(df_NA |> filter(.data[[nombre_columna]] < (media - (x * desv_tipica)) |
                             .data[[nombre_columna]] > (media + (x * desv_tipica))))
  } else {
    return(list(
      sin_nulos = df_NA,
      mayor_media = df_NA |> filter(.data[[nombre_columna]] > media),
      menor_media = df_NA |> filter(.data[[nombre_columna]] < media),
      mayor_mediana = df_NA |> filter(.data[[nombre_columna]] > mediana),
      menor_mediana = df_NA |> filter(.data[[nombre_columna]] < mediana),
      rango_normal = df_NA |> filter(.data[[nombre_columna]] >= (media - desv_tipica) & .data[[nombre_columna]] <= (media + desv_tipica)),
      atipicos = df_NA |> filter(.data[[nombre_columna]] < (media - (x * desv_tipica)) | .data[[nombre_columna]] > (media + (x * desv_tipica)))
    ))
  }
}




