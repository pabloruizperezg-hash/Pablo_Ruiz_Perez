#'Data.Frame Filters
#'
#'
#'\code{filtrar_df_col} permite filtrar un dataframe basado en estadísticas descriptivas
#' (media, mediana, desviación típica) de una columna específica
#'
#'@param df Un objeto de la clase \code{data.frame}
#'@param nombre_columna Nombre de la columna que se desea filtrar
#'@param filtro Tipo de filtro que queremos aplicar. Existiendo las siguientes posibilidades: mayor_media, menor_media, mayor_mediana, menor_mediana, rango_normal y atipicos
#'@param eliminar_NA Boleano que controla la eliminacion o no de NA (na.rm)
#'@param x Valor numérico multiplicador para el cálculo de atípicos (Media +/- x*SD)
#'
#'@return Un data.frame filtrado según el criterio solicitado
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' mi.df <- data.frame(ID = 1:7, Valor = c(10, 12, NA, 15, 100, 9, NA))
#' solo_atipicos <- filtrar_df_col(mi.df, "Valor", filtro = "atipicos")
#'}
#'@import dplyr filter
#'@importFrom stats median sd
#'@importFrom rlang .data
#'@export


filtrar_df_col <- function(df, nombre_columna, filtro = "todos", eliminar_NA = TRUE, x = 2) {
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




