
#' Data.Frame Filters
#'
#' \code{filtrar_df_col} permite filtrar un dataframe basado en estadísticas descriptivas
#' (media, mediana, desviación típica) de una columna específica.
#'
#' @param df Un objeto de la clase \code{data.frame}
#' @param nombre_columna Nombre de la columna que se desea filtrar
#' @param filtro Tipo de filtro que queremos aplicar. Opciones: "mayor_media",
#' "menor_media", "mayor_mediana", "menor_mediana", "rango_normal", "atipicos" o "todos".
#' @param eliminar_NA Booleano que controla la eliminacion o no de NA (na.rm)
#' @param x Valor numérico multiplicador para el cálculo de atípicos (Media +/- x*SD)
#'
#' @return Un data.frame filtrado o una lista si el filtro es "todos".
#'
#' @examples
#' \dontrun{
#' mi.df <- data.frame(ID = 1:7, Valor = c(10, 12, NA, 15, 100, 9, NA))
#' solo_atipicos <- filtrar_df_col(mi.df, "Valor", filtro = "atipicos")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom stats median sd
#' @importFrom rlang .data
#' @export
filtrar_df_col <- function(df, nombre_columna, filtro = "todos", eliminar_NA = TRUE, x = 2) {
#Checks
  if (!inherits(df, "data.frame")) {
    stop("`df`: este archivo debe ser un data.frame")
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
      dplyr::filter(!is.na(.data[[nombre_columna]]))
  } else {
    df_NA <- df
  }

#Statistics
# prefix stats:: to avoid any conflict
  media        <- mean(df_NA[[nombre_columna]], na.rm = TRUE)
  mediana      <- stats::median(df_NA[[nombre_columna]], na.rm = TRUE)
  desv_tipica  <- stats::sd(df_NA[[nombre_columna]], na.rm = TRUE)

  #Filtering
  # prefix dplyr:: to avoid any conflict
  if (filtro == "mayor_media") {
    return(df_NA |> dplyr::filter(.data[[nombre_columna]] > media))

  } else if (filtro == "menor_media") {
    return(df_NA |> dplyr::filter(.data[[nombre_columna]] < media))

  } else if (filtro == "mayor_mediana") {
    return(df_NA |> dplyr::filter(.data[[nombre_columna]] > mediana))

  } else if (filtro == "menor_mediana") {
    return(df_NA |> dplyr::filter(.data[[nombre_columna]] < mediana))

  } else if (filtro == "rango_normal") {
    return(df_NA |> dplyr::filter(.data[[nombre_columna]] >= (media - desv_tipica) &
                                    .data[[nombre_columna]] <= (media + desv_tipica)))

  } else if (filtro == "atipicos") {
    return(df_NA |> dplyr::filter(.data[[nombre_columna]] < (media - (x * desv_tipica)) |
                                    .data[[nombre_columna]] > (media + (x * desv_tipica))))
}}
