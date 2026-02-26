

library(dplyr)

filtrar_df_col <- function(df, nombre_columna, eliminar_NA = TRUE, filtro = c("atipicos", "mayor_mediana"), x = 2) {

#Checks
  if (!inherits(df, "data.frame")) {
    stop("`df`:  este archivo debe ser un data.frame")
  }

  if (!nombre_columna %in% names(df)) {
    stop("`nombre_columna`: esta columna no existe en `df`")
  }

  if (!is.numeric(df[[nombre_columna]])) {
    stop("La columna elegida debe ser numérica")
  }

#NA Filter
  if(eliminar_NA) {
    df_NA <- df |>
    filter(!is.na(.data[[nombre_columna]]))
  } else {
    df_NA <- df
  }

#existen otros tipos de NA, posibilidad de añadir clasificacion de los NA


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



mi.df <- data.frame(ID = 1:20, Valor = c(10, 12, NA, 15, 100, 9, NA, 50, 1, 34, NA, 56, 78, 87, NA, 78, 787, 8, 78, 1000 ))

prueba <- filtrar_df_col(mi.df, "Valor", eliminar_NA = FALSE,filtro = "todos" , x = 1.5)



