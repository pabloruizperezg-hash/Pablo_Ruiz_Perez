

library(dplyr)

datos_NA

df_NA = df |>
  filter(!is.na(.data[[nombre_columna]]))



media <- mean(df_NA[[nombre_columna]])
mediana <- median(df_NA[[nombre_columna]])
desv_tipica <- sd(df_NA[[nombre_columna]])


df_mayor_media <- df |>
  filter(.data[[nombre_columna]] > media)

df_mayor_mediana <- df |>
  filter(.data[[nombre_columna]] > mediana)

df_menor_media <- df |>
  filter(.data[[nombre_columna]] <media)

df_menor_mediana <- df |>
  filter(.data[[nombre_columna]] <mediana)

df_rango_normal <- df |>
  filter(.data[[nombre_columna]] >= (media-desv_tipica) &
         .data[[nombre_columna]] <= (media+desv_tipica))


df_atipicos <- df |>
  filter(.data[[nombre_columna]] > (media - 2 * desv_tipica) |>
         .data[[nombre_columna]] < (media + 2 * desv_tipica))

return(list(
  mayor_media = df_mayor_media,
  mayor_mediana = df_mayor_mediana,
  menor_media = df_menor_media,
  menor_mediana = df_menor_mediana,
  rengo_normal = df_rango_normal,
  atipicos = df_atipicos
))

















