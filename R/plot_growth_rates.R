
###############################################################################
#' Plota
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param dados banco de dados de resultados
#' @return Lista com informações sobre erros
#' @examples
#' data(brasil2000)
#' @export
#'

plot_growth_rates <- function(
  censos,
  census_date,
  pais_name = NULL,
  save_plot = T,
  path = getwd()
){

  #cria path
  if (!dir.exists(path)) dir.create(path)

  census_year <- floor(census_date)  # Años de los censos
  # Agrupa y suma datos por departamento
  datos <- censos %>%
    group_by(DAM) %>%  # Agrupa por departamento
    mutate(DAM_census1 = sum(DAMe_census1),  # Suma población de 2002 por departamento
           DAM_census2 = sum(DAMe_census2),  # Suma población de 2012 por departamento
           DAM_census3 = sum(DAMe_census3),  # Suma población de 2022 por departamento
           DAM = factor(DAM, levels = unique(censos$DAM)))  # Convierte DAM a factor con niveles únicos

  # Filtra distritos con datos de población en 2002
  datos <- datos %>% filter(DAMe_census1 > 0)

  # Calcula tasas de crecimiento y proyecciones
  datos <- datos %>%
    rowwise() %>%  # Aplica funciones fila por fila
    mutate(
      crecim = (log(DAMe_census2) - log(DAMe_census1)) / (census_date[2] - census_date[1]),  # Tasa de crecimiento 2002-2012
      crecim_class = case_when(  # Clasificación de la tasa de crecimiento 2002-2012
        crecim < 0 ~ "<0",
        crecim >= 0 & crecim <= 0.006 ~ "0-0.6%",
        crecim > 0.006 & crecim <= 0.014 ~ "0.6%-1.4%",
        crecim > 0.014 ~ ">1.4%"
      ),
      crecim2 = (log(DAMe_census3) - log(DAMe_census2)) / (census_date[3] - census_date[2]),  # Tasa de crecimiento 2012-2022
      crecim_class2 = case_when(  # Clasificación de la tasa de crecimiento 2012-2022
        crecim2 < 0 ~ "<0",
        crecim2 >= 0 & crecim2 <= 0.006 ~ "0-0.6%",
        crecim2 > 0.006 & crecim2 <= 0.014 ~ "0.6%-1.4%",
        crecim2 > 0.014 ~ ">1.4%"
      ),
      crecim3 = (log(DAMe_census3) - log(DAMe_census1)) / (census_date[3] - census_date[1]),  # Tasa de crecimiento 2002-2022
      crecim_class3 = case_when(  # Clasificación de la tasa de crecimiento 2002-2022
        crecim2 < 0 ~ "<0",
        crecim2 >= 0 & crecim2 <= 0.006 ~ "0-0.6%",
        crecim2 > 0.006 & crecim2 <= 0.014 ~ "0.6%-1.4%",
        crecim2 > 0.014 ~ ">1.4%"
      )) %>%
    ungroup() %>%  # Desagrupa los datos
    mutate(tend = ifelse(DAMe_census1 > DAMe_census2, "decrecim", "crecim"))  # Tendencia de crecimiento

  #titulo
  title.aux = ifelse(
     is.null(pais_name),
     paste0("Box Plot del crecimiento poblacional entre ",
            census_year[1], " y ", census_year[2]),
     paste0("Box Plot del crecimiento poblacional entre ",
            census_year[1], " y ", census_year[2], " - ", pais_name)
  )

  # Crear gráficos de boxplot y guardar
  boxplot_census1_census2 <- ggplot(datos, aes(x = DAM, y = crecim)) +
    theme_minimal() +  # Tema minimalista para el gráfico
    geom_boxplot(outliers = TRUE) +  # Gráfico de boxplot con outliers
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotación de etiquetas en el eje x
    labs(
      title = title.aux,  # Título del gráfico
      x = DAM_name,  # Etiqueta del eje x
      y = "Tasa de Crecimiento Anual") +  # Etiqueta del eje y
    geom_hline(yintercept = 0, color = "red")  # Línea horizontal en y=0


  print(boxplot_census1_census2)
  if (save_plot) ggsave(plot = boxplot_census1_census2,
         file = paste0(path,'/01_boxplot_census1_census2.svg'),  # Guarda el gráfico en formato SVG
         dpi = 300,  # Resolución del gráfico
         height = 8,  # Altura del gráfico
         width = 12)  # Ancho del gráfico


  #titulo
  title.aux = ifelse(
    is.null(pais_name),
    paste0("Box Plot del crecimiento poblacional entre ",
           census_year[2], " y ", census_year[3]),
    paste0("Box Plot del crecimiento poblacional entre ",
           census_year[2], " y ", census_year[3], " - ", pais_name)
  )

  boxplot_census2_census3 <- ggplot(datos, aes(x = DAM, y = crecim2)) +
    theme_minimal() +  # Tema minimalista para el gráfico
    geom_boxplot(outliers = TRUE) +  # Gráfico de boxplot con outliers
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotación de etiquetas en el eje x
    labs(
      title = title.aux,  # Título del gráfico
      x = DAM_name,  # Etiqueta del eje x
      y = "Tasa de Crecimiento Anual") +  # Etiqueta del eje y
    geom_hline(yintercept = 0, color = "red")  # Línea horizontal en y=0

  print(boxplot_census2_census3)
  # Guardar el gráfico en formato SVG
  if (save_plot) ggsave(plot = boxplot_census2_census3,
         file = paste0(path,'/02_boxplot_census2_census3.svg'),  # Nombre del archivo
         dpi = 300,  # Resolución del gráfico
         height = 8,  # Altura del gráfico
         width = 12)  # Ancho del gráfico



  # Crear gráfico de boxplot para el crecimiento poblacional entre census 1 y census 3
  #titulo
  title.aux = ifelse(
    is.null(pais_name),
    paste0("Box Plot del crecimiento poblacional entre ",
           census_year[1], " y ", census_year[3]),
    paste0("Box Plot del crecimiento poblacional entre ",
           census_year[], " y ", census_year[3], " - ", pais_name)
  )

  boxplot_census1_census3 <- ggplot(datos, aes(x = DAM, y = crecim2)) +
    theme_minimal() +  # Aplicar tema minimalista al gráfico
    geom_boxplot(outliers = TRUE) +  # Crear gráfico de boxplot incluyendo outliers
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x para mejor legibilidad
    labs(
      title = title.aux,  # Título del gráfico
      x = DAM_name,  # Etiqueta del eje x
      y = "Tasa de Crecimiento Anual") +  # Etiqueta del eje y
    geom_hline(yintercept = 0, color = "red")  # Añadir línea horizontal en y=0

  print(boxplot_census1_census3)
  # Guardar el gráfico en formato SVG
  if (save_plot) ggsave(plot = boxplot_census1_census3,
                        file = paste0(path,'/03_boxplot_census1_census3.svg'),  # Nombre del archivo
                        dpi = 300,  # Resolución del gráfico
                        height = 8,  # Altura del gráfico
                        width = 12)  # Ancho del gráfico

  # Crear gráfico de dispersión para comparar tasas de crecimiento poblacional
  title.aux = ifelse(
    is.null(pais_name),
    paste0("Crecimiento poblacional para ",
           census_year[1], "-", census_year[2], " vs ",
           census_year[2], "-", census_year[3]),
    paste0("Crecimiento poblacional para ",
           census_year[1], "-", census_year[2], " vs ",
           census_year[2], "-", census_year[3], " - ", pais_name)
  )

  crec_censos <- ggplot(datos) +
    geom_point(aes(x = crecim, y = crecim2), color = "blue", size = 3) +  # Puntos azules para cada distrito
    theme_minimal() +  # Aplicar tema minimalista al gráfico
    labs(
      title = title.aux,  # Título del gráfico
      x = paste0("Tasa de Crecimiento Anual ",
                 census_year[1], "-", census_year[2]),  # Etiqueta del eje x
      y = paste0("Tasa de Crecimiento Anual ",
                 census_year[2], "-", census_year[3])) +  # Etiqueta del eje y
    geom_hline(yintercept = 0, color = "gray") +  # Añadir línea horizontal en y=0
    geom_vline(xintercept = 0, color = "gray") +  # Añadir línea vertical en x=0
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "red")  # Añadir línea diagonal con pendiente 1

  # Guardar el gráfico en formato SVG
  print(crec_censos)
  if (save_plot) ggsave(plot = crec_censos,
         file = paste0(path,'/04_crec_censos.svg'),
         dpi = 300,  # Resolución del gráfico
         height = 8,  # Altura del gráfico
         width = 12)  # Ancho del gráfico

  # Crear gráfico de dispersión para comparar tasas de crecimiento poblacional por departamento
  crec_censos_dpto <- ggplot(datos) +
    geom_point(aes(x = crecim, y = crecim2), color = "blue", size = 3) +  # Puntos azules para cada distrito
    facet_wrap(~DAM, ncol = 5, scales = "fixed") +  # Crear facetas por departamento
    labs(
      title = title.aux,  # Título del gráfico
      x = paste0("Tasa de Crecimiento Anual ",
                 census_year[1], "-", census_year[2]),  # Etiqueta del eje x
      y = paste0("Tasa de Crecimiento Anual ",
                 census_year[2], "-", census_year[3])) +  # Etiqueta del eje y
    theme_minimal() +  # Aplicar tema minimalista al gráfico
    geom_hline(yintercept = 0, color = "gray") +  # Añadir línea horizontal en y=0
    geom_vline(xintercept = 0, color = "gray") +  # Añadir línea vertical en x=0
    geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "red")  # Añadir línea diagonal con pendiente 1

  print(crec_censos_dpto)
  # Guardar el gráfico en formato SVG
  if (save_plot) ggsave(plot = crec_censos_dpto,
         file = paste0(path,'/05_crec_censos_dpto.svg'),
         dpi = 300,  # Resolución del gráfico
         height = 8,  # Altura del gráfico
         width = 12)  # Ancho del gráfico


}

