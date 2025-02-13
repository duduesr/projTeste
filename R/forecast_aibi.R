
###############################################################################
#' Função para projeção linear (Share of Growth) - AiBi
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param census1 população do primeiro censo
#' @param census2 população do segundo censo
#' @param anos vetor com os anos do primeiro e segundo censo e do ano a ser projetado
#' @return População projetada n anos
#' @examples
#' data(brasil2000)
#' @export

forecast_aibi <- function(census1, census2, UF_census1, UF_census2, UF_proj_2022) {
  anos_base <- c(UF_census1, UF_census2)
  populacao <- c(census1, census2)
  modelo <- lm(populacao ~ anos_base)
  previsao <- predict(modelo, newdata = data.frame(anos_base = UF_proj_2022))
  return(previsao)
}


