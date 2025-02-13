
###############################################################################
#' Função para projeção linear
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param census1 população do primeiro censo
#' @param census2 população do segundo censo
#' @param anos vetor com os anos do primeiro e segundo censo e do ano a ser projetado
#' @return População projetada n anos
#' @examples
#' data(brasil2000)
#' @export

forecast_linear <- function(census1, census2, anos) {
  anos_base <- c(anos[1], anos[2])
  populacao <- c(census1, census2)
  modelo <- lm(populacao ~ anos_base)
  previsao <- predict(modelo, newdata = data.frame(anos_base = anos[3]))
  return(previsao)
}
