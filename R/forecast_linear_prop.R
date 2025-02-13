
###############################################################################
#' Função para projeção linear de proporções
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param census1 proporção de população do primeiro censo
#' @param census2 proporção de população do segundo censo
#' @param anos vetor com os anos do primeiro e segundo censo e do ano a ser projetado
#' @return População projetada n anos
#' @examples
#' data(brasil2000)
#' @export

forecast_linear_prop <- function(prop_census1, prop_census2, anos) {
  anos_base <- c(anos[1], anos[2])
  populacao <- c(prop_census1, prop_census2)
  modelo <- lm(populacao ~ anos_base)
  previsao <- predict(modelo, newdata = data.frame(anos_base = anos[3]))
  return(previsao)
}
