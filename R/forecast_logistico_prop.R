
###############################################################################
#' Função para projeção logística de proporções
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param census1 proporção de população do primeiro censo
#' @param census2 proporção de população do segundo censo
#' @param anos vetor com os anos do primeiro e segundo censo e do ano a ser projetado
#' @return População projetada n anos
#' @examples
#' data(brasil2000)
#' @export

forecast_logistico_prop <- function(prop_census1, prop_census2, anos, assint) {
  anos_base <- c(anos[1], anos[2])
  pop_assint <- c(min(prop_census1, prop_census2) * assint[1], max(prop_census1, prop_census2) * assint[2])
  pop_logito <- c(log((prop_census1 - pop_assint[1]) / (pop_assint[2] - prop_census1)),
                  log((prop_census2 - pop_assint[1]) / (pop_assint[2] - prop_census2)))
  modelo <- lm(pop_logito ~ anos_base)
  previsao_log <- predict(modelo, newdata = data.frame(anos_base = anos[3]))
  previsao <- pop_assint[2] - ((pop_assint[2] - pop_assint[1])) / (1 + exp(previsao_log))
  return(previsao)
}
