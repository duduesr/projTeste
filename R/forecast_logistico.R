
###############################################################################
#' Função para projeção logística
#'
#' @description Gera projeção de população total logística
#' @param census1 população do primeiro censo
#' @param census2 população do segundo censo
#' @param anos vetor com os anos do primeiro e segundo censo e do ano a ser projetado
#' @param assint limite inferior e superior das assintotas da curva logística
#' @return População projetada n anos
#' @examples
#' data(brasil2000)
#' @export

forecast_logistico <- function(census1, census2, anos, assint) {
  anos_base <- c(anos[1], anos[2])
  pop_assint <- c(min(census1, census2) * assint[1], max(census1, census2) * assint[2])
  pop_logito <- c(log((census1 - pop_assint[1]) / (pop_assint[2] - census1)),
                  log((census2 - pop_assint[1]) / (pop_assint[2] - census2)))
  modelo <- lm(pop_logito ~ anos_base)
  previsao_log <- predict(modelo, newdata = data.frame(anos_base = anos[3]))
  previsao <- pop_assint[2] - ((pop_assint[2] - pop_assint[1])) / (1 + exp(previsao_log))
  return(previsao)
}
