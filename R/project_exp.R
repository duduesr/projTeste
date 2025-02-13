#' Função para projeção exponencial usando a fórmula fornecida e anos flexíveis
#'
#' Gera rojeção exponencial usando a fórmula fornecida e anos flexíveis
#' @param census1 população do primeiro censo
#' @param census2 população do segundo censo
#' @param anos vetor com os anos do primeiro e segundo censo e do no a ser projetado
#' @return População projetada n anos
#' @examples
#' data(brasil2000)
#' @export

project_exp <- function(census1, census2, anos) {
  K <- log(census2 / census1) / (anos[2] - anos[1])
  populacao_futura <- census2 * exp(K * (anos[3] - anos[2]))
  return(populacao_futura)
}
