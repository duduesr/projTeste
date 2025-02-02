# Instalando e carregando o pacote tidyverse
library(tidyverse)

#' Função para projeção logística
#'
#' Gera prpjção de população total logística
#' @param F_temp The temperature in degrees Fahrenheit
#' @return The temperature in degrees Celsius
#' @examples
#' temp1 <- F_to_C(50);
#' temp2 <- F_to_C( c(50, 63, 23) );
#' @export
#
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

# Função para projeção exponencial usando a fórmula fornecida e anos flexíveis
project_exp <- function(census1, census2, anos) {
  K <- log(census2 / census1) / (anos[2] - anos[1])
  populacao_futura <- census2 * exp(K * (anos[3] - anos[2]))
  return(populacao_futura)
}

# Função para projeção linear
forecast_linear <- function(census1, census2, anos) {
  anos_base <- c(anos[1], anos[2])
  populacao <- c(census1, census2)
  modelo <- lm(populacao ~ anos_base)
  previsao <- predict(modelo, newdata = data.frame(anos_base = anos[3]))
  return(previsao)
}

# Função para projeção linear de proporções
forecast_linear_prop <- function(prop_census1, prop_census2, anos) {
  anos_base <- c(anos[1], anos[2])
  populacao <- c(prop_census1, prop_census2)
  modelo <- lm(populacao ~ anos_base)
  previsao <- predict(modelo, newdata = data.frame(anos_base = anos[3]))
  return(previsao)
}

# Função para projeção logística de proporções
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

# Função para projeção linear (Share of Growth) - AiBi
forecast_aibi <- function(census1, census2, UF_census1, UF_census2, UF_proj_2022) {
  anos_base <- c(UF_census1, UF_census2)
  populacao <- c(census1, census2)
  modelo <- lm(populacao ~ anos_base)
  previsao <- predict(modelo, newdata = data.frame(anos_base = UF_proj_2022))
  return(previsao)
}

