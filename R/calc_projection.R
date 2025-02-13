
###############################################################################
#' Aplica os métodos de projeção
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param census banco de dados de população para 3 censos
#' @param anos vetor com os anos do primeiro e segundo censo e do ano a ser projetado
#' @return População projetada n anos
#' @examples
#' data(brasil2000)
#' @export

calc_projection <- function(
  census,     #base de dados do censo (3 pontos)
  anos,#anos dos censos
  assin       #assintotas
){

  census_year = floor(anos) #arredonda ano

  #cria DAM
  dados <- censos %>%
    group_by(DAM) %>%
    mutate(DAM_census1 = sum(DAMe_census1),
           DAM_census2 = sum(DAMe_census2),
           DAM_census3 = sum(DAMe_census3))

  dados <- dados %>%
    rowwise() %>%
    mutate(
      # Calculate growth rate for period 1
      cresc = (log(DAMe_census2) - log(DAMe_census1)) / (census_date[2] - census_date[1]),
      cresc_class = case_when(
        cresc < 0 ~ "<0",
        cresc >= 0 & cresc <= 0.006 ~ "0-0.6%",
        cresc > 0.006 & cresc <= 0.014 ~ "0.6%-1.4%",
        cresc > 0.014 ~ ">1.4%"
      ),

      # Calculate growth rate for period 2
      cresc2 = (log(DAMe_census3) - log(DAMe_census2)) / (census_date[3] - census_date[2]),
      cresc_class2 = case_when(
        cresc2 < 0 ~ "<0",
        cresc2 >= 0 & cresc2 <= 0.006 ~ "0-0.6%",
        cresc2 > 0.006 & cresc2 <= 0.014 ~ "0.6%-1.4%",
        cresc2 > 0.014 ~ ">1.4%"
      ),

      # Project to year3 usind different methods
      exponential = project_exp(DAMe_census1, DAMe_census2, census_date),
      linear = forecast_linear(DAMe_census1, DAMe_census2, census_date),
      logistic = forecast_logistico(DAMe_census1, DAMe_census2, census_date, assint),
      prop_DAMe_census1 = DAMe_census1 / DAM_census1,
      prop_DAMe_census2 = DAMe_census2 / DAM_census2,
      const_share = DAM_census3 * prop_DAMe_census2,
      est_linear_prop = DAM_census3 * forecast_linear(prop_DAMe_census1, prop_DAMe_census2, census_date),
      est_logistico_prop = DAM_census3 * forecast_logistico_prop(prop_DAMe_census1, prop_DAMe_census2, census_date, assint),
      est_aibi = forecast_aibi(DAMe_census1, DAMe_census2, DAM_census1, DAM_census2, DAM_census3),
      # wilson = mean(c(est_logistico_prop, est_aibi)),
      wilson2014 = mean(c(ifelse(DAMe_census1 > DAMe_census2, est_logistico_prop, est_linear_prop)))
    ) %>%
    ungroup() %>%
    mutate(tend = ifelse(DAMe_census1 > DAMe_census2, "decresc", "cresc"))

  return(dados)
}


