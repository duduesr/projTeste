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
#'
n_projection <- function(
  census,     #base de dados do censo (2 pontos)
  anos,#anos dos censos (2 pontos),
  DAM_proj,    #base de dados com os totais do DAM para o ano projetado
  ano_proj,    #ano da projecao
  assint       #assintotas
){


  saida = NULL
  if (length(ano_proj) > 1){

    for (i in 1:length(ano_proj)){
      DAM_proj_aux = DAM_proj[,c("DAM", paste0("DAM_proj",i))]
      colnames(DAM_proj_aux) = c("DAM", "DAM_proj")
      dados = n_projection(
        census,
        anos,
        DAM_proj_aux,
        ano_proj[i],
        assint
      )
      saida = saida %>% bind_rows(dados)
    }
  }else{
    #cria DAM
    census_date = c(anos, ano_proj)
    census_year = floor(anos) #arredonda ano

    dados <- census %>%
      group_by(DAM) %>%
      mutate(DAM_census1 = sum(DAMe_census1),
             DAM_census2 = sum(DAMe_census2)) %>%
      left_join(DAM_proj)

    dados <- dados %>%
      rowwise() %>%
      mutate(
        # Calculate growth rate for period 1
        cresc = (log(DAMe_census2) - log(DAMe_census1)) / (census_date[2] - census_date[1]),
        # Project to year3 usind different methods
        exponential = project_exp(DAMe_census1, DAMe_census2, census_date),
        linear = forecast_linear(DAMe_census1, DAMe_census2, census_date),
        logistic = forecast_logistico(DAMe_census1, DAMe_census2, census_date, assint),
        prop_DAMe_census1 = DAMe_census1 / DAM_census1,
        prop_DAMe_census2 = DAMe_census2 / DAM_census2,
        const_share_census2 = DAM_proj * prop_DAMe_census2,
        const_share_census1 = DAM_proj * prop_DAMe_census1,
        est_linear_prop = DAM_proj * forecast_linear(prop_DAMe_census1, prop_DAMe_census2, census_date),
        est_logistico_prop = DAM_proj * forecast_logistico_prop(prop_DAMe_census1, prop_DAMe_census2, census_date, assint),
        est_aibi = forecast_aibi(DAMe_census1, DAMe_census2, DAM_census1, DAM_census2, DAM_proj),
        # wilson = mean(c(est_logistico_prop, est_aibi)),
        wilson2014 = mean(c(ifelse(DAMe_census1 > DAMe_census2, est_logistico_prop, est_linear_prop)))
      ) %>%
      select(-prop_DAMe_census2, -prop_DAMe_census1) %>%
      ungroup() %>%
      group_by(DAM) %>%
      mutate(
        est_logistico_prop_red = est_logistico_prop*DAM_proj/sum(est_logistico_prop),
        exponential_red = exponential*DAM_proj/sum(exponential),
        linear_red = linear*DAM_proj/sum(linear),
        logistic_red = logistic*DAM_proj/sum(logistic),
        wilson2014_red = 	wilson2014*DAM_proj/sum(wilson2014)
      ) %>%
      mutate(ano = ano_proj) %>%
      ungroup()
      saida = saida %>% bind_rows(dados)
  }

  if (length(ano_proj) == 1) saida = saida %>%
    select(DAM, DAMe, exponential:ano) %>%
    pivot_longer(-c(DAM,DAMe, ano), names_to = 'metodo', values_to = 'projecao')

  return(saida)
}


