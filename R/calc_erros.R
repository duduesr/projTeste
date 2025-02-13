
###############################################################################
#' Calcula estrutura de erros
#'
#' @description XXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param dados banco de dados de resultados
#' @return Lista com informações sobre erros
#' @examples
#' data(brasil2000)
#' @export
calc_erros <- function(
  dados
){
  
  # Rescale population
  dados_rescaled <- dados %>%
    group_by(DAM) %>%
    mutate(
      scaling_factor = DAM_census3 / sum(linear),
      linear = linear * scaling_factor,
      
      scaling_factor = DAM_census3 / sum(exponential ),
      exponential  = exponential * scaling_factor,
      
      scaling_factor = DAM_census3 / sum(logistic  ),
      logistic   = logistic  * scaling_factor,
      
      scaling_factor = DAM_census3 / sum(est_logistico_prop   ),
      est_logistico_prop    = est_logistico_prop   * scaling_factor,
      
      scaling_factor = DAM_census3 / sum(wilson2014   ),
      wilson2014    = wilson2014   * scaling_factor
    ) %>%
    ungroup() %>%
    select(-scaling_factor)
  
  # Check
  Check <- dados_rescaled %>% group_by(DAM) %>% 
    summarise(exponential = sum(exponential),
              linear = sum(linear),
              logistic = sum(logistic),
              const_share = sum(const_share),
              est_linear_prop = sum(est_linear_prop),
              est_logistico_prop = sum(est_logistico_prop),
              est_aibi = sum(est_aibi),
              prop_DAMe_census1 = sum(prop_DAMe_census1),
              prop_DAMe_census2 = sum(prop_DAMe_census2),
              DAM_census3 = sum(DAMe_census3),
              wilson2014 = sum(wilson2014))
  
  # Calculate the mean quadratic error and relative error
  erros <- dados_rescaled %>%
    mutate(
      APE_exponential = abs(exponential - DAMe_census3) / DAMe_census3,
      APE_linear = abs(linear - DAMe_census3) / DAMe_census3,
      APE_logistic = abs(logistic - DAMe_census3) / DAMe_census3,
      APE_const_share = abs(const_share - DAMe_census3) / DAMe_census3,
      APE_est_linear_prop = abs(est_linear_prop - DAMe_census3) / DAMe_census3,
      APE_est_logistico_prop = abs(est_logistico_prop - DAMe_census3) / DAMe_census3,
      APE_est_aibi = abs(est_aibi - DAMe_census3) / DAMe_census3,
      APE_est_wilson2014 = abs(wilson2014 - DAMe_census3) / DAMe_census3
    )
  
  # Summarise the mean quadratic error and sum of relative error by DAM
  erros_median <- erros %>%
    summarise(
      MedAPE_exponential = median(APE_exponential, na.rm = TRUE),
      MedAPE_linear = median(APE_linear, na.rm = TRUE),
      MedAPE_logistic = median(APE_logistic, na.rm = TRUE),
      MedAPE_const_share = median(APE_const_share, na.rm = TRUE),
      MedAPE_est_linear_prop = median(APE_est_linear_prop, na.rm = TRUE),
      MedAPE_est_logistico_prop = median(APE_est_logistico_prop, na.rm = TRUE),
      MedAPE_est_aibi = median(APE_est_aibi, na.rm = TRUE),
      MedAPE_est_wilson2014 = median(APE_est_wilson2014, na.rm = TRUE)
    ) %>% t()
  
  # Summarise the mean quadratic error and sum of relative error by DAM
  erros_median_DAM <- erros %>%
    group_by(DAM) %>%
    summarise(
      MedAPE_exponential = median(APE_exponential, na.rm = TRUE),
      MedAPE_linear = median(APE_linear, na.rm = TRUE),
      MedAPE_logistic = median(APE_logistic, na.rm = TRUE),
      MedAPE_const_share = median(APE_const_share, na.rm = TRUE),
      MedAPE_est_linear_prop = median(APE_est_linear_prop, na.rm = TRUE),
      MedAPE_est_logistico_prop = median(APE_est_logistico_prop, na.rm = TRUE),
      MedAPE_est_aibi = median(APE_est_aibi, na.rm = TRUE),
      MedAPE_est_wilson2014 = median(APE_est_wilson2014, na.rm = TRUE)
    ) 
  
  
  # Find the method (column name) that gives the lowest value for each DAM
  lowest_methods_DAM <- erros_median_DAM %>%
    rowwise() %>%
    mutate(lowest_method = colnames(erros_median_DAM)[which.min(c_across(-DAM)) + 1]) %>%
    select(DAM, lowest_method) %>% arrange(lowest_method)
  
  # Summarise the mean quadratic error and sum of relative error by DAM
  erros_median_crec <- erros %>%
    group_by(cresc_class) %>%
    summarise(
      MedAPE_exponential = median(APE_exponential, na.rm = TRUE),
      MedAPE_linear = median(APE_linear, na.rm = TRUE),
      MedAPE_logistic = median(APE_logistic, na.rm = TRUE),
      MedAPE_const_share = median(APE_const_share, na.rm = TRUE),
      MedAPE_est_linear_prop = median(APE_est_linear_prop, na.rm = TRUE),
      MedAPE_est_logistico_prop = median(APE_est_logistico_prop, na.rm = TRUE),
      MedAPE_est_aibi = median(APE_est_aibi, na.rm = TRUE),
      MedAPE_est_wilson2014 = median(APE_est_wilson2014, na.rm = TRUE)
    ) 
  
  # Find the method (column name) that gives the lowest value for each DAM
  lowest_methods_crec <- erros_median_crec %>%
    rowwise() %>%
    mutate(lowest_method = colnames(erros_median_crec)[which.min(c_across(-cresc_class)) + 1]) %>%
    select(cresc_class, lowest_method)
  
  # Summarise the mean quadratic error and sum of relative error by DAM
  erros_median_DAM_crec <- erros %>%
    group_by(DAM, cresc_class) %>%
    summarise(
      MedAPE_exponential = median(APE_exponential, na.rm = TRUE),
      MedAPE_linear = median(APE_linear, na.rm = TRUE),
      MedAPE_logistic = median(APE_logistic, na.rm = TRUE),
      MedAPE_const_share = median(APE_const_share, na.rm = TRUE),
      MedAPE_est_linear_prop = median(APE_est_linear_prop, na.rm = TRUE),
      MedAPE_est_logistico_prop = median(APE_est_logistico_prop, na.rm = TRUE),
      MedAPE_est_aibi = median(APE_est_aibi, na.rm = TRUE),
      MedAPE_est_wilson2014 = median(APE_est_wilson2014, na.rm = TRUE)
    )
  
  
  lowest_methods_DAM_crec <- erros_median_DAM_crec %>%
    rowwise() %>%
    mutate(lowest_method = colnames(erros_median_DAM_crec)[which.min(c_across(MedAPE_exponential:MedAPE_est_wilson2014)) + 2]) %>%
    select(DAM, cresc_class, lowest_method)
  
  return(
    list(
      dados_rescaled = dados_rescaled,
      Check = Check,
      erros = erros,
      erros_median = erros_median,
      erros_median_DAM = erros_median_DAM,
      lowest_methods_DAM = lowest_methods_DAM,
      erros_median_crec = erros_median_crec,
      lowest_methods_crec = lowest_methods_crec,
      erros_median_DAM_crec = erros_median_DAM_crec,
      lowest_methods_DAM_crec  = lowest_methods_DAM_crec 
    )
  )
}

