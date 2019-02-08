####### Relatorias ######
#' @title Fetches proposição's relatoria historic
#' @description Returns a dataframe containing the relatoria's historic, with date and relator's name. 
#' @details Senado contains details about the end of each relatoria and camara contains initials of local 
#' from relatoria
#' @param proposicao_id Proposição's ID
#' @param casa senado or câmara
#' @param last_n Fetch last N relatores
#' @return Dataframe containing detailed information about relatorias of a proposição
#' @examples fetch_relatorias(91341, 'senado', 3)
#' @export
fetch_relatorias <- function(proposicao_id, casa, last_n=NULL) {
  relatorias <- data.frame()
  if(tolower(casa) == 'senado'){
    relatorias <- .fetch_relatorias_senado(proposicao_id)
  }
  else if(tolower(casa) == 'camara') {
    relatorias <- .fetch_relatorias_camara(proposicao_id)
  }
  
  if(!is.null(last_n)){
    relatorias <- 
      relatorias %>%
      utils::head(last_n)
  }
  return(relatorias)
}

#' @title Fetches proposição's relatoria historic from Senado
#' @description Returns a dataframe containing the relatoria's historic, with date and relator's name from Senado. 
#' @param proposicao_id Proposição's ID from Senado
#' @return Dataframe containing detailed information about relatorias of a proposição from Senado
#' @export
.fetch_relatorias_senado <- function(proposicao_id) {
  url_relatorias <-
    paste0(.SENADO_PATH, .RELATORIA_SENADO_PATH)
  
  path <- paste0(url_relatorias, proposicao_id)
  json_relatorias <- .senado_api(path = path, asList = TRUE)
  relatorias_data <-
    json_relatorias %>%
    magrittr::extract2("RelatoriaMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("HistoricoRelatoria")
  
  relatorias_df <-
    relatorias_data %>%
    magrittr::extract2("Relator") %>%
    as.data.frame() %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest()
  
  relatorias_df <- 
    relatorias_df[,!sapply(relatorias_df, is.list)] %>%
    .rename_relatorias_senado_columns
}

#' @title Renames columns from Senado Relatoria's dataframe 
#' @description Replaces spaces to underscore and format to lowercase
#' @param df Senado Relatoria's dataframe 
#' @return Senado Relatoria's dataframe renamed
.rename_relatorias_senado_columns <- function(df) {
  new_names <- names(df) %>%
    .to_underscore() %>%
    stringr::str_replace("identificacao_parlamentar_|identificacao_comissao_", "")
  names(df) <- new_names
  df
}

#' @title Fetches proposição's relatoria historic from Camara
#' @description Returns a dataframe containing the relatoria's historic, with date and relator's name from Camara. 
#' @param proposicao_id Proposição's ID from Camara
#' @return Dataframe containing detailed information about relatorias of a proposição from Camara
#' @export
.fetch_relatorias_camara <- function(proposicao_id) {
  tibble::tibble()
}