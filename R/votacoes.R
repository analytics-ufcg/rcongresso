#' @title Fetches details about a voting on Senate
#' @description Fetches details about a voting on Senate.
#' @param proposicao_id Proposition Id
#' @return Dataframe containing details about a voting on Senate
#' @examples
#' votacoes_senado <- fetch_votacoes_senado(91341)
#' @rdname fetch_votacoes_senado
#' @export
fetch_votacoes_senado <- function(proposicao_id) {

  json_votacoes <- .senado_api(paste0(.SENADO_VOTACOES_PATH, proposicao_id), asList = TRUE)
  votacoes_data <-
    json_votacoes %>%
    magrittr::extract2("VotacaoMateria") %>%
    magrittr::extract2("Materia")
  votacoes_ids <-
    votacoes_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as_tibble() %>%
    unique()
  votacoes_df <-
    votacoes_data %>%
    magrittr::extract2("Votacoes") %>%
    purrr::map_df( ~ .) %>%
    tidyr::unnest()
  
  votacoes_df <-
    votacoes_df %>%
    tibble::add_column(!!!votacoes_ids)
  
  votacoes_df <- votacoes_df[,!sapply(votacoes_df, is.list)]
  
  .rename_votacoes_df(votacoes_df) 
  votacoes_df 
}
