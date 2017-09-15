#' Recupera da API uma proposição contendo detalhes adicionais.
#'
#' @param id_prop ID da proposição
#'
#' @return Lista contendo informações sobre a proposição
#'
#' @examples
#' pec241 <- fetch_proposicao(2088351)
#'
#' @export
fetch_proposicao <- function(id_props){
  tibble::tibble(id = id_props) %>%
    dplyr::mutate(path = paste0(.PROPOSICOES_PATH, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)$dados %>%
        .remove_lists_and_nulls()
      ) %>%
    return()
}

#' Recupera da API todas as votações pelas quais uma proposição já passou.
#'
#' @param id_prop ID da proposição
#'
#' @return Dataframe contendo as várias votações pelas quais uma proposição passou.
#'
#' @examples
#' votacoes_pec241 <- fetch_votacoes(2088351)
#'
#' @export
fetch_votacoes <- function(id_props){
  tibble::tibble(id = id_props) %>%
    dplyr::mutate(path = paste0(.PROPOSICOES_PATH, "/", .$id, "/votacoes")) %>%
    dplyr::rowwise() %>%
    dplyr::do(.congresso_api(.$path)[[1]]) %>%
    return()
}

#' Recupera da API o ID da preposição de acordo com o tipo, número da proposição e ano que
#' esta foi feita.
#'
#' @param tipo Tipo da proposição (i.e., PEC, PL, PDC)
#' @param numero Número da proposição
#' @param ano Ano da proposição
#'
#' @return ID da proposição
#'
#' @examples
#' pec241_id <- fetch_id_proposicao("PEC", 241, 2016)
#'
#' @export
fetch_id_proposicao <- function(tipo, numero, ano){

  query <- list(siglaTipo=tipo, numero=numero, ano=ano, ordem="ASC", ordenarPor="id")

  prop_object <- .congresso_api(.PROPOSICOES_PATH, query)

  return(prop_object$dados$id)
}

#' Recupera da API os tipos de proposição disponíveis.
#'
#' @return Tipos de proposição
#'
#' @examples
#' tipos_proposicao <- fetch_tipos_proposicao()
#'
#' @export
fetch_tipos_proposicao <- function(){

  prop_types <- .congresso_api(.TIPOS_PROPOSICOES_PATH)

  return(prop_types$dados)
}
