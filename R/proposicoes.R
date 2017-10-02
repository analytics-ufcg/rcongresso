#' Recupera da API uma proposição contendo detalhes adicionais.
#'
#' @param id_prop ID da proposição
#'
#' @return Dataframe contendo informações sobre a proposição
#'
#' @examples
#' pec241 <- fetch_proposicao(2088351)
#'
#' @export
fetch_proposicao <- function(id_prop){
  tibble::tibble(id = id_prop) %>%
    dplyr::mutate(path = paste0(.PROPOSICOES_PATH, "/", .$id)) %>%
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
fetch_votacoes <- function(id_prop){
  tibble::tibble(id = id_prop) %>%
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
  tibble::tibble(tipo, numero, ano) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.PROPOSICOES_PATH,
                     list(siglaTipo=.$tipo, numero=.$numero, ano=.$ano, ordem="ASC", ordenarPor="id"))$dados$id %>%
        .to_tibble()
    ) %>%
    unlist() %>%
    as.vector() %>%
    return()
}

#' Recupera da API os tipos de proposição disponíveis.
#'
#' @return Tipos de proposição
#'
#' @examples
#' tipos_proposicao <- fetch_tipos_proposicao()
#'
#' @export
.fetch_tipos_proposicao <- function(){

  prop_types <- .congresso_api(.TIPOS_PROPOSICOES_PATH)

  return(prop_types$dados)
}

#' Dado um ID do tipo de proposição, retorna seu tipo.
#'
#' @param id_tipo_prop ID do tipo da proposição
#'
#' @return Dataframe contendo o tipo da proposição
#'
#' @examples
#' tipo_prop129 <- fetch_tipo_proposicao(129)
#'
#' @export
fetch_tipo_proposicao <- function(id_tipo_prop){
  prop_types <- .fetch_tipos_proposicao() %>%
    dplyr::mutate(id = as.numeric(.$id))

  tibble::tibble(id = id_tipo_prop) %>%
    dplyr::left_join(prop_types, by="id") %>%
    return()

}

#' Recupera da API o status da proposição passada, incluindo sequência, órgão,
#' informações sobre a tramitação e data/hora da proposição.
#'
#' @param id_prop ID da proposição
#'
#' @return Dataframe contendo informações sobre o status da proposição
#'
#' @examples
#' pec241_status <- fetch_status_proposicao(2088351)
#'
#' @export
fetch_status_proposicao <- function(id_prop){
  id <- NULL
  tibble::tibble(id = id_prop) %>%
    dplyr::mutate(path = paste0(.PROPOSICOES_PATH, "/", .$id)) %>%
    dplyr::group_by(id) %>%
    dplyr::do(
      .congresso_api(.$path)$dados$statusProposicao %>%
        .remove_lists_and_nulls()
    ) %>%
    return()
}
