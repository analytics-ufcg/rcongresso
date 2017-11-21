#' @title Retrieves details about a deputy
#' @description ID, name, birth date, birth city among other informations are returned.
#' @param id deputy's ID
#' @param idLegislatura Id of one or more mandates of the deputy
#' @param siglaUf State's abbreviation of the deputy
#' @param siglaPartido Party's abbreviation of the deputy
#' @param siglaSexo Gender's abbreviation
#' @param itens Items quantity by request
#' @param ordenarPor Element that will be used to sort the returned list
#' @return Dataframe containing details about the deputy.
#' @examples
#' abel_mesquita_info <- fetch_deputado(id = 178957)
#' @rdname fetch_deputado
#' @export
fetch_deputado <- function(id = NULL, idLegislatura = NULL, siglaUf = NULL, siglaPartido = NULL,
                           siglaSexo = NULL, itens = NULL, ordenarPor = NULL){

  parametros <- as.list(environment(), all = TRUE)

  if(!length(.verifica_parametros_entrada(parametros)))
    .congresso_api(.DEPUTADOS_PATH)$dados
  else if(is.null(id))
    .fetch_using_queries(parametros, .DEPUTADOS_PATH)
  else
    .fetch_using_id(id, .DEPUTADOS_PATH)
}

#' @title Fetches expenditures from deputy using its id
#' @description Fetches expenditures from deputy with his/her parlamentary quota in
#' the last six months.
#' @param dep_id deputy's ID
#' @return Dataframe containing details about the deputy's expenditures
#' @examples
#' gastos_abel_mesquita <- fetch_despesas_deputado(dep_id = 178957)
#' @rdname fetch_despesas_deputado
#' @export
fetch_despesas_deputado <- function(dep_id) {
  id <- path <- NULL
  query <- list(ordem = "ASC", ordenarPor = "numAno")

  tibble::tibble(id = dep_id) %>%
    dplyr::mutate(path = paste0(.DEPUTADOS_PATH, "/", id, "/despesas")) %>%
    dplyr::group_by(id, path) %>%
    dplyr::do(
      .congresso_api(.$path, query)$dados
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(idDep = .$id) %>%
    dplyr::select(-path, -id)
}

#' @title Fetches the last information about the deputy's mandate
#' @description Fetches information such as party and electoral name.
#' @param dep_id deputy's ID
#' @return Dataframe containing details about deputy's party, electoral name and url photo
#' @examples
#' abel_mesquita_ultimo_estado <- fetch_ultimo_status_deputado(dep_id = 178957)
#' @rdname fetch_ultimo_status_deputado
#' @export
fetch_ultimo_status_deputado <- function(dep_id) {
  id <- NULL
  tibble::tibble(id = dep_id) %>%
    dplyr::mutate(path = paste0(.DEPUTADOS_PATH, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)$dados$ultimoStatus %>%
        .remove_lists_and_nulls()
    ) %>%
    dplyr::ungroup()
}
