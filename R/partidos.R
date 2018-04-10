#' @title Fetches parties from API using a query.
#' @description Returns a list containing basic information about parties which has or had been any deputy
#' in the lower house. If none parameters are passed, the function returns the parties
#' which has active deputies on that moment.
#' @param id Party's ID
#' @param sigla Party's abbreviation
#' @param dataInicio Party's processing starting date
#' @param dataFim Party's processing end date
#' @param idLegislatura Legislature's number
#' @param ordenarPor Order by: 'id', 'sigla' or 'nome'
#' @param itens Items quantity. '-1' returns all parties which have
#' deputies at the request moment
#' @return Dataframe containing information about the parties.
#' @details Note that if you have the party's ID, it's not necessary to add any other parameter on the
#' function call. The call to this function using the party's ID returns more details than using the
#' others parameters. If you don't have the party's ID, the \code{\link[rcongresso]{fetch_id_partido}}
#' function may be helpful.
#' @examples
#' PMB <- fetch_partido(id = 36887)
#' PT <- fetch_partido(sigla = "PT")
#' partidos2010a2014 <- fetch_partido(dataInicio = "2010-01-01", dataFim = "2014-12-31", itens = 100)
#' @seealso
#'  \code{\link[rcongresso]{get_votos_partidos}}, \code{\link[rcongresso]{fetch_orientacoes}}
#' @rdname fetch_partido
#' @export
fetch_partido <- function(id = NULL, sigla = NULL, dataInicio = NULL,
                          dataFim = NULL, idLegislatura = NULL, itens = NULL, ordenarPor = NULL){

  parametros <- as.list(environment(), all=TRUE)

  if(!length(.verifica_parametros_entrada(parametros)))
    .congresso_api(.PARTIDOS_PATH) %>%
    .assert_dataframe_completo(.COLNAMES_PARTIDOS) %>%
    .coerce_types(.COLNAMES_PARTIDOS)
  else if(is.null(id))
    .fetch_using_queries(parametros, .PARTIDOS_PATH)%>%
    .assert_dataframe_completo(.COLNAMES_PARTIDOS) %>%
    .coerce_types(.COLNAMES_PARTIDOS)
  else
    .fetch_using_id(id, .PARTIDOS_PATH) %>%
    .assert_dataframe_completo(.COLNAMES_PARTIDOS_ID) %>%
    .coerce_types(.COLNAMES_PARTIDOS_ID)
}

#' @title Retrieves the party's ID from its abbreviation.
#' @description The function can be used to fetch a vector of ids as well, in case of many parties.
#' @param sigla Party's abbreviation
#' @return Party's ID.
#' @examples
#' p <- fetch_id_partido(c("PT","PSDB","PP","PMDB"))
#' @seealso
#'   \code{\link[rcongresso]{fetch_id_proposicao}}
#' @rdname fetch_id_partido
#' @export
fetch_id_partido <- function(sigla) {
  tibble::tibble(sigla) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      fetch_partido(sigla=.$sigla)$id %>%
        .verifica_id(.WARNING_SIGLA_PARTIDO) %>%
        .to_tibble()
    ) %>%
    unlist() %>%
    as.vector()
}
