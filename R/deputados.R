#' Retrieves details about a deputy.
#'
#' @param dep_id deputy's ID.
#'
#' @return Dataframe containing details about the deputy.
#'
#' @examples
#' abel_mesquita_info <- fetch_deputado(178957)
#'
#' @export
fetch_deputado <- function(dep_id){
  id <- NULL
  tibble::tibble(id = dep_id) %>%
    dplyr::mutate(path = paste0(.DEPUTADOS_PATH, "/", id)) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .congresso_api(.$path)$dados %>%
        .remove_lists_and_nulls()
    ) %>%
    dplyr::ungroup() %>%
    return()
}

#' Fetches expenditures from deputy with his/her parlamentary quota in
#' the last six months.
#'
#' @param dep_id deputy's ID.
#
#' @return Dataframe containing details about the deputy's expenditures.
#'
#' @examples
#' gastos_abel_mesquita <- fetch_despesas_deputado(178957)
#'
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
    dplyr::select(-path, -id) %>%
    return()
}
