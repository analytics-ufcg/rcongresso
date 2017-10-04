#' Recupera da API os detalhes de um deputado.
#'
#' @param dep_id ID do Deputado
#'
#' @return Lista contendo detalhes sobre o deputado.
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

#' Recupera todos os gastos da cota parlamentar, dos últimos seis meses, um deputado específico.
#'
#' @param dep_id ID do deputado
#'
#' @return Dataframe contendo detalhes sobre os gastos do deputado
#'
#' @examples
#' gastos_abel_mesquita <- fetch_despesas_deputado(178957)
#'
#' @export
fetch_despesas_deputado <- function(dep_id) {
  id <- path <- NULL
  query <- list(ordem="ASC", ordenarPor="numAno")

  tibble::tibble(id = dep_id) %>%
    dplyr::mutate(path = paste0(.DEPUTADOS_PATH, "/", id, "/despesas")) %>%
    dplyr::group_by(id, path) %>%
    dplyr::do(
      .congresso_api(.$path, query)$dados
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(idDep=.$id) %>%
    dplyr::select(-path, -id) %>%
    return()
}
