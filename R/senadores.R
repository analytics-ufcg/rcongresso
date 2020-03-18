#' @title Fetches info of the senators
#' @description Returns its id, name, uf, gender.
#' @param legis_initial Initial legisture
#' @param legis_final Final legisture
#' @return Dataframe containing the senators info.
#' @examples
#' senators <- fetch_senadores(40, 56)
#' @rdname fetch_senadores
#' @export
fetch_senadores <- function(legis_initial, legis_final) {
  senator_data <- .senado_api(paste0(.SENADORES_PATH, legis_initial, "/", legis_final), asList = TRUE)
  senator_data <- senator_data$ListaParlamentarLegislatura$Parlamentares

  senators <-
    senator_data %>%
    magrittr::extract2("Parlamentar")

  senators
}
