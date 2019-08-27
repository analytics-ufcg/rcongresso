#' @title Fetches info about frente
#' @description Fetches a dataframe containing information about the frentes of the Câmara
#' @param id_frente Frente ID
#' @return A dataframe containing details about the frente
#' @examples
#' fetch_frentes_camara(54012)
#' @export
fetch_frentes_camara <- function(id_frente) {
  frente_data <- .camara_api(paste0(.FRENTES_PATH, id_frente))

  if (!"coordenador.id" %in% names(frente_data)) {
    frente_data$coordenador.id = NA
  }

  if (!"coordenador.siglaPartido" %in% names(frente_data)) {
    frente_data$coordenador.siglaPartido = NA
  }

  if (!"coordenador.siglaUf" %in% names(frente_data)) {
    frente_data$coordenador.siglaUf = NA
  }

  if (!"coordenador.idLegislatura" %in% names(frente_data)) {
    frente_data$coordenador.idLegislatura = NA
  }

  if (!"coordenador.uri" %in% names(frente_data)) {
    frente_data$coordenador.uri = NA
  }

  frente <- frente_data %>%
    dplyr::select(id_frente = id,
                  titulo_frente = titulo,
                  id_legislatura = idLegislatura,
                  telefone = telefone,
                  situacao = situacao,
                  url_documento = urlDocumento,
                  id_coordenador = coordenador.id,
                  nome_coordenador = coordenador.nome,
                  partido_coordenador = coordenador.siglaPartido,
                  uf_coordenador = coordenador.siglaUf,
                  id_legislatura_coordenador = coordenador.idLegislatura,
                  url_coordenador = coordenador.uri) %>%
    .assert_dataframe_completo(.COLNAMES_FRENTES) %>%
    .coerce_types(.COLNAMES_FRENTES)

}
