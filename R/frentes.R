#' @title Fetches info about frente
#' @description Fetches a dataframe containing information about the frentes
#' @param id_frente Frente ID
#' @param casa Camara ou senado
#' @return A dataframe containing details about the frente
#' @examples
#' fetch_frentes(54012, "camara")
#' @export
fetch_frentes <- function(id_frente, casa) {
  frente <- tibble::as_tibble()
  if (casa == 'camara') {
    frente <- .fetch_frentes_camara(id_frente)
  } else if (casa == 'senado') {
    warning("Metodo nao implementado.")
  } else {
    warning("Parametro 'casa' nao identificado.")
  }
  return(frente)
}

#' @title Fetches info about frente of Câmara
#' @description Fetches a dataframe containing information about the frentes of the Câmara
#' @param id_frente Frente ID
#' @return A dataframe containing details about the frente
#' @examples
#' fetch_frentes_camara(54012)
.fetch_frentes_camara <- function(id_frente) {
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

#' @title Fetches info about members of frente
#' @description Fetches a dataframe containing information about the members frentes
#' @param id_frente Frente ID
#' @param casa Camara ou senado
#' @return A dataframe containing details about the members frente
#' @examples
#' fetch_membros_frentes(54012, "camara")
#' @export
fetch_membros_frentes <- function(id_frente, casa) {
  frente <- tibble::as_tibble()
  if (casa == 'camara') {
    frente <- .fetch_membros_frentes_camara(id_frente)
  } else if (casa == 'senado') {
    warning("Metodo nao implementado.")
  } else {
    warning("Parametro 'casa' nao identificado.")
  }
  return(frente)
}

#' @title Fetches info about members of frente
#' @description Fetches a dataframe containing information about the members frentes of the Câmara
#' @param id_frente Frente ID
#' @return A dataframe containing details about the members frente
#' @examples
#' .fetch_membros_frentes_camara(54012)
.fetch_membros_frentes_camara <- function(id_frente) {
  membros_data <- .camara_api(paste0(.FRENTES_PATH, id_frente,"/membros"))
  membros <- membros_data %>%
    dplyr::mutate(id_frente = id_frente) %>%
    rename_table_to_underscore() %>%
    .assert_dataframe_completo(.COLNAMES_FRENTES_MEMBROS) %>%
    .coerce_types(.COLNAMES_FRENTES_MEMBROS)
}

