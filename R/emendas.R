
#' @title Returns emendas of a proposição from Chamber of deputies or Senate
#' @description Fetchs a dataframe with emendas's data of a proposição from Chamber of deputies or Senate
#' @param id Proposição's ID from congress
#' @param casa senado or camara
#' @param sigla Proposition type (i.e., PEC, PL, PDC)
#' @param numero Proposition number
#' @param ano Proposition year
#' @return Dataframe with informations about emendas of a proposição from Chamber of deputies or Senate
#' @examples
#' fetch_emendas(91341,'senado')
#' @rdname fetch_emendas
#' @export
fetch_emendas <- function(id, casa, sigla=NULL, numero=NULL, ano=NULL) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    if(is.null(sigla) & is.null(numero) & is.null(ano)) {
      print("Para retornar as emendas da camara, faz-se necessarios a sigla, o numero e o ano")
      return()
    } else {
      emendas <- fetch_emendas_camara(sigla, numero, ano)
    }
  } else if (casa == 'senado') {
    emendas <- fetch_emendas_senado(id)
  } else {
    print('Parametro "casa" nao identificado.')
    return()
  }

  emendas  <-
    emendas %>%
    dplyr::mutate(prop_id = id, codigo_emenda = as.integer(codigo_emenda)) %>%
    dplyr::select(
      prop_id, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor)
}

#' @title Returns emendas of a proposição from Senado
#' @description Fetchs a dataframe with emendas's data of a proposição from Senado.
#' @param bill_id Proposição's ID from senado.
#' @return Dataframe with informations about emendas of a proposição from Senado.
#' @rdname fetch_emendas_senado
#' @export
fetch_emendas_senado <- function(bill_id) {
  path <- paste0(.SENADO_PATH, .EMENDAS_SENADO_PATH, bill_id)

  json_emendas <- .senado_api(path = path, asList = TRUE)

  emendas_df <- json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("Emendas") %>%
    purrr::map_df( ~ .) %>% .rename_df_columns()

  num_emendas = nrow(emendas_df)

  if (num_emendas == 0) {
    emendas_df <- stats::setNames(
      data.frame(matrix(ncol = length(.COLNAMES_EMENDAS_SENADO), nrow = 0)), names(.COLNAMES_EMENDAS_SENADO)
      )

  } else if (num_emendas == 1) {
    texto <- .generate_dataframe(emendas_df$textos_emenda) %>%
      dplyr::select("tipo_documento", "url_texto")

    autoria <- .generate_dataframe(emendas_df$autoria_emenda)

    autoria <- autoria %>%
      dplyr::mutate(
        partido =
          paste0(
          autoria$identificacao_parlamentar_sigla_partido_parlamentar,
          "/",
          autoria$identificacao_parlamentar_uf_parlamentar)
        )
      )

    emendas_df <- emendas_df %>%
      dplyr::mutate("autor" = autoria$nome_autor,
                    "partido" = autoria$partido,
                    "tipo_documento" = texto$tipo_documento,
                    "inteiro_teor" = texto$url_texto,
                    "id_autor" = autoria$identificacao_parlamentar_codigo_parlamentar,
                    "casa" = 'senado') %>%
      dplyr::select(-"autoria_emenda", -"textos_emenda", numero = "numero_emenda", local = "colegiado_apresentacao")


  } else {
    emendas_df <- emendas_df %>%
      tidyr::unnest() %>%
      dplyr::rename(
          numero = "numero_emenda",
          local = "colegiado_apresentacao",
          autor = "autoria_emenda_autor_nome_autor",
          inteiro_teor = "textos_emenda_texto_emenda_url_texto",
          tipo_documento = "textos_emenda_texto_emenda_tipo_documento",
          partido = "autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar",
          uf = "autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar",
          id_autor = "autoria_emenda_autor_identificacao_parlamentar_codigo_parlamentar"
        )%>%
      dplyr::mutate(
        "partido" = paste0(partido, "/", uf),
        "casa" = "senado"
      )

  }

  emendas_df %>%
    dplyr::mutate("autor" = paste0(autor, " ", partido),
                  "numero" = as.integer(numero),
                  "tipo_documento" = as.character(tipo_documento),
                  "inteiro_teor" = as.character(inteiro_teor)) %>%
      dplyr::select(-dplyr::starts_with("autoria_emenda"),
                    -dplyr::starts_with("textos_emenda"),
                    -dplyr::starts_with("uf")) %>%
    tibble::as_tibble()
}

#' @title Fetches proposition's emendas
#' @description Fetches a dataframe containing the emendas of the proposition
#' @param sigla Proposition type (i.e., PEC, PL, PDC)
#' @param numero Proposition number
#' @param ano Proposition year
#' @return A dataframe containing details about the emendas of the proposition
#' @examples
#' fetch_emendas_camara('pl', 6726, 2016)
#' @rdname fetch_emendas_camara
#' @export
fetch_emendas_camara <- function(sigla=NULL, numero=NULL, ano=NULL) {
  emendas_substitutivos_redacaofinal_list <-
    .get_from_url(base_url = .CAMARA_WEBSITE_LINK,
                  path = .EMENDAS_SUBSTITUTIVOS_REDACAOFINAL_CAMARA_PATH,
                  query = paste0('tipo=', sigla, '&numero=', numero, '&ano=', ano))$content %>%
    rawToChar() %>%
    XML::xmlParse() %>%
    XML::xmlToList()

  emendas_df <-
    emendas_substitutivos_redacaofinal_list %>%
    magrittr::extract2('Emendas') %>%
    purrr::map_dfr(as.list)

  if(nrow(emendas_df) == 0) {
    return(tibble::tibble(codigo_emenda = integer(), data_apresentacao = character(), numero = numeric(), local = character(),
                          autor = character(), casa = character(), tipo_documento = character(), inteiro_teor = character()))
  }

  new_names <- c("cod_proposicao", "descricao")
  names(emendas_df) <- new_names
  emendas <- purrr::map_df(emendas_df$cod_proposicao, .fetch_emendas_camara_auxiliar)
  normalizes_names <- c("codigo_emenda", "data_apresentacao", "numero", "local", "autor", "casa", "tipo_documento", "inteiro_teor")
  names(emendas) <- normalizes_names

  emendas %>%
    dplyr::mutate(data_apresentacao = as.character(as.Date(data_apresentacao))) %>%
    .assert_dataframe_completo(.COLNAMES_EMENDAS_CAMARA)
}

#' @title Auxiliar function for fetch_emendas_camara
#' @description Return dataframe with data of an emenda
.fetch_emendas_camara_auxiliar <- function(id) {
  rcongresso::fetch_proposicao_camara(id) %>%
    dplyr::mutate(autor = .extract_autor_in_camara(.$id)[1,]$autor.nome, casa = "camara") %>%
    dplyr::select(c(id, dataApresentacao, numero, statusProposicao.siglaOrgao, autor, casa, siglaTipo, ementa))
}

#' @title Generates a dataframe from a column with a linked list
#' @description Returns a dataframe from a column with a linked list.
#' @param column Column
#' @return Dataframe
.generate_dataframe <- function (column) {
  as.data.frame(column) %>%
    tidyr::unnest() %>%
    .rename_df_columns()
}
