#' @title Fetches proposition from API using a query
#' @description Fetches information about law's projects, resolutions, provisional measures,
#' law amendments, opinions and all the other propositions types on the
#' Deputies' Chamber.
#' Several parameters can be used to select and filter the final result. By default, the function
#' returns all the proposition which were presented or had some situation change in the last
#' 15 days.
#' @param id Proposition's ID
#' @param siglaUfAutor State's abbreviation of the proposition's author
#' @param siglaTipo Proposition type (i.e., PEC, PL, PDC)
#' @param siglaPartidoAutor Party's abbreviation of the proposition's author
#' @param numero Proposition number
#' @param ano Proposition year
#' @param dataApresentacaoInicio Proposition's presentation starting date
#' @param dataApresentacaoFim Proposition's presentation end date
#' @param dataInicio Proposition's processing starting date
#' @param dataFim Proposition's processing end date
#' @param idAutor Author's ID
#' @param autor Author's name
#' @param codPartido Party code
#' @param itens Items quantity. '-1' returns all the propositions which had been processed
#' in the last 30 days
#' @return Dataframe containing information about the proposition.
#' @details Note that if you have the proposition's ID, it's not necessary to add any other parameter on the
#' function call. The call to this function using the proposition's ID returns more details than using the
#' others parameters. If you don't have the proposition's ID, the \code{\link[rcongresso]{fetch_id_proposicao_camara}}
#' function may be helpful.
#' @examples
#' pec241 <- fetch_proposicao_camara(id = 2088351)
#' pec241 <- fetch_proposicao_camara(siglaTipo = "PEC", numero = 241, ano = 2016)
#' @seealso
#'  \code{\link[rcongresso]{fetch_tipo_proposicao}}, \code{\link[rcongresso]{fetch_id_proposicao_camara}}
#' @rdname fetch_proposicao_camara
#' @export
fetch_proposicao_camara <- function(id = NULL, siglaUfAutor = NULL, siglaTipo = NULL,
                             siglaPartidoAutor = NULL, numero = NULL, ano = NULL,
                             dataApresentacaoInicio = NULL, dataApresentacaoFim = NULL,
                             dataInicio = NULL, dataFim = NULL, idAutor = NULL,
                             autor = NULL, codPartido = NULL, itens = NULL) {

  parametros <- as.list(environment(), all=TRUE)

  if ( !length(.verifica_parametros_entrada(parametros))) {
    .camara_api(.CAMARA_PROPOSICOES_PATH) %>%
      .assert_dataframe_completo(.COLNAMES_PROPOSICAO_CAMARA) %>%
      .coerce_types(.COLNAMES_PROPOSICAO_CAMARA)
  } else if ( is.null(id)) {
    .fetch_using_queries(parametros, .CAMARA_PROPOSICOES_PATH)%>%
      .assert_dataframe_completo(.COLNAMES_PROPOSICAO_CAMARA) %>%
      .coerce_types(.COLNAMES_PROPOSICAO_CAMARA)
  } else {
    .fetch_using_id(id, .CAMARA_PROPOSICOES_PATH)%>%
      .assert_dataframe_completo(.COLNAMES_PROPOSICAO_POR_ID_CAMARA) %>%
      .coerce_types(.COLNAMES_PROPOSICAO_POR_ID_CAMARA)
  }
}

#' @title Fetches a proposition in the Senate
#' @description Returns the proposition info
#' @param id Proposition's ID
#' @return Dataframe containing all the info about the proposition;
#' @examples
#' prop_pls229 <- fetch_proposicao_senado(91341)
#' @rdname fetch_proposicao_senado
#' @export
fetch_proposicao_senado <- function(id = NULL) {
  proposicao_data <- .senado_api(paste0(.SENADO_PROPOSICAO_PATH, id), asList = TRUE)$DetalheMateria$Materia

  proposicao_ids <-
    proposicao_data %>%
    magrittr::extract2("IdentificacaoMateria") %>%
    tibble::as.tibble()

  proposicao_info <-
    proposicao_data %>%
    magrittr::extract2("DadosBasicosMateria") %>%
    purrr::flatten() %>%
    tibble::as.tibble()

  proposicao_author <-
    proposicao_data %>%
    magrittr::extract2("Autoria") %>%
    magrittr::extract2("Autor") %>%
    dplyr::transmute(
      autor = paste(
        paste(NomeAutor,
              ifelse("IdentificacaoParlamentar.SiglaPartidoParlamentar" %in% names(.),
                     IdentificacaoParlamentar.SiglaPartidoParlamentar, "")),
          ifelse("UfAutor" %in% names(.), paste("/", UfAutor), "")))

  proposicao_specific_assunto <-
    proposicao_data$Assunto$AssuntoEspecifico %>%
    tibble::as.tibble()
  if (nrow(proposicao_specific_assunto) == 0) {
    proposicao_specific_assunto <-
      tibble::tribble(~ codigo_assunto_especifico, ~ assunto_especifico,
                      0, "Nao especificado")
  }else {
    proposicao_specific_assunto <-
      proposicao_specific_assunto %>%
      dplyr::rename(assunto_especifico = Descricao, codigo_assunto_especifico = Codigo)
  }
  proposicao_general_assunto <-
    proposicao_data$Assunto$AssuntoGeral %>%
    tibble::as.tibble()
  if (nrow(proposicao_general_assunto) == 0) {
    proposicao_general_assunto <-
      tibble::tribble(~ codigo_assunto_geral, ~ assunto_geral,
                      0, "Nao especificado")
  }else {
    proposicao_general_assunto <-
      proposicao_general_assunto %>%
      dplyr::rename(assunto_geral = Descricao, codigo_assunto_geral = Codigo)
  }

  proposicao_source <-
    proposicao_data %>%
    magrittr::extract2("OrigemMateria") %>%
    tibble::as.tibble()

  anexadas <-
    proposicao_data$MateriasAnexadas$MateriaAnexada$IdentificacaoMateria.CodigoMateria
  relacionadas <-
    proposicao_data$MateriasRelacionadas$MateriaRelacionada$IdentificacaoMateria.CodigoMateria

  proposicao_complete <-
    proposicao_info %>%
    tibble::add_column(
      !!!proposicao_ids,
      !!!proposicao_specific_assunto,
      !!!proposicao_general_assunto,
      !!!proposicao_source,
      autor_nome = proposicao_author[[1]] %>% tail(1),
      proposicoes_relacionadas = paste(relacionadas, collapse = " "),
      proposicoes_apensadas = paste(anexadas, collapse = " ")
    )

  proposicao_complete <-
    proposicao_complete[,!sapply(proposicao_complete, is.list)] %>%
    rename_table_to_underscore()

  proposicao_complete[, names(proposicao_complete) %in% names(.COLNAMES_PROPOSICAO_SENADO)] %>%
    .assert_dataframe_completo(.COLNAMES_PROPOSICAO_SENADO) %>%
      .coerce_types(.COLNAMES_PROPOSICAO_SENADO)
}

# fetch_textos_proposicao <- function(id) {
#   proposicao_data <- .senado_api(paste0(.SENADO_TEXTOS_MATERIA, id), asList = TRUE)$TextoMateria$Materia
#
#   if (is.null(proposicao_data$Textos)) {
#     proposicao_complete <-
#       tibble::as_tibble()
#   } else {
#     proposicao_ids <-
#       proposicao_data %>%
#       magrittr::extract2("IdentificacaoMateria") %>%
#       tibble::as_tibble()
#
#     proposicao_texto <-
#       proposicao_data %>%
#       magrittr::extract2("Textos") %>%
#       magrittr::extract2("Texto") %>%
#       tibble::as_tibble()
#
#     proposicao_complete <-
#       proposicao_texto %>%
#       tibble::add_column(
#         !!!proposicao_ids)
#
#     proposicao_complete <-
#       proposicao_complete %>%
#       dplyr::filter(DescricaoTipoTexto %in% c("Avulso de requerimento", "Requerimento")) %>%
#       tibble::as_tibble()
#   }
#   return(proposicao_complete)
# }


# .extract_descricao_requerimento <- function(id) {
#   proposicao_data <- .senado_api(paste0(.SENADO_TEXTOS_MATERIA, id), asList = TRUE)
#   proposicao_data <- proposicao_data$TextoMateria$Materia
#
#   if (is.null(proposicao_data$Textos)) {
#     descricao_df <-
#       tibble::as_tibble()
#
#   } else {
#     cod_texto <-
#       proposicao_data %>%
#       magrittr::extract2("Textos") %>%
#       magrittr::extract2("Texto") %>%
#       magrittr::extract2("CodigoTexto") %>%
#       tibble::as_tibble()
#
#     req_numero <-
#       proposicao_data %>%
#       magrittr::extract2("Textos") %>%
#       magrittr::extract2("Texto") %>%
#       magrittr::extract2("DescricaoTexto") %>%
#       tibble::as_tibble()
#
#     comissao <-
#       proposicao_data %>%
#       magrittr::extract2("Textos") %>%
#       magrittr::extract2("Texto") %>%
#       magrittr::extract2("IdentificacaoComissao.SiglaComissao") %>%
#       tibble::as_tibble()
#
#     descricao_texto <-
#       proposicao_data %>%
#       magrittr::extract2("Textos") %>%
#       magrittr::extract2("Texto") %>%
#       magrittr::extract2("DescricaoTipoTexto") %>%
#       tibble::as_tibble()
#
#     descricao_df <- data.frame(cod_texto, req_numero, comissao, descricao_texto)
#     descricao_df <- descricao_df %>%
#       dplyr::filter(value.3 %in% c("Avulso de requerimento", "Requerimento", "Requerimento."))
#
#     descricao_df$SiglaRequerimento <- unlist(strsplit(descricao_df$value.1, " "))[1]
#     descricao_df$numero_ano <- unlist(strsplit(descricao_df$value.1, " "))[2]
#
#     descricao_df$descricao_req <-
#       paste0(descricao_df$SiglaRequerimento, "/", descricao_df$numero_ano, "?comissao=", descricao_df$value.2)
#   }
#
#   return(descricao_df)
# }

#' @title Fetches all propositions related to a proposition
#' @description Returns all propositions related to a proposition by its id.
#' @param id_casa Proposition's ID
#' @param casa senado or camara
#' @return Dataframe containing all the related propositions.
#' @examples
#' \dontrun{
#' relacionadas_pec241 <- fetch_relacionadas("camara",2088351)
#' }
#' @seealso
#'   \code{\link[rcongresso]{fetch_id_proposicao_camara}}
#' @rdname fetch_relacionadas_camara
#' @export
fetch_relacionadas <- function(casa, id_casa){
  casa <- tolower(casa)
  if (casa == "camara") {
    .fetch_relacionadas_camara(id_casa)
  } else if (casa == "senado") {
    .fetch_relacionadas_senado(id_casa)
  } else {
    return("Parametro 'casa' nao identificado.")
  }
}

#' @title Fetches all propositions'ids related to a proposition
#' @description Returns all propositions'ids related to a proposition.
#' @param id Proposition's ID
#' @param casa Câmara or sendo
#' @return Dataframe containing all the related propositions'ids.
#' @rdname fetch_ids_relacionadas
#' @export
fetch_ids_relacionadas <- function(id, casa) {
  relacionadas <- tibble::tibble()
  if (casa == "camara") {
  relacionadas <- .fetch_relacionadas_camara(id)
    if (nrow(relacionadas) == 0) {
      warning("A proposição não possui documentos relacionados.")
    } else {
      relacionadas <- relacionadas %>%
        dplyr::select(id_relacionada = id,
                      id_prop) %>%
        dplyr::mutate(casa = "camara")
    }

  } else if (casa == "senado") {
   relacionadas <- .fetch_relacionadas_senado(id)
   if (nrow(relacionadas) == 0) {
     warning("A proposição não possui documentos relacionados.")
   } else {
     relacionadas <- relacionadas %>%
       dplyr::mutate(id_prop = id,
                     casa = "senado")
   }
  } else {
    warning("Parâmetro 'casa' não identificado")
  }

  return(relacionadas)
}


#' @title Fetches all propositions related to a proposition
#' @description Returns all propositions related to a proposition by its id.
#' @param id_prop Proposition's ID
#' @return Dataframe containing all the related propositions.
#' @seealso
#'   \code{\link[rcongresso]{fetch_id_proposicao_camara}}
#' @rdname fetch_relacionadas_camara
.fetch_relacionadas_camara <- function(id_prop){
  path <- NULL
  unique(id_prop) %>%
    as.integer %>%
    tibble::tibble(id_prop = .) %>%
    dplyr::mutate(path = paste0(.CAMARA_PROPOSICOES_PATH, "/", id_prop, "/relacionadas")) %>%
    dplyr::group_by(id_prop, path) %>%
      dplyr::do(
               .camara_api(.$path)
             ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-path) %>%
      dplyr::distinct() %>% #Remove duplicate relacionadas
      .assert_dataframe_completo(.COLNAMES_RELACIONADAS) %>%
      .coerce_types(.COLNAMES_RELACIONADAS)
}

#' @title Fetches all propositions related to a proposition
#' @description Returns all propositions related to a proposition by its id.
#' @param id_prop Proposition's ID
#' @return Dataframe containing all the related propositions.
.fetch_relacionadas_senado <- function(id_prop) {
  relacionadas_prop <- fetch_proposicao_senado(id_prop)
  if (relacionadas_prop$proposicoes_relacionadas == "") {
    return(tibble::tibble())
  } else {
    relacionadas_ids <- unlist(strsplit(relacionadas_prop$proposicoes_relacionadas, " "))
    relacionadas_complete <- tibble::tibble(id_relacionada = relacionadas_ids)
    return(relacionadas_complete)
  }
}

# .fetch_relacionadas_senado <- function(id_prop) {
#   relacionadas_textos <- fetch_textos_proposicao(id_prop)
#   relacionadas_prop <- fetch_proposicao_senado(id_prop)
#
#   relacionadas_textos <- .rename_df_columns(relacionadas_textos)
#
#   relacionadas_ids <- unlist(strsplit(relacionadas_prop$proposicoes_relacionadas, " "))
#   relacionadas_req <- purrr::map_df(relacionadas_ids, ~ fetch_proposicao_senado(.x))
#
#   if (nrow(relacionadas_req) == 0) {
#     relacionadas <- relacionadas_textos
#   } else {
#     relacionadas_textos <- relacionadas_textos %>%
#       dplyr::select(-codigo_materia) %>%
#       dplyr::rename(codigo_materia = codigo_texto)
#
#     relacionadas <-
#       dplyr::full_join(relacionadas_req, relacionadas_textos, by = "codigo_materia")
#   }
#
#   if (nrow(relacionadas) == 0) {
#     print("A proposição não possui documentos relacionados.")
#   } else {
#     relacionadas_complete <- relacionadas %>%
#       .assert_dataframe_completo(.COLNAMES_RELACIONADAS_SENADO) %>%
#       .coerce_types(.COLNAMES_RELACIONADAS_SENADO)
#   }
#
# }

#' @title Retrieves the proposition ID from its type, number and year
#' @description The function can be used to fetch a vector of ids as well, in case of many propositions.
#' @param tipo Proposition type (i.e., PEC, PL, PDC)
#' @param numero Proposition number
#' @param ano Proposition year
#' @return Proposition's ID.
#' @examples
#' pec241_id <- fetch_id_proposicao_camara("PEC", 241, 2016)
#' @seealso
#'   \code{\link[rcongresso]{fetch_id_partido}}
#' @rdname fetch_id_proposicao_camara
#' @export
fetch_id_proposicao_camara <- function(tipo, numero, ano){
  tibble::tibble(tipo, numero, ano) %>%
    dplyr::rowwise() %>%
    dplyr::do(
      .camara_api(.CAMARA_PROPOSICOES_PATH,
                     list(siglaTipo = .$tipo, numero = .$numero, ano = .$ano,
                          ordem = "ASC", ordenarPor = "id", dataInicio = paste0(ano,"-01-01")))$id %>%
        .verifica_id(.WARNING_PROPOSICAO_ID) %>%
        .to_tibble()
    ) %>%
    unlist() %>%
    as.vector()
}

#' Fetches all the proposition types.
#'
#' @return Proposition types
#'
#' @export
fetch_tipos_proposicao <- function(){
  .camara_api(.TIPOS_PROPOSICOES_PATH)
}

#' @title Fetches the type of the proposition from its id
#' @description Returns its id, abbreviation, name and description.
#' @param id_tipo_prop Proposition's type ID
#' @return Dataframe containing the proposition's type info.
#' @examples
#' tipo_prop129 <- fetch_tipo_proposicao(129)
#' @rdname fetch_tipo_proposicao
#' @export
fetch_tipo_proposicao <- function(id_tipo_prop){
  prop_types <- fetch_tipos_proposicao() %>%
    dplyr::mutate(cod = as.numeric(.$cod))

  tibble::tibble(cod = id_tipo_prop) %>%
    dplyr::left_join(prop_types, by = "cod") %>%
    .assert_dataframe_completo(.COLNAMES_TIPO_PROPOSICAO) %>%
    .coerce_types(.COLNAMES_TIPO_PROPOSICAO)
}

#' @title Fetches proposition's author
#' @description Fetches a dataframe containing information about the author of the proposition
#' @param proposicao_id Proposition's ID
#' @return A dataframe containing details about the author of the proposition
#' @examples
#' fetch_autor_camara(2121442)
#' @export
fetch_autor_camara <- function (proposicao_id = NULL) {
  autor_uri <- paste0(.CAMARA_PROPOSICOES_PATH, '/', proposicao_id, "/autores")
  autor_info <- .camara_api(autor_uri)
  if(any(is.na(autor_info$uri))){
    autores <- .camara_api(autor_uri) %>%
      .assert_dataframe_completo(.COLNAMES_AUTORES) %>%
      .coerce_types(.COLNAMES_AUTORES)
  } else {
    autores <- purrr::map_df(autor_info$uri, ~.auxiliary_fetch_autor_camara(.x)) %>%
      dplyr::left_join(
        autor_info %>% dplyr::select(-nome),
        by = "uri")
  }

  return(autores)

}

#' @title Fetches proposition's authors
#' @description Fetches a dataframe containing basic information about the authors of the proposition
#' @param proposicao_id Proposition's ID
#' @param casa senado or camara
#' @return A dataframe containing the basic information about the authors of the proposition
#' @examples
#' \dontrun{
#' fetch_autores(2121442, 'camara')
#' fetch_autores(91341, 'senado')
#' }
#' @export
fetch_autores <- function(proposicao_id = NULL, casa) {
  if (casa == "camara") {
    .fetch_autores_camara(proposicao_id)
  } else if (casa == "senado") {
    .fetch_autores_senado(proposicao_id)
  } else {
    return("Parâmetro 'casa' não identificado.")
  }
}

#' @title Fetches proposition's authors
#' @description Fetches a dataframe containing basic information about the authors of the proposition
#' @param proposicao_id Proposition's ID
#' @return A dataframe containing the basic information about the authors of the proposition
#' @examples
#' \dontrun{
#' .fetch_autores_senado(91341)
#' }
#' @export
.fetch_autores_senado <- function(proposicao_id) {
  autor_data <- .senado_api(paste0(.SENADO_PROPOSICAO_PATH, proposicao_id),
                            asList = TRUE)$DetalheMateria$Materia$Autoria$Autor

  autores_complete <-
    .rename_df_columns(autor_data)

  if(ncol(autores_complete) < 6) {
    autores_complete <- autores_complete %>%
      dplyr::mutate(id_parlamentar = NA,
                    uf_autor = NA,
                    nome = NA,
                    nome_completo = NA,
                    sexo = NA,
                    forma_de_tratamento = NA,
                    url_foto = NA,
                    url_pagina = NA,
                    email = NA,
                    sigla_partido = NA,
                    uf_parlamentar = NA)
  } else {
    autores_complete <- autores_complete %>%
      dplyr::mutate(id_parlamentar = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_codigo_parlamentar'),
                    nome = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_nome_parlamentar'),
                    nome_completo = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_nome_completo_parlamentar'),
                    sexo = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_sexo_parlamentar'),
                    forma_de_tratamento = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_forma_tratamento'),
                    url_foto = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_url_foto_parlamentar'),
                    url_pagina = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_url_pagina_parlamentar'),
                    email = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_email_parlamentar'),
                    sigla_partido = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_sigla_partido_parlamentar'),
                    uf_parlamentar = .safe_get_value_coluna(autores_complete,'identificacao_parlamentar_uf_parlamentar'))
  }
  
  unwanted_cols <- names(autores_complete)[startsWith(names(autores_complete), 'identificacao_parlamentar')]
  
  # quebra aqui
  autores_complete <- autores_complete %>%
    dplyr::select(-unwanted_cols)

  autores_complete %>%
      .assert_dataframe_completo(.COLNAMES_AUTORES_SENADO) %>%
      .coerce_types(.COLNAMES_AUTORES_SENADO)
}

#' @title Fetches proposition's authors
#' @description Fetches a dataframe containing basic information about the authors of the proposition
#' @param proposicao_id Proposition's ID
#' @param sigla_tipo Proposition's initials
#' @return A dataframe containing the basic information about the authors of the proposition
#' @examples
#' \dontrun{
#' .fetch_autores_camara(2121442)
#' @export
.fetch_autores_camara <- function (proposicao_id = NULL, sigla_tipo = "" ) {
  autor_uri <- paste0(.CAMARA_PROPOSICOES_PATH, '/', proposicao_id, "/autores")
  autores_info <- .camara_api(autor_uri) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(id_autor = dplyr::if_else(!is.na(uri),
                                            stringr::str_split(uri, '/')[[1]] %>%
                                              dplyr::last() %>%
                                              as.numeric(),-1),
                  uri = dplyr::if_else(!is.na(uri), as.character(uri), "")) %>%
    dplyr::ungroup() %>%
    dplyr::select(id_autor, nome, cod_tipo = codTipo, tipo, uri)
  if(sigla_tipo %in% c("EMC","PEC")){
    scrap_df <- tibble::tibble(nome = scrap_autores_from_website(proposicao_id)) %>%
      tidyr::separate_rows(nome, sep=", ") %>%
      tidyr::separate(nome, c("nome","partido_uf"), sep=' - ', extra = "drop", fill = "right") %>%
      dplyr::select(-partido_uf)
    autores_info <- autores_info %>%
      dplyr::inner_join(scrap_df, by = "nome")
  }

  return(autores_info)
}

#' @export
fetch_autores_camara <- function (proposicao_id = NULL, sigla_tipo = "" ) {
  .fetch_autores_camara(proposicao_id,sigla_tipo)
}


#' @title Scraps autores da proposição from website
#' @description Return the author(s) name(s)
#' @param id_prop proposição's ID
#' @return String with authors names separated by comma
#' @export
scrap_autores_from_website <- function(id_prop) {
  autores_prop_text <-
    .get_from_url(paste0(.CAMARA_WEBSITE_LINK_2, .AUTORES_CAMARA_PATH, "?idProposicao=", id_prop))%>%
    httr::content('text', encoding = 'utf-8') %>%
    xml2::read_html()  %>%
    rvest::html_nodes('#content') %>%
    rvest::html_nodes('span') %>%
    rvest::html_text()
  Sys.sleep(2)

  paste0(autores_prop_text[3:length(autores_prop_text)], collapse = ", ")
}


#' @title Retrieves details about an author of a proposition
#' @description Fetches a dataframe containing detailed information about the author of the proposition
#' @param uri URL relative to the Deputy url
#' @return A dataframe containing details about the author of a proposition
#' @export
#' @examples
#' .auxiliary_fetch_autor_camara('https://dadosabertos.camara.leg.br/api/v2/deputados/178854')
.auxiliary_fetch_autor_camara <- function(uri) {
  strsplit(uri, '/')[[1]] %>% tail(1) %>%
    .fetch_using_id(.DEPUTADOS_PATH)
}

#' @title Fetch the propositions appended to a proposition in the Camara
#' @description Returns a vector containing the ids of the appended propositions
#' @param prop_id Proposition's ID
#' @return A vector of characters containing the ids of the appended propositions
#' @examples
#' fetch_apensadas_camara(2121442)
#' @export
fetch_apensadas_camara <- function(prop_id) {
  .get_from_url(.CAMARA_WEBSITE_LINK, .APENSADAS_CAMARA_PATH, paste0('idProp=', prop_id)) %>%
    xml2::read_xml() %>%
    xml2::xml_find_all('//apensadas/proposicao/codProposicao') %>%
    xml2::xml_text()
}
