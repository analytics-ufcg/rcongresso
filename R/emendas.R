
#' @title Returns emendas of a proposição from Chamber of deputies or Senate
#' @description Fetchs a dataframe with emendas's data of a proposição from Chamber of deputies or Senate
#' @param id Proposição's ID from congress
#' @param casa senado or camara
#' @param sigla Proposition type (i.e., PEC, PL, PDC)
#' @param numero Proposition number
#' @param ano Proposition year
#' @return Dataframe with informations about emendas of a proposição from Chamber of deputies or Senate
#' @examples
#' \dontrun{
#' fetch_emendas(91341,'senado')
#' }
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

  emendas_raw_df <- 
    json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia") %>%
    magrittr::extract2("Emendas") %>%
    magrittr::extract2("Emenda")
  
  parsed_emendas <- emendas_raw_df %>% 
    .parse_emendas_senado()
}

#' @title Parses emendas json data received from Senado API
#' @description Parses emendas's data of a proposição from Senado.
#' @param emendas_raw_df Dataframe with raw data about the emendas of a proposição.
#' @return Dataframe with structured information about emendas of a proposição from Senado.
.parse_emendas_senado <- function(emendas_raw_df) {
  base_emendas_cols <- c("CodigoEmenda", "NumeroEmenda", "DataApresentacao", 
                         "ColegiadoApresentacao", "DescricaoTurno", "DescricaoTipoEmenda")

  if (is.null(emendas_raw_df) || (nrow(emendas_raw_df) == 0)) {
    emendas_full <- stats::setNames(
        data.frame(matrix(ncol = length(.COLNAMES_EMENDAS_SENADO), nrow = 0)), 
        names(.COLNAMES_EMENDAS_SENADO)) %>% 
      tibble::as.tibble()
    return(emendas_full)
  }
    
  emendas_df <- emendas_raw_df
  subemendas_df <- tibble::tibble()
  textos_emendas <- emendas_df %>% dplyr::select_at(base_emendas_cols)
  autoria_emendas <- emendas_df %>% dplyr::select_at(base_emendas_cols)
  decisao_emendas <- emendas_df %>% dplyr::select_at(base_emendas_cols)
  
  if ('TextosEmenda.TextoEmenda' %in% names(emendas_df)) {
    textos_emendas <- .unnest_df_column(emendas_df,base_emendas_cols,'TextosEmenda.TextoEmenda')
    emendas_df <- emendas_df %>% dplyr::select(-TextosEmenda.TextoEmenda)
  }
  
  if ('AutoriaEmenda.Autor' %in% names(emendas_df)) {
    autoria_emendas <- .unnest_df_column(emendas_df,base_emendas_cols,'AutoriaEmenda.Autor')
    emendas_df <- emendas_df %>% dplyr::select(-AutoriaEmenda.Autor)
  }
  
  if (sum(stringr::str_ends(names(emendas_df),'Decisao')) > 0) {
    decisao_column <- names(emendas_df)[stringr::str_ends(names(emendas_df),'Decisao')]
    decisao_emendas <-.unnest_df_column(emendas_df,base_emendas_cols,decisao_column)
    emendas_df <- emendas_df %>% dplyr::select(-all_of(decisao_column))
  }
  
  
  if ('Subemendas.Subemenda' %in% names(emendas_df)) {
    subemendas_df <- .parse_emendas_senado(emendas_df$Subemendas.Subemenda %>% purrr::map_df( ~ .))
    emendas_df <- emendas_df %>% dplyr::select(-Subemendas.Subemenda)
  }
  
  emendas_full <-
    emendas_df %>% 
    dplyr::select_at(base_emendas_cols) %>% 
    dplyr::left_join(textos_emendas, by=base_emendas_cols) %>% 
    dplyr::left_join(autoria_emendas, by=base_emendas_cols) %>% 
    dplyr::left_join(decisao_emendas, by=base_emendas_cols) %>% 
    .rename_df_columns() %>% 
    dplyr::distinct()
  
  num_emendas = nrow(emendas_full)
  
  parsed_emendas <- emendas_full %>%
    tidyr::unnest(cols = c()) %>%
    dplyr::rename(numero = numero_emenda,
                  local = colegiado_apresentacao,
                  autor = nome_autor,
                  inteiro_teor = url_texto,
                  partido = identificacao_parlamentar_sigla_partido_parlamentar,
                  uf = identificacao_parlamentar_uf_parlamentar,
                  id_autor = identificacao_parlamentar_codigo_parlamentar) %>%
    dplyr::mutate(partido = paste0(partido, "/", uf)) %>% 
    dplyr::mutate(casa = "senado",
                  autor = paste0(autor, " ", partido),
                  numero = suppressWarnings(ifelse(is.na(as.integer(numero)),0,as.integer(numero))),
                  tipo_documento = as.character(tipo_documento),
                  inteiro_teor = as.character(inteiro_teor)) %>%
    dplyr::select(-dplyr::starts_with("autoria_emenda"),
                  -dplyr::starts_with("textos_emenda"),
                  -dplyr::starts_with("uf")) %>%
    tibble::as_tibble() %>% 
    dplyr::bind_rows(subemendas_df) %>% 
    dplyr::arrange(codigo_emenda, numero, data_apresentacao) %>% 
    dplyr::group_by(codigo_emenda, numero, descricao_tipo_emenda, data_apresentacao, local, descricao_turno) %>% 
    dplyr::arrange({if ('decisao_data' %in% names(.)) decisao_data else data_apresentacao}) %>% 
    dplyr::filter(dplyr::row_number() == dplyr::n()) %>% 
    dplyr::ungroup()
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
    .get_with_exponential_backoff_cached(base_url = .CAMARA_WEBSITE_LINK,
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
 rcongresso::fetch_proposicao(id, 'camara') %>%
    dplyr::mutate(autor = rcongresso::scrap_autores_from_website(.$id), casa = "camara") %>%
    dplyr::select(c(id, dataApresentacao, numero, statusProposicao.siglaOrgao, autor, casa, siglaTipo, urlInteiroTeor))
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
