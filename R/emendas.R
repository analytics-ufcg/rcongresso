
#' @title Returns emendas of a proposição from Senado
#' @description Fetchs a dataframe with emendas's data of a proposição from Senado.
#' @param bill_id Proposição's ID from senado.
#' @return Dataframe with informations about emendas of a proposição from Senado.
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
        partido = paste0(
          autoria$identificacao_parlamentar_sigla_partido_parlamentar,
          "/",
          autoria$identificacao_parlamentar_uf_parlamentar
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
    .assert_dataframe_completo(.COLNAMES_EMENDAS_SENADO) %>%
    .coerce_types(.COLNAMES_EMENDAS_SENADO) %>% 
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
  
  emendas_df %>%
    .assert_dataframe_completo(.COLNAMES_EMENDAS_CAMARA) %>%
    .coerce_types(.COLNAMES_EMENDAS_CAMARA)
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