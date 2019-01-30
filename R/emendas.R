#' @title Retorna as emendas de uma proposição no Congresso
#' @description Retorna dataframe com os dados das emendas de uma proposição no Congresso.
#' @param bill_id ID de uma proposição do Congresso
#' @return Dataframe com as informações sobre as emendas de uma proposição no Congresso.
#' @examples
#' fetch_emendas(91341,'senado')
#' @export
fetch_emendas <- function(id, casa) {
  casa <- tolower(casa)
  if (casa == 'camara') {
    emendas <- fetch_emendas_camara(id)
  } else if (casa == 'senado') {
    emendas <- fetch_emendas_senado(id)
  } else {
    print('Parâmetro "casa" não identificado.')
    return()
  }
  
  emendas  <-
    emendas %>%
    dplyr::mutate(prop_id = id, codigo_emenda = as.integer(codigo_emenda)) %>%
    dplyr::select(
      prop_id, codigo_emenda, data_apresentacao, numero, local, autor, casa, tipo_documento, inteiro_teor) 
}


#' @title Retorna as emendas de uma proposição no Senado
#' @description Retorna dataframe com os dados das emendas de uma proposição no Senado.
#' @param bill_id ID de uma proposição do Senado
#' @return Dataframe com as informações sobre as emendas de uma proposição no Senado.
fetch_emendas_senado <- function(bill_id) {
  url <- paste0(.SENADO_API_LINK, .EMENDAS_SENADO_PATH, bill_id)
  
  json_emendas <- fetch_json_try(url)
  
  emendas_df <- json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia") %>% 
    magrittr::extract2("Emendas") %>%
    purrr::map_df( ~ .) %>% rename_df_columns()
  
  num_emendas = nrow(emendas_df)
  
  if (num_emendas == 0) {
    emendas_df <- setNames(
      data.frame(matrix(ncol = length(.COLNAMES_EMENDAS), nrow = 0)), names(.COLNAMES_EMENDAS)
      )
    
  } else if (num_emendas == 1) {
    texto <- .generate_dataframe(emendas_df$textos_emenda) %>%
      dplyr::select(tipo_documento, url_texto)
    
    autoria <- .generate_dataframe(emendas_df$autoria_emenda) %>%
      dplyr::mutate(
        partido = paste0(
          identificacao_parlamentar_sigla_partido_parlamentar,
          "/",
          identificacao_parlamentar_uf_parlamentar
        )
      )
    
    emendas_df <- emendas_df %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local"
        )
      ) %>%
      dplyr::mutate(autor = autoria$nome_autor,
                    partido = autoria$partido,
                    tipo_documento = texto$tipo_documento,
                    inteiro_teor = texto$url_texto,
                    partido = autoria$partido,
                    id_autor = autoria$identificacao_parlamentar_codigo_parlamentar,
                    casa = 'senado') %>% 
      dplyr::select(-autoria_emenda, -textos_emenda)
    
    
  } else{
    emendas_df <- emendas_df %>%
      tidyr::unnest() %>%
      plyr::rename(
        c(
          "numero_emenda" = "numero",
          "colegiado_apresentacao" = "local",
          "autoria_emenda_autor_nome_autor" = "autor",
          "textos_emenda_texto_emenda_url_texto" = "inteiro_teor",
          "textos_emenda_texto_emenda_tipo_documento" = "tipo_documento",
          "autoria_emenda_autor_identificacao_parlamentar_sigla_partido_parlamentar" = "partido",
          "autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar" = "uf",
          "autoria_emenda_autor_identificacao_parlamentar_codigo_parlamentar" = "id_autor"
        )
      ) %>%
      dplyr::mutate(
        partido = paste0(partido, "/", uf),
        casa = "senado"
      ) %>% 
      dplyr::select(-dplyr::starts_with("autoria_emenda"), 
                    -dplyr::starts_with("textos_emenda"),
                    -uf)
    
  }
  
  emendas_df %>%
    dplyr::mutate(autor = paste0(autor, " ", partido), 
                  numero = as.integer(numero),
                  tipo_documento = as.character(tipo_documento),
                  inteiro_teor = as.character(inteiro_teor)) 
  
}

#' @title Retorna um dataframe a partir de uma coluna com listas encadeadas
#' @description Retorna um dataframe a partir de uma coluna com listas encadeadas.
#' @param column Coluna
#' @return Dataframe com as informações provenientes de uma coluna com listas encadeadas.
#' @examples
#' generate_dataframe(column)
#' @export
.generate_dataframe <- function (column) {
  as.data.frame(column) %>%
    tidyr::unnest() %>%
    rename_df_columns()
}
