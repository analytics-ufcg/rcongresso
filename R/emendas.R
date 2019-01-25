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
  url_base_emendas <-
    "http://legis.senado.leg.br/dadosabertos/materia/emendas/"
  url <- paste0(url_base_emendas, bill_id)
  
  json_emendas <- fetch_json_try(url)
  
  emendas_df <- json_emendas %>%
    magrittr::extract2("EmendaMateria") %>%
    magrittr::extract2("Materia") %>% 
    magrittr::extract2("Emendas") %>%
    purrr::map_df( ~ .) %>% rename_df_columns()
  
  num_emendas = nrow(emendas_df)
  
  if (num_emendas == 0) {
    emendas_df <-
      tibble::frame_data( ~ codigo_emenda, ~ data_apresentacao, ~ numero, ~ local, ~ autor, ~ partido, ~ casa, ~ tipo_documento, ~ inteiro_teor)
    
  } else if (num_emendas == 1) {
    texto <- generate_dataframe(emendas_df$textos_emenda) %>%
      dplyr::select(tipo_documento, url_texto)
    
    autoria <- generate_dataframe(emendas_df$autoria_emenda) %>%
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
                    casa = 'senado') 
    
    
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
          "autoria_emenda_autor_identificacao_parlamentar_uf_parlamentar" = "uf"
        )
      ) %>%
      dplyr::mutate(
        partido = paste0(partido, "/", uf),
        casa = "senado"
      ) 
    
  }
  
  emendas_df %>%
    dplyr::mutate(autor = paste0(autor, " ", partido), 
                  numero = as.integer(numero),
                  tipo_documento = as.character(tipo_documento),
                  inteiro_teor = as.character(inteiro_teor)) %>%
    dplyr::select(-partido)
  
}
