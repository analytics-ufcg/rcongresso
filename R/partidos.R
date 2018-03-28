
fetch_partido <- function(id = NULL, sigla = NULL, dataInicio = NULL,
                          dataFim = NULL, idLegislatura = NULL, itens = NULL, ordenarPor = NULL){

  parametros <- as.list(environment(), all=TRUE)

  if(!length(.verifica_parametros_entrada(parametros)))
    .congresso_api(.PARTIDOS_PATH) %>%
    .assert_dataframe_completo(.COLNAMES_PARTIDOS) %>%
    .coerce_types(.COLNAMES_PARTIDOS)
  else if(is.null(id))
    .fetch_using_queries(parametros, .PARTIDOS_PATH)%>%
    .assert_dataframe_completo(.COLNAMES_PARTIDOS) %>%
    .coerce_types(.COLNAMES_PARTIDOS)
  else
    .fetch_using_id(id, .PARTIDOS_PATH) %>%
    .assert_dataframe_completo(.COLNAMES_PARTIDOS_ID) %>%
    .coerce_types(.COLNAMES_PARTIDOS_ID)
}
