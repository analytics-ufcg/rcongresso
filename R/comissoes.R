#' @title Fetches camara comissao
#' @description Fetches a dataframe containing information about the comissao requested
#' @param sigla Comissao's id
#' @return Returns a dataframe containing information about the comissao requested
#' @rdname fetch_comissao_camara
#' @export
fetch_comissao_camara <- function(sigla = NULL) {
  parametros <- as.list(environment(), all = TRUE)
  .camara_api(.ORGAOS_CAMARA_PATH, parametros) %>%
    .assert_dataframe_completo(.COLNAMES_COMISSOES) %>%
    .coerce_types(.COLNAMES_COMISSOES)
}

#' @title Fetches comissions on Senate
#' @description Fetches a dataframe containing information about the comission requested
#' @param sigla Comissao's id
#' @return Returns a dataframe containing information about the comission requested
fetch_composicao_comissoes_senado <- function(sigla) {
  url <- paste0(.SENADO_API_LINK, .ORGAOS_SENADO_PATH, sigla)
  tryCatch(
    {
      json_sessions <- .senado_api(url, asList = T)

      colegiado <-
        json_sessions %>%
        magrittr::extract2('DetalheComissao') %>%
        magrittr::extract2('COLEGIADO') %>%
        magrittr::extract2('COLEGIADO_ROW')

      colegiado[sapply(colegiado, is.null)] <- NULL
      comissao <-
        colegiado %>%
        tibble::as.tibble()

      cargos <-
        comissao %>%
        magrittr::extract2('CARGOS') %>%
        magrittr::extract2('CARGOS_ROW') %>%
        tibble::as.tibble()

      membros <-
        comissao %>%
        magrittr::extract2('MEMBROS_BLOCO') %>%
        magrittr::extract2('MEMBROS_BLOCO_ROW')

      if(!is.null(membros)) {

        if('PARTIDOS_BLOCO.PARTIDOS_BLOCO_ROW' %in% names(membros) |
           "MEMBROS.MEMBROS_ROW" %in% names(membros) &
           typeof(membros$MEMBROS.MEMBROS_ROW) == "list") {
          membros <-
            membros %>%
            dplyr::select(-PARTIDOS_BLOCO.PARTIDOS_BLOCO_ROW) %>%
            tidyr::unnest()
        }
        membros <-
          membros %>%
          tidyr::unnest()

        if (nrow(cargos) == 0 | !('HTTP' %in% names(cargos))) {
          membros %>%
            dplyr::mutate(CARGO = NA) %>%
            dplyr::select(c("CARGO", "@num", "PARTIDO", "UF", "TIPO_VAGA", "PARLAMENTAR"))
        } else {
          if ("MEMBROS.MEMBROS_ROW.HTTP" %in% names(membros)) {
            membros <-
              membros %>%
              dplyr::left_join(cargos, by = c ("MEMBROS.MEMBROS_ROW.HTTP" = "HTTP")) %>%
              dplyr::select(c("CARGO", "@num.x", "MEMBROS.MEMBROS_ROW.PARTIDO", "MEMBROS.MEMBROS_ROW.UF", "MEMBROS.MEMBROS_ROW.TIPO_VAGA", "MEMBROS.MEMBROS_ROW.PARLAMENTAR"))
          }else {
            membros <-
              membros %>%
                dplyr::left_join(cargos, by = 'HTTP') %>%
                dplyr::select(c("CARGO", "@num.x", "PARTIDO", "UF", "TIPO_VAGA", "PARLAMENTAR.x"))
          }
        }
      }else {
        membros <-
          tibble::frame_data(~ CARGO, ~ num.x, ~ PARTIDO, ~ UF, ~ TIPO_VAGA, ~ PARLAMENTAR.x)
      }

      membros %>%
        .assert_dataframe_completo(.COLNAMES_COMISSOES_SENADO) %>%
        .coerce_types(.COLNAMES_COMISSOES_SENADO)

    },
    error=function(cond) {
      return(tibble::frame_data(~ CARGO, ~ num.x, ~ PARTIDO, ~ UF, ~ TIPO_VAGA, ~ PARLAMENTAR.x) %>%
               .assert_dataframe_completo(.COLNAMES_COMISSOES_SENADO) %>%
               .coerce_types(.COLNAMES_COMISSOES_SENADO))
    }
  )
}
