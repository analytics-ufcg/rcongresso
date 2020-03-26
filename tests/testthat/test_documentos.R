context("Proposições")

documentos_endpoint_completos_115926 <<- fetch_textos_proposicao_senado(115926)
documentos_endpoint_completos_135061 <<- fetch_textos_proposicao_senado(135061)

documentos_scrap_completos_115926 <<- scrap_senado_congresso_documentos(115926, 'senado', F)
documentos_scrap_filtrados_115926 <<- scrap_senado_congresso_documentos(115926, 'senado', T)
documentos_scrap_completos_135061 <<- scrap_senado_congresso_documentos(135061, 'congresso', F)
documentos_scrap_completo_41703 <<- 
  scrap_senado_congresso_documentos(41703, 'senado', F) %>% 
  dplyr::mutate(acao_legislativa = trimws(acao_legislativa,which = "b"),
                identificacao = trimws(identificacao,which = "b"))
documentos_scrap_filtrado_41703 <<- 
  scrap_senado_congresso_documentos(41703, 'senado', T) %>% 
  dplyr::mutate(acao_legislativa = trimws(acao_legislativa,which = "b"),
                identificacao = trimws(identificacao,which = "b"))
documentos_scrap_completo_41703_gabarito <<- tibble::tibble(acao_legislativa = c(NA, "A Comissão aprova a Emenda n° 1-CAS (Substitutivo), com a Subemenda n° 1 - CAS. Anexo Parecer da emenda, lista de votação nominal, redação final e ofício ao Presidente do Senado. (fls. nº 26 a 36). À SSCLSF.", 
                                                                                 "Juntados aos autos do processo o original de manifestação do Ofício 03-54/2019, da Câmara Municipal de Mairinque - SP, e a cópia da carta-resposta encaminhada pelo Secretário-Geral da Mesa, Luiz Fernando Bandeira de Mello. (fls. 54/57)"),
                                                            autor = c("Senador Blairo Maggi (S/Partido/MT)", NA, "Cidadão Cidadão"),
                                                            casa = c("senado", "senado", "senado"),
                                                            data = c("31/08/1999", "12/12/2001", "13/05/2019"),
                                                            descricao_ementa = c('Altera a Lei nº 7802, de 11 de julho de 1999, que "dispõe sobre a pesquisa, a experimentação, a produção, a embalagem e rotulagem, o transporte, o armazenamento, a comercialização, a propaganda comercial, a utilização, a importação, o destino, a classificação, o controle, a inspeção e a fiscalização de agrotóxicos, seus componentes e afins, e dá outras providências."',
                                                                                 NA, "Ofício 03-54/2019 da Câmara Municipal de Mairinque-SP."),
                                                            identificacao = c("Texto inicial - PLS 526/1999", "Parecer", "Ofício"),
                                                            id_principal = c("41703", "41703", "41703"),
                                                            local = c(NA, "Comissão de Assuntos Sociais", "Plenário do Senado Federal")) 
test_that("fetch_textos_proposicao_senado()", {
  expect_true(is.data.frame(documentos_endpoint_completos_115926))
  expect_true(is.data.frame(documentos_endpoint_completos_135061))
  
  expect_true(nrow(documentos_endpoint_completos_115926) != 0)
  expect_true(nrow(documentos_endpoint_completos_135061) != 0)
  
  expect_true(all(sapply(documentos_endpoint_completos_115926, class) %in% .COLNAMES_DOCUMENTOS_SENADO))
  expect_true(all(sapply(documentos_endpoint_completos_135061, class) %in% .COLNAMES_DOCUMENTOS_SENADO))
})

test_that("scrap_senado_congresso_documentos()", {
  expect_true(is.data.frame(documentos_scrap_completos_115926))
  expect_true(is.data.frame(documentos_scrap_filtrados_115926))
  expect_true(is.data.frame(documentos_scrap_completos_135061))
  
  expect_true(nrow(documentos_scrap_completos_115926) != 0)
  expect_true(nrow(documentos_scrap_filtrados_115926) != 0)
  expect_true(nrow(documentos_scrap_completos_135061) != 0)
  expect_true(nrow(documentos_scrap_filtrados_115926) < nrow(documentos_scrap_completos_115926))
  
  expect_true(all(sapply(documentos_scrap_completos_115926, class) %in% .COLNAMES_SCRAP))
  expect_true(all(sapply(documentos_scrap_filtrados_115926, class) %in% .COLNAMES_SCRAP))
  expect_true(all(sapply(documentos_scrap_completos_135061, class) %in% .COLNAMES_SCRAP))
  
  testthat::expect_warning(scrap_senado_congresso_documentos(135061, 'asdd', F), "Casa deve ser: congresso ou senado.")
  testthat::expect_warning(scrap_senado_congresso_documentos(135061, NA, F), "Casa deve ser: congresso ou senado.")
  testthat::expect_warning(scrap_senado_congresso_documentos(135061, 'asdd', NA), "filter_texto_materia deve ser: T ou F.")
  testthat::expect_equal(documentos_scrap_completo_41703, documentos_scrap_completo_41703_gabarito)
  testthat::expect_equal(documentos_scrap_filtrado_41703, documentos_scrap_completo_41703_gabarito %>% tail(2))
})