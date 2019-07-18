context("Proposições")

pec_241 <<- fetch_proposicao_camara(siglaTipo = "PEC", numero = 241, ano = 2016, dataInicio = "2016-01-01")
pec_241_id <<- fetch_id_proposicao_camara("PEC", 241, 2016)
pec_241_por_id <<- fetch_proposicao_camara(pec_241_id)
pls_91341 <<- fetch_proposicao_senado(91341)
pls_229_2009 <<- fetch_proposicao_senado_sigla('pls', 229, 2009)
pls_1489 <<- fetch_proposicao_senado(1489)
relacionadas_pec_241 <<- .fetch_relacionadas_camara(pec_241_id)
deferimento <<- fetch_deferimento(c("102343", "109173", "115853"))
relacionadas_91341 <<- .fetch_relacionadas_senado(91341)
relacionadas_58276 <<- .fetch_relacionadas_senado(58276)
relacionadas_120143 <<- .fetch_relacionadas_senado(120143)

# Constantes
PEC241_ID <- 2088351
TIPO_PROP_PEC <- 136
TIPO_PROP_PL <- 139
TAM_DF_10REQ <- 10
TAM_DF_100REQ <- 100
TAM_DF_386REQ <- 386
TAM_DF_DEFAULT <- 15


# Testa erros
expect_that(fetch_id_proposicao_camara(tipo = "AAA", numero = -1, ano = 1870), gives_warning(.WARNING_PROPOSICAO_ID))
test_that("GET ID de proposição inexistente", {expect_equal(suppressWarnings(fetch_id_proposicao_camara(tipo = "AAA", numero = -1, ano = 1870)), NULL)})
test_that("GET proposição com ID inexistente", {expect_error(fetch_proposicao_camara(id = -1))})

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(pec_241))
  expect_true(is.data.frame(pec_241_por_id))
  expect_true(is.data.frame(deferimento))
})

test_that("Not Empty", {
  expect_true(nrow(pec_241) != 0)
  expect_true(nrow(pec_241_por_id) != 0)
  expect_true(nrow(deferimento) != 0)
})

test_that("fetch_proposicao_camara()", {
  expect_true(all(sapply(pec_241, class) %in% .COLNAMES_PROPOSICAO_CAMARA))
})

test_that("fetch_proposicao_camara() usando ID", {
  expect_true(all(sapply(pec_241_por_id, class) %in% .COLNAMES_PROPOSICAO_POR_ID_CAMARA))
})

test_that("fetch_proposicao_senado()", {
  expect_true(all(sapply(pls_91341, class) %in% .COLNAMES_PROPOSICAO_SENADO))
  expect_true(nrow(pls_91341) != 0)
  expect_true(all(sapply(pls_1489, class) %in% .COLNAMES_PROPOSICAO_SENADO))
  expect_true(nrow(pls_1489) != 0)
})

test_that("fetch_proposicao_senado_sigla()", {
  expect_true(nrow(pls_229_2009) != 0)
  expect_true(is.data.frame(pls_229_2009))
  expect_true(all(sapply(pls_229_2009, class) %in% .COLNAMES_PROPOSICAO_SENADO_SIGLA))
  expect_warning(fetch_proposicao_senado_sigla(NA, NA, NA))
  expect_warning(fetch_proposicao_senado_sigla('pls', 100, NA))
  expect_warning(fetch_proposicao_senado_sigla('', 100, 2019))
  expect_warning(fetch_proposicao_senado_sigla('pls', 100, -2019))
  expect_warning(fetch_proposicao_senado_sigla('pls', -100, 2019))
  expect_true(nrow(fetch_proposicao_senado_sigla('pls', -100, 2019)) == 0)
})

test_that(".fetch_relacionadas_senado()", {
  expect_true(all(sapply(relacionadas_91341, class) %in% .COLNAMES_RELACIONADAS_SENADO))
  expect_true(all(sapply(relacionadas_58276, class) %in% .COLNAMES_RELACIONADAS_SENADO))
})

test_that("fetch_proposicao_senado() not empty", {
  expect_true(nrow(pls_91341) != 0)
  expect_true(nrow(pls_1489) != 0)
})

test_that("fetch_proposicao_senado() is dataframe", {
  expect_true(is.data.frame(pls_91341))
  expect_true(is.data.frame(pls_1489))
})

test_that(".fetch_relacionadas_camara()", {
  expect_true(all(sapply(relacionadas_pec_241, class) %in% .COLNAMES_RELACIONADAS))
})

test_that("fetch_deferimento()", {
  expect_true(all(sapply(deferimento, class) %in% .COLNAMES_DEFRIMENTO))
})

test_that("ID Correto", {expect_equal(pec_241_id, fetch_id_proposicao_camara("PEC", 241, 2016))})

# Existe um idTipo que identifica os tipos de proposição em tramitação/votação na
#    Câmara. Nesse caso, 136 é PEC.
test_that("ID PEC241", {expect_equal(pec_241_id, PEC241_ID)})
test_that("Tipo PEC241=='PEC'", {expect_equal(pec_241$codTipo, TIPO_PROP_PEC)})
test_that("Numero PEC241", {expect_equal(pec_241$numero, 241)})
test_that("Ano PEC241", {expect_equal(pec_241$ano, 2016)})

# Testa tipos de proposições
r <- compare::compareEqual(fetch_tipo_proposicao(TIPO_PROP_PL)$sigla, "PL")
test_that("Tipo de proposição", {expect_true(r$result)})

# Testa quantidade de itens por requisição
test_that("Quantidade de itens por requisição",{
  expect_equal(dim(fetch_proposicao_camara(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 10))[1], TAM_DF_10REQ)
  expect_equal(dim(fetch_proposicao_camara(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 100))[1], TAM_DF_100REQ)
  expect_equal(dim(fetch_proposicao_camara(dataInicio = "2007-01-01", dataFim = "2017-01-01", itens = 386))[1], TAM_DF_386REQ)
})

test_that("Quantidade default por requisição, atualmente 15",{
  expect_equal(dim(fetch_proposicao_camara())[1], TAM_DF_DEFAULT)
})

test_that("fetch_autor_camara()",{
  expect_true(is.data.frame(fetch_autor_camara(257161)))
  expect_true(is.data.frame(fetch_autor_camara(2192352)))
  expect_true(nrow(fetch_autor_camara(257161)) != 0)
  expect_true(nrow(fetch_autor_camara(2192352)) != 0)
})

test_that(".fetch_relacionadas_senado()", {
  expect_true(is.data.frame(.fetch_relacionadas_senado(91341)))
  expect_true(nrow(.fetch_relacionadas_senado(58276)) == 0)

  pl_91341 <- .fetch_relacionadas_senado(91341)
  expect_true(nrow(pl_91341) == 13)
  expect_equal(pl_91341, tibble::tibble(
    id_relacionada = c("96123", "96979", "111753", "112563", "112568", "112573",
                       "113330", "114920", "116166", "116195", "119106", "122726", "120384")
  ))

  pl_120768 <- .fetch_relacionadas_senado(120768)
  expect_true(nrow(pl_120768) == 2)
  expect_equal(pl_120768, tibble::tibble(
    id_relacionada = c("130202", "134953")
  ))

  pl_1430 <- .fetch_relacionadas_senado(1430)
  expect_true(nrow(pl_1430) == 0)
})

test_that("fetch_ids_relacionadas()", {
  expect_true(is.data.frame(fetch_ids_relacionadas(91341, 'senado')))
  expect_warning(fetch_ids_relacionadas(58276, 'senado'))
  expect_warning(fetch_ids_relacionadas(1430, 'senado'))
  expect_true(is.data.frame(fetch_ids_relacionadas(257161, 'camara')))
})

test_that("fetch_autores_camara()",{
  pl_ids <- c(257161, 604557, 2170839, 604888, 2192352)
  pec_ids <- c(2192459, 1198512)
  emr_ids <- c(2201520, 2204664)
  emd_ids <- c(2199697, 2169806, 2172604, 2141565, 275584)
  emc_ids <- c(2206183, 2206181, 2206181, 2202452)
  emp_ids <- c(2193064, 2193062, 2193059 )

  proposicoes_df <- tibble::tibble(prop_ids = pl_ids, sigla_tipo = "PL") %>%
    dplyr::bind_rows(tibble::tibble(prop_ids = pec_ids, sigla_tipo = "PEC")) %>%
    dplyr::bind_rows(tibble::tibble(prop_ids = emr_ids, sigla_tipo = "EMR")) %>%
    dplyr::bind_rows(tibble::tibble(prop_ids = emc_ids, sigla_tipo = "EMC")) %>%
    dplyr::bind_rows(tibble::tibble(prop_ids = emp_ids, sigla_tipo = "EMP"))
  expect_true(all(as.logical(purrr::pmap(proposicoes_df, ~ is.data.frame(fetch_autores_camara(.x, .y))))))
  expect_true(all(as.logical(purrr::pmap(proposicoes_df, ~ nrow(fetch_autores_camara(.x, .y)) != 0))))
  emc_2206183 <- fetch_autores_camara(2206183, "EMC")
  expect_equal(tibble::tibble(
    id_autor = c(204371, 204534),
    nome = c("Felipe Rigoni", "Tabata Amaral"),
    cod_tipo = c(as.integer(10000),as.integer(10000)),
    tipo = c("Deputado", "Deputado"),
    uri = c("https://dadosabertos.camara.leg.br/api/v2/deputados/204371",
            "https://dadosabertos.camara.leg.br/api/v2/deputados/204534")
    ), emc_2206183)
})

test_that("scrap_senado_relacionadas_from_website()", {
  pls_ids <- c(96123, 91341, 112563, 104930, 90919)

  df_pls_ids_relacionadas <- purrr::map_df(pls_ids, ~.scrap_senado_relacionadas_from_website(.x))

  expect_true(is.data.frame(df_pls_ids_relacionadas))

  expect_true(nrow(df_pls_ids_relacionadas) > 0)

  pl_104930 <- .scrap_senado_relacionadas_from_website(104930)

  expect_true(nrow(pl_104930) == 5)

  pl_127753 <- .scrap_senado_relacionadas_from_website(127753)
  expect_equal(pl_127753, tibble::tibble(
    url_relacionada = c("https://www25.senado.leg.br/web/atividade/materias/-/materia/127452",
                        "https://www25.senado.leg.br/web/atividade/materias/-/materia/127754",
                        "https://www25.senado.leg.br/web/atividade/materias/-/materia/127756"),
    id_relacionada = c("127452", "127754", "127756")))

  pl_135060 <- .scrap_senado_relacionadas_from_website(135060)
  expect_true(nrow(pl_135060) == 0)
})

test_that("fetch_autores_senado()", {
  expect_true(is.data.frame(.fetch_autores_senado(91341)))
  expect_true(is.data.frame(.fetch_autores_senado(1430)))
})

test_that("fetch_autores()", {
  expect_true(is.character(fetch_autores(91341, "invalid")))
  expect_true(is.character(fetch_autores(257161, "invalid")))
  expect_true(is.data.frame(fetch_autores(91341, "senado")))
  expect_true(is.data.frame(fetch_autores(257161, "camara")))
})
