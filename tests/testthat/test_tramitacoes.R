context("Tramitações")

SENADO_ID <<- c(91341, 127753)
PROPOSICOES_ID <<- c(91341, 257161, 2121442, 127753)
tramitacao_pec_241 <<- fetch_tramitacao_camara(pec_241_id)
tramitacao_pec_241_x2 <<- fetch_tramitacao_camara(replicate(2, pec_241_id))
pls229 <- fetch_tramitacao_senado(91341)

test_that("fetch_tramitacao_camara()", {
    expect_true(all(sapply(tramitacao_pec_241, class) %in% .COLNAMES_TRAMITACOES_CAMARA))
    expect_equal(nrow(tramitacao_pec_241), nrow(tramitacao_pec_241_x2))
})

test_that("fetch_tramitacao_senado()", {
    expect_true(all(sapply(pls229, class) %in% .COLNAMES_TRAMITACOES_SENADO))
})

test_that("fetch_tramitacao() returns dataframe", {
    proposicoes_fetch_tramitacao <- as.data.frame(SENADO_ID) %>%
        dplyr::rowwise() %>%
        dplyr::do(fetch_tramitacao(.$SENADO_ID, "senado"))
    expect_true(is.data.frame(proposicoes_fetch_tramitacao))
})
