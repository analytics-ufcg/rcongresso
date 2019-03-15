context("Eventos")

PROPOSICOES_ID <<- c(2121442)

test_that('fetch_eventos_camara() returns dataframe', {
    proposicoes_fetch_events <- as.data.frame(PROPOSICOES_ID) %>%
        dplyr::rowwise() %>%
        dplyr::do(fetch_eventos_camara(.$PROPOSICOES_ID))

    expect_true(is.data.frame(proposicoes_fetch_events))
})

test_that('get_latest_eventos_camara() returns dataframe', {
    proposicoes_latest_events <- as.data.frame(PROPOSICOES_ID) %>%
        dplyr::rowwise() %>%
        dplyr::do(get_latest_eventos_camara(.$PROPOSICOES_ID))

    expect_true(is.data.frame(proposicoes_latest_events))
})

test_that('get_next_eventos_camara() returns dataframe', {
    proposicoes_next_events <- as.data.frame(PROPOSICOES_ID) %>%
        dplyr::rowwise() %>%
        dplyr::do(get_next_eventos_camara(.$PROPOSICOES_ID))

    expect_true(is.data.frame(proposicoes_next_events))
})
