context("Comissões")

comissao_capadr <<- fetch_comissao_camara("CAPADR")
comissao_cae_senado <<- fetch_composicao_comissoes_senado("CAE")


test_that("Is dataframe", {
  expect_true(is.data.frame(comissao_capadr))
  expect_true(is.data.frame(comissao_cae_senado))
})

test_that("Not Empty", {
  expect_true(nrow(comissao_capadr) != 0)
  expect_true(nrow(comissao_cae_senado) != 0)
})

test_that("fetch_comissoes_camara()", {
  expect_true(all(sapply(comissao_capadr, class) %in% .COLNAMES_COMISSOES))
  expect_true(all(sapply(comissao_cae_senado, class) %in% .COLNAMES_COMISSOES_SENADO))
})