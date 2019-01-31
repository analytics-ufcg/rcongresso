context("Comissões")

comissao_capadr <<- fetch_comissao_camara("CAPADR")


test_that("Is dataframe", {
  expect_true(is.data.frame(comissao_capadr))
})

test_that("Not Empty", {
  expect_true(nrow(comissao_capadr) != 0)
})

test_that("fetch_comissoes_camara()", {
  expect_true(all(sapply(comissao_capadr, class) %in% .COLNAMES_COMISSOES))
})
