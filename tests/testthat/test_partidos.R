# Testa erros
test_that("GET partido inexistente", {
  expect_error(fetch_partido(22))
})

partido_info <- fetch_partido(sigla="PT")
partido_info_id <- fetch_partido(id=36844)

TAM_DF_DEFAULT <- c(15, 4)

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(partido_info))
  expect_true(is.data.frame(partido_info_id))
})

test_that("Not Empty", {
  expect_true(nrow(partido_info) != 0)
  expect_true(nrow(partido_info_id) != 0)
})

test_that("fetch_partido() usando ID", {
  expect_true(all(sapply(partido_info_id, class) %in% .COLNAMES_PARTIDOS_ID))
})

test_that("fetch_partido() usando queries", {
  expect_true(all(sapply(partido_info, class) %in% .COLNAMES_PARTIDOS))
})

test_that("Quantidade default por requisição, atualmente 15",{
  expect_equal(dim(fetch_partido()), TAM_DF_DEFAULT)
})
