id_abel_mesquita <- 178957
abel_mesquita_info <- fetch_deputado(id_abel_mesquita)
NOME_ESPERADO <- "ABEL SALVADOR MESQUITA JUNIOR"

test_that("Nome do deputado", {expect_equal(abel_mesquita_info$nomeCivil, NOME_ESPERADO)})
test_that("Partido do deputado", {expect_equal(abel_mesquita_info$ultimoStatus$siglaPartido, "DEM")})
test_that("UF do deputado", {expect_equal(abel_mesquita_info$ultimoStatus$siglaUf, "RR")})

abel_mesquita_gastos <- fetch_despesas_deputado(id_abel_mesquita)
test_that("Dataframe de despesas", {expect_true(is.data.frame(abel_mesquita_gastos))})
