context("Deputados")

# Setup
dep_info <<- fetch_deputado(siglaUf = "RR", siglaSexo = "M")
dep_info_por_id <<- fetch_deputado(178957)
dep_gastos <<- fetch_despesas_deputado(178957)
dep_partido_estado <<- extract_partido_estado_autor("https://dadosabertos.camara.leg.br/api/v2/deputados/178957")


# Testa erros
test_that("GET deputado inexistente", {
  expect_error(fetch_deputado(-1))
})

# Constantes
ABEL_MESQUITA_ID <- 178957
NOME_ESPERADO <- "ABEL SALVADOR MESQUITA JUNIOR"
ABEL_MESQUITA_DN <- "1962-03-29"
ABEL_MESQUITA_UF <-"RR"
ABEL_MESQUITA_PARTIDO <- "DEM"

TAM_DF_DEFAULT <- c(15, 8)
#TAM_DF_DEP_ATIVOS <- c(513, 8)

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(dep_info))
  expect_true(is.data.frame(dep_info_por_id))
  expect_true(is.data.frame(dep_gastos))
})

test_that("Not Empty", {
  expect_true(nrow(dep_info) != 0)
  expect_true(nrow(dep_info_por_id) != 0)
  expect_true(nrow(dep_gastos) != 0)
})

test_that("fetch_deputado() usando ID", {
  expect_true(all(sapply(dep_info_por_id, class) %in% .COLNAMES_DEP_INFO_ID))
})

test_that("fetch_deputado() usando queries", {
  expect_true(all(sapply(dep_info, class) %in% .COLNAMES_DEP_INFO))
})

test_that("fetch_despesas_deputado()", {
  expect_true(all(sapply(dep_gastos, class) %in% .COLNAMES_DEP_GASTOS))
})

test_that("ID do deputado", {expect_equal(dep_info_por_id$id, ABEL_MESQUITA_ID)})
test_that("Nome do deputado", {expect_equal(dep_info_por_id$nomeCivil, NOME_ESPERADO)})
test_that("Data de nascimento do deputado", {expect_equal(dep_info_por_id$dataNascimento, ABEL_MESQUITA_DN)})
test_that("UF de nascimento do deputado", {expect_equal(dep_info_por_id$ufNascimento, ABEL_MESQUITA_UF)})
test_that("Partido + UF do deputado", {expect_equal(dep_partido_estado,paste0(ABEL_MESQUITA_PARTIDO, "/", ABEL_MESQUITA_UF))})

## test_that("Quantidade default por requisição, atualmente 15",{
##   expect_equal(dim(fetch_deputado()), TAM_DF_DEFAULT)
## })

