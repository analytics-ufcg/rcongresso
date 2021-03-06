context("Deputados")

# Setup
dep_info <<- fetch_deputado(siglaUf = "RR", siglaSexo = "M")
dep_info_por_id <<- fetch_deputado(178957)
dep_gastos <<- fetch_despesas_deputado(178912)
dep_frentes <<- fetch_frentes_deputado(160511)
dep_partido_estado <<- extract_partido_estado_autor("https://dadosabertos.camara.leg.br/api/v2/deputados/178957")
all_deputados_ids_vazio <<- tibble::tibble()
fetch_deputados_parametro_invalido <<- 11
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

all_deputados_ids <- fetch_ids_deputados() %>% dplyr::sample_n(15)



all_deputados <- fetch_all_deputados(all_deputados_ids)


all_deputados_ids_leg <- fetch_ids_deputados_by_leg() %>% dplyr::sample_n(15)

all_deputados_leg <- fetch_all_deputados(all_deputados_ids_leg)

# Testes
# Os nomes das colunas e os tipos estão definidos em colunas_constants.R
test_that("Is dataframe", {
  expect_true(is.data.frame(dep_info))
  expect_true(is.data.frame(dep_info_por_id))
  expect_true(is.data.frame(dep_gastos))
  expect_true(is.data.frame(dep_frentes))
})

test_that("Not Empty", {
  expect_true(nrow(dep_info) != 0)
  expect_true(nrow(dep_info_por_id) != 0)
  expect_true(nrow(dep_gastos) != 0)
  expect_true(nrow(dep_frentes) != 0)
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

test_that("fetch_frentes_deputado()", {
  expect_true(all(sapply(dep_frentes, class) %in% .COLNAMES_DEPUTADO_FRENTES))
  expect_warning(fetch_frentes_deputado(1), "ID invalido ou deputado nao faz parte de nenhuma frente.")
})

test_that("ID do deputado", {expect_equal(dep_info_por_id$id, ABEL_MESQUITA_ID)})
test_that("Nome do deputado", {expect_equal(dep_info_por_id$nomeCivil, NOME_ESPERADO)})
test_that("Data de nascimento do deputado", {expect_equal(dep_info_por_id$dataNascimento, ABEL_MESQUITA_DN)})
test_that("UF de nascimento do deputado", {expect_equal(dep_info_por_id$ufNascimento, ABEL_MESQUITA_UF)})
test_that("Partido + UF do deputado", {expect_equal(dep_partido_estado,paste0(ABEL_MESQUITA_PARTIDO, "/", ABEL_MESQUITA_UF))})

## test_that("Quantidade default por requisição, atualmente 15",{
##   expect_equal(dim(fetch_deputado()), TAM_DF_DEFAULT)
## })

test_that("fetch_all_deputados()", {
  expect_true(all(sapply(all_deputados, class) %in% .COLNAMES_DEP_INFO_ID))
  expect_true(nrow(all_deputados_ids_vazio) == 0)
  expect_warning(fetch_all_deputados(all_deputados_ids_vazio), "Dataframe vazio")
  expect_warning(fetch_all_deputados(fetch_deputados_parametro_invalido), "Objeto deve ser um dataframe nao-nulo")
  expect_true(nrow(fetch_all_deputados(fetch_deputados_parametro_invalido)) == 0)
  expect_true(nrow(fetch_all_deputados(all_deputados_ids_vazio)) == 0)
  expect_true(all(all_deputados$ultimo_status_id_legislatura >= 40))
})

test_that("fetch_ids_deputados_by_leg()", {
  expect_true(all(sapply(all_deputados_leg, class) %in% .COLNAMES_DEP_INFO_ID))
  expect_warning(fetch_ids_deputados_by_leg(0), "Não há dados para esta legislatura")
  expect_true(all(all_deputados_leg$ultimo_status_id_legislatura >= 56))
})


