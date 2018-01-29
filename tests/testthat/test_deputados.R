# Testa erros
test_that("GET deputado inexistente", {
  expect_error(fetch_deputado(22))
})

# Setup
dep_info <- fetch_deputado(siglaUf = "RR", siglaSexo = "M")
dep_info_por_id <- fetch_deputado(178957)
dep_gastos <- fetch_despesas_deputado(178957)

# Constantes
ABEL_MESQUITA_ID <- 178957
NOME_ESPERADO <- "ABEL SALVADOR MESQUITA JUNIOR"
ABEL_MESQUITA_DN <- "1962-03-29"
ABEL_MESQUITA_UF <-"RR"

# Colunas dos dataframes
colnames_dep_info <- c("id"="integer","uri"="character","nome"="character","siglaPartido"="character",
                            "uriPartido"="character","siglaUf"="character","idLegislatura"="integer","urlFoto"="character")

colnames_dep_info_por_id <- c("id"="numeric","uri"="character","nomeCivil"="character","ultimoStatus.id"="numeric",
                                 "ultimoStatus.uri"="character","ultimoStatus.nome"="character","ultimoStatus.siglaPartido"="character",
                                 "ultimoStatus.uriPartido"="character","ultimoStatus.siglaUf"="character",
                                 "ultimoStatus.idLegislatura"="numeric", "ultimoStatus.urlFoto"="character",
                                 "ultimoStatus.data"="character","ultimoStatus.nomeEleitoral"="character",
                                 "ultimoStatus.gabinete.nome"="character", "ultimoStatus.gabinete.predio"="character",
                                 "ultimoStatus.gabinete.sala"="numeric","ultimoStatus.gabinete.andar"="numeric",
                                 "ultimoStatus.gabinete.telefone"="character","ultimoStatus.gabinete.email"="character",
                                 "ultimoStatus.situacao"="numeric","ultimoStatus.condicaoEleitoral"="character","cpf"="character",
                                 "sexo"="character","urlWebsite"="character","dataNascimento"="character","dataFalecimento"="character",
                                 "ufNascimento"="character","municipioNascimento"="character","escolaridade"="character")

colnames_dep_gastos <- c("ano"="numeric","mes"="numeric","tipoDespesa"="character","idDocumento"="numeric","tipoDocumento"="numeric",
                                   "idTipoDocumento"="numeric","dataDocumento"="character","numDocumento"="numeric","valorDocumento"="numeric",
                                   "urlDocumento","nomeFornecedor","cnpjCpfFornecedor"="character","valorLiquido"="numeric","valorGlosa"="numeric",
                                   "numRessarcimento"="numeric","idLote"="numeric","parcela"="numeric", "idDep"="numeric")

# Testes
test_that("Is dataframe", {
  expect_true(is.data.frame(dep_info))
  expect_true(is.data.frame(dep_info_por_id))
  expect_true(is.data.frame(dep_gastos))
})

test_that("fetch_deputado() usando ID", {
  expect_true(all(sapply(dep_info_por_id, class) %in% colnames_dep_info_por_id))
})

test_that("fetch_deputado() usando queries", {
  expect_true(all(sapply(dep_info, class) %in% colnames_dep_info))
})

test_that("fetch_despesas_deputado()", {
  expect_true(all(sapply(dep_gastos, class) %in% colnames_dep_gastos))
})

test_that("ID do deputado", {expect_equal(dep_info_por_id$id, ABEL_MESQUITA_ID)})
test_that("Nome do deputado", {expect_equal(dep_info_por_id$nomeCivil, NOME_ESPERADO)})
test_that("Data de nascimento do deputado", {expect_equal(dep_info_por_id$dataNascimento, ABEL_MESQUITA_DN)})
test_that("UF de nascimento do deputado", {expect_equal(dep_info_por_id$ufNascimento, ABEL_MESQUITA_UF)})
