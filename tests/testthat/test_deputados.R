# Testa erros
test_that("GET deputado inexistente", {
  expect_error(fetch_deputado(22))
})

# Setup
NOME_ESPERADO <- "ABEL SALVADOR MESQUITA JUNIOR"
id_abel_mesquita <- 178957
abel_mesquita_info <- fetch_deputado(id_abel_mesquita)
abel_mesquita_gastos <- fetch_despesas_deputado(id_abel_mesquita)

colnames_abel_mesquita_info <- c("id","uri","nomeCivil","cpf","sexo","dataNascimento",
                        "ufNascimento","municipioNascimento","escolaridade")

colnames_abel_mesquita_gastos <- c("ano","mes","tipoDespesa","idDocumento","tipoDocumento", "idTipoDocumento",
                                   "dataDocumento","numDocumento","valorDocumento",
                                   "urlDocumento","nomeFornecedor","cnpjCpfFornecedor",
                                   "valorLiquido","valorGlosa","numRessarcimento","idLote","parcela", "idDep")

tipos_abel_mesquita_info <- c("integer","character","character","character",
                              "character","character","character","character",
                              "character")

tipos_abel_mesquita_gastos <- rep("character", 17) %>%
  append("numeric", 17)

names(tipos_abel_mesquita_info) <- colnames_abel_mesquita_info
names(tipos_abel_mesquita_gastos) <- colnames_abel_mesquita_gastos

# Testes
test_that("Is dataframe", {
  expect_true(is.data.frame(abel_mesquita_info))
  expect_true(is.data.frame(abel_mesquita_gastos))
})

test_that("Dimensoes do dataframe",{
  expect_equal(dim(abel_mesquita_info), c(1, 9))
  expect_equal(ncol(abel_mesquita_gastos), 18)
})

test_that("Atributos do dataframe",{
  expect_equal(attributes(abel_mesquita_info)$names, colnames_abel_mesquita_info)
  expect_equal(attributes(abel_mesquita_gastos)$names, colnames_abel_mesquita_gastos)
})

test_that("Campos do dataframe",{
  expect_equal(sapply(abel_mesquita_info, class), tipos_abel_mesquita_info)
  expect_equal(sapply(abel_mesquita_gastos, class), tipos_abel_mesquita_gastos)
})

test_that("ID do deputado", {expect_equal(abel_mesquita_info$id, 178957)})
test_that("Nome do deputado", {expect_equal(abel_mesquita_info$nomeCivil, NOME_ESPERADO)})
test_that("Data de nascimento do deputado", {expect_equal(abel_mesquita_info$dataNascimento, "1962-03-29")})
test_that("UF de nascimento do deputado", {expect_equal(abel_mesquita_info$ufNascimento, "RR")})
