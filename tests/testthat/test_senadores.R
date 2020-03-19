context("Senadores")

#Setup
legislatura_inicial <- 55
legislatura_final <- 56
sen_info_por_id <- fetch_senador(5573)
all_senadores_ids_vazio <<- tibble::tibble()
fetch_senadores_parametro_invalido <<- 11
senadores <- fetch_senadores(legislatura_inicial, legislatura_final)
all_senadores_ids <- fetch_ids_senadores(legislatura_inicial, legislatura_final) %>% dplyr::sample_n(15)
all_senadores <- fetch_all_senadores(all_senadores_ids)

test_that("Is dataframe", {
  expect_true(is.data.frame(sen_info_por_id))
  expect_true(is.data.frame(all_senadores))
  expect_true(is.data.frame(senadores))
  expect_true(is.data.frame(all_senadores_ids))
})

test_that("Not Empty", {
  expect_true(nrow(sen_info_por_id) != 0)
  expect_true(nrow(all_senadores) != 0)
  expect_true(nrow(senadores) != 0)
  expect_true(nrow(all_senadores_ids) != 0)
})

test_that("fetch_senador() usando ID", {
  expect_true(all(sapply(sen_info_por_id, class) %in% .COLNAMES_SENADORES_INFO))
})

test_that("fetch_all_senadores()", {
  expect_true(all(sapply(all_senadores, class) %in% .COLNAMES_SENADORES_INFO))
  expect_true(nrow(all_senadores_ids_vazio) == 0)
  expect_warning(fetch_all_senadores(all_senadores_ids_vazio), "Dataframe vazio")
  expect_warning(fetch_all_senadores(fetch_senadores_parametro_invalido <<- 11), "Objeto deve ser um dataframe nao-nulo")
  expect_true(nrow(fetch_all_senadores(fetch_senadores_parametro_invalido <<- 11)) == 0)
  expect_true(nrow(fetch_all_senadores(all_senadores_ids_vazio)) == 0)
})

test_that("GET senador inexistente", {
  expect_error(fetch_senador(-1))
})

test_that("fetch_senadores(legis_inicial, legis_final)", {
  expect_true(is.data.frame(senadores))
  expect_true(all(sapply(senadores, class) %in% .COLNAMES_LEGISLATURA_SENADORES))
})

test_that("fetch_senadores(legis_inicial, legis_final)", {
  expect_error(fetch_senadores(-1, -1))
})

test_that("fetch_ids_senadores_em_exercicio()", {
  expect_true(nrow(fetch_ids_senadores_em_exercicio()) == 81)
})
