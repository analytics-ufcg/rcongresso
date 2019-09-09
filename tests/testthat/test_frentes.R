context("Frentes")

# Setup
frente_amazonia <- fetch_frentes(54013, "camara")
frente_corrupcao <- fetch_frentes(53888, "camara")
frente_senado <- fetch_frentes(53888, "senado")

membros_amazonia <- fetch_membros_frentes(54013, "camara")
membros_corrupcao <- fetch_membros_frentes(53888, "camara")

test_that("Is dataframe", {
  expect_true(is.data.frame(frente_amazonia))
  expect_true(is.data.frame(frente_corrupcao))

  expect_true(is.data.frame(membros_corrupcao))
  expect_true(is.data.frame(membros_amazonia))

})

test_that("Not Empty", {
  expect_true(nrow(frente_amazonia) != 0)
  expect_true(nrow(frente_corrupcao) != 0)

  expect_true(nrow(membros_amazonia) != 0)
  expect_true(nrow(membros_corrupcao) != 0)
})

test_that("Columns", {
  expect_true(ncol(frente_amazonia) == 12)
  expect_true(ncol(frente_corrupcao) == 12)

  expect_true(ncol(membros_amazonia) == 14)
  expect_true(ncol(membros_corrupcao) == 14)

})

test_that("Rows", {
  expect_true(nrow(membros_amazonia) == 215)
  expect_true(nrow(membros_corrupcao) == 232)
})

