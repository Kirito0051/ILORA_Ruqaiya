library(testthat)
library(ILORA)

test_that("get_species_names returns a character vector", {
  species <- get_species_names()
  expect_type(species, "character")
  expect_gt(length(species), 0)
  expect_true("Rubus buergeri Miq." %in% species)
})

test_that("get_variable_names returns a character vector", {
  variables <- get_variable_names()
  expect_type(variables, "character")
  expect_gt(length(variables), 0)
  expect_true("genus" %in% variables)
})

test_that("get_data returns a data frame for valid inputs", {
  species_data <- get_data("Rubus buergeri Miq.", c("orders", "0700_Fuels2", "genus", "species"))
  expect_s3_class(species_data, "data.frame")
  expect_gt(nrow(species_data), 0)
  expect_true(all(c("orders", "0700_Fuels2", "genus", "species") %in% names(species_data)))
})

test_that("get_data returns NULL for invalid species", {
  species_data <- get_data("Invalid species", c("orders", "0700_Fuels2", "genus", "species"))
  expect_null(species_data)
})


test_that("get_data handles empty species vector", {
  output <- capture.output({
    species_data <- get_data(character(0), c("orders", "0700_Fuels2", "genus", "species"))
  }, type = "message")

  expect_true(any(grepl("Species vector cannot be empty.", output)))
  expect_null(species_data)
})

test_that("get_data handles empty variable list", {
  output <- capture.output({
    species_data <- get_data("Rubus buergeri Miq.", character(0))
  }, type = "message")

  expect_true(any(grepl("Variables vector cannot be empty.", output)))
  expect_null(species_data)
})


#test_that("get_data handles empty species list", {
 # expect_error(
  #  get_message("", c("orders", "0700_Fuels2", "genus", "species")),
   # "Species vector cannot be empty."
  #)
  #expect_null(species_data)#
#})

#test_that("get_data handles empty variable list", {
 # expect_error(
  #  get_message("Rubus buergeri Miq.", c()),
   # "Species vector cannot be empty."
  #)
  #expect_null(species_data)
#})

