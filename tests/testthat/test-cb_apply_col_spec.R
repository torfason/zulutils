


# Test application of col_spec
test_that("cb_apply_col_spec works", {

  if ( !requireNamespace("readr") ) stop("cb_apply_col_spec() depends on readr")

  # Input
  sw <- dplyr::starwars %>%
    dplyr::select(name, height, ends_with("_color")) %>%
    dplyr::slice(c(1,4,5,19))

  # A correct result for a full-level col_spec
  sw_fct <- structure(list(
    name = c("Luke Skywalker", "Darth Vader", "Leia Organa",  "Yoda"),
    height = c(172L, 202L, 150L, 66L),
    hair_color = structure(c(2L,  4L, 1L, 3L), class = "factor",
                           .Label = c("brown", "blond", "white",  "none")),
    skin_color = structure(c(3L, 4L, 2L, 1L), class = "factor",
                           .Label = c("green",  "light", "fair", "white")),
    eye_color = structure(c(1L, 3L, 2L,  2L), class = "factor",
                          .Label = c("blue", "brown", "yellow"))),
    row.names = c(NA,  -4L), class = c("tbl_df", "tbl", "data.frame"))

  # A correct result when the hair_color factor does not equal "none"
  sw_fct_na <- structure(list(
      name = c("Luke Skywalker", "Darth Vader", "Leia Organa", "Yoda"),
      height = c(172, 202, 150, 66),
      hair_color = structure(c(2L,  NA, 1L, 3L), class = "factor",
                             .Label = c("brown", "blond", "white")),
      skin_color = structure(c(3L, 4L, 2L, 1L), class = "factor",
                             .Label = c("green",  "light", "fair", "white")),
      eye_color = structure(c(1L,  3L, 2L, 2L), class = "factor",
                            .Label = c("blue", "brown", "yellow"))),
      row.names = c(NA,  -4L), class = c("tbl_df", "tbl", "data.frame"))


  # A col_spec specifying all required values
  cspec <- readr::cols(
    hair_color = readr::col_factor(c("brown", "blond", "white", "none")),
    skin_color = readr::col_factor(c( "green", "light", "fair", "white")),
    eye_color  = readr::col_factor(c("blue", "brown", "yellow"))
  )

  # A col_spec that does not specify a value for "none" in hair_color
  cspec_na <- readr::cols(
    hair_color = readr::col_factor(c("brown", "blond", "white")),
    skin_color = readr::col_factor(c( "green", "light", "fair", "white")),
    eye_color  = readr::col_factor(c("blue", "brown", "yellow"))
  )

  # # A col_spec specifying the NA levels explicitly would be the
  # # best solution, but that is currently not implemented by the
  # # col_factor() function
  # cspec_na_explicit <- readr::cols(
  #   hair_color = readr::col_factor(c("brown", "blond", "white"), na="none"),
  #   skin_color = readr::col_factor(c( "green", "light", "fair", "white")),
  #   eye_color  = readr::col_factor(c("blue", "brown", "yellow"))
  # )

  # Standard, all levels present
  sw_result <- cb_apply_col_spec(sw, cspec)
  expect_equal(sw_result, sw_fct)

  # The result with warn_missing_levels==TRUE, but zapping the problems afterwards
  sw_result_na_warn <- expect_warning(cb_apply_col_spec(sw, cspec_na))
    attr(sw_result_na_warn,"problems")<-NULL
    attr(sw_result_na_warn$hair_color,"problems")<-NULL
  expect_equal(sw_result_na_warn, sw_fct_na)

  # The result with warn_missing_levels==TRUE, but zapping the problems afterwards
  sw_result_na_warn <- expect_warning(cb_apply_col_spec(sw, cspec_na))
    attr(sw_result_na_warn,"problems")<-NULL
    attr(sw_result_na_warn$hair_color,"problems")<-NULL
  expect_equal(sw_result_na_warn, sw_fct_na)

  # The result with warn_missing_levels==FALSE
  expect_equal(cb_apply_col_spec(sw, cspec_na, warn_missing_levels=FALSE), sw_fct_na)

  # The result with set_problems_attribute==FALSE
  sw_result_na_warn_but_no_problems_attribute <-
    expect_warning(cb_apply_col_spec(sw, cspec_na, set_problems_attribute=FALSE))
  expect_equal(sw_result_na_warn_but_no_problems_attribute, sw_fct_na)

  # The application should preserve variable labels on the affected columns
  sw_lab <- sw
  attr(sw_lab$name,       "label") <- "Name"
  attr(sw_lab$height,     "label") <- "Height"
  attr(sw_lab$hair_color, "label") <- "Skin color"
  attr(sw_lab$skin_color, "label") <- "Skin color"
  attr(sw_lab$eye_color,  "label") <- "Eye Color"

  sw_fct_na_lab <- sw_fct_na
  attr(sw_fct_na_lab$name,       "label") <- "Name"
  attr(sw_fct_na_lab$height,     "label") <- "Height"
  attr(sw_fct_na_lab$hair_color, "label") <- "Skin color"
  attr(sw_fct_na_lab$skin_color, "label") <- "Skin color"
  attr(sw_fct_na_lab$eye_color,  "label") <- "Eye Color"
  #cb_apply_col_spec(sw_lab, cspec_na, warn_missing_levels=FALSE) |> View()
  #View(sw_fct_na_lab)

  expect_equal(cb_apply_col_spec(sw_lab, cspec_na, warn_missing_levels=FALSE), sw_fct_na_lab)
})

