

# wget https://vincentarelbundock.github.io/Rdatasets/csv/COUNT/titanic.csv
# wget https://vincentarelbundock.github.io/Rdatasets/csv/AER/ResumeNames.csv
# wget https://vincentarelbundock.github.io/Rdatasets/csv/openintro/evals.csv

#### test factor_spec() ####
test_that("factor_spec works with titanic data", {

  if ( !require(readr) ) stop("factor_spec() depends on readr")

  csv_file <- testthat::test_path("testdata/titanic.csv")
  d <- read_csv(csv_file, show_col_types = FALSE)

  expected =
'factor_levels_1 = c("adults", "child")
factor_levels_2 = c("man", "women")
factor_levels_3 = c("no", "yes")
factor_levels_4 = c("1st class", "2nd class", "3rd class")

cols(
  age = col_factor(factor_levels_1),
  sex = col_factor(factor_levels_2),
  survived = col_factor(factor_levels_3),
  class = col_factor(factor_levels_4)
)
'

  # Ignore that dput() puts space after comma in multiline objects
  have <- factor_spec(d) |> stringr::str_replace_all(" \n","\n")
  want <- expected       |> stringr::str_replace_all(" \n","\n")

  if ( interactive () ){
    waldo::compare(have, want)
  }

  expect_equal(have, want)

})


#### test factor_spec() ####
test_that("factor_spec works with evals data", {

  if ( !require(readr) ) stop("factor_spec() depends on readr")

  csv_file <- testthat::test_path("testdata/evals.csv")
  d <- read_csv(csv_file, show_col_types = FALSE)

  expected =
'factor_levels_1 = c("minority", "not minority")
factor_levels_2 = c("female", "male")
factor_levels_3 = c("english", "non-english")
factor_levels_4 = c("lower", "upper")
factor_levels_5 = c("multiple", "single")
factor_levels_6 = c("multi credit", "one credit")
factor_levels_7 = c("formal", "not formal")
factor_levels_8 = c("black&white", "color")
factor_levels_9 = c("teaching", "tenure track", "tenured")

cols(
  ethnicity = col_factor(factor_levels_1),
  gender = col_factor(factor_levels_2),
  language = col_factor(factor_levels_3),
  cls_level = col_factor(factor_levels_4),
  cls_profs = col_factor(factor_levels_5),
  cls_credits = col_factor(factor_levels_6),
  pic_outfit = col_factor(factor_levels_7),
  pic_color = col_factor(factor_levels_8),
  rank = col_factor(factor_levels_9)
)
'

  # Ignore that dput() puts space after comma in multiline objects
  have <- factor_spec(d) |> stringr::str_replace_all(" \n","\n")
  want <- expected       |> stringr::str_replace_all(" \n","\n")

  if ( interactive () ){
    waldo::compare(have, want)
  }

  expect_equal(have, want)

})



#### test factor_spec() ####
test_that("factor_spec works with ResumeNames data", {

  if ( !require(readr) ) stop("factor_spec() depends on readr")

  csv_file <- testthat::test_path("testdata/ResumeNames.csv")
  d <- read_csv(csv_file, show_col_types = FALSE)

  expected =
'factor_levels_1 = c("no", "yes")
factor_levels_2 = c("female", "male")
factor_levels_3 = c("afam", "cauc")
factor_levels_4 = c("high", "low")
factor_levels_5 = c("boston", "chicago")
factor_levels_6 = c("manager", "office support", "other", "retail sales", "secretary",
"supervisor")
factor_levels_7 = c("business/personal services", "finance/insurance/real estate",
"health/education/social services", "manufacturing", "trade",
"transport/communication", "unknown")
factor_levels_8 = c("0", "0.5", "1", "10", "2", "3", "4", "5", "6", "7", "8", "none",
"some")
factor_levels_9 = c("Aisha", "Allison", "Anne", "Brad", "Brendan", "Brett", "Carrie",
"Darnell", "Ebony", "Emily", "Geoffrey", "Greg", "Hakim", "Jamal",
"Jay", "Jermaine", "Jill", "Kareem", "Keisha", "Kenya", "Kristen",
"Lakisha", "Latonya", "Latoya", "Laurie", "Leroy", "Matthew",
"Meredith", "Neil", "Rasheed", "Sarah", "Tamika", "Tanisha",
"Todd", "Tremayne", "Tyrone")

cols(
  call = col_factor(factor_levels_1),
  honors = col_factor(factor_levels_1),
  volunteer = col_factor(factor_levels_1),
  military = col_factor(factor_levels_1),
  holes = col_factor(factor_levels_1),
  school = col_factor(factor_levels_1),
  email = col_factor(factor_levels_1),
  computer = col_factor(factor_levels_1),
  special = col_factor(factor_levels_1),
  college = col_factor(factor_levels_1),
  equal = col_factor(factor_levels_1),
  requirements = col_factor(factor_levels_1),
  reqexp = col_factor(factor_levels_1),
  reqcomm = col_factor(factor_levels_1),
  reqeduc = col_factor(factor_levels_1),
  reqcomp = col_factor(factor_levels_1),
  reqorg = col_factor(factor_levels_1),
  gender = col_factor(factor_levels_2),
  ethnicity = col_factor(factor_levels_3),
  quality = col_factor(factor_levels_4),
  city = col_factor(factor_levels_5),
  wanted = col_factor(factor_levels_6),
  industry = col_factor(factor_levels_7),
  minimum = col_factor(factor_levels_8),
  name = col_factor(factor_levels_9)
)
'

  # Ignore that dput() puts space after comma in multiline objects
  have <- factor_spec(d) |> stringr::str_replace_all(" \n","\n")
  want <- expected       |> stringr::str_replace_all(" \n","\n")

  if ( interactive () ){
    waldo::compare(have, want)
  }

  expect_equal(have, want)

})



