

test_that("factor conversion works", {
  if ( require(dplyr) && require(forcats) ) {
    x <- rev(as_factor(c("charles", "bob", "alice")))
    X <- rev(as_factor(c("CHARLES", "BOB", "ALICE")))
    result <- x %>%
      as.character() %>%
      toupper() %>%
      factor(levels=toupper(levels(x)))
    expect_equal(result, X)
  }
})


test_that("gg_apply works", {

  # We are conservative and just bail on the tests if packages are not available
  if( require(dplyr) && require(ggplot2) && require(snakecase) && require(forcats) ) {

    # The plot from the original example
    p <- starwars %>%
      filter(mass < 1000) %>%
      mutate(species = species %>% fct_infreq %>%  fct_lump(5) %>% fct_explicit_na) %>%
      ggplot(aes(height, mass, color=species, size=birth_year)) +
      geom_point()
    expect_silent(   p                                  )
    expect_silent(   gg_apply(p,to_sentence_case)       )
    expect_warning(  gg_apply_labs(p,to_sentence_case)  )


    # Another plot, where updating vars is applicable as well
    p <- starwars %>%
      filter(mass<1000) %>%
    ggplot() +
      aes(mass, height, color=sex) +
      geom_point() +
      theme_light()
    expect_silent(   p                                                )
    expect_silent(   gg_apply(p, to_sentence_case)                    )
    expect_silent(   gg_apply(p, to_sentence_case, .labs=FALSE)        )
    expect_silent(   gg_apply(p, to_sentence_case, .vars=FALSE)        )

    # Specify particular labs or variables

    expect_silent(   gg_apply(p, to_sentence_case, .labs=c("x"), .vars="sex")                    )
    expect_silent(   gg_apply(p, to_sentence_case, .labs=c("x", "y", "colour"), .vars=c("sex") )                   )
    expect_silent(   gg_apply(p, to_sentence_case, .labs=c("x", "y", "z"), .vars=c("xes","sex") )                   )

    # Use lookup_enframed instead (use same plot as above)
  }

})

test_that("gg_apply works with additional arguments to fun", {

  # We are conservative and just bail on the tests if packages are not available
  if( require(dplyr) && require(ggplot2) && require(tibble) ) {

    # Plot to be translated into Icelandic
    # We also use the opportunity to test that this works with factor vars
    p <- starwars %>%
      filter(mass<1000) %>%
      mutate(sex=as.factor(sex)) %>%
    ggplot() +
      aes(mass, height, color=sex) +
      geom_point() +
      theme_light()

    # Translations
    v.trans = c(
      height="Hæð",
      mass="Þyngd",
      sex="Kyn",
      female="Kona",
      male="Karl",
      none="Ekki tilgreint"
    )
    d.trans = enframe(v.trans)
    l.trans = as.list(v.trans)

    # Visual verifications demonstrates that for factor variables,
    # the order of items is preserved
    expect_silent(    gg_apply(p, lookup, d.trans)   )
    expect_silent(    gg_apply(p, lookup, v.trans)   )
    expect_silent(    gg_apply(p, lookup, l.trans)   )
  }
})

test_that("gg_integer_breaks works", {
  if ( require(ggplot2) ) {
    p <- ggplot(mtcars) +
      aes(wt,drat) +
      geom_point()

    expect_silent(p)
    expect_silent(
      p + scale_y_continuous(breaks=gg_integer_breaks(mtcars$drat))
    )
  }
})

