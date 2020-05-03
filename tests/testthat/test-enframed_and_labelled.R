

#### test lookup_enframed() ####
test_that("lookup_enframed works", {

    # Validation data
    d.pets = tibble::enframe(c(
        cat="mammal",
        lizard="reptile",
        parrot="bird"
    ))
    x.species  = c("lizard", "cat")
    x.kingdoms = c("reptile","mammal")

    # Standard lookup
    expect_equal( lookup_enframed(x.species,d.pets),  x.kingdoms  )

    # Order should not matter, only the names of the name/value columns.
    d.pets.rearranged = d.pets %>%
        dplyr::mutate(ga = "ga", rb="rb", age="age") %>%
        dplyr::select(ga,name,rb,value,age)
    expect_equal( lookup_enframed(x.species,d.pets.rearranged),  x.kingdoms)
})

#### test rename_enframed() ####
test_that("rename_enframed works", {

    # Test data
    data   = tibble::tibble(plain_numbers=1:4,plain_letters=letters[1:4])
    final  = tibble::tibble(`The Numbers`=1:4, `The Letters`=letters[1:4])
    labels = tibble::enframe(c(plain_numbers="The Numbers",plain_letters="The Letters"))

    # The main test
    expect_equal(rename_enframed(data,labels), final)

    # Test that both inputs are data.frames
    expect_error(rename_enframed(data,letters))
    expect_error(rename_enframed(letters,labels))
})


#### test get_labels_enframed() ####
test_that("get_labels_enframed works", {

    # Test data (input)
    data_all     = cars
    attr(data_all$speed,"label") <- "Speed (mph)"
    attr(data_all$dist, "label") <- "Stopping distance (ft)"
    data_partial = cars
    attr(data_partial$speed,"label") <- "Speed (mph)"
    data_none    = cars

    # Test data (output)
    out_all = tibble::tibble(
        name=c("speed","dist"),
        value=c("Speed (mph)","Stopping distance (ft)")
    )
    out_none = tibble::tibble(
        name=c("speed","dist"),
        value=c(NA, NA)
    )
    out_partial = tibble::tibble(
        name=c("speed","dist"),
        value=c("Speed (mph)",NA)
    )

    # The main test
    expect_equal(get_labels_enframed(data_all), out_all)

    # Missing labels
    expect_equal(get_labels_enframed(data_partial), out_partial)
    expect_equal(get_labels_enframed(data_none), out_none)
})





