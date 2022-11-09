
# This script manages the development build process for zulutils.
# The starting point is a clean checkout, typically from Github:
# > git clone https://github.com/torfason/zulutils




## Development builds of documentation and package
{
  devtools::document()
  system("R CMD INSTALL --preclean --no-multiarch --with-keep.source .")
  devtools::build_readme()
  devtools::build_site()
}


## Additional build tasks
{
  devtools::build()
  devtools::build_vignettes()
  devtools::build_manual()
}


## Final checks before release
{
  devtools::spell_check()
  devtools::test()
  devtools::check()
  devtools::release_checks()
  devtools:::git_checks()
}


## Remote checks (commented out, copy to terminal and run manually)
# devtools::check_rhub()
# devtools::check_win_devel()

## Finally submit to cran (commented out, copy to terminal and run manually)
# devtools::release()

## There should be no need to run the following cleanup tasks
# devtools::clean_vignettes()
# pkgdown::clean_site()
