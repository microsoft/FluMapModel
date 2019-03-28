# "makefile" for building predictModelTestPkg and installing locally
# assumes working directory is package root

roxygen2::roxygenise()
devtools::build_vignettes()
devtools::install()
