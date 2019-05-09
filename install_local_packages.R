# install_local_packages

pkgs <- c('dbViewR','incidenceMapR','modelTestR','modelServR')

for(pkg in pkgs){
  roxygen2::roxygenise(pkg)
  devtools::build_vignettes(pkg)
  devtools::install(pkg)
}

