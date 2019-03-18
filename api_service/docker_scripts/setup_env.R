# This script is meant to be ran before the model. it manages out dependencies for
# our flu model R scripts

# load packages that are available in CRAN
cranPkgLoad <- function( packages = "favourites" ) {

    if( length( packages ) == 1L && packages == "favourites" ) {
        packages <- c(
            "optparse"
        )
    }

    # we should probably change this to devtools::install_version and parse a requirement.txt file
    # this will make it easier to maintain our R packages in both docker and dev environments
    packagecheck <- match( packages, utils::installed.packages()[,1] )

    packagestoinstall <- packages[ is.na( packagecheck ) ]

    if( length( packagestoinstall ) > 0L ) {
        utils::install.packages( packagestoinstall, repos = "http://cran.csiro.au" )
    } else {
        print( "All cran requested packages already installed" )
    }

    for( package in packages ) {
        suppressPackageStartupMessages(
            library( package, character.only = TRUE, quietly = TRUE )
        )
    }

}




# Later, replace this with reading the packages from a file so we can control them
# similar to python's setup.py or requirement.txt


cranPkgLoad(
    c(
    "optparse",
    "jsonlite",
    "magrittr",
    "dplyr",
    "tidyr"
    )
)

# install all avaiable models
setwd("./models")
pkgs <- list.files(path='./')
print(pkgs)
sapply(pkgs, function(x) {
    paste("file://", x, sep="")
})
print(pkgs)
install.packages(c(print(as.character(pkgs), collapse="\",\"")), dependencies=TRUE)