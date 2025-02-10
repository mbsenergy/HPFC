.onLoad <- function(libname, pkgname) {
    # Suppress startup messages for specific packages
    suppressPackageStartupMessages({
        require(data.table)
        require(crayon)
    })

    # Create a colorful message
    cat(crayon::cyan("HPFC package loaded.\n"))
    cat(crayon::yellow("Version: 0.7.0\n"))
    cat(crayon::yellow("Author: Eleonora Gasparri & Alejandro Abraham\n"))
    cat(crayon::yellow("For internal use only. Reproduction and distribution are forbidden by license.\n"))

}