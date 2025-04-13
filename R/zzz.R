.onLoad <- function(libname, pkgname) {
    # Suppress startup messages for specific packages
    suppressPackageStartupMessages({
        require(data.table)
        require(crayon)
    })

    # Create a colorful message
    cat(crayon::cyan("HPFC.\n"))
    cat(crayon::yellow("Version: 1.0.0\n"))
    cat(crayon::yellow("Author: Eleonora Gasparri & Alejandro Abraham\n"))
    cat(crayon::yellow("Reproduction and distribution are forbidden by license.\n"))

}