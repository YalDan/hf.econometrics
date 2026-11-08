# Development-only dependencies, declared for renv's implicit snapshot.
#
# This file is never sourced. renv scans the project for package usage, and
# these packages are needed to work on the package but are not imported by
# it: roxygen2 regenerates NAMESPACE and man/; testthat and pkgload run the
# tests and the simulation scripts in tests/simulations/.
library(roxygen2)
library(testthat)
library(pkgload)
