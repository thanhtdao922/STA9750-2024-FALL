#!/usr/bin/env Rscript
# Install quarto

if(!require("quarto")){
  install.packages("quarto")
}
library(quarto)
if(!quarto::quarto_binary_sitrep()){
  stop("Something is wrong with your quarto installation. Try again.")
}
quarto::quarto_render(".")
system("git add docs/*")
if(!any(grepl("rstudio", search()))){q("no")}