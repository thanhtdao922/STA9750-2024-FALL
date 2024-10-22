#!/usr/bin/env Rscript

# Install quarto if not installed
if(!require("quarto")){
  install.packages("quarto")
}
library(quarto)

# Check quarto installation
if(!quarto::quarto_binary_sitrep()){
  stop("Something is wrong with your quarto installation. Try again.")
}

# Render the site to the 'docs/' folder
quarto::quarto_render(input = ".", output_dir = "docs")

# Add the 'docs' folder to Git
system("git add docs/*")

# Commit and push changes to GitHub
system("git commit -m 'Updated site with new HTML files'")
system("git push origin main") 