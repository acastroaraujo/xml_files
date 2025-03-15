# Set up -----------------------------------------------------------------

library(xml2)
library(dplyr)
library(purrr)
library(fs)

source("helpers.R")

file_names <- fs::dir_ls("data/")

# Rename files ------------------------------------------------------------

# This chunk removes "Sample_USC_" prefix from the XML file names, but only if it
# hasn't been done already.

if (all(grepl(pattern = "Sample_USC_", x = file_names))) {
  fs::file_move(
    path = file_names,
    new_path = base::sub(
      pattern = "data/Sample_USC_",
      replacement = "data/",
      x = file_names
    )
  )
  file_names <- fs::dir_ls("data/")
}

# Read Data --------------------------------------------------------------

df <- map_df(file_names, \(x) {
  out <- process_file(x)
  out$query <- as.character(gsub(pattern = "^data/", replacement = "", x))
  relocate(out, query)
})

df
