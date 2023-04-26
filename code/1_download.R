# code/1_download.R
# Script for downloading data using the FjordLight package


# Setup -------------------------------------------------------------------

source("code/0_functions.R")

# List of available fjords
fl_ListFjords()


# Download data -----------------------------------------------------------

# Kongsfjorden
fl_DownloadFjord("kong", dirdata = "data/PAR")

