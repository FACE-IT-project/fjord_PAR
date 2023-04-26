# code/1_download.R
# Script for downloading data using the FjordLight package


# Setup -------------------------------------------------------------------

source("code/0_functions.R")

# List of available fjords
fl_ListFjords()


# Download data -----------------------------------------------------------

# Kongsfjorden
fl_DownloadFjord("kong", dirdata = "data/PAR")

# Isfjorden
fl_DownloadFjord("is", dirdata = "data/PAR")

# Storfjorden
fl_DownloadFjord("stor", dirdata = "data/PAR")

# Young Sound
fl_DownloadFjord("young", dirdata = "data/PAR")

# Disko Bay
fl_DownloadFjord("disko", dirdata = "data/PAR")

# Nuup Kangerlua
fl_DownloadFjord("nuup", dirdata = "data/PAR")

# Porsangerfjorden
fl_DownloadFjord("por", dirdata = "data/PAR")

# Tromso
## NB: Not used in this project

