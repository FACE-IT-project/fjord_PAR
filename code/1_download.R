# code/1_download.R
# Script for downloading data using the FjordLight package


# Setup -------------------------------------------------------------------

source("code/0_functions.R")

# List of available fjords
fl_ListFjords()


# Download data -----------------------------------------------------------

# Kongsfjorden
fl_DownloadFjord("kong", dirdata = "data/PAR")
fl_DownloadFjord("kong", "K_PAR", dirdata = "data/PAR")
fl_DownloadFjord("kong", "ClimSD", dirdata = "data/PAR")
fl_DownloadFjord("kong", "YearlySD", dirdata = "data/PAR")


# Isfjorden
fl_DownloadFjord("is", dirdata = "data/PAR")
fl_DownloadFjord("is", "K_PAR", dirdata = "data/PAR")
fl_DownloadFjord("is", "ClimSD", dirdata = "data/PAR")
fl_DownloadFjord("is", "YearlySD", dirdata = "data/PAR")

# Storfjorden
fl_DownloadFjord("stor", dirdata = "data/PAR")
fl_DownloadFjord("stor", "K_PAR", dirdata = "data/PAR")
fl_DownloadFjord("stor", "ClimSD", dirdata = "data/PAR")
fl_DownloadFjord("stor", "YearlySD", dirdata = "data/PAR")

# Young Sound
fl_DownloadFjord("young", dirdata = "data/PAR")
fl_DownloadFjord("young", "K_PAR", dirdata = "data/PAR")
fl_DownloadFjord("young", "ClimSD", dirdata = "data/PAR")
fl_DownloadFjord("young", "YearlySD", dirdata = "data/PAR")

# Disko Bay
fl_DownloadFjord("disko", dirdata = "data/PAR")
fl_DownloadFjord("disko", "K_PAR", dirdata = "data/PAR")
fl_DownloadFjord("disko", "ClimSD", dirdata = "data/PAR")
fl_DownloadFjord("disko", "YearlySD", dirdata = "data/PAR")

# Nuup Kangerlua
fl_DownloadFjord("nuup", dirdata = "data/PAR")
fl_DownloadFjord("nuup", "K_PAR", dirdata = "data/PAR")
fl_DownloadFjord("nuup", "ClimSD", dirdata = "data/PAR")
fl_DownloadFjord("nuup", "YearlySD", dirdata = "data/PAR")

# Porsangerfjorden
fl_DownloadFjord("por", dirdata = "data/PAR")
fl_DownloadFjord("por", "K_PAR", dirdata = "data/PAR")
fl_DownloadFjord("por", "ClimSD", dirdata = "data/PAR")
fl_DownloadFjord("por", "YearlySD", dirdata = "data/PAR")

# Tromso
## NB: Not used in this project

