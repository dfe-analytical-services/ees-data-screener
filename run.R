## Running Script - turn this to a markdown?

# Load packages
library(tidyverse)

# Load your files
dataset <- read_csv("data_metadata/4and6digit_simple.csv")
metadata <- read_csv("data_metadata/4and6digit_simple.meta.csv")

# Build the screener functions
source("setup_file.R")

# Run the screener tests if needed for testing
# screening_tests(dataset,metadata)

# Attempts to get the proxy to work to download govdown
#set_config(use_proxy(url="192.168.2.40",port=8080,username="<AD\\CRACE>",password=getPass()))
#
#devtools::install_github("ukgovdatascience/govdown")