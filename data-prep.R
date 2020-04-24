library(plyr) # for count function
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)

#setwd('/home/sean/Dropbox/workspace/database-compare/') # when I'm using NVim-R
dbterms <- read.csv(file = "data/data.csv", header = TRUE, sep = "\t")

# convert terms to characters
dbterms$Term <- as.character(dbterms$Term)

# Merge 1911 with 1910 and 1982 with 1980 start years
dbterms$StartYear[dbterms$StartYear == 1911 ] <- 1910
dbterms$StartYear[dbterms$StartYear == 1982 ] <- 1980
# Merge odd year out with 1999 end year
dbterms$EndYear[dbterms$EndYear == 1991] <- 1999

