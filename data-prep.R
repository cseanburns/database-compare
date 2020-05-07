library(plyr) # for count function
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)

dbterms <- read.csv(file = "data/data.csv", header = TRUE, sep = "\t")

# convert terms to characters
dbterms$Term <- as.character(dbterms$Term)

# Merge 1886 with 1880
dbterms$StartYear[dbterms$StartYear == 1886] <- 1880
# Merge odd year out with 1999 end year
dbterms$EndYear[dbterms$EndYear == 1920] <- 1929
dbterms$EndYear[dbterms$EndYear == 1981] <- 1989
