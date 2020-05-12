source("0-load-libraries.R")

dbterms <- read.csv(file = "data/data.csv", header = TRUE, sep = "\t")

# convert terms to characters
dbterms$Term <- as.character(dbterms$Term)
dbterms$Freq <- as.integer(dbterms$Freq)

# Assign years to start of decade
dbterms$StartYear[dbterms$StartYear == 1886] <- 1880
dbterms$StartYear[dbterms$StartYear == 1982] <- 1980

# Assign years to end of decade
dbterms$EndYear[dbterms$EndYear == 1920] <- 1929
dbterms$EndYear[dbterms$EndYear == 1981] <- 1989
