source("0-load-libraries.R")

dbterms <- read.csv(file = "data/data.csv", header = TRUE, sep = "\t")

# convert terms to characters
dbterms$Term <- as.character(dbterms$Term)
dbterms$Freq <- as.integer(dbterms$Freq)

# Check StartYears and assign years to start of decade
table(dbterms$StartYear)
dbterms$StartYear[dbterms$StartYear == 1886] <- 1880
dbterms$StartYear[dbterms$StartYear == 1982] <- 1980
table(dbterms$StartYear)

# Assign years to end of decade
table(dbterms$EndYear)
dbterms$EndYear[dbterms$EndYear == 1981] <- 1989
dbterms$EndYear[dbterms$EndYear == 2008] <- 2009
table(dbterms$EndYear)
