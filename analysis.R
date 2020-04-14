library("RColorBrewer") # for plot colors
library("plyr") # for count function

#setwd('/home/sean/Dropbox/workspace/database-compare/') # when I'm using NVim-R
dbterms <- read.csv(file = "data/data.csv", header = TRUE, sep = "\t")

# convert terms to characters
dbterms$Term <- as.character(dbterms$Term)

# Merge 1911 with 1910 and 1982 with 1980 start years
dbterms$StartYear[dbterms$StartYear == 1911 ] <- 1910
dbterms$StartYear[dbterms$StartYear == 1982 ] <- 1980
# Merge odd year out with 1999 end year
dbterms$EndYear[dbterms$EndYear == 1991] <- 1999

# Calculate the total number of terms by decade
yearsterms <- as.table(tapply(dbterms$Freq, dbterms$StartYear, FUN = sum))
# Convert to data frame
yearstermsdb <- data.frame(yearsterms)
names(yearstermsdb) <- c("Decade", "Frequency")
yearstermsdb$Decade <- as.Date(yearstermsdb$Decade, "%Y")

# Line plot total number of terms by decade
#jpeg('plots/growth.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
plot(yearstermsdb$Decade, yearstermsdb$Frequency,
     type = "b",
     xlab = "Decades",
     ylab = "Subject Frequencies",
     main = "Growth of Non-Unique Terms Related to Queries For Fake News")
text(yearstermsdb$Decade, yearstermsdb$Frequency, pos = 2,
     offset = 1,
     labels = yearstermsdb$Frequency)
#dev.off()

# Focus on frequency of database, weighted by frequency of terms per database
plotcolors <- brewer.pal(8, "Spectral") 

weightedDB <- count(dbterms, vars = 'Database', wt_var = 'Freq')
names(weightedDB) <- c('Database', 'Term_Frequency')
weightedDB$Percentage <- round(weightedDB$Term_Frequency / sum(dbterms$Freq) * 100, 2)

#jpeg('plots/weightedDB.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
barplot(weightedDB$Term_Frequency, col = plotcolors,
        names.arg = weightedDB$Database)
#dev.off()

# Set up barplots by decade and frequency of terms per database
weightedDB1890 <- subset(dbterms, subset = dbterms$StartYear == 1890)
weightedDB1890 <- count(weightedDB1890, vars = 'Database', wt_var = 'Freq')
weightedDB1890$Decade <- "1890"

weightedDB1900 <- subset(dbterms, subset = dbterms$StartYear == 1900)
weightedDB1900 <- count(weightedDB1900, vars = 'Database', wt_var = 'Freq')
weightedDB1900$Decade <- "1900"

weightedDB1910 <- subset(dbterms, subset = dbterms$StartYear == 1910)
weightedDB1910 <- count(weightedDB1910, vars = 'Database', wt_var = 'Freq')
weightedDB1910$Decade <- "1910"

weightedDB1920 <- subset(dbterms, subset = dbterms$StartYear == 1920)
weightedDB1920 <- count(weightedDB1920, vars = 'Database', wt_var = 'Freq')
weightedDB1920$Decade <- "1920"

weightedDB1930 <- subset(dbterms, subset = dbterms$StartYear == 1930)
weightedDB1930 <- count(weightedDB1930, vars = 'Database', wt_var = 'Freq')
weightedDB1930$Decade <- "1930"

weightedDB1940 <- subset(dbterms, subset = dbterms$StartYear == 1940)
weightedDB1940 <- count(weightedDB1940, vars = 'Database', wt_var = 'Freq')
weightedDB1940$Decade <- "1940"

weightedDB1950 <- subset(dbterms, subset = dbterms$StartYear == 1950)
weightedDB1950 <- count(weightedDB1950, vars = 'Database', wt_var = 'Freq')
weightedDB1950$Decade <- "1950"

weightedDB1960 <- subset(dbterms, subset = dbterms$StartYear == 1960)
weightedDB1960 <- count(weightedDB1960, vars = 'Database', wt_var = 'Freq')
weightedDB1960$Decade <- "1960"

weightedDB1970 <- subset(dbterms, subset = dbterms$StartYear == 1970)
weightedDB1970 <- count(weightedDB1970, vars = 'Database', wt_var = 'Freq')
weightedDB1970$Decade <- "1970"

weightedDB1980 <- subset(dbterms, subset = dbterms$StartYear == 1980)
weightedDB1980 <- count(weightedDB1980, vars = 'Database', wt_var = 'Freq')
weightedDB1980$Decade <- "1980"

weightedDB1990 <- subset(dbterms, subset = dbterms$StartYear == 1990)
weightedDB1990 <- count(weightedDB1990, vars = 'Database', wt_var = 'Freq')
weightedDB1990$Decade <- "1990"

weightedDB2000 <- subset(dbterms, subset = dbterms$StartYear == 2000)
weightedDB2000 <- count(weightedDB2000, vars = 'Database', wt_var = 'Freq')
weightedDB2000$Decade <- "2000"

weightedDB2010 <- subset(dbterms, subset = dbterms$StartYear == 2010)
weightedDB2010 <- count(weightedDB2010, vars = 'Database', wt_var = 'Freq')
weightedDB2010$Decade <- "2010"

#jpeg('plots/decadefreqbarplots.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
par(mfrow = c(4, 4))
barplot(weightedDB1890$freq, names.arg = weightedDB1890$Database, las = 2, col = plotcolors, main = "1890s")
barplot(weightedDB1900$freq, names.arg = weightedDB1900$Database, las = 2, col = plotcolors, main = "1900s")
barplot(weightedDB1910$freq, names.arg = weightedDB1910$Database, las = 2, col = plotcolors, main = "1910s")
barplot(weightedDB1920$freq, names.arg = weightedDB1920$Database, las = 2, col = plotcolors, main = "1920s")
barplot(weightedDB1930$freq, names.arg = weightedDB1930$Database, las = 2, col = plotcolors, main = "1930s")
barplot(weightedDB1940$freq, names.arg = weightedDB1940$Database, las = 2, col = plotcolors, main = "1940s")
barplot(weightedDB1950$freq, names.arg = weightedDB1950$Database, las = 2, col = plotcolors, main = "1950s")
barplot(weightedDB1960$freq, names.arg = weightedDB1960$Database, las = 2, col = plotcolors, main = "1960s")
barplot(weightedDB1970$freq, names.arg = weightedDB1970$Database, las = 2, col = plotcolors, main = "1970s")
barplot(weightedDB1980$freq, names.arg = weightedDB1980$Database, las = 2, col = plotcolors, main = "1980s")
barplot(weightedDB1990$freq, names.arg = weightedDB1990$Database, las = 2, col = plotcolors, main = "1990s")
barplot(weightedDB2000$freq, names.arg = weightedDB2000$Database, las = 2, col = plotcolors, main = "2000s")
barplot(weightedDB2010$freq, names.arg = weightedDB2010$Database, las = 2, col = plotcolors, main = "2010s")
#dev.off()

rm(weightedDB1890, weightedDB1900, weightedDB1910, weightedDB1920, weightedDB1930,
   weightedDB1940, weightedDB1950, weightedDB1960, weightedDB1970, weightedDB1980,
   weightedDB1990, weightedDB2000, weightedDB2010)