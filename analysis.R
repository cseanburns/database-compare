library(RColorBrewer) # for plot colors

setwd('/home/sean/Dropbox/workspace/database-compare/')
dbterms <- read.csv(file = "data.csv", header = TRUE, sep = "\t")

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

# Line plot total number of terms by decade
png('plots/growth.png', width = 1920, height = 1080, pointsize = 24)
plot(yearsterms,
     type = "l",
     xlab = "Decades",
     ylab = "Subject Frequencies",
     main = "Growth of Non-Unique Terms Related to Queries For Fake News")
dev.off()

# Calcualte percentage change
# https://stackoverflow.com/questions/20724203/need-to-calculate-rate-of-change-of-two-data-sets-over-time-individually-and-net
100 * diff(yearstermsdb$Freq) / yearstermsdb[-nrow(yearstermsdb),]$Freq

# Focus on frequency of database
# This needs to be re-coded and take $Freq into consideration
year2db <- table(dbterms$Database, dbterms$StartYear)
year2dbp <- prop.table(year2db, 2)

plotcolors <- brewer.pal(8, "Spectral") 

png('plots/year2dbcounts.png', width = 1920, height = 1080, pointsize = 24)
barplot(year2db, main = "Count Database Distribution by Decade",
        xlab = "Decades",
        col = plotcolors)
legend("left", col = plotcolors, fill = plotcolors, legend = rownames(year2db))
dev.off()

png('plots/year2dbproportions.png', width = 1920, height = 1080, pointsize = 24)
barplot(year2dbp, main = "Proportion of Database Distribution by Decade",
        xlab = "Decades",
        col = plotcolors)
legend("bottomleft", col = plotcolors,
       fill = plotcolors, legend = rownames(year2dbp), ncol = 1)
dev.off()

# Notes from meeting with Jenny
# look at terms by database in order to capture field differences, but
# also combine terms to be database agnostic 
# look at terms by decade in order to capture time series changes, but
# also combine terms to be date agnostic

# data with high frequency (HF) terms only for that decade (greater than mean) 

png('plots/heatmap1930.png', width = 1920, height = 1080, pointsize = 24)
dbtermsHF <- dbterms[dbterms$Freq > 4, ]
x <- dbtermsHF$Term[dbtermsHF$StartYear == 1930]
y <- dbtermsHF$Database[dbtermsHF$StartYear == 1930]
mdata <- as.matrix(table(x, y))
heatmap(mdata, Colv = NA, Rowv = NA, scale = "row", col = plotcolors)
dev.off()

png('plots/heatmap2010.png', width = 1920, height = 1080, pointsize = 24)
dbtermsHF <- dbterms[dbterms$Freq > 110, ]
x <- dbtermsHF$Term[dbtermsHF$StartYear == 2010]
y <- dbtermsHF$Database[dbtermsHF$StartYear == 2010]
mdata <- as.matrix(table(x, y))
heatmap(mdata, Colv = NA, Rowv = NA, scale = "row", col = plotcolors)
dev.off()
