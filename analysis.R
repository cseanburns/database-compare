library(RColorBrewer) # for plot colors
library(wordcloud)

dbterms <- read.csv(file = "database-compare/data.csv", header = TRUE, sep = ":")

dbterms$Term <- as.character(dbterms$Term)

# Merge 1911 with 1910 and 1982 with 1980 start years
dbterms$StartYear[dbterms$StartYear == 1911 ] <- 1910
dbterms$StartYear[dbterms$StartYear == 1982 ] <- 1980
dbterms$EndYear[dbterms$EndYear == 1991] <- 1999

# Remove all WoS records for subject analysis
dbtermsSub <- dbterms
dbtermsSub <- subset(dbtermsSub, Database != "WOS")
yearsterms <- as.table(tapply(dbtermsSub$Freq, dbtermsSub$StartYear, FUN = sum))
yearstermsdb <- data.frame(yearsterms)

png('database-compare/plots/growth.png', width = 1920, height = 1080, pointsize = 24)
plot(yearsterms,
     type = "l",
     xlab = "Decades",
     ylab = "Subject Frequencies",
     main = "Growth of Non-Unique Terms Related to Queries For Fake News")
dev.off()

# percentage change
# https://stackoverflow.com/questions/20724203/need-to-calculate-rate-of-change-of-two-data-sets-over-time-individually-and-net
100 * diff(yearstermsdb$Freq) / yearstermsdb[-nrow(yearstermsdb),]$Freq

year2db <- table(dbtermsSub$Database, dbtermsSub$StartYear)
year2dbp <- prop.table(year2db, 2)

plotcolors <- brewer.pal(8, "Spectral") 

png('database-compare/plots/year2dbcounts.png', width = 1920, height = 1080, pointsize = 24)
barplot(year2db, main = "Count Database Distribution by Decade",
        xlab = "Decades",
        col = plotcolors)
legend("left", col = plotcolors, fill = plotcolors, legend = rownames(year2db))
dev.off()

png('database-compare/plots/year2dbproportions.png', width = 1920, height = 1080, pointsize = 24)
barplot(year2dbp, main = "Proportion of Database Distribution by Decade",
        xlab = "Decades",
        col = plotcolors)
legend("topleft", col = plotcolors,
       fill = plotcolors, legend = rownames(year2dbp), ncol = 1)
dev.off()

# Notes from meeting with Jenny
# look at terms by database in order to capture field differences, but
# also combine terms to be database agnostic 
# look at terms by decade in order to capture time series changes, but
# also combine terms to be date agnostic

t1890 <- dbterms$Term[dbterms$StartYear == 1890]
f1890 <- dbterms$Freq[dbterms$StartYear == 1890]

wordcloud(words = t1890, freq = f1890,
          min.freq = mean(f1890),
          max.words = length(t1890),
          random.order = FALSE, colors = plotcolors)


t1900 <- dbterms$Term[dbterms$StartYear == 1900]
f1900 <- dbterms$Freq[dbterms$StartYear == 1900]

wordcloud(words = t1900, freq = f1900,
          min.freq = mean(f1900),
          max.words = length(t1900),
          random.order = FALSE, colors = plotcolors)


t1910 <- dbterms$Term[dbterms$StartYear == 1910]
f1910 <- dbterms$Freq[dbterms$StartYear == 1910]

wordcloud(words = t1910, freq = f1910,
          min.freq = mean(f1910),
          max.words = length(t1910),
          random.order = FALSE, colors = plotcolors)

wordcloud(words = dbterms$Term[dbterms$StartYear == 1890],
          freq = dbterms$Freq[dbterms$StartYear == 1890],
          min.freq = mean(dbterms$Freq[dbterms$StartYear == 1890]),
          max.words = length(dbterms$Freq[dbterms$Freq > mean(dbterms$Freq)]),
          random.order = FALSE, colors = plotcolors)

# examine abi-noft, only top terms
abi <- cbind(dbterms$Term[dbterms$Database == "ABI-NOFT"],
            dbterms$Freq[dbterms$Database == "ABI-NOFT"])
abi <- data.frame(abi)
abi$X2 <- as.integer(abi$X2)
wordcloud(abi$X1[abi$X2 > 30], abi$X2[abi$X2 > 30])
wordcloud(abi$X1, abi$X2,
          rot.per = 0, colors = plotcolors,
          random.order = FALSE)


# examine eric only top terms
eric <- cbind(dbterms$Term[dbterms$Database == "ERIC"],
            dbterms$Freq[dbterms$Database == "ERIC"])
eric <- data.frame(eric)
eric$X2 <- as.integer(eric$X2)
wordcloud(eric$X1[eric$X2 > 50], eric$X2[eric$X2 > 50])
wordcloud(eric$X1[eric$X2 > 40], eric$X2[eric$X2 > 40])
wordcloud(eric$X1[eric$X2 > 30], eric$X2[eric$X2 > 30],
          rot.per = 0, colors = plotcolors,
          random.order = FALSE)
wordcloud(eric$X1, eric$X2,
          rot.per = 0, colors = plotcolors,
          random.order = FALSE)

# examine library literature (ll) only top terms
ll <- cbind(dbterms$Term[dbterms$Database == "LL"],
            dbterms$Freq[dbterms$Database == "LL"])
ll <- data.frame(ll)
ll$X2 <- as.integer(ll$X2)
wordcloud(ll$X1[ll$X2 > 50], ll$X2[ll$X2 > 50])
wordcloud(ll$X1[ll$X2 > 40], ll$X2[ll$X2 > 40],
          rot.per = 0, colors = plotcolors,
          random.order = FALSE)
wordcloud(ll$X1, ll$X2,
          rot.per = 0, colors = plotcolors,
          random.order = FALSE)

# examine wos/medline only top terms
mw <- cbind(dbterms$Term[dbterms$Database == "MW"],
            dbterms$Freq[dbterms$Database == "MW"])
mw <- data.frame(mw)
mw$X2 <- as.integer(mw$X2)
wordcloud(mw$X1[mw$X2 > 50], mw$X2[mw$X2 > 50])
wordcloud(mw$X1[mw$X2 > 60], mw$X2[mw$X2 > 60],
          rot.per = 0, colors = plotcolors,
          random.order = FALSE)




x <- dbterms$Term
y <- dbterms$Freq
allterms <- rep(x, times = c(y))
head(allterms)
rle(allterms)

png('plots/termsbydb.png', width = 1920, height = 1080, pointsize = 24)
plot(dbterms$Database, log(dbterms$Freq))
dev.off()


dbterms$Term <- as.character(dbterms$Term)

png('plots/termsbydb.png', width = 1920, height = 1080, pointsize = 24)
plot(dbterms$Database, log(dbterms$Freq))
dev.off()

png('plots/heatmap-200.png', width = 1920, height = 1080, pointsize = 24)
heatmap(as.matrix(table(dbterms$Term[dbterms$Freq > 200],
                        dbterms$Database[dbterms$Freq > 200])),
        scale = "column",
        col = rainbow(256))
dev.off()

png('plots/heatmap-1890.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1890]
y <- dbterms$Database[dbterms$StartYear == 1890]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1900.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1900]
y <- dbterms$Database[dbterms$StartYear == 1900]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1910.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1910]
y <- dbterms$Database[dbterms$StartYear == 1910]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1920.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1920]
y <- dbterms$Database[dbterms$StartYear == 1920]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1930.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1930]
y <- dbterms$Database[dbterms$StartYear == 1930]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1940.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1940]
y <- dbterms$Database[dbterms$StartYear == 1940]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1950.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1950]
y <- dbterms$Database[dbterms$StartYear == 1950]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1960.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1960]
y <- dbterms$Database[dbterms$StartYear == 1960]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1970.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1970]
y <- dbterms$Database[dbterms$StartYear == 1970]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1980.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1980]
y <- dbterms$Database[dbterms$StartYear == 1980]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-1990.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 1990]
y <- dbterms$Database[dbterms$StartYear == 1990]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-2000.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 2000]
y <- dbterms$Database[dbterms$StartYear == 2000]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()

png('plots/heatmap-2010.png', width = 1920, height = 1080, pointsize = 24)
x <- dbterms$Term[dbterms$StartYear == 2010]
y <- dbterms$Database[dbterms$StartYear == 2010]
heatmap(as.matrix(table(x,y), scale = "column", col = rainbow(256)))
dev.off()
