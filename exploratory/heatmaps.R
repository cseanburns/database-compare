# https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
# build corpus
myCorpus <- Corpus(VectorSource(dbterms$Term))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removePunctuation) 
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- stopwords("english")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

# keep copy
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >=50)
df <- data.frame(term = names(term.freq), freq = term.freq)
df <- df[order(df$freq),]

plot(df, las = 2)
barplot(df$freq, names.arg = df$term, las = 2, horiz = TRUE)

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

### -- older version:

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
x <- dbterms$Term[dbterms$StartYear == 1990 & dbterms$Freq > 50]
y <- dbterms$Database[dbterms$StartYear == 1990 &dbterms$Freq > 50]
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
