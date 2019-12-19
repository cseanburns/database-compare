library(wordcloud)

wordcloud(words = dbterms$Term, freq = dbterms$Freq,
          min.freq = 100,
          max.words = 100,
          colors = dbterms$Database)

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


