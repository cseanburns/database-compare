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
