## Term Analysis
## Main reference: https://www.tidytextmining.com/tidytext.html

# Needed for 2-basic-analysis.R but not here
#detach("package:RColorBrewer", unload = TRUE)

##### Section 1: tf-idf faceted by Database #####

# 1890 - 1929

termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- subset(termstfidf, EndYear < 1930)
table(termstfidf$StartYear) ; table(termstfidf$EndYear)

# Note that I remove terms, but not documents (databases), that appear only once

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  filter(n != 1) %>%
  head(50)

# the term 'propaganda in the schools' has a high tf-idf because
# it appears frequently in the data but the database (EIR, the document)
# only appears very infrequently in the data. 
# interpretation: so the above term becomes highly relevant when
# measured against the other databases, such that EIR, as a document,
# is considered rare. That is, the above term is frequent for the EIR
# but rare for the time period and with respect to the other databases
jpeg('plots/terms-by-database-1880-1920s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for 1880s through 1920s") +
  xlab("Terms") +
  ylab("Term Frequencies") +
  theme(text = element_text(face = "bold"))
dev.off()

# 1930 - 1959
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1930 & EndYear < 1960)
table(termstfidf$StartYear) ; table(termstfidf$EndYear)

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  filter(n != 1) %>%
  head(50)

jpeg('plots/terms-by-database-1930-1950s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for 1930s through 1950s") +
  xlab("Terms") +
  ylab("Term Frequencies") +
  theme(text = element_text(face = "bold"))
dev.off()

# 1960 - 1989

termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1960 & EndYear < 1990)
table(termstfidf$StartYear) ; table(termstfidf$EndYear)

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  filter(n != 1) %>%
  head(50)

jpeg('plots/terms-by-database-1960-1980s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for 1960s through 1980s") +
  xlab("Terms") +
  ylab("Term Frequencies") +
  theme(text = element_text(face = "bold"))
dev.off()

# 1990 - 2019

termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1990)
table(termstfidf$StartYear) ; table(termstfidf$EndYear)

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  filter(n != 1) %>%
  head(50)

jpeg('plots/terms-by-database-1990-2010s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for 1990s through 2010s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

rm(termstfidf)

##### Section 2: tf-idf Per Database #####

termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)

termstfidfBSP     <- subset(termstfidf, Database == "BSP")
termstfidfCINAHL  <- subset(termstfidf, Database == "CINAHL")
termstfidfCMMC    <- subset(termstfidf, Database == "CMMC")
termstfidfEconLit <- subset(termstfidf, Database == "EconLit")
termstfidfEFT     <- subset(termstfidf, Database == "EFT")
termstfidfEIR     <- subset(termstfidf, Database == "EIR")
termstfidfGW      <- subset(termstfidf, Database == "GW")
termstfidfLISTA   <- subset(termstfidf, Database == "LISTA")
termstfidfLL      <- subset(termstfidf, Database == "LL")
termstfidfPI      <- subset(termstfidf, Database == "PI")

# All databases

termstfidfall <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidf$Database)

jpeg('plots/terms-per-all-databases.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfall %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for all Databases and All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# BSP
  
termstfidfBSP <- termstfidfBSP %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfBSP$Database)

jpeg('plots/terms-in-BSP.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfBSP %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for BSP and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# CINAHL

termstfidfCINAHL <- termstfidfCINAHL %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfCINAHL$Database)

jpeg('plots/terms-in-CINAHL.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfCINAHL %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for CINAHL and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# CMMC

termstfidfCMMC <- termstfidfCMMC %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfCMMC$Database)

jpeg('plots/terms-in-CMMC.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfCMMC %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for CMMC and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# EconLit

termstfidfEconLit <- termstfidfEconLit %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfEconLit$Database)

jpeg('plots/terms-in-EconLit.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfEconLit %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for EconLit and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# EFT

termstfidfEFT <- termstfidfEFT %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfEFT$Database)

jpeg('plots/terms-in-EFT.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfEFT %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for EFT and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# EIR

termstfidfEIR <- termstfidfEIR %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfEIR$Database)

jpeg('plots/terms-in-EIR.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfEIR %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for EIR and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# GW

termstfidfGW <- termstfidfGW %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfGW$Database)

jpeg('plots/terms-in-GW.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfGW %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for GW and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# LISTA

termstfidfLISTA <- termstfidfLISTA %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfLISTA$Database)

jpeg('plots/terms-in-LISTA.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfLISTA %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for LISTA and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# LL

termstfidfLL <- termstfidfLL %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfLL$Database)

jpeg('plots/terms-in-LL.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfLL %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for LL and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

# PI

termstfidfPI <- termstfidfPI %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)
table(termstfidfPI$Database)

jpeg('plots/terms-in-PI.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfPI %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Top Fifty Terms by tf-idf for PI and for All Years") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

rm(termstfidfBSP, termstfidfCINAHL, termstfidfCMMC, termstfidfEconLit,
   termstfidfEFT, termstfidfEIR, termstfidfGW, termstfidfLISTA, 
   termstfidfLL, termstfidfPI, termstfidfall, termstfidf)
