## Term Analysis
## Main reference: https://www.tidytextmining.com/tidytext.html

source("1-data-prep.R")
detach("package:RColorBrewer", unload = TRUE)

##### Section 1: tf*idf by Term Type #####

## 1880 - 1929
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- subset(termstfidf, EndYear < 1930)

termstfidf <- termstfidf %>%
  count(TermType, Term) %>%
  bind_tf_idf(Term, TermType, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = TermType)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1880s through 1920s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))

## 1930 - 1959
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1930 & EndYear < 1960)

termstfidf <- termstfidf %>%
  count(TermType, Term) %>%
  bind_tf_idf(Term, TermType, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = TermType)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1930s through 1950s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))

## 1960 - 1989
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1960 & EndYear < 1990)

termstfidf <- termstfidf %>%
  count(TermType, Term) %>%
  bind_tf_idf(Term, TermType, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = TermType)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1960s through 1980s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))

## 1990 - 2019
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1990)

termstfidf <- termstfidf %>%
  count(TermType, Term) %>%
  bind_tf_idf(Term, TermType, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = TermType)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1990s through 2010s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))

##### Section 2: tf*idf by Database #####

## 1890 - 1929
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- subset(termstfidf, EndYear < 1930)

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

jpeg('plots/termsbyDB1880-1920s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1880s through 1920s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

## 1930 - 1959
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1930 & EndYear < 1960)

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

jpeg('plots/termsbyDB1930-1950s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1930s through 1950s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

## 1960 - 1989
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1960 & EndYear < 1990)

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

jpeg('plots/termsbyDB1960-1980s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1960s through 1980s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

## 1990 - 2019
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1990)

termstfidf <- termstfidf %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

jpeg('plots/termsbyDB1990-2010s.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1990s through 2010s") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()

rm(termstfidf)

##### Section 3: Per Database #####

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

# BSP
  
termstfidfBSP <- termstfidfBSP %>%
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf)) %>%
  head(50)

jpeg('plots/termsinBSP.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfBSP %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for BSP") +
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

jpeg('plots/termsinCINAHL.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfCINAHL %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for CINAHL") +
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

jpeg('plots/termsinCMMC.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfCMMC %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for CMMC") +
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

jpeg('plots/termsinEconLit.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfEconLit %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for EconLit") +
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

jpeg('plots/termsinEFT.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfEFT %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for EFT") +
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

jpeg('plots/termsinEIR.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfEIR %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for EIR") +
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

jpeg('plots/termsinGW.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfGW %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for GW") +
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

jpeg('plots/termsinLISTA.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfLISTA %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for LISTA") +
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

jpeg('plots/termsinLL.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfLL %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for LL") +
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

jpeg('plots/termsinPI.jpg', width = 3840, height = 2160, pointsize = 12, res = 300)
termstfidfPI %>%
  ggplot(aes(x = reorder(Term, tf), y = tf)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Frequent Terms for PI") +
  xlab("Terms") +
  ylab("") +
  theme(text = element_text(face = "bold"))
dev.off()
