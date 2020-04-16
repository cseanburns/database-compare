## Term Analysis
## Main reference: https://www.tidytextmining.com/tidytext.html

source("data-prep.R")
detach("package:RColorBrewer", unload = TRUE)

# Section 1: tf*idf by Term Type

## 1890 - 1929
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
  xlab("Terms") + ylab("tf*idf") +
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
  xlab("Terms") + ylab("tf*idf") +
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
  xlab("Terms") + ylab("tf*idf") +
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
  xlab("Terms") + ylab("tf*idf") +
  theme(text = element_text(face = "bold"))

# Section 2: tf*idf by Database

## 1890 - 1929
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- subset(termstfidf, EndYear < 1930)

termstfidf <- termstfidf %>% 
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1880s through 1920s") +
  xlab("Terms") + ylab("tf*idf") +
  theme(text = element_text(face = "bold"))

## 1930 - 1959
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1930 & EndYear < 1960)

termstfidf <- termstfidf %>% 
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1930s through 1950s") +
  xlab("Terms") + ylab("tf*idf") +
  theme(text = element_text(face = "bold"))

## 1960 - 1989
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1960 & EndYear < 1990)

termstfidf <- termstfidf %>% 
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1960s through 1980s") +
  xlab("Terms") + ylab("tf*idf") +
  theme(text = element_text(face = "bold"))

## 1990 - 2019
termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)
termstfidf <- termstfidf %>% filter(EndYear > 1990)

termstfidf <- termstfidf %>% 
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(50)

termstfidf %>%
  ggplot(aes(x = reorder(Term, tf_idf), y = tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  ggtitle("Fifty Most Highly Ranked Terms for 1990s through 2010s") +
  xlab("Terms") + ylab("tf*idf") +
  theme(text = element_text(face = "bold"))
