## Topic Modeling

library(topicmodels)

termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)

termstfidf <- termstfidf %>% count(Database, Term)

termstfidf <- termstfidf %>% cast_dtm(Database, Term, n)
termstfidf <- LDA(termstfidf, k = 10, control = list(seed = 1000))

alltopics <- tidy(termstfidf, matrix = "beta")
dbtopics  <- tidy(termstfidf, matrix = "gamma")

dbtopterms <- alltopics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

dbtopterms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Each document (database) as a mixture of topics

