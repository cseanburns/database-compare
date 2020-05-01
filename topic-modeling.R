## Topic Modeling

termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)

termstfidf <- termstfidf %>% count(Database, Term)

termstfidf <- termstfidf %>% cast_dtm(Database, Term, n)
termstfidf <- LDA(termstfidf, k = 2, control = list(seed = 1000))

dbtopics <- tidy(termstfidf, matrix = "beta")

dbtopterms <- dbtopics %>%
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
