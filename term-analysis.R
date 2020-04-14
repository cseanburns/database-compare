## Term Analysis
## Main reference: https://www.tidytextmining.com/tidytext.html

source("analysis.R")
detach("package:RColorBrewer", unload = TRUE)
library(dplyr)
library(scales)
library(tidyr)
library(tidytext)
library(ggplot2)

textdf <- tibble(dbterms)

### Word frequencies ###
# Examine single terms only
textdf <- textdf %>% 
  unnest_tokens(word, Term)

# Don't remove stop words in n-gram analysis later
textdf <- textdf %>%
  anti_join(stop_words)

# Chose n > 96 to get the top ten terms
textdf %>% 
  count(word, sort = TRUE) %>%
  #filter(n > 96) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_bw()

textdf %>% 
  count(Database, word) %>%
  filter(n > 4) %>%
  group_by(Database) %>%
  mutate(proportion = n / sum(n)) %>%
  ggplot(aes(word, proportion)) +
  geom_abline() +
  geom_jitter() +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  facet_wrap(~Database)