## Term Analysis
## Main reference: https://www.tidytextmining.com/tidytext.html

source("analysis.R")
detach("package:RColorBrewer", unload = TRUE)
library(dplyr)
library(scales)
library(tidyr)
library(tidytext)
library(ggplot2)

### Section 1: Word frequencies ###
# The following section examines single terms only; but this is misleading
# because these are database terms or controlled terminology and so terms 
# with more than one word need to be maintained in the analysis. Save that for 
# Section 2.

textdf <- tibble(dbterms)

textdf <- textdf %>% 
  unnest_tokens(word, Term)

# Don't remove stop words in n-gram analysis later
textdf <- textdf %>%
  anti_join(stop_words)

# expand data based on the Freq variable so that each observation is multiplied by its Freq count
textdf <- textdf %>% uncount(Freq)

# Chose n > 2870 to get the top ten terms
textdf %>% 
  count(word, sort = TRUE) %>%
  filter(n > 2870) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_bw() 

# Choose n > 700 so that all databases are plotted
textdf %>%
  count(Database, word, sort = TRUE) %>%
  filter(n > 700) %>%
  group_by(Database) %>%
  mutate(proportion = n / sum(n)) %>% 
  ggplot(aes(word, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~Database, ncol = 8) +
  theme_bw()


### Section 2: n-grams ###

textdf <- tibble(dbterms)
