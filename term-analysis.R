## Term Analysis
## Main reference: https://www.tidytextmining.com/tidytext.html

source("data-prep.R")
detach("package:RColorBrewer", unload = TRUE)


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

### Section 2: whole terms by entire data set ###
# This is another exploratory section, like Section 1.
# The problem is that the second facet plot facets by database
# but by frequency of terms for the entire set.
# The next step is to plot the most frequent terms by database.

termsall <- tibble(dbterms)

# expand data based on the Freq variable so that each observation is multiplied by its Freq count
termsall <- termsall %>% uncount(Freq)

# Chose n > 1372 to get the top ten terms
termsall %>% 
  count(Term, sort = TRUE) %>%
  filter(n > 1372) %>%
  mutate(word = reorder(Term, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_bw() 

# Choose n > 350 so that all databases are plotted and readable
termsall %>%
  count(Database, Term, sort = TRUE) %>%
  filter(n > 350) %>%
  group_by(Database) %>%
  mutate(proportion = n / sum(n)) %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(~Database, ncol = 8) +
  theme_bw()

### Section 3: whole terms by database ###
# 4/15/2020: this is still an awful way to plot all these, need 
# to figure out how to do this better

# 1890
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1890 & n > 0) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database) +
  theme_bw()

# 1910
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1910 & n > 1) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database) +
  theme_bw()

# 1920
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1920 & n > 2) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database) +
  theme_bw()

# 1930
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1930 & n > 4) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database) +
  theme_bw()

# 1940
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1940 & n > 7) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 4) +
  theme_bw()

# 1950
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1950 & n > 8) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 5) +
  theme_bw()

# 1960
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1960 & n > 10) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 6) +
  theme_bw()

# 1970
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1970 & n > 17) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 6) +
  theme_bw()

# 1980
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1980 & n > 23) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 7) +
  theme_bw()

# 1990
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 1990 & n > 50) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 6) +
  theme_bw()

# 2000
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 2000 & n > 110) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 6) +
  theme_bw()

# 2010
termsbydb <- tibble(dbterms)
termsbydb <- termsbydb %>% uncount(Freq)

termsbydb <- termsbydb %>%
  count(Database, StartYear, Term, sort = TRUE) %>%
  filter(StartYear == 2010 & n > 120) %>%
  group_by(StartYear) %>%
  mutate(proportion = n / sum(n))
  
termsbydb %>% 
  ggplot(aes(Term, proportion)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  facet_wrap(StartYear~Database, ncol = 7) +
  theme_bw()

# Section 4: tf*idf

termstfidf <- tibble(dbterms)
termstfidf <- termstfidf %>% uncount(Freq)

termstfidf.1 <- termstfidf %>% 
  count(Database, Term) %>%
  bind_tf_idf(Term, Database, n) %>%
  arrange(desc(tf_idf)) %>%
  head(75) #%>%
  #arrange(desc(tf_idf))

termstfidf.1 %>%
  ggplot(aes(Term, tf_idf, fill = Database)) +
  geom_col(show.legend = TRUE) +
  coord_flip() +
  theme_classic() +
  theme(text = element_text(face = "bold"))