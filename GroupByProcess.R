library(tidyverse)
library(tidytext)

# Simple methods to read and tokenize a list of job descriptions

sample_lines <- c(
  'wheels.txt', 'spokeo.txt', 'webtoon.txt',
  'tradesy.txt', 'esalon.txt', 'truedata.txt') %>%
  map(read_lines)


# Example of taking one job description and getting one-word-per-row
sample_lines[[1]] %>%
  tibble(txt = .) %>%
  unnest_tokens(word, txt)


# Mapping functions to my list of character vectors
# I don't remove any "stop words", so articles like "an" and "the" appear
df_no_stops <- sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(unnest_tokens, word, txt) %>%
  bind_rows(.id = 'id') %>%
  count(id, word, sort = TRUE)

df_no_stops



# By default, single letters are included in the stop words
# Consequently, any mentions of the R programming language are removed
# Ensuring 'r' and other possibly useful words are counted
stop_words <- stop_words %>%
  anti_join(tibble(word = c('r', 'problem', 'problems')))




# Want a data frame that counts words per document, word, id, n
# Also accounts for stop words
df <- sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(unnest_tokens, word, txt) %>%
  bind_rows(.id = 'id') %>%
  anti_join(stop_words) %>%
  count(id, word, sort = TRUE)

df




# Term frequency (tf) represents how often a word is in a document
# Inverse document frequency (idf) represents weighted score when a word
# isn't found much in other documents
# Multiplied together for tf-idf, word frequency adjusted for how rare it is
book_words %>%
  bind_tf_idf(word, book, n)


df %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


df_no_stops %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))



# Important to note that focusing only on tf_idf is going to bias towards
# Words that are rare per document, when I might be more interested in
# pure term frequency-- 'data' has a tf_idf of 0,  because it
# appears in each document



df %>%
  bind_tf_idf(word, id, n) %>%
  filter(id == '1' | id == '2') %>% 
  arrange(desc(tf))


df %>%
  bind_tf_idf(word, id, n) %>%
  filter(id == '1' | id == '2') %>% 
  arrange(desc(n))


wordcloud(df$word, df$n, colors = brewer.pal(6, 'Dark2'))


four_gram  <- sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(~ unnest_tokens(., ngram, txt, 'ngrams', n = 4)) %>%
  bind_rows(.id = 'id') %>%
  count(id, ngram, sort = TRUE)

four_gram_tf <- four_gram %>%
  bind_tf_idf(ngram, id, n) %>%
  arrange(desc(tf_idf))


four_gram_tf$tf %>% range()



four_gram_tf %>%
  filter(id == '1' | id == '2') %>%
ggplot(aes(x = tf, y = idf)) + 
  geom_point(aes(color = id)) + 
  geom_text(aes(label = ngram))





# 4 word n grams
sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(~ unnest_tokens(., ngram, txt, 'ngrams', n = 4)) %>%
  bind_rows(.id = 'id') %>%
  count(id, ngram, sort = TRUE) %>%
  bind_tf_idf(ngram, id, n) %>%
  arrange(desc(tf_idf))



sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(~ unnest_tokens(., ngram, txt, 'ngrams', n = 2)) %>%
  bind_rows(.id = 'id') %>%
  count(id, ngram, sort = TRUE) %>%
  bind_tf_idf(ngram, id, n) %>%
  arrange(desc(tf_idf))


