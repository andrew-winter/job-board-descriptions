# Simple methods to read and tokenize a list of job descriptions

sample_lines <- c('webtoon.txt', 'esalon.txt', 'tradesy.txt') %>%
  map(read_lines)


# Example of taking one job description and getting one-word-per-row
sample_lines[[1]] %>%
  tibble(txt = .) %>%
  unnest_tokens(word, txt)


# By default, single letters are included in the stop words
# Consequently, any mentions of the R programming language are removed
# Ensuring 'r' and other possibly useful words are counted
stop_words <- stop_words %>%
  anti_join(tibble(word = c('r', 'problem', 'problems')))






# Joining on stop words shows that I need to manually change some stop_words
# Counting words per document (?), so a df with id, word, n
df_stopped <- sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(unnest_tokens, word, txt) %>%
  bind_rows(.id = 'id') %>%
  anti_join(stop_words) %>%
  count(id, word, sort = TRUE)


df <- sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(unnest_tokens, word, txt) %>%
  bind_rows(.id = 'id') %>%
  count(id, word, sort = TRUE)



df %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))



# Term frequency (tf) represents how often a word is in a document
# Inverse document frequency (idf) represents weighted score when a word
# isn't found much in other documents
# Multiplied together for tf-idf, word frequency adjusted for how rare it is
book_words %>%
  bind_tf_idf(word, book, n)

df %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

df_stopped %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))











sample_lines %>%
  map(~ tibble(txt = .)) %>%
  map(~ unnest_tokens(., ngram, txt, 'ngrams', n = 2)) %>%
  bind_rows(.id = 'id') %>%
  count(id, ngram, sort = TRUE)


