library(janeaustenr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(gutenbergr)
data(stop_words)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,
                                     regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 575) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)

lincoln_texts <- gutenberg_works(author =="Lincoln, Abraham") |>
  gutenberg_download(meta_fields = "title")
lincoln_texts

tidy_lincoln <- lincoln_texts %>%
  unnest_tokens(word, text)
tidy_lincoln

