library(tidyverse)
library(rvest)
library(tidytext)


## Demonstration on what rvest can do, basically
sample_link <- paste0('https://www.indeed.com/viewjob?',
                      'jk=8659dad5f489c18e&tk=1ellrbt1uo2ii800&from=serp&vjs=3')
sample_html <- read_html(sample_link)
sample_raw_text <- sample_html %>%
  html_nodes(".jobsearch-jobDescriptionText") %>%
  html_text()


## Function to scrape text using a CSS selector
safe_mine_text <- function(html_class, str_to_css){
  if ("xml_node" %in% class(html_class) |
      "xml_document" %in% class(html_class)) {
    html_class %>%
      html_nodes(str_to_css) %>%
      html_text()  
  } else {
    NA
  }
}


# Demonstration of the function
safe_mine_text(sample_html, ".jobsearch-jobDescriptionText")
safe_mine_text(sample_html, ".jobsearch-JobInfoHeader-title")+


# Getting the company name, the number of reviews, and the location is sloppy
# First get the subtitle (which is the whole thing)
# Then get the icl-u-lg-mr--sm (just name, reviews, hyphen), collapse it
# str_split the subtitle, unlist it, last element 

  
## Demonstration of process to get all the info we want (for now)
role_desc <- safe_mine_text(sample_html, ".jobsearch-jobDescriptionText")
role_desc
role_name <- safe_mine_text(sample_html, ".jobsearch-JobInfoHeader-title")
role_subtitles <- safe_mine_text(sample_html,
                                 ".jobsearch-JobInfoHeader-subtitle")


role_icl <- safe_mine_text(sample_html, ".icl-u-lg-mr--sm")
role_area <- role_subtitles %>% str_split(paste(role_icl, collapse = "")) %>%
  unlist() %>% paste(collapse = "")
role_org <- role_icl[[1]]


## Splitting the text into one-token-per-row format
role_desc_tokens <- role_desc %>%
  str_split("\n") %>%
  unlist() %>%
  tibble(lines = .) %>%
  unnest_tokens(role_words, lines)


## Use a data frame, for smoother analysis
tibble(role_desc_tokens, role_name, role_org, role_area)


## Function to scrape all the info we want, and use df for further analysis
scrape_indeed <- function(html_class,
                          select_desc = ".jobsearch-jobDescriptionText",
                          select_name = ".jobsearch-JobInfoHeader-title",
                          select_subt = ".jobsearch-JobInfoHeader-subtitle",
                          select_icl  = ".icl-u-lg-mr--sm"){
  # use CSS selectors to scrape text from an xml document/nodeset
  # default selectors are from indeed.com
  role_desc <- safe_mine_text(html_class, select_desc)
  role_name <- safe_mine_text(html_class, select_name)
  role_subt <- safe_mine_text(html_class, select_subt)
  role_icl  <- safe_mine_text(html_class, select_icl)
  
  role_desc_tokens <- role_desc %>%
    str_split("\n") %>%
    unlist() %>%
    tibble(lines = .) %>%
    unnest_tokens(role_words, lines)
  
  role_org <- role_icl[[1]]
  
  role_area <- role_subt %>%
    str_split(paste(role_icl, collapse = "")) %>%
    unlist() %>%
    paste(collapse = "")
  
  tibble(role_desc_tokens, role_name, role_org, role_area)
}


## Demonstration of the function
some_html <- paste0("https://www.indeed.com/viewjob?jk=1ace0176ee3fbbb4&q=",
       "sql+R+health&l=90004&tk=1ellrbt1uo2ii800&from=web&vjs=3.html") %>%
  read_html()
scrape_indeed(some_html)


new_link <- 'https://www.indeed.com/viewjob?jk=4b36413bff25cde6&tk=1ellrv8jto275800&from=serp&vjs=3'
new_link %>% read_html() %>%
  scrape_indeed()
