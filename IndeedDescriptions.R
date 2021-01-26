library(tidyverse)
library(rvest)
library(tidytext)

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

organize_text <- function(html_class,
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


# let me try testing the functions I wrote before
indeed_base <- 'https://www.indeed.com/jobs?q='
indeed_supp <- 'R%2C+Data+Analyst&l=Los+Angeles%2C+CA&radius=15' # R, 15 mi
test_link <- paste0(indeed_base, indeed_supp)


read_html(test_link) %>%
  html_nodes(".jobtitle") %>%
  html_text()


read_html(test_link) %>%
  html_nodes(".jobtitle")

read_html(test_link) %>%
  html_nodes(".jobtitle") %>%
  html_attr("href") %>%
  paste0("https://indeed.com", .) %>%
  map(read_html) %>%
  map(organize_text)


search_df$role_url[10] %>%
  read_html() %>%
  html_nodes(".jobsearch-DesktopStickyContainer-companyrating div") %>%
  html_text()


