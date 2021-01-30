library(purrr)
library(stringr)



# What if we want to search for a censored word?
# Ex. I went to T*rget to get more hand sanitizer
str_replace(fruit, "[aeiou]", "-")

str_replace(fruit[1], 
            map_chr(seq(nchar(fruit[1])), ~ str_sub(fruit[1], ., .)),
            '-')

censor_chr <- function(strn, repn = '-', ...) {
  # Takes a string and returns one character replaced each
  strn %>%
    str_replace(
      map_chr(
        seq(nchar(strn)), ~ str_sub(strn, ., .)), repn
    )
}

some <- fruit[1:15]

map(some, censor_chr)

fruit[1:10] %>%
  map(censor_chr)




# Maybe not a data frame because different number of rows
# depending on string length

more <- fruit[-(1:40)]

more_censored <- more %>%
  map(censor_chr)


# Next time- read more about why unlist means you could
# be mapping already vectorized functions on each other



# I may have to write an do call to check if it's a grouped data frame
# Not right now though
do_censor <- function(strn, repn = '-', ...) {
  if (dplyr::is_grouped_df(strn)) {
    return(dplyr::do(strn, do_censor(.)))
  }
  strn %>%
    str_replace(
      map_chr(
        seq(nchar(strn)), ~ str_sub(strn, ., .)), repn
      )
}
