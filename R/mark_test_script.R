library(tidyverse)
library(stringr)

Trap <- rep(c('JCEM','JCAPE'), 4)
Year <- rep(c('2000', '2001'), 4)
`Applied Marks` <- c('ROP, LOP', NA, 'LOP', 'AD', 'RON', 'EF', NA, NA)
`Applied Tags` <- c(NA, NA, '3D9.1BF', NA, NA, '3BB', NA, NA)

df <- tibble(Trap, Year, `Applied Marks`, `Applied Tags`)


Trap <- c("JCEM", 'JCAPE')
Year <- c('2000', '2001', '2000', '2001')
mark_list <- c('ROP|LOP|RON|LON', 'ROP|LOP', 'LON', 'LON|RON')
tag_list <- c('3D9|3BB')

protocol <- tibble(Trap = rep(Trap, each = 2),
               Year = Year,
               mark_type = mark_list,
               tag_type = rep(tag_list,4))

tmp_df <- left_join(df, protocol)

tmp_df %>%
  mutate(Marked = ifelse(str_detect(`Applied Marks`, mark_type), TRUE,
                         ifelse(str_detect(`Applied Tags`, tag_type), TRUE, FALSE)),
         Marked = ifelse(is.na(Marked), FALSE, Marked))


# test stuff
x <- c('apple', 'banana', 'pear')
y <- c('red', 'yellow', 'green')

df %>%
  mutate(tmp = str_extract(x, "an"),
         tmp2 = grepl("an", x),
         tmp3 = str_detect(x, "an|ar"),
         tmp4 = str_replace_all(x, "an", "XX"),
         tmp5 = gsub("an", "XX", x),
         tmp6 = str_replace_all(y, " ", ""))



str_extract(x, "an")
str_extract(x, "pp")

grepl("an", x)
