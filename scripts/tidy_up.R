library(tidyverse)

WHO <- read_csv("./WHO.csv")
WHO <- WHO %>% mutate_at(3:13, as.numeric)
summary(WHO)


NAs <- WHO %>% summarise_all(funs(sum(is.na(.))))


