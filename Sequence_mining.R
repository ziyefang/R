install.packages("tidyverse")
install.packages("arulesSequences")

library(dplyr)
library(tidyverse)
library(arulesSequences)

setwd("C:/Users/rache/Dropbox/Rachel/University/Master year 1/02 Fundamentals of data science in medicine/Week 3/Sequence Mining")

## Part ONE ##

d <- read_baskets("ICD9tf.txt", info = c("sequenceID","eventID","SIZE"))
head(as(d, "data.frame"))

s1 <- cspade(d, parameter = list(support = 0.3), control = list(verbose = TRUE))
s1.df <- as(s1, "data.frame")
s1.df
# No sequences > 1

s2 <- cspade(d, parameter = list(support = 0.2), control = list(verbose = TRUE))
s2.df <- as(s2, "data.frame")
s2.df
# No sequences >1

s3 <- cspade(d, parameter = list(support = 0.1), control = list(verbose = TRUE))
s3.df <- as(s3, "data.frame")
s3.df
# 2 sequences of length 2

s4 <- cspade(d, parameter = list(support = 0.08), control = list(verbose = TRUE))
s4.df <- as(s4, "data.frame")
s4.df
# 3 sequences of length 2

s5 <- cspade(d, parameter = list(support = 0.06), control = list(verbose = TRUE))
s5.df <- as(s5, "data.frame")
s5.df
# 5 sequences of length 2

s6 <- cspade(d, parameter = list(support = 0.04), control = list(verbose = TRUE))
s6.df <- as(s6, "data.frame")
s6.df

#Apply ruleInduction
r1 <- as(ruleInduction(s6, confidence = 0.5, control = list(verbose = TRUE)), "data.frame")
r1

# Search icd9 to match the codes to descriptions
filter(icd9, V1 == 25000)

#Export it into excel
library(xlsx)
write.xlsx(s6, "D:/下载/mam2/1.xlsx")
write.xlsx(r1, "D:/下载/mam2/2.xlsx")

##############
## Part TWO ##

# Create table of codes
codex = read.csv("")
codes <- read.csv("codes1.csv")
head(as(codes, "data.frame"))

# create baskets from file
c <- read_baskets("diabetes_sequences_short", info = c("sequenceID","eventID"))
head(as(c, "data.frame"))

# Apply SPADE
g1 <- cspade(c, parameter = list(support = 0.6), control = list(verbose = TRUE))
g1.df <- as(g1, "data.frame")
g1.df

a=sapply(g1.df$sequence, function(x) {
  codes = str_match_all(x, "\\{(.*?)\\}")[[1]][,2]
  #translate codes
  paste(translated_codes, collapse = "; ")
})

#Apply ruleInduction
r2 <- as(ruleInduction(g1, confidence = 0.5, control = list(verbose = TRUE)), "data.frame")
r2

#Export it into excel
write.xlsx(g1, "D:/下载/3.xlsx")
write.xlsx(r2, "D:/下载/4.xlsx")
