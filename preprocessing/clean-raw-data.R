#! /usr/bin/env Rscript

## this script cleans the raw data from Yurong's corpus of initial vowel
## devoicing in Chahar (wordlist in sentence frame recordings). It is somewhat
## interactive, and the comments inline indicate intermediate results that
## guided the structure of subsequent lines.

data.dir <- file.path("..", "raw-data")
rawdata <- read.csv(file.path(data.dir, "initVowelDeletion.csv"),
                    stringsAsFactors=FALSE, na.strings=c("NA", "无"))

## ## ## ## ## ## ## ## ## ##
## convert to long format  ##
## ## ## ## ## ## ## ## ## ##
groups <- list(3:4, 5:6, 7:8, 9:10, 11:12, 13:14, 15:16, 17:18, 19:20, 21:22,
               23:24, 25:26, 27:28, 29:30, 31:32, 33:34, 35:36, 37:38)
cleandata <- lapply(groups, function(i) cbind(rawdata[1:2], rawdata[i]))
cleandata <- lapply(cleandata, function(i) {
    i$speaker <- substr(colnames(i)[3], 1, 2)
    i$repetition <- substr(colnames(i)[3], 4, 4)
    colnames(i) <- c("word", "first.syll.expected", "first.syll.actual",
                     "reduction", "speaker", "repetition")
    return(i)
    })
cleandata <- do.call(rbind, cleandata)

## ## ## ## ## ## ## ## ## ##
## recode reduction column ##
## ## ## ## ## ## ## ## ## ##
recodeString <- "'半'='devoiced'; '全'='deleted'; ''='none'"
cleandata$reduction <- with(cleandata, car::recode(reduction, recodeString,
                                                   as.factor.result=TRUE))
cleandata$reduction.binary <- cleandata$reduction %in% c('devoiced', 'deleted')

## ## ## ## ## ## ## ## ##
## find all the vowels  ##
## ## ## ## ## ## ## ## ##
unique(substr(cleandata$first.syll.expected, 1, 1))
## "n" "p" "x" "k" "m" "l" "s" "ʃ" "t" "j" "̄"
unique(substr(cleandata$first.syll.expected, 2, 2))
## "a" "æ" "ǝ" "i" "ɿ" "ɪ" "ɔ" "œ" "ʊ" "o" "u" "ʰ" "ɑ" "̄"   "ʃ" "j"
unique(substr(cleandata$first.syll.expected, 3, 3))
## "n" "w" "+" "x" "k" "m" "l" "s" "t" "j" "r" "ŋ" "p" "ʃ"
## "a" "ǝ" "i" "ɔ" "o" "u" "＋" "̩"   "æ"  "̱"  "œ" "ʊ"  "ʰ"  "ɑ"  ":"
unique(substr(cleandata$first.syll.expected, 4, 4))
## ""  "x" "k" "m" "l" "s" "ʰ" "t" "ʃ" "j" "r" "ʲ" " " "p" "n" "w" "+" "ŋ" "d"
## "ǝ" "ʊ" "a" "æ" "i" "ɪ" "ɔ" "o" "u"
## no vowels for remaining positions:
## position 5: ""  " " "+" "ʃ" "ʰ" "ʲ" "j" "k" "l" "m" "n" "ŋ" "r" "s" "t" "w" "x" "ʒ"
## position 6: ""  " " "ʰ" "ʃ" "ʲ" "k" "l" "m" "n" "r" "s" "t" "w" "x"
## position 7: ""  "ʰ" "ʃ" "t"
## position 8: ""  "ʰ" "ʃ"
## position 9: ""  "ʰ"

## just the vowels
pos2 <- c("a", "æ", "ǝ", "i", "ɿ", "ɪ", "ɔ", "œ", "ʊ", "o", "u", "ɑ")
pos3 <- c("a", "ǝ", "i", "ɔ", "o", "u", "æ", "œ", "ʊ", "ɑ")
pos4 <- c("ǝ", "ʊ", "a", "æ", "i", "ɪ", "ɔ", "o", "u")
vowels <- sort(unique(c(pos2, pos3, pos4)))

cleandata$vowel <- with(cleandata, ifelse(substr(first.syll.expected, 2, 2) %in% vowels,
                                          substr(first.syll.expected, 2, 2),
                                          ifelse(substr(first.syll.expected, 3, 3) %in% vowels,
                                                 substr(first.syll.expected, 3, 3),
                                                 substr(first.syll.expected, 4, 4))))
## make sure we didn't mess up:
stopifnot(cleandata$vowel %in% vowels)

## ## ## ## ## ## ## ## ## ## ##
##  reorder columns and rows  ##
## ## ## ## ## ## ## ## ## ## ##
cleandata <- cleandata[c("speaker", "repetition", "vowel", "reduction",
                         "reduction.binary", "first.syll.actual",
                         "first.syll.expected", "word")]
cleandata <- cleandata[with(cleandata, order(speaker, repetition)),]

## ## ## ## ## ## ## ## ## ## ##
## add consonant environment  ##
## ## ## ## ## ## ## ## ## ## ##
library(stringi)
vowelRegex <- paste("[", paste(vowels, collapse=""), "]", sep="")
consEnvExp <- stri_split_regex(cleandata$first.syll.expected, vowelRegex)
consEnvAct <- stri_split_regex(cleandata$first.syll.actual, vowelRegex)
cleandata$before.expected <- sapply(consEnvExp, function(i) i[1])
cleandata$after.expected <- sapply(consEnvExp, function(i) i[2])
cleandata$before.actual <- sapply(consEnvAct, function(i) i[1])
cleandata$after.actual <- sapply(consEnvAct, function(i) i[2])
cleandata$notes <- ""
weirdos <- c("̩k", "tʰ̱", "t̄ʰ", "̄j", ":", " ")
explain <- c("syllabic marker on vowel", "macron under aspiration", "macron over t",
             "macron at start of word", "colon after vowel", "space character")
for (ix in seq_along(weirdos)) {
    weird1 <- stri_detect_fixed(cleandata$first.syll.expected, weirdos[ix])
    weird2 <- stri_detect_fixed(cleandata$first.syll.actual, weirdos[ix])
    if (weirdos[ix] %in% ":") {
        ## ignore colon-after-vowel for later syllables
        weird3 <- rep(FALSE, length(weird2))
    } else {
        weird3 <- stri_detect_fixed(cleandata$word, weirdos[ix])
    }
    weird <- weird1 | weird2 | weird3
    weird[is.na(weird)] <- FALSE
    cleandata[weird, "notes"] <- explain[ix]
}
write.table(cleandata, "clean_cons_env.tsv", sep="\t", row.names=FALSE,
            fileEncoding="UTF-8")
## write alternate table with chinese headings
cleandata.zh <- cleandata
zhRecodeString <- "'devoiced'='半'; 'deleted'='全'; 'none'=''"
cleandata.zh$reduction <- with(cleandata.zh, car::recode(reduction, zhRecodeString,
                                                      as.factor.result=TRUE))
translations <- c(speaker="人", repetition="重", vowel="元音", reduction="无声化",
                         reduction.binary="无声化（二进制）", first.syll.actual="音节（说的）",
                         first.syll.expected="音节（预期）", word="词", notes="笔记",
                         before.expected="前辅音（预期）", before.actual="前辅音（说的）",
                         after.expected="後辅音（预期）", after.actual="後辅音（说的）")
colnames(cleandata.zh) <- translations[colnames(cleandata)]
write.table(cleandata.zh, "clean_cons_env_zh.tsv", sep="\t", row.names=FALSE,
            fileEncoding="UTF-8")
