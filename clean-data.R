#!/usr/bin/env Rscript

## Script to analyze Yurong's corpus of initial vowel devoicing in Chahar
## (wordlist recordings). This script takes the output of clean-raw-data.R
## (after hand-correction of various errors), and creates an analyzable
## data frame written out as tab-separated values.
library(stringi)

cleandata <- read.delim("clean_cons_env_zh_corrected.tsv",
                        stringsAsFactors=FALSE, na.strings=c("NA", "无"))

## mappings to go back and forth between English and Chinese column headings
en.zh.transl <- c(speaker="人", rep="重", vowel="元音", reduction="无声化",
                  red.bin="无声化.二进制", syl.act="音节.说的",
                  syl.exp="音节.预期", word="词", notes="笔记",
                  bef.exp="前辅音.预期", bef.act="前辅音.说的",
                  aft.exp="後辅音.预期", aft.act="後辅音.说的",
                  exclude="排除")
zh.en.transl <- c(人="speaker", 重="rep", 元音="vowel", 无声化="reduction",
                  无声化.二进制="red.bin", 音节.说的="syl.act",
                  音节.预期="syl.exp", 词="word", 笔记="notes",
                  前辅音.预期="bef.exp", 前辅音.说的="bef.act",
                  後辅音.预期="aft.exp", 後辅音.说的="aft.act",
                  怎么办="resolution", 排除="exclude")

## switch to English
colnames(cleandata) <- zh.en.transl[colnames(cleandata)]

## recode reduction factor
zh.en.recode <- "'半'='devoiced'; '全'='deleted'; ''='none'"
en.zh.recode <- "'devoiced'='半'; 'deleted'='全'; 'none'=''"
cleandata$reduction <- with(cleandata, car::recode(reduction, zh.en.recode,
                                                   as.factor.result=FALSE))

## exclusions related to hand-correction notes columns
cleandata <- cleandata[is.na(cleandata$exclude),]
## exclusions due to talker skipping / mispronouncing a word
cleandata <- cleandata[!is.na(cleandata$syl.act),]
## clean up unwanted columns
unwanted.cols <- c("notes", "resolution", "exclude")
keep.cols <- colnames(cleandata)[!colnames(cleandata) %in% unwanted.cols]
cleandata <- cleandata[keep.cols]

##

## natural classes
asp.stp <- c("pʰ", "tʰ", "+tʰ")
pln.stp <- c("p", "t", "k", "kʲ", "+p", "+t", "+k")
stp <- c(asp.stp, pln.stp)
asp.afr <- c("tʃʰ", "+tʃʰ")
pln.afr <- c("tʃ", "+tʃ")
afr <- c(asp.afr, pln.afr)
fri <- c("x", "s", "ʃ", "xʲ", "+x", "+xʲ", "+s", "+ʃ")
obs <- c(stp, afr, fri)
nas <- c("m", "+m", "+mʲ", "n", "+n", "+nʲ", "ŋ")
liq <- c("l", "+l", "+lʲ", "r", "+r", "+rʲ", "rʲ")
gli <- c("j", "+j", "w", "+w", "+wʲ")
voi <- c(nas, liq, gli)
asp <- c(asp.stp, asp.afr)
phary <- c("ɪ", "œ", "ɐ", "ɔ", "ʊ", "æ")
round <- c("œ", "ɔ", "o", "ʊ", "u")

cleandata <- within(cleandata, {
    coda <- !substr(aft.exp, 1, 1) %in% "+"
    #     astp.bef <- bef.act %in% asp.stp
    #     pstp.bef <- bef.act %in% pln.stp
    #     aafr.bef <- bef.act %in% asp.afr
    #     pafr.bef <- bef.act %in% pln.afr
    #     stp.bef <- bef.act %in% stp
    #     afr.bef <- bef.act %in% afr
    #     fri.bef <- bef.act %in% fri
    #     asp.bef <- bef.act %in% asp
    #     obs.bef <- bef.act %in% obs
    #     nas.bef <- bef.act %in% nas
    #     liq.bef <- bef.act %in% liq
    #     gli.bef <- bef.act %in% gli
    #     voi.bef <- bef.act %in% voi
    #     astp.aft <- aft.act %in% asp.stp
    #     pstp.aft <- aft.act %in% pln.stp
    #     aafr.aft <- aft.act %in% asp.afr
    #     pafr.aft <- aft.act %in% pln.afr
    #     stp.aft <- aft.act %in% stp
    #     afr.aft <- aft.act %in% afr
    #     fri.aft <- aft.act %in% fri
    #     asp.aft <- aft.act %in% asp
    #     obs.aft <- aft.act %in% obs
    #     nas.aft <- aft.act %in% nas
    #     liq.aft <- aft.act %in% liq
    #     gli.aft <- aft.act %in% gli
    #     voi.aft <- aft.act %in% voi
    astp.bef <- bef.exp %in% asp.stp
    pstp.bef <- bef.exp %in% pln.stp
    aafr.bef <- bef.exp %in% asp.afr
    pafr.bef <- bef.exp %in% pln.afr
    stp.bef <- bef.exp %in% stp
    afr.bef <- bef.exp %in% afr
    fri.bef <- bef.exp %in% fri
    asp.bef <- bef.exp %in% asp
    obs.bef <- bef.exp %in% obs
    nas.bef <- bef.exp %in% nas
    liq.bef <- bef.exp %in% liq
    gli.bef <- bef.exp %in% gli
    voi.bef <- bef.exp %in% voi
    astp.aft <- aft.exp %in% asp.stp
    pstp.aft <- aft.exp %in% pln.stp
    aafr.aft <- aft.exp %in% asp.afr
    pafr.aft <- aft.exp %in% pln.afr
    stp.aft <- aft.exp %in% stp
    afr.aft <- aft.exp %in% afr
    fri.aft <- aft.exp %in% fri
    asp.aft <- aft.exp %in% asp
    obs.aft <- aft.exp %in% obs
    nas.aft <- aft.exp %in% nas
    liq.aft <- aft.exp %in% liq
    gli.aft <- aft.exp %in% gli
    voi.aft <- aft.exp %in% voi
    astp.coda <- astp.aft & coda
    pstp.coda <- pstp.aft & coda
    aafr.coda <- aafr.aft & coda
    pafr.coda <- pafr.aft & coda
    stp.coda <- stp.aft & coda
    afr.coda <- afr.aft & coda
    fri.coda <- fri.aft & coda
    asp.coda <- asp.aft & coda
    obs.coda <- obs.aft & coda
    nas.coda <- nas.aft & coda
    liq.coda <- liq.aft & coda
    gli.coda <- gli.aft & coda
    voi.coda <- voi.aft & coda
    astp.ambi <- astp.aft & !coda
    pstp.ambi <- pstp.aft & !coda
    aafr.ambi <- aafr.aft & !coda
    pafr.ambi <- pafr.aft & !coda
    stp.ambi <- stp.aft & !coda
    afr.ambi <- afr.aft & !coda
    fri.ambi <- fri.aft & !coda
    asp.ambi <- asp.aft & !coda
    obs.ambi <- obs.aft & !coda
    nas.ambi <- nas.aft & !coda
    liq.ambi <- liq.aft & !coda
    gli.ambi <- gli.aft & !coda
    voi.ambi <- voi.aft & !coda
    phary <- vowel %in% phary
    round <- vowel %in% round
    frication.bef <- fri.bef | afr.bef
})

manners <- c("gli", "liq", "nas", "fri", "afr", "stp")  # glide first = baseline
man.bef <- paste0(manners, ".bef")
man.aft <- paste0(manners, ".aft")
man.ambi <- paste0(manners, ".ambi")
man.coda <- paste0(manners, ".coda")
cleandata$man.bef <- apply(cleandata[man.bef], 1, function(i) manners[i])
cleandata$man.aft <- apply(cleandata[man.aft], 1, function(i) manners[i])
cleandata$man.ambi <- apply(cleandata[man.ambi], 1,
                            function(i) if (all(!i)) NA else manners[i])
cleandata$man.coda <- apply(cleandata[man.coda], 1,
                            function(i) if (all(!i)) NA else manners[i])

## vowels
vowels_decr_f2 <- c("i", "ɪ", "œ", "æ", "ɐ", "ǝ", "ɔ", "o", "ʊ", "u")
vowels_by_phary <- c("œ", "ɪ", "æ", "ɐ", "ɔ", "ʊ",  # pharyngeal
                     "i", "ǝ", "o", "u")            # non-pharyngeal

## ## ## ## ## ## ##
##  CORPUS STATS  ##
## ## ## ## ## ## ##
types.word <- length(unique(cleandata$word))                                # 1204
types.syll <- length(unique(cleandata$syl.exp))               # 1389 act, 1198 exp
types.emic <- length(unique(gsub("+", "", cleandata$syl.exp, fixed=TRUE)))  #  803
types.etic <- length(unique(gsub("+", "", cleandata$syl.act, fixed=TRUE)))  #  912
tokens <- nrow(cleandata)                                                 #  21556

## ## ## ## ## ## ## ## ## ##
##  NUMBER OF LONG VOWELS  ##
## ## ## ## ## ## ## ## ## ##
vowel.indices <- stri_locate_all_regex(cleandata$word,
                                       paste(c("[", vowels_by_phary, "ː]+"),
                                             collapse=""))
## keep start index for monophthongs; keep start-end for long vowels & diphthongs
vowel.indices <- lapply(vowel.indices, function(i) {
    vi <- apply(i, 1, function(j) ifelse(j[1] == j[2],
                                         as.character(j[1]),
                                         paste(j, collapse="-")))
})
##
cleandata$num.vowels <- sapply(vowel.indices, length)
cleandata$num.long.or.diph <- sapply(vowel.indices, function(i) sum(nchar(i) > 2))
cleandata$second.vowel.long.or.diph <- sapply(vowel.indices,
                                              function(i) nchar(i[2]) > 2)

write.table(cleandata, file="cleandata.tsv", row.names=FALSE, col.names=TRUE,
            sep="\t")
