#!/usr/bin/env Rscript

## Script to analyze Yurong's corpus of initial vowel devoicing in Chahar
## (wordlist recordings). This script takes the output of clean-raw-data.R
## (after hand-correction of various errors), and looks at vowel devoicing based
## on consonantal context.

cleandata <- read.delim("clean_cons_env_zh_corrected.tsv",
                        stringsAsFactors=FALSE, na.strings=c("NA", "无"))

## fix bad column names (contained parentheses and question marks)
colnames(cleandata) <- c("人", "重", "元音", "无声化", "无声化.二进制",
                         "音节.说的", "音节.预期", "词", "前辅音.预期",
                         "後辅音.预期", "前辅音.说的", "後辅音.说的", "笔记",
                         "怎么办", "排除")

## corrections not made in the cleaned data file
cleandata$元音[cleandata$元音 %in% c("a", "ɑ")] <- "ɐ"
cleandata$元音[cleandata$元音 %in% "ɿ"] <- "ɪ"
dzh.cols <- c("词", "音节.预期", "音节.说的", "前辅音.预期", "前辅音.说的",
              "後辅音.预期", "後辅音.说的")
cleandata[dzh.cols] <- lapply(cleandata[dzh.cols],
                              function(i) gsub("dʒ", "tʃ", i, fixed=TRUE))

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

## convert Chinese plus sign \uFF0B to ASCII \u002B (input is mixture of both)
cleandata$aft.exp <- gsub("＋", "+", cleandata$aft.exp, fixed=TRUE)
cleandata$aft.act   <- gsub("＋", "+", cleandata$aft.act, fixed=TRUE)
## also get rid of extra spaces between plus and following consonant
cleandata$aft.exp <- gsub("+ ", "+", cleandata$aft.exp, fixed=TRUE)
cleandata$aft.act   <- gsub("+ ", "+", cleandata$aft.act, fixed=TRUE)

## exclusions related to hand-correction notes columns
cleandata <- cleandata[cleandata$exclude != 1,]
## exclusions due to talker skipping / mispronouncing a word
cleandata <- cleandata[!is.na(cleandata$syl.act),]
## clean up unwanted columns
unwanted.cols <- c("notes", "resolution", "exclude")
keep.cols <- colnames(cleandata)[!colnames(cleandata) %in% unwanted.cols]
cleandata <- cleandata[keep.cols]

## add natural class columns
stp <- c("p", "pʰ", "t", "tʰ", "k", "kʲ", "+p", "+t", "+tʰ", "+k")
afr <- c("tʃ", "tʃʰ", "+tʃ", "+tʃʰ")
fri <- c("x", "s", "ʃ", "xʲ", "+x", "+xʲ", "+s", "+ʃ")
obs <- c(stp, afr, fri)
nas <- c("m", "+m", "+mʲ", "n", "+n", "+nʲ", "ŋ")
liq <- c("l", "+l", "+lʲ", "r", "+r", "+rʲ", "rʲ")
gli <- c("j", "+j", "w", "+w", "+wʲ")
voi <- c(nas, liq, gli)
asp <- c("pʰ", "tʰ", "tʃʰ", "+tʃʰ")
phary <- c("ɪ", "œ", "ɐ", "ɔ", "ʊ", "æ")
round <- c("œ", "ɔ", "o", "ʊ", "u")

cleandata <- within(cleandata, {
    coda <- !substr(aft.exp, 1, 1) %in% "+"
    stp.bef <- bef.act %in% stp
    afr.bef <- bef.act %in% afr
    fri.bef <- bef.act %in% fri
    asp.bef <- bef.act %in% asp
    obs.bef <- bef.act %in% obs
    nas.bef <- bef.act %in% nas
    liq.bef <- bef.act %in% liq
    gli.bef <- bef.act %in% gli
    voi.bef <- bef.act %in% voi
    stp.coda <- aft.act %in% stp & coda
    afr.coda <- aft.act %in% afr & coda
    fri.coda <- aft.act %in% fri & coda
    asp.coda <- aft.act %in% asp & coda
    obs.coda <- aft.act %in% obs & coda
    nas.coda <- aft.act %in% nas & coda
    liq.coda <- aft.act %in% liq & coda
    gli.coda <- aft.act %in% gli & coda
    voi.coda <- aft.act %in% voi & coda
    stp.aft <- aft.act %in% stp
    afr.aft <- aft.act %in% afr
    fri.aft <- aft.act %in% fri
    asp.aft <- aft.act %in% asp
    obs.aft <- aft.act %in% obs
    nas.aft <- aft.act %in% nas
    liq.aft <- aft.act %in% liq
    gli.aft <- aft.act %in% gli
    voi.aft <- aft.act %in% voi
    stp.ambi <- aft.act %in% stp & !coda
    afr.ambi <- aft.act %in% afr & !coda
    fri.ambi <- aft.act %in% fri & !coda
    asp.ambi <- aft.act %in% asp & !coda
    obs.ambi <- aft.act %in% obs & !coda
    nas.ambi <- aft.act %in% nas & !coda
    liq.ambi <- aft.act %in% liq & !coda
    gli.ambi <- aft.act %in% gli & !coda
    voi.ambi <- aft.act %in% voi & !coda
    phary <- vowel %in% phary
})
manners <- c("stp", "afr", "fri", "nas", "liq", "gli")
man.bef <- paste0(manners, ".bef")
man.aft <- paste0(manners, ".aft")
man.coda <- paste0(manners, ".coda")
cleandata$man.bef <- apply(cleandata[man.bef], 1, function(i) manners[i])
cleandata$man.aft <- apply(cleandata[man.aft], 1, function(i) manners[i])
cleandata$man.coda <- apply(cleandata[man.coda], 1, function(i) manners[i])

## make some factors
cleandata$reduction <- factor(cleandata$reduction,
                              levels=c("none", "devoiced", "deleted"),
                              ordered=TRUE)
vowels_decr_f2 <- c("i", "ɪ", "œ", "æ", "ɐ", "ǝ", "ɔ", "o", "ʊ", "u")
vowels_by_phary <- c("i", "ǝ", "o", "u",            # non-pharyngeal
                     "œ", "ɪ", "æ", "ɐ", "ɔ", "ʊ")  # pharyngeal
cleandata$vfact <- factor(cleandata$vowel, levels=vowels_by_phary)  # vowels_decr_f2
cleandata$mbfact <- factor(cleandata$man.bef, levels=manners)
cleandata$mafact <- factor(cleandata$man.aft, levels=manners)

## ## ## ## ## ## ##
##  CORPUS STATS  ##
## ## ## ## ## ## ##
types.word <- length(unique(cleandata$word))
types.syll <- length(unique(cleandata$syl.act))                             # 1393
types.emic <- length(unique(gsub("+", "", cleandata$syl.exp, fixed=TRUE)))  #  845
types.etic <- length(unique(gsub("+", "", cleandata$syl.act, fixed=TRUE)))  #  961
tokens <- nrow(cleandata)                                                 #  21558

## ## ## ## ## ## ##
##  MOSAIC PLOTS  ##
## ## ## ## ## ## ##
cairo_pdf("mosaics_consonants.pdf", height=8.27, width=11.69, family="Charis SIL")
par(mfrow=c(2, 3))
threeshade <- hcl(0, 0, c(70, 50, 30))
bordercol <- "#FFFFFF33"  # NA  # hcl(0, 0, 80)
offset <- 0
# nothing
mosaicplot(with(cleandata, table(reduction)), off=offset,
           dir="h", color=threeshade, las=1, border=bordercol,
           main="reduction type")
## pharyngeal
mosaicplot(with(cleandata, table(phary, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × harmony class", xlab="pharyngeal")
## vowel
mosaicplot(with(cleandata, table(vfact, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × vowel quality", xlab="vowel quality")
## manner: before
mosaicplot(with(cleandata, table(mbfact, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × preceding consonant", xlab="manner")
## manner: after
mosaicplot(with(cleandata, table(mafact, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × following consonant", xlab="manner")
dev.off()

## ## ## ## ## ## ## ## ##
## ORDINAL MIXED MODEL  ##
## ## ## ## ## ## ## ## ##
stop()
library(ordinal)
## model 1: {stop, affricate, fricative, aspiration} x before/after,
## repetition, morpheme boundary
stp_afr_fri_asp <- clmm(reduction ~ stp.bef + stp.coda + stp.ambi +
                            afr.bef + afr.coda + afr.ambi +
                            fri.bef + fri.coda + fri.ambi +
                            asp.bef + asp.coda + asp.ambi +
                            as.factor(rep) + (1|speaker) + (1|vowel) + (1|word),
                        data=cleandata)
sink("model1-summary.txt")
summary(stp_afr_fri_asp)
sink()
save(stp_afr_fri_asp, file="model1.Rdata")

## ## ## ## ## ##
##  OLD STUFF  ##
## ## ## ## ## ##
stop()
## there is near-complete separation of obs & voi (only one phoneme +dʒ
## is in both categories, and occurs in only 1 word). This leads to
## rank-deficient model matrix and convergence probs.
ord_asp <- clmm(reduction ~ asp.bef.act + asp.aft.act + bef.morph +
                    as.factor(rep) + (1|speaker) + (1|vowel) + (1|word),
                data=cleandata)

## aspiration interaction (asp_bef * asp_aft)
# ord_asp_int <- clmm(reduction ~ asp.bef.act * asp.aft.act +
#                     bef.morph + as.factor(rep) +
#                     (1|speaker) + (1|vowel) + (1|word), data=cleandata)
# ## interaction not needed:
# anova(ord_asp_int, ord_asp)

## include obstruents
ord_asp_obs <- clmm(reduction ~ asp.bef.act + asp.aft.act +
                        obs.bef.act + obs.aft.act +
                        bef.morph + as.factor(rep) +
                        (1|speaker) + (1|vowel) + (1|word), data=cleandata)
## better fit when including obs:
anova(ord_asp_obs, ord_asp)

## obstruent interaction (obs_bef * obs_aft)
# ord_asp_obs_int <- clmm(reduction ~ asp.bef.act + asp.aft.act +
#                             obs.bef.act * obs.aft.act +
#                             bef.morph + as.factor(rep) +
#                             (1|speaker) + (1|vowel) + (1|word), data=cleandata)
# ## interaction not needed:
# anova(ord_asp_obs_int, ord_asp_obs)

## include pharyngeal?
# ord_asp_obs_phr <- clmm(reduction ~ asp.bef.act + asp.aft.act +
#                             obs.bef.act + obs.aft.act + phary +
#                             bef.morph + as.factor(rep) +
#                             (1|speaker) + (1|vowel) + (1|word), data=cleandata)
# ## pharyngeal not needed:
# anova(ord_asp_obs_phr, ord_asp_obs)

## choose tabulation factor
tab.fact <- cleandata$bef.morph

## tabulate
tab <- table(cleandata$reduction, tab.fact)
normtab <- t(apply(tab, 1, function(i) i / colSums(tab)))

## collapse reduction to binary
bintab <- table(cleandata$red.bin, tab.fact)
bintab <- apply(bintab, 2, rev)
bin.pt <- apply(bintab, 2, function(i) prop.test(i[1], sum(i), correct=FALSE))
bin.chsq <- sapply(bin.pt, function(i) as.vector(i$statistic))
bin.degf <- sapply(bin.pt, function(i) as.vector(i$parameter))
bin.pval <- sapply(bin.pt, function(i) as.vector(i$p.value))
bin.prop <- sapply(bin.pt, function(i) as.vector(i$estimate))
bin.lower <- sapply(bin.pt, function(i) as.vector(i$conf.int)[1])
bin.upper <- sapply(bin.pt, function(i) as.vector(i$conf.int)[2])
bin <- as.data.frame(cbind(bin.prop, bin.lower, bin.upper, bin.chsq, bin.degf, bin.pval))
colnames(bin) <- gsub("bin.", "", colnames(bin))
binproptab <- apply(bintab, 2, function(i) i / sum(i))

## plots
cairo_pdf("plots_consonants.pdf", width=11.69, height=8.27)
par(mar=c(3,3,2,2))
layout(matrix(c(1,0,2,3), nrow=2))
barplot(tab, legend.text=TRUE, args.legend=list(x="topright", xpd=TRUE))
barplot(normtab, ylim=c(0, 0.25), xpd=FALSE)
barx <- barplot(binproptab, ylim=c(0, 0.25), xpd=FALSE, col=c('#999999', '#DDDDDD'))
with(bin, plotrix::plotCI(barx, prop, upper, lower, add=TRUE, lwd=1.5, pch=NA))
dev.off()
