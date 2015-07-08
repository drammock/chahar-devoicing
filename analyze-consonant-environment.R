#!/usr/bin/env Rscript

## Script to analyze Yurong's corpus of initial vowel devoicing in Chahar
## (wordlist recordings). This script takes the output of clean-raw-data.R
## (after hand-correction of various errors), and looks at vowel devoicing based
## on consonantal context.

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
    astp.bef <- bef.act %in% asp.stp
    pstp.bef <- bef.act %in% pln.stp
    aafr.bef <- bef.act %in% asp.afr
    pafr.bef <- bef.act %in% pln.afr
    stp.bef <- bef.act %in% stp
    afr.bef <- bef.act %in% afr
    fri.bef <- bef.act %in% fri
    asp.bef <- bef.act %in% asp
    obs.bef <- bef.act %in% obs
    nas.bef <- bef.act %in% nas
    liq.bef <- bef.act %in% liq
    gli.bef <- bef.act %in% gli
    voi.bef <- bef.act %in% voi
    astp.aft <- aft.act %in% asp.stp
    pstp.aft <- aft.act %in% pln.stp
    aafr.aft <- aft.act %in% asp.afr
    pafr.aft <- aft.act %in% pln.afr
    stp.aft <- aft.act %in% stp
    afr.aft <- aft.act %in% afr
    fri.aft <- aft.act %in% fri
    asp.aft <- aft.act %in% asp
    obs.aft <- aft.act %in% obs
    nas.aft <- aft.act %in% nas
    liq.aft <- aft.act %in% liq
    gli.aft <- aft.act %in% gli
    voi.aft <- aft.act %in% voi
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
})

manners <- c("gli", "liq", "nas", "fri", "pafr", "aafr", "pstp", "astp")  # glide first = baseline
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

## make some factors
cleandata$reduction <- factor(cleandata$reduction,
                              levels=c("none", "devoiced", "deleted"),
                              ordered=TRUE)
vowels_decr_f2 <- c("i", "ɪ", "œ", "æ", "ɐ", "ǝ", "ɔ", "o", "ʊ", "u")
vowels_by_phary <- c("œ", "ɪ", "æ", "ɐ", "ɔ", "ʊ",  # pharyngeal
                     "i", "ǝ", "o", "u")            # non-pharyngeal

cleandata$vfact <- factor(cleandata$vowel, levels=vowels_by_phary)  # œ baseline
cleandata$man.bef <- factor(cleandata$man.bef, levels=manners)    # gli baseline
cleandata$man.aft <- factor(cleandata$man.aft, levels=manners)    # gli baseline
cleandata$man.ambi <- factor(cleandata$man.ambi, levels=manners)  # gli baseline
cleandata$man.coda <- factor(cleandata$man.coda, levels=manners)  # gli baseline

## ## ## ## ## ## ##
##  CORPUS STATS  ##
## ## ## ## ## ## ##
types.word <- length(unique(cleandata$word))                                # 1204
types.syll <- length(unique(cleandata$syl.act))                             # 1389
types.emic <- length(unique(gsub("+", "", cleandata$syl.exp, fixed=TRUE)))  #  803
types.etic <- length(unique(gsub("+", "", cleandata$syl.act, fixed=TRUE)))  #  912
tokens <- nrow(cleandata)                                                 #  21556
#
## ## ## ## ## ## ##
##  MOSAIC PLOTS  ##
## ## ## ## ## ## ##
cairo_pdf("mosaics_consonants.pdf", height=8.27, width=11.69, family="Charis SIL")
par(mfrow=c(2, 4))
threeshade <- hcl(0, 0, c(70, 50, 30))
bordercol <- "#FFFFFF33"  # NA  # hcl(0, 0, 80)
offset <- 0
# nothing
mosaicplot(with(cleandata, table(reduction)), off=offset,
           dir="h", color=threeshade, las=1, border=bordercol,
           main="reduction type")
## talker
mosaicplot(with(cleandata, table(speaker, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × talker", xlab="talker")
## pharyngeal
mosaicplot(with(cleandata, table(phary, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × harmony class", xlab="pharyngeal")
## vowel
mosaicplot(with(cleandata, table(vfact, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × vowel quality", xlab="vowel quality")
## repetition
mosaicplot(with(cleandata, table(as.factor(rep), reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × repetition", xlab="repetition")
## manner: before
mosaicplot(with(cleandata, table(man.bef, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × preceding consonant", xlab="manner")
## manner: intervocalic
mosaicplot(with(cleandata, table(man.ambi, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × intervocalic consonant", xlab="manner")
## manner: coda
mosaicplot(with(cleandata, table(man.coda, reduction)), off=offset,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × coda consonant", xlab="manner")
dev.off()

## ## ## ## ## ##
## SIMULATIONS ##
## ## ## ## ## ##
stop()
library(ordinal)
outfile <- "simulation-results.txt"
sink(outfile)
cat("## SIMULATION RESULTS\n\n")
sink()
n <- nrow(cleandata)
prop <- c(0.16, 0.08, 0.04, 0.02, 0.01)
reduced <- rbinom(n, 1, 0.16)
deleted <- rbinom(sum(reduced), 1, 0.5) + 1
reduced[reduced == 1] <- deleted
ixs <- which(reduced > 0)
for (p in prop) {
    frac <- round(0.16 / p)
    reduc <- reduced
    reduc[ixs[seq_along(ixs) %% frac != 0]] <- 0
    reduc <- factor(reduc, labels=c("none", "devoiced", "deleted"))
    mod <- with(cleandata, clmm(reduc ~ asp.bef + obs.bef + asp.aft +
                                    obs.aft * coda + as.factor(rep) +
                                    (1|speaker) + (1|word) + (1|vowel)))
    save(mod, file=paste0("mod_", p, ".Rdata", collapse=""))
    sink(outfile, append=TRUE)
    cat("## proportion of events: ", p, "\n\n")
    print(summary(mod))
    cat("\n\n")
    sink()
}

## ## ## ## ## ## ## ##
## SIMULATION TESTS  ##
## ## ## ## ## ## ## ##
stop()
stdevs <- data.frame()
ests <- data.frame()
for (p in prop) {
    load(paste0("mod_", p, ".Rdata", collapse=""))
    nam <- paste0("mod_", p, collapse="")
    ## coefficient estimates
    start.ix <- which(names(mod$coefficients) %in% "asp.befTRUE")
    end.ix <- which(names(mod$coefficients) %in% "obs.aftTRUE:codaTRUE")
    foo <- mod$coefficients[start.ix:end.ix]
    baz <- as.data.frame(t(foo))
    colnames(baz) <- names(foo)
    rownames(baz) <- nam
    ests <- rbind(ests, baz)
    ## standard deviations of estimates
    fixef.stdev <- sqrt(diag(vcov(mod)))
    start.ix <- which(names(fixef.stdev) %in% "asp.befTRUE")
    end.ix <- which(names(fixef.stdev) %in% "obs.aftTRUE:codaTRUE")
    fixef.stdev <- fixef.stdev[start.ix:end.ix]
    bar <- as.data.frame(t(fixef.stdev))
    colnames(bar) <- names(fixef.stdev)
    rownames(bar) <- nam
    stdevs <- rbind(stdevs, bar)
}
## plot
cairo_pdf("simulation.pdf", width=40, height=15)
x <- seq(-10, 10, 0.02)
par(mfrow=dim(ests), mar=c(2,3,3,2), oma=c(2, 4, 4, 2))
for (row in 1:nrow(ests)) {
    for (col in 1:ncol(ests)) {
        curve(dnorm(x, mean=ests[row, col], sd=stdevs[row, col]), xlab="", ylab="")
        if (col == 1) mtext(rownames(ests)[row], side=2, line=4)
        if (row == 1) mtext(colnames(ests)[col], side=3, line=4)
    }
}
dev.off()


## ## ## ## ## ## ## ## ##
## ORDINAL MIXED MODEL  ##
## ## ## ## ## ## ## ## ##
stop()
library(ordinal)
## model 1: factor levels for all manners of articulation
## (model has numerically singular hessian)
all_manners_model <- clmm(reduction ~ man.bef + man.aft * coda +
                              as.factor(rep) + (1|speaker) + (1|word) +
                              (1|vowel), data=cleandata)
sink("model1-summary.txt")
summary(all_manners_model)
sink()
## model 2: aspiration, obstruent, cluster
mod2 <- clmm(reduction ~ asp.bef + obs.bef + asp.aft + obs.aft * coda +
                 as.factor(rep) + (1|speaker) + (1|word) + (1|vowel),
             data=cleandata)
sink("model2-summary.txt")
print(summary(mod2))
sink()
save(mod2, file="model2.Rdata")

## ## ## ## ## ## ## ## ## ## ##
## EXACT LOGISTIC REGRESSION  ##
## ## ## ## ## ## ## ## ## ## ##
# stop()
# library(elrm)
# xdata <- xtabs(~ red.bin + interaction(man.bef, man.aft, coda, vfact,
#                                        as.factor(rep), speaker), data=cleandata)
# elrm(formula=red.bin ~ man.bef + man.ambi + man.coda + vfact + as.factor(rep) +
#          speaker + word,
#      interest= ~ man.bef + man.ambi + man.coda + vfact,
#      iter=22000, burnIn=2000)


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
