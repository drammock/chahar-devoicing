#!/usr/bin/env Rscript

## Script to analyze Yurong's corpus of initial vowel devoicing in Chahar
## (wordlist recordings). This script takes the output of clean-raw-data.R
## (after hand-correction of various errors), and looks at vowel devoicing based
## on consonantal context.
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

stop()
## ## ## ## ## ## ##
##  MOSAIC PLOTS  ##
## ## ## ## ## ## ##
#threeshade <- hcl(0, 0, c(70, 60, 50), fixup=FALSE)
#threeshade <- rev(c("#1F4124", "#0B6E64", "#4298B4"))
oranges <- hcl(60, seq(40, 80, length.out=3), seq(85, 40, length.out=3), fixup=TRUE)
blues <- hcl(260, seq(40, 80, length.out=3), seq(85, 40, length.out=3), fixup=TRUE)
grays <- hcl(260, 0, seq(85, 40, length.out=3), fixup=FALSE)

bordercol <- NA  # "white"  # NA  # "#FFFFFF33"  # hcl(0, 0, 80)
offs <- 0.8

tab <- with(cleandata, table(vfact, reduction))
renamer <- c(none="unreduced", devoiced="partial dev.", deleted="full dev.")
colnames(tab) <- renamer[colnames(tab)]
ord <- c("i", "ɪ", "u", "ʊ", "o", "ɔ", "ǝ", "œ", "ɐ", "æ")
## orange + blue = reduction x vowel quality
## to be converted to one graph in illustrator
cairo_pdf("mosaic-orange.pdf", height=4, width=8, family="Myriad Pro")
par(mar=c(1, 0, 1, 0))
mosaicplot(tab[rev(ord),], off=offs, dir=c("v", "h"),
           color=oranges, las=1, border=bordercol, cex.axis=2,
           main="", xlab="", ylab="")
dev.off()
cairo_pdf("mosaic-blue.pdf", height=4, width=8, family="Myriad Pro")
par(mar=c(1, 0, 1, 0))
mosaicplot(tab[rev(ord),], off=offs, dir=c("v", "h"),
           color=blues, las=1, border=bordercol, cex.axis=2,
           main="", xlab="", ylab="")
dev.off()
## talker
tab <- with(cleandata, table(speaker, reduction))
renamer <- c(none="unreduced", devoiced="partial dev.", deleted="full dev.")
colnames(tab) <- renamer[colnames(tab)]
cairo_pdf("mosaic-talker.pdf", height=4, width=8, family="Myriad Pro")
par(mar=c(1, 0, 1, 0))
mosaicplot(tab, off=offs, dir=c("v", "h"), color=grays, las=1,
           border=bordercol, cex.axis=2, main="", xlab="", ylab="")
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
## plot simulation
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
## null model
nullmod <- clmm(reduction ~ 1 + (1|speaker) + (1|word) + (1|vowel),
                data=cleandata)
save(nullmod, file="nullmodel.Rdata")
## model 1: factor levels for all manners of articulation
## (model has numerically singular hessian)
# mod1 <- clmm(reduction ~ man.bef + man.aft + coda +
#                  (1|speaker) + (1|word) + (1|vowel), data=cleandata)
# save(mod1, file="model1.Rdata")
## model 2: aspiration, obstruency, cluster
# mod2 <- clmm(reduction ~ asp.bef + obs.bef + asp.aft + obs.aft * coda +
#                  as.factor(rep) + (1|speaker) + (1|word) + (1|vowel),
#              data=cleandata)
# save(mod2, file="model2.Rdata")
# mod2b <- clmm(reduction ~ asp.bef + obs.bef + asp.aft * coda + obs.aft * coda +
#                   as.factor(rep) + (1|speaker) + (1|word) + (1|vowel),
#               data=cleandata)
# save(mod2b, file="model2b.Rdata")
## model 3: focus on phonetics
mod3 <- clmm(reduction ~ asp.bef + frication.bef + asp.aft * coda +
                 fri.aft * coda + as.factor(rep) + (1|speaker) + (1|word) +
                 (1|vowel), data=cleandata)
save(mod3, file="model3.Rdata")
## mod 4: keeping fricatives & affricates separate
# mod4 <- clmm(reduction ~ asp.bef + fri.bef + afr.bef + asp.aft * coda +
#                  fri.aft * coda + afr.aft * coda + as.factor(rep) +
#                  (1|speaker) + (1|word) + (1|vowel), data=cleandata)
# save(mod4, file="model4.Rdata")
## mod 5: aspiration only
mod5a <- clmm(reduction ~ asp.bef*asp.aft + asp.aft*coda + as.factor(rep) +
                  (1|speaker) + (1|word) + (1|vowel), data=cleandata)
mod5b <- clmm(reduction ~ asp.bef*asp.aft + asp.aft*coda + as.factor(rep) +
                  frication.bef + fri.aft*coda +
                  (1|speaker) + (1|word) + (1|vowel), data=cleandata)
mod5c <- clmm(reduction ~ asp.bef*asp.aft + asp.aft*coda + as.factor(rep) +
                  fri.bef + fri.aft*coda + afr.bef + afr.aft*coda +
                  (1|speaker) + (1|word) + (1|vowel), data=cleandata)
## test
lrtest <- anova(nullmod, mod5a, mod5b, mod5c)
save(mod5a, file="model5a.Rdata")
save(mod5b, file="model5b.Rdata")
save(mod5c, file="model5c.Rdata")

## ## ## ## ## ## ##
##  plot model 5c ##
## ## ## ## ## ## ##
stop()
load("model5c.Rdata")
mod <- mod5c
library(ordinal)  # otherwise summary, etc won't work
library(coefplot2)
coefplot2(mod)
prettynames <- c(## aspiration / frication
                 asp.aftTRUE="C₂ aspirated",
                 fri.aftTRUE="C₂ fricative",
                 asp.befTRUE="C₁ aspirated",
                 fri.befTRUE="C₁ fricative",
                 #frication.befTRUE="C₁ fricative/affricate",
                 ## affricate asymmetry
                 afr.befTRUE="C₁ affricate",
                 afr.aftTRUE="C₂ affricate",
                 codaTRUE="C₂ in cluster",
                 ## list effects
                 `as.factor(rep)2`="list rep. 2",
                 `as.factor(rep)3`="list rep. 3",
                 ## not signif / negative
                 `codaTRUE:afr.aftTRUE`="C₂ affricate & in cluster",
                 `codaTRUE:fri.aftTRUE`="C₂ fricative & in cluster",
                 `asp.aftTRUE:codaTRUE`="C₂ aspirated & in cluster",
                 `asp.befTRUE:asp.aftTRUE`="C₁ & C₂ both aspirated")
#fixefs <- names(mod$beta)[order(mod$beta)]
fixefs <- rev(names(prettynames))  # manually set best order
colors <- hcl(h=seq(250, -110, length.out=5), c=c(100, 85, 70, 55, 0),
              l=seq(30, 60, length.out=5), fixup=TRUE)
colors <- c("#4477aa", "#cc6677", "#117733", "#999933", "#666666")
cols <- rev(rep(colors, times=c(4, 2, 1, 2, 4)))
#desat <- hcl(h=0, c=0, l=seq(50, 70, length.out=4), fixup=TRUE)
#cols <- rev(c(rep(colors[-4], times=c(4, 3, 2)), rep(desat[4], times=4)))
stdevs <- sqrt(diag(vcov(mod)))
xvals <- mod$coefficients[fixefs]
yvals <- seq_along(xvals)
# pval stars
pvals <- coef(summary(mod))[, 4]
stars <- symnum(pvals, corr=FALSE, na=FALSE,
                cutpoints = c(0, 0.01, 1),
                symbols = c("*", " "), legend=FALSE)
# crosshairs
onesd <- stdevs[fixefs]
minusone <- xvals - onesd
plusone <- xvals + onesd
minustwo <- xvals - 2*onesd
plustwo <- xvals + 2*onesd
xlim <- c(floor(min(minustwo)), ceiling(max(plustwo)))
xtick <- seq(xlim[1], xlim[2], 1)
xlab <- paste0(ifelse(xtick<0, "−", ""), abs(xtick))  # proper unicode minus

cairo_pdf("mod_coefplot.pdf", width=14.5, height=9, family="Myriad Pro")
par(mar=c(1, 25, 6, 1))
plot(xvals, yvals, type="n", axes=FALSE, ann=FALSE, xlim=xlim)
abline(v=0, lty=2, col="#999999")
segments(minustwo, yvals, plustwo, lwd=3, lend="butt", col=cols)
segments(minusone, yvals, plusone, lwd=10, lend="butt", col=cols)
points(xvals, yvals, pch="|", cex=2, col=cols)
axis(3, at=xtick, labels=xlab, cex.axis=2.5)
# left axis
Map(function(y, txt, color)
    axis(2, at=y, col.axis=color, labels=txt, cex.axis=2.5, tick=FALSE, line=NA, las=1),
    yvals, prettynames[fixefs], cols)
# right axis
Map(function(y, txt, color)
    axis(2, at=y, col.axis=color, labels=txt, cex.axis=2.5, tick=FALSE, line=-1.5, las=1),
    yvals, stars[fixefs], cols)
title("Regression parameter estimates", line=4, cex.main=3)
dev.off()

## coefficient estimates
# start.ix <- which(names(mod$coefficients) %in% "asp.befTRUE")
# end.ix <- which(names(mod$coefficients) %in% "codaTRUE:fri.aftTRUE")
# ests <- mod$coefficients[start.ix:end.ix]
# ## standard deviations of estimates
# fixef.stdev <- sqrt(diag(vcov(mod)))
# start.ix <- which(names(fixef.stdev) %in% "asp.befTRUE")
# end.ix <- which(names(fixef.stdev) %in% "codaTRUE:fri.aftTRUE")
# stdevs <- fixef.stdev[start.ix:end.ix]
# ## plot
# cairo_pdf("model3.pdf", width=15, height=15)
# x <- seq(-10, 10, 0.02)
# par(mfrow=c(3, 3), mar=c(2,3,3,2), oma=c(2, 4, 4, 2))
# for (n in names(ests)) {
#     curve(dnorm(x, mean=ests[n], sd=stdevs[n]), main=n)
# }
# dev.off()


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


# old mosaics
cairo_pdf("mosaics_consonants.pdf", height=8.5, width=11, family="Charis SIL")
par(mfrow=c(2, 4))
# nothing
mosaicplot(with(cleandata, table(reduction)), off=offs,
           dir="h", color=threeshade, las=1, border=bordercol,
           main="reduction type")
## talker
mosaicplot(with(cleandata, table(speaker, reduction)), off=offs,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × talker", xlab="talker")
## pharyngeal
mosaicplot(with(cleandata, table(phary, reduction)), off=offs,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × harmony class", xlab="pharyngeal")
## vowel
mosaicplot(with(cleandata, table(vfact, reduction)), off=offs,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × vowel quality", xlab="vowel quality")
## repetition
mosaicplot(with(cleandata, table(as.factor(rep), reduction)), off=offs,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × repetition", xlab="repetition")
## manner: before
mosaicplot(with(cleandata, table(man.bef, reduction)), off=offs,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × preceding consonant", xlab="manner")
## manner: intervocalic
mosaicplot(with(cleandata, table(man.ambi, reduction)), off=offs,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × intervocalic consonant", xlab="manner")
## manner: coda
mosaicplot(with(cleandata, table(man.coda, reduction)), off=offs,
           dir=c("v", "h"), color=threeshade, las=1, border=bordercol,
           main="reduction × coda consonant", xlab="manner")
dev.off()

## manner: bef vs after
with(cleandata, table(man.bef, man.aft))
cairo_pdf("mosaic_cons_bef_aft.pdf", height=4.5, width=4.5, family="Charis SIL")
mosaicplot(with(cleandata, table(man.bef, man.aft)), off=offs,
           dir=c("v", "h"), color=TRUE, las=1, border=bordercol,
           main="onset cons. vs. following cons.", xlab="onset",
           ylab="following")
dev.off()
