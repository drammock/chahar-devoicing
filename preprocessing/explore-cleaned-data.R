#! /usr/bin/env Rscript

## this script does some basic preliminary analysis of Yurong's corpus of
## initial vowel devoicing in Chahar (wordlist in sentence frame recordings).

cleandata <- read.delim("clean_cons_env.tsv", sep="\t")

## ## ## ## ## ## ##
## summary table  ##
## ## ## ## ## ## ##
tab <- with(cleandata, table(reduction, vowel))
#plot(t(tab), color=TRUE, las=1)
normtab <- t(apply(tab, 1, function(i) i / colSums(tab)))

## collapse reduction to binary
bintab <- with(cleandata, table(reduction.binary, vowel))
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
cairo_pdf("exploratory_plots.pdf", width=11.69, height=8.27)
par(mar=c(3,3,2,2))
layout(matrix(c(1,0,2,3), nrow=2))
barplot(tab, legend.text=TRUE, args.legend=list(x="topright", xpd=TRUE))
barplot(normtab, ylim=c(0, 0.25), xpd=FALSE)
barx <- barplot(binproptab, ylim=c(0, 0.25), xpd=FALSE, col=c('#999999', '#DDDDDD'))
with(bin, plotrix::plotCI(barx, prop, upper, lower, add=TRUE, lwd=1.5, pch=NA))
dev.off()

## chisquare tests
chsq <- with(cleandata, chisq.test(vowel, reduction))
chsqbin <- with(cleandata, chisq.test(vowel, reduction.binary))
