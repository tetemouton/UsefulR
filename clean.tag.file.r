# Summarise tag file
library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(R4MFCL)

setwd("C:/skj/2016/assessment/Model_Runs/run2014_1")

    tag <- read.tag("skj.tag")
    
    relmat <- tag$rel.lens %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint)) %>%
                               as.data.frame()
    
    tagdat <- data.frame(group = 1:tag$hd$nrel,
                         region = tag$rel$reg,
                         year = tag$rel$y,
                         mon = tag$rel$m,
                         qtr = (tag$rel$m + 1)/3,
                         Nrel = apply(relmat, 1, sum))
    
    relmat <- tag$rel.lens %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint)) %>%
                               as.data.frame()
    
    
    
    nrec <- tag$rel.recov %>% group_by(grp) %>% summarise(recN = sum(n))
    
    tagdat$Nrec <- nrec$recN[match(tagdat$group, nrec$grp)]
    
    recmat <- tag$rel.recov %>% group_by(grp, len) %>% summarise(recN = sum(n)) %>%
                                dcast(grp ~ len, value.var="recN")
    
    tmpmat <- recmat[, match(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint), colnames(recmat)[-1])]
    
    tmpmat <- matrix(NA, nrow=tag$hd$nrel, ncol=tag$hd$nint) %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint))
    
    tmpmat[recmat$grp, match(colnames(recmat)[-1], colnames(tmpmat))] <- as.matrix(recmat[,-1])

    tagdat <- cbind(tagdat, relmat, tmpmat)

    tagdat[is.na(tagdat)] <- 0
