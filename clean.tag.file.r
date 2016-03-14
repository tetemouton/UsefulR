# Summarise tag file
library(dplyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(R4MFCL)
library(scales)
library(grid)
library(stringr)

setwd("C:/skj/2016/assessment/Model_Runs/run2014_1")

    fdesc <- read.table("C:/skj/2016/assessment/Setup/fdesc.txt", header=TRUE)

    theme_set(theme_bw())

    tag <- read.tag("skj.tag")
    
    relmat <- tag$rel.lens %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint)) %>%
                               as.data.frame()
    
    tagdat <- data.frame(group = 1:tag$hd$nrel,
                         Program = tag$tagprog,
                         region = tag$rel$reg,
                         year = tag$rel$y,
                         mon = tag$rel$m,
                         qtr = (tag$rel$m + 1)/3,
                         Nrel = apply(relmat, 1, sum))
    
    relmat <- tag$rel.lens %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint)) %>%
                               as.data.frame()
       
    nrec <- tag$rel.recov %>% group_by(grp) %>% summarise(recN = sum(n))
    
    tagdat$Nrec <- nrec$recN[match(tagdat$group, nrec$grp)]
    tagdat$yrqtr <- tagdat$year + tagdat$qtr/4 - 0.125
    tagdat$Prec <- tagdat$Nrec/tagdat$Nrel
    
    recmat <- tag$rel.recov %>% group_by(grp, len) %>% summarise(recN = sum(n)) %>%
                                dcast(grp ~ len, value.var="recN")
    
    tmpmat <- matrix(NA, nrow=tag$hd$nrel, ncol=tag$hd$nint) %>% set_colnames(seq(from=tag$hd$l1, by=tag$hd$iwd, length.out=tag$hd$nint))
    
    tmpmat[recmat$grp, match(colnames(recmat)[-1], colnames(tmpmat))] <- as.matrix(recmat[,-1])

    tagdat <- cbind(tagdat, relmat, tmpmat)

    tagdat[is.na(tagdat)] <- 0
    tagdat$Region <- paste("Region", tagdat$region)


#     rel.pl <- ggplot(data=tagdat, aes(x=yrqtr, y=Nrel)) + geom_line() + facet_wrap(~ prog)
# 
#     rel.pl <- ggplot(data=tagdat, aes(x=yrqtr, y=Nrel, fill=Program)) + geom_bar(stat="identity", colour="black") +#geom_line() + geom_point() +
#                      scale_colour_brewer(palette="Set1") +
#                      theme(legend.position="none",
#                            panel.grid.major=element_blank(),
#                            panel.grid.minor=element_blank())
# 
#     rec.pl <- ggplot(data=tagdat, aes(x=yrqtr, y=Nrec, fill=Program)) + geom_bar(stat="identity", colour="black") +#geom_line() + geom_point() +
#                      scale_colour_brewer(palette="Set1") +
#                      theme(legend.position="none",
#                            panel.grid.major=element_blank(),
#                            panel.grid.minor=element_blank())
# 
#     Prec.pl <- ggplot(data=tagdat, aes(x=yrqtr, y=Prec, fill=Program)) + geom_hline(yintercept=1, colour=alpha("grey",0.5), size=1.3) +
#                      scale_colour_brewer(palette="Set1") + geom_bar(stat="identity", colour="black") +#geom_line() + geom_point() + #geom_point(aes(size=Nrel)) +
#                      theme(legend.key.size=unit(0.1, "cm"),
#                            legend.justification=c(0,1),
#                            legend.position=c(0,1.1),
#                            legend.background = element_rect(fill="transparent"),
#                            panel.grid.major=element_blank(),
#                            panel.grid.minor=element_blank())
# 
# 
#     
    reg.pl <- ggplot(data=tagdat, aes(x=yrqtr, y=Nrel)) + geom_bar(stat="identity") + ggtitle("Releases by region") +
                     scale_colour_brewer(palette="Set1") + facet_wrap(~ Region, ncol=1) + ylab("No. fish") + xlab("") +
                     theme(legend.position="none",
                           plot.title=element_text(colour=grey(0.3)),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank())


    tmpdat <- tagdat[,1:10] %>% group_by(Program, yrqtr) %>% summarise(Trel = sum(Nrel), Trec = sum(Nrec)) %>%
                                mutate(Prec = Trec/Trel)


    rel.pl <- ggplot(data=tmpdat, aes(x=yrqtr, y=Trel, fill=Program)) + geom_bar(stat="identity", width=0.25, colour="black") +
                     scale_fill_brewer(palette="Set2") + facet_wrap(~ Program, scales="free") + ylab("No. fish") + xlab("") +
                     scale_x_continuous(breaks=pretty_breaks(n=3)) + scale_y_continuous(breaks=pretty_breaks(n=3)) + ggtitle("Releases") +
                     theme(legend.position="none",
                           plot.title=element_text(colour=grey(0.3)),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))
    
    rec.pl <- ggplot(data=tmpdat, aes(x=yrqtr, y=Trec, fill=Program)) + geom_bar(stat="identity", width=0.25, colour="black") +
                     scale_fill_brewer(palette="Set2") + facet_wrap(~ Program, scales="free") + ylab("No. fish") + xlab("") +
                     scale_x_continuous(breaks=pretty_breaks(n=3)) + scale_y_continuous(breaks=pretty_breaks(n=3)) + ggtitle("Recaptures") +
                     theme(legend.position="none",
                           plot.title=element_text(colour=grey(0.3)),
                           panel.grid.major=element_blank(),
                           panel.grid.minor=element_blank(),
                           plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))
    
    Prec.pl <- ggplot(data=tmpdat, aes(x=yrqtr, y=Prec, fill=Program)) + geom_hline(yintercept=1, colour=alpha("grey",0.5), size=1.3) +
                      scale_colour_brewer(palette="Set2") + geom_bar(stat="identity", width=0.25, colour="black") +
                      facet_wrap(~ Program, scales="free") + ylab("P(recaptured)") + xlab("") + ggtitle("Recapture rate") +
                      scale_x_continuous(breaks=pretty_breaks(n=3)) + scale_y_continuous(breaks=pretty_breaks(n=3)) +
                      theme(legend.position="none",
                            plot.title=element_text(colour=grey(0.3)),
                            legend.key.size=unit(0.1, "cm"),
                            legend.justification=c(0,1),
                            legend.position=c(0,1.1),
                            legend.background = element_rect(fill="transparent"),
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"))








    windows(2000,2000)    
        pushViewport(viewport(layout = grid.layout(3,5)))   # make a grid of 3 rows by 2 columns
            print(rel.pl, vp = viewport(layout.pos.row=1, layout.pos.col=1:3))
            print(rec.pl, vp = viewport(layout.pos.row=2, layout.pos.col=1:3))
            print(Prec.pl, vp=viewport(layout.pos.row=3, layout.pos.col=1:3))        
            print(reg.pl, vp=viewport(layout.pos.row=1:3, layout.pos.col=4:5))
    dev.off()


    tmprelL <- tagdat[, 1:(tag$hd$nint + 10)] %>% melt(id.vars=1:10, variable.name="len") %>%
                                                  group_by(Program, region, len) %>% 
                                                  summarise(N = sum(value)) %>% mutate(Category = "Release")
    
    tmprecL <- tagdat[, c(1:10,(tag$hd$nint + 11):dim(tagdat)[2])] %>% melt(id.vars=1:10, variable.name="len") %>%
                                                                       group_by(Program, region, len) %>%
                                                                       summarise(N = sum(value)) %>%
                                                                       mutate(Category = "Recapture")
    
    tmprelL <- rbind(tmprelL, tmprecL)
    
    
    relL.pl <- ggplot(data=tmprelL, aes(x=len, y=N, fill=Category)) + geom_vline(xintercept=25, colour=alpha("grey",0.5), size=1.3) +
                      geom_bar(stat="identity", colour="black", position="dodge") + #geom_line() + geom_point() + #geom_point(aes(size=Nrel)) +
                      facet_grid(region ~ Program) + scale_x_discrete(breaks=pretty_breaks(n=5)) +                      
                      theme(legend.position="none",
                            legend.background = element_rect(fill="transparent"),
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank())


    tmprelL2 <- tmprelL %>% mutate(len2 = as.numeric(as.character(len)))

    relL.pl2 <- ggplot(data=tmprelL2, aes(x=len2, y=N, fill=Category)) + geom_vline(xintercept=50, colour=alpha("grey",0.7), size=1.1) +
                      geom_density(stat="identity") + scale_fill_manual(values=c(alpha("blue",0.7),alpha("red",0.3))) +#geom_line() + geom_point() + #geom_point(aes(size=Nrel)) +
                      facet_grid(region ~ Program, scales="free_y") + scale_y_continuous(breaks=pretty_breaks(n=2)) +  
                      xlab("Length (cm)") + ylab("Number fish") +
                      theme(legend.position="none",
                            legend.background = element_rect(fill="transparent"),
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank())
    
    movmat <- tag$rel.recov %>% mutate(relreg = tag$rel$reg[grp], recreg = fdesc$region[fsh]) %>% group_by(relreg, recreg) %>% summarise(recN = sum(n))
    
    regtots <- movmat %>% group_by(relreg) %>% summarise(recN = sum(recN))

    movmat$Proportion <- movmat$recN/regtots$recN[movmat$relreg]
    
    mat.pl <- ggplot(movmat, aes(x=relreg, y=recreg)) + geom_tile(aes(fill=Proportion), colour = "black") + 
                     scale_fill_gradient(low="white", high="red", trans="sqrt") +
                     geom_text(aes(label=recN)) + coord_equal() + #scale_x_discrete(expand=c(0, 0)) +
                     xlab("Release region") + ylab("Recapture region") + #scale_y_discrete(expand=c(0, 0)) +
                     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                           axis.ticks = element_blank(), panel.border = element_blank(), axis.ticks.length = unit(-0.2, "cm"))

    windows(2200,2500)    
        pushViewport(viewport(layout = grid.layout(2,5)))   # make a grid of 2 rows by 2 columns
            print(mat.pl, vp = viewport(layout.pos.row=1, layout.pos.col=1:4))
            print(relL.pl2, vp = viewport(layout.pos.row=2, layout.pos.col=1:5))
    dev.off()



















