####################################################################################
####    Step 7. Plot the temporal trends in changes in LImiting factors  ####
####################################################################################
# Vars use 
EnvVarUse <- c("ET",#ff7f00
               "PET",#fdbf6f
               "NPP",#33a02c
               "MCM",#a6cee3
#               "MWM",#fb9a99
               "TS",#6a3d9a
               "TAP", #e31a1c
               "PDM",#b2df8a
               "PWM",#1f78b4
               "PS")#cab2d6

ColUse <- c("#ff7f00",#"ET"
            "#fdbf6f",#"PET"
            "#33a02c",#"NPP",
            "#a6cee3",#"MCM"
#           "#fb9a99",#"MWM"
            "#6a3d9a",#"TS"
            "#e31a1c",#"TAP", 
            "#b2df8a",#"PDM",
            "#1f78b4",#"PWM",
            "#cab2d6")#"PS"
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
# .................................................................................#
#          Estimate the 50-percentile model limiting factor                        #
# .................................................................................#

## load the LimFact50 estimate
LimFact50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/LimFact50.RData")

LimFact50Lst <- lapply(1:length(LimFact50), 
                       function(i){
# Name of the period to use
                         PerUse <- names(IceList)[i]
#Load the limiting factors Raster
                         LimFact <- LimFact50[[i]][]
#Turn the limiting factors Raster to a factors table
                         LimFact <- LimFact[!is.na(LimFact)]
                         LimFact <- factor(LimFact,
                                           levels=1:length(EnvVarUse),
                                           labels = EnvVarUse)
# table of proportions
                         Prop <- round((table(LimFact)/sum(table(LimFact)))*100,2)
                         Out <-data.frame(Time = PerUse)
                         for( i in EnvVarUse){Out[,i] <- Prop[i]}
                         return(Out)
})

pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/BarplotLimFact-50p.PDF")
par(mar=c(5,4,1,4))
# Build teh final summary for the tables
LimFact50Tbl <- do.call("rbind",LimFact50Lst)
row.names(LimFact50Tbl) <- LimFact50Tbl[,1]
# make the BarPlot of proportion of Europe where each variable is the limiting factor
a<-barplot(as.matrix(t(LimFact50Tbl[,-1][,order(apply(LimFact50Tbl[,-1],2,mean))])),
           col=ColUse[order(apply(LimFact50Tbl[,-1],2,mean))],
           names.arg = NA)
# add the y axis
axis(2,las=2)
mtext("Proportion (%)",side=2,line=2.5,cex=1.2,font=2)
# add the x axis
axis(1,at=a,labels = NA)
text(x=a-0.7,
     y=rep(-6,length(a)),
     labels = sprintf("%.2f",as.numeric(gsub("BP.","",names(IceList)))/100),
     xpd=NA,
     srt=45,
     pos=1)
mtext("Thosands of Years Before Preent (kaBP)",
      side=1,line=3,cex=1.2,font=2)
# Add the legebd
legend("topright",
       legend=EnvVarUse,
       fill=ColUse,
       inset=c(-0.13,0),
       xpd=NA,
       bty="n")
dev.off()
# .................................................................................#
#          Estimate the 90-percentile model limiting factor                        #
# .................................................................................#

## load the LimFact90 estimate
LimFact90 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/LimFact90.RData")

LimFact90Lst <- lapply(1:length(LimFact90), 
                       function(i){
                         # Name of the period to use
                         PerUse <- names(IceList)[i]
                         #Load the limiting factors Raster
                         LimFact <- LimFact90[[i]][]
#Turn the limiting factors Raster to a factors table
                         LimFact <- LimFact[!is.na(LimFact)]
                         LimFact <- factor(LimFact,
                                           levels=1:length(EnvVarUse),
                                           labels = EnvVarUse)
                         # table of proportions
                         Prop <- round((table(LimFact)/sum(table(LimFact)))*100,2)
                         Out <-data.frame(Time = PerUse)
                         for( i in EnvVarUse){Out[,i] <- Prop[i]}
                         return(Out)
                       })

# Build teh final summary for the tables
LimFact90Tbl <- do.call("rbind",LimFact90Lst)
row.names(LimFact90Tbl) <- LimFact90Tbl[,1]


pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/BarplotLimFact-90p.PDF")
par(mar=c(5,4,1,4))
# Build teh final summary for the tables
LimFact90Tbl <- do.call("rbind",LimFact90Lst)
row.names(LimFact90Tbl) <- LimFact90Tbl[,1]
# make the BarPlot of proportion of Europe where each variable is the limiting factor
a<-barplot(as.matrix(t(LimFact90Tbl[,-1][,order(apply(LimFact90Tbl[,-1],2,mean))])),
           col=ColUse[order(apply(LimFact90Tbl[,-1],2,mean))],
           names.arg = NA)
# add the y axis
axis(2,las=2)
mtext("Proportion (%)",side=2,line=2.5,cex=1.2,font=2)
# add the x axis
axis(1,at=a,labels = NA)
text(x=a-0.7,
     y=rep(-6,length(a)),
     labels = sprintf("%.2f",as.numeric(gsub("BP.","",names(IceList)))/100),
     xpd=NA,
     srt=45,
     pos=1)
mtext("Thosands of Years Before Preent (kaBP)",
      side=1,line=3,cex=1.2,font=2)
# Add the legebd
legend("topright",
       legend=EnvVarUse,
       fill=ColUse,
       inset=c(-0.13,0),
       xpd=NA,
       bty="n")
dev.off()
# .................................................................................#
#          Estimate the 10-percentile model limiting factor                        #
# .................................................................................#

## load the LimFact10 estimate
LimFact10 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/LimFact10.RData")

LimFact10Lst <- lapply(1:length(LimFact10), 
                       function(i){
                         # Name of the period to use
                         PerUse <- names(IceList)[i]
                         #Load the limiting factors Raster
                         LimFact <- LimFact10[[i]][]
#Turn the limiting factors Raster to a factors table
                         LimFact <- LimFact[!is.na(LimFact)]
                         LimFact <- factor(LimFact,
                                           levels=1:length(EnvVarUse),
                                           labels = EnvVarUse)
                         # table of proportions
                         Prop <- round((table(LimFact)/sum(table(LimFact)))*100,2)
                         Out <-data.frame(Time = PerUse)
                         for( i in EnvVarUse){Out[,i] <- Prop[i]}
                         return(Out)
                       })

# Build teh final summary for the tables
LimFact10Tbl <- do.call("rbind",LimFact10Lst)
row.names(LimFact10Tbl) <- LimFact10Tbl[,1]


pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/BarplotLimFact-10p.PDF")
par(mar=c(5,4,1,4))
# Build teh final summary for the tables
LimFact10Tbl <- do.call("rbind",LimFact10Lst)
row.names(LimFact10Tbl) <- LimFact10Tbl[,1]
# make the BarPlot of proportion of Europe where each variable is the limiting factor
a<-barplot(as.matrix(t(LimFact10Tbl[,-1][,order(apply(LimFact10Tbl[,-1],2,mean))])),
           col=ColUse[order(apply(LimFact10Tbl[,-1],2,mean))],
           names.arg = NA)
# add the y axis
axis(2,las=2)
mtext("Proportion (%)",side=2,line=2.5,cex=1.2,font=2)
# add the x axis
axis(1,at=a,labels = NA)
text(x=a-0.7,
     y=rep(-6,length(a)),
     labels = sprintf("%.2f",as.numeric(gsub("BP.","",names(IceList)))/100),
     xpd=NA,
     srt=45,
     pos=1)
mtext("Thosands of Years Before Preent (kaBP)",
      side=1,line=3,cex=1.2,font=2)
# Add the legebd
legend("topright",
       legend=EnvVarUse,
       fill=ColUse,
       inset=c(-0.13,0),
       xpd=NA,
       bty="n")
dev.off()
