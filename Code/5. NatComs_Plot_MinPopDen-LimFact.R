####################################################################################
####    Step 5a. Map the estimated population denisty given the limiting factor  ####
####################################################################################
rm(list = ls());gc()
require(raster)

# .................................................................................#
#          Estimate population density using the 50-percentile model               #
# .................................................................................#
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the PaleoPopDenEst50 estimate
#PaleoPopDenEst50 <- readRDS("./Data/50Percent/PaleoPopDenEst50.RData")
PaleoPopDenEst50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/MinPopPopDen50.RData")


# Determine the Raster limits 
RstLim <- sapply(PaleoPopDenEst50, function(PaleoPopDenEst){
                 PaleoPopDenEst[is.infinite(PaleoPopDenEst[])] <- NA
                  round(c(PaleoPopDenEst@data@ min, PaleoPopDenEst@data@ max),2)})

pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/PaleoPopDenEst/PaleoPopDenEst-50.PDF",
    width=9, height = 7)
for (i in 1:length(PaleoPopDenEst50)){#(i<-1)
# Name of the period to use
  PerUse <- names(IceList)[i]
# make sure the Pop density raster has no infinte vales after the Ice masking
  PaleoPopDenEst <- PaleoPopDenEst50[[i]]
  PaleoPopDenEst[is.infinite(PaleoPopDenEst[])] <- NA

# Plot the population density estimate for the evaluated period
  par(mfrow=c(1,2))
  plot(PaleoPopDenEst,
       main = paste0(gsub("BP.","",PerUse),"0 yrs-BP"), cex.main=2,
       col = rev(hcl.colors(100, palette = "RdYlBu")),
       zlim= c(min(RstLim[1,]),max(RstLim[2,])),
       las=1,
       legend.args=list(text='Population Denisty', side=4, font=2, line=2.5, cex=0.8))
# Add the Ice margins 
  image(IceList[[i]], col=c(NA,"darkgrey"),add=T)

# Add a histogram of population density
  a<-hist(PaleoPopDenEst,main=NA,xlab=NA,ylab=NA,cex.axis=0.6,las=1,col="cyan",axes=F)
  axis(1, at = seq(min(a$breaks), max(a$breaks), length.out=3))
  axis(2,las=2)
  mtext("Population\nDenisty",font=2)
}
dev.off()

# .................................................................................#
#          Estimate population density using the 90-percentile model               #
# .................................................................................#

# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the PaleoPopDenEst90 estimate
#PaleoPopDenEst90 <- readRDS("./Data/90Percent/PaleoPopDenEst90.RData")
PaleoPopDenEst90 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/MinPopPopDen90.RData")


# Determine the Raster limits 
RstLim <- sapply(PaleoPopDenEst90, function(PaleoPopDenEst){
  PaleoPopDenEst[is.infinite(PaleoPopDenEst[])] <- NA
  round(c(PaleoPopDenEst@data@ min, PaleoPopDenEst@data@ max),2)})

pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/PaleoPopDenEst/PaleoPopDenEst-90.PDF",
    width=9, height = 7)
for (i in 1:length(PaleoPopDenEst90)){#(i<-1)
  # Name of the period to use
  PerUse <- names(IceList)[i]
  # make sure the Pop density raster has no infinte vales after the Ice masking
  PaleoPopDenEst <- PaleoPopDenEst90[[i]]
  PaleoPopDenEst[is.infinite(PaleoPopDenEst[])] <- NA
  
  # Plot the population density estimate for the evaluated period
  par(mfrow=c(1,2))
  plot(PaleoPopDenEst,
       main = paste0(gsub("BP.","",PerUse),"0 yrs-BP"), cex.main=2,
       col = rev(hcl.colors(100, palette = "RdYlBu")),
       zlim= c(min(RstLim[1,]),max(RstLim[2,])),
       las=1,
       legend.args=list(text='Population Denisty', side=4, font=2, line=2.5, cex=0.8))
  # Add the Ice margins 
  image(IceList[[i]], col=c(NA,"darkgrey"),add=T)
  
  # Add a histogram of population density
  a<-hist(PaleoPopDenEst,main=NA,xlab=NA,ylab=NA,cex.axis=0.6,las=1,col="cyan",axes=F)
  axis(1, at = seq(min(a$breaks), max(a$breaks), length.out=3))
  axis(2,las=2)
  mtext("Population\nDenisty",font=2)
}
dev.off()

# .................................................................................#
#          Estimate population density using the 10-percentile model               #
# .................................................................................#

# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the PaleoPopDenEst10 estimate
#PaleoPopDenEst10 <- readRDS("./Data/10Percent/PaleoPopDenEst10.RData")
PaleoPopDenEst10 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/MinPopPopDen10.RData")


# Determine the Raster limits 
RstLim <- sapply(PaleoPopDenEst10, function(PaleoPopDenEst){
  PaleoPopDenEst[is.infinite(PaleoPopDenEst[])] <- NA
  round(c(PaleoPopDenEst@data@ min, PaleoPopDenEst@data@ max),2)})

pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/PaleoPopDenEst/PaleoPopDenEst-10.PDF",
    width=9, height = 7)
for (i in 1:length(PaleoPopDenEst10)){#(i<-1)
  # Name of the period to use
  PerUse <- names(IceList)[i]
  # make sure the Pop density raster has no infinte vales after the Ice masking
  PaleoPopDenEst <- PaleoPopDenEst10[[i]]
  PaleoPopDenEst[is.infinite(PaleoPopDenEst[])] <- NA
  
  # Plot the population density estimate for the evaluated period
  par(mfrow=c(1,2))
  plot(PaleoPopDenEst,
       main = paste0(gsub("BP.","",PerUse),"0 yrs-BP"), cex.main=2,
       col = rev(hcl.colors(100, palette = "RdYlBu")),
       zlim= c(min(RstLim[1,]),max(RstLim[2,])),
       las=1,
       legend.args=list(text='Population Denisty', side=4, font=2, line=2.5, cex=0.8))
  # Add the Ice margins 
  image(IceList[[i]], col=c(NA,"darkgrey"),add=T)
  
  # Add a histogram of population density
  a<-hist(PaleoPopDenEst,main=NA,xlab=NA,ylab=NA,cex.axis=0.6,las=1,col="cyan",axes=F)
  axis(1, at = seq(min(a$breaks), max(a$breaks), length.out=3))
  axis(2,las=2)
  mtext("Population\nDenisty",font=2)
}
dev.off()

####################################################################################
####    Step 5b. Map the limiting factor                                        ####
####################################################################################

# .................................................................................#
#          Estimate the 50-percentile model limiting factor                        #
# .................................................................................#
rm(list = ls());gc()
require(raster)
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
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the LimFact50 estimate
#LimFact50 <- readRDS("./Data/50Percent/LimFact50.RData")
LimFact50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/LimFact50.RData")


pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/LimFact50Ver1.PDF")
for (i in 1:length(LimFact50)){#i<-1
  # Name of the period to use
  PerUse <- names(IceList)[i]
  #Load the limiting factoirs Raster
  LimFact <- LimFact50[[i]]
  # table of proportions
  Prop <- round((table(factor(LimFact[],level=1:10))/sum(table(factor(LimFact[],level=1:10))))*100,1)
  #Plot the Limiting factors
  plot(LimFact,
       zlim=c(1,length(EnvVarUse)),
       col = ColUse,
       legend.width=2, legend.shrink=1,
       main = paste0(gsub("BP.","",PerUse),"0 yrs-BP"), cex.main=2,
       las=1,
       legend.args=list(text='Limiting\nFactor', side=3, font=2, line=1, cex=1),
       axis.args=list(at=1:10,
                      labels=paste0(EnvVarUse,"\n",as.numeric(Prop),"%"), 
                      cex.axis=0.8))
  # Add the Ice margins 
  image(IceList[[i]], col=c(NA,"darkgrey"),add=T)
}
dev.off()

pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/LimFact50Ver2.PDF",)
for (i in 1:length(LimFact50)){#i<-20
  #Load the limiting factoirs Raster
  LimFact <- LimFact50[[i]]
  # table of proportions
  Prop <- round((table(factor(LimFact[],level=1: length(EnvVarUse)))/sum(table(factor(LimFact[],level=1: length(EnvVarUse)))))*100,1)
  # Add the cie to the raster
  LimFact[IceList[[i]][]==100] <- c(length(EnvVarUse)+1)
  # Make the rastr cathegorical
  LimFactFct <- ratify(LimFact)
  # add a attribute table to the raster
  rat <- data.frame(ID=1:c(length(EnvVarUse)+1),
                    Env = c(paste0(EnvVarUse,": ",as.numeric(Prop),"%"),"Ice"),
                    code = 1:c(length(EnvVarUse)+1))
  levels(LimFactFct) <- rat
  # define the colors
  rat$Col <- c(ColUse,"Grey")
  # plot the raster using a trellis plot
  print(rasterVis::levelplot(LimFactFct,
                             main = paste0(gsub("BP.","",names(IceList)[i]),"0 yrs-BP"),
                             col.regions=rat$Col,
                             cex=0.6))
}
dev.off()
# .................................................................................#
#          Estimate the 90-percentile model limiting factor                        #
# .................................................................................#
rm(list = ls());gc()
require(raster)
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
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the LimFact90 estimate
#LimFact90 <- readRDS("./Data/90Percent/LimFact90.RData")
LimFact90 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/LimFact90.RData")


pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/LimFact90Ver1.PDF",)
for (i in 1:length(LimFact90)){#i<-1
  # Name of the period to use
  PerUse <- names(IceList)[i]
  #Load the limiting factoirs Raster
  LimFact <- LimFact90[[i]]
  # table of proportions
  Prop <- round((table(factor(LimFact[],level=1:10))/sum(table(factor(LimFact[],level=1:10))))*100,1)
  #Plot the Limiting factors
  plot(LimFact,
       zlim=c(1,length(EnvVarUse)),
       col = ColUse,
       legend.width=2, legend.shrink=1,
       main = paste0(gsub("BP.","",PerUse),"0 yrs-BP"), cex.main=2,
       las=1,
       legend.args=list(text='Limiting\nFactor', side=3, font=2, line=1, cex=1),
       axis.args=list(at=1:10,
                      labels=paste0(EnvVarUse,"\n",as.numeric(Prop),"%"), 
                      cex.axis=0.8))
  # Add the Ice margins 
  image(IceList[[i]], col=c(NA,"darkgrey"),add=T)
}
dev.off()

pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/LimFact90Ver2.PDF",)
for (i in 1:length(LimFact90)){#i<-20
  #Load the limiting factoirs Raster
  LimFact <- LimFact90[[i]]
  # table of proportions
  Prop <- round((table(factor(LimFact[],level=1: length(EnvVarUse)))/sum(table(factor(LimFact[],level=1: length(EnvVarUse)))))*100,1)
  # Add the cie to the raster
  LimFact[IceList[[i]][]==100] <- c(length(EnvVarUse)+1)
  # Make the rastr cathegorical
  LimFactFct <- ratify(LimFact)
  # add a attribute table to the raster
  rat <- data.frame(ID=1:c(length(EnvVarUse)+1),
                    Env = c(paste0(EnvVarUse,": ",as.numeric(Prop),"%"),"Ice"),
                    code = 1:c(length(EnvVarUse)+1))
  levels(LimFactFct) <- rat
  # define the colors
  rat$Col <- c(ColUse,"Grey")
  # plot the raster using a trellis plot
  print(rasterVis::levelplot(LimFactFct,
                             main = paste0(gsub("BP.","",names(IceList)[i]),"0 yrs-BP"),
                             col.regions=rat$Col,
                             cex=0.6))
}
dev.off()
# .................................................................................#
#          Estimate the 10-percentile model limiting factor                        #
# .................................................................................#
rm(list = ls());gc()
require(raster)
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
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the LimFact10 estimate
#LimFact10 <- readRDS("./Data/10Percent/LimFact10.RData")
LimFact10 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/LimFact10.RData")


pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/LimFact10Ver1.PDF",)
for (i in 1:length(LimFact10)){#i<-1
  # Name of the period to use
  PerUse <- names(IceList)[i]
  #Load the limiting factoirs Raster
  LimFact <- LimFact10[[i]]
  # table of proportions
  Prop <- round((table(factor(LimFact[],level=1:10))/sum(table(factor(LimFact[],level=1:10))))*100,1)
  #Plot the Limiting factors
  plot(LimFact,
       zlim=c(1,length(EnvVarUse)),
       col = ColUse,
       legend.width=2, legend.shrink=1,
       main = paste0(gsub("BP.","",PerUse),"0 yrs-BP"), cex.main=2,
       las=1,
       legend.args=list(text='Limiting\nFactor', side=3, font=2, line=1, cex=1),
       axis.args=list(at=1:10,
                      labels=paste0(EnvVarUse,"\n",as.numeric(Prop),"%"), 
                      cex.axis=0.8))
  # Add the Ice margins 
  image(IceList[[i]], col=c(NA,"darkgrey"),add=T)
}
dev.off()

pdf("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/LimFact/LimFact10Ver2.PDF",)
for (i in 1:length(LimFact10)){#i<-20
  #Load the limiting factoirs Raster
  LimFact <- LimFact10[[i]]
  # table of proportions
  Prop <- round((table(factor(LimFact[],level=1: length(EnvVarUse)))/sum(table(factor(LimFact[],level=1: length(EnvVarUse)))))*100,1)
  # Add the cie to the raster
  LimFact[IceList[[i]][]==100] <- c(length(EnvVarUse)+1)
  # Make the rastr cathegorical
  LimFactFct <- ratify(LimFact)
  # add a attribute table to the raster
  rat <- data.frame(ID=1:c(length(EnvVarUse)+1),
                    Env = c(paste0(EnvVarUse,": ",as.numeric(Prop),"%"),"Ice"),
                    code = 1:c(length(EnvVarUse)+1))
  levels(LimFactFct) <- rat
  # define the colors
  rat$Col <- c(ColUse,"Grey")
  # plot the raster using a trellis plot
  print(rasterVis::levelplot(LimFactFct,
                             main = paste0(gsub("BP.","",names(IceList)[i]),"0 yrs-BP"),
                             col.regions=rat$Col,
                             cex=0.6))
}
dev.off()