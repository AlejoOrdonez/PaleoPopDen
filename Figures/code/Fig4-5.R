###############################################################################################
###     Map Mean Population density 
###############################################################################################
rm(list=ls());gc()
require(raster)
require(mgcv)


# Get the estimates of the Mean Population size
PopDenFinalRast <- readRDS('~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/MinPopPopDen50.RData')

PopDenMap <- stack(PopDenFinalRast[[1]],
                   mean(do.call("stack",lapply(14:17,function(x){PopDenFinalRast[[x]]}))),
                   mean(do.call("stack",lapply(17:19,function(x){PopDenFinalRast[[x]]}))),
                   mean(do.call("stack",lapply(20:23,function(x){PopDenFinalRast[[x]]}))),
                   mean(do.call("stack",lapply(23:27,function(x){PopDenFinalRast[[x]]}))))
names(PopDenMap) <- c("Greenland Stadial 2",
                      "Greenland Interstadial 1",
                      "Greenland Stadial 1",
                      "Holocene initiation",
                      "Mid-Holocene")
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
IceList <- stack(IceList[[1]],
                 IceList[[14]],
                 IceList[[17]],
                 IceList[[20]],
                 IceList[[24]])
pdf("~/Desktop/Fig_4.pdf",width=9,height = 9)                
print(rasterVis::levelplot(PopDenMap,
                     margin=F,
                     col.regions = rev(hcl.colors(100, palette = "RdYlBu"))) + 
  rasterVis::levelplot(IceList, col.regions =c(NA,"grey"),add=T, title="NA"))
dev.off()

###############################################################################################
###     Map Limiting factors 
###############################################################################################
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

# Get the estimates of the Mean Population size
LimFact50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/LimFact50.RData")

rat <- data.frame(ID=1:c(length(EnvVarUse)+1),
                  Env = c(EnvVarUse,"Ice"),
                  code = 1:c(length(EnvVarUse)+1))

for (i in c(1,14,17,20,27)){#i<-1
  LimFact <- LimFact50[[i]]
  # Add the cie to the raster
  LimFact[IceList[[i]][]==100] <- c(length(EnvVarUse)+1)
  LimFact <- ratify(LimFact)
  levels(LimFact) <- rat
  if(i==1){LimFact50Map <- LimFact}
  else{LimFact50Map <- stack(LimFact50Map,LimFact)}
}
names(LimFact50Map) <- c("Greenland Stadial 2",
                         "Greenland Interstadial 1",
                         "Greenland Stadial 1",
                         "Holocene initiation",
                         "Mid-Holocene")
pdf("~/Desktop/Fig_5.pdf",width=9,height = 9)    
print(rasterVis::levelplot(LimFact50Map,
                           margin=F,
                           col.regions = c(ColUse,"Grey")))
dev.off()
