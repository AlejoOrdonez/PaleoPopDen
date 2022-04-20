####################################################################################
####     Step 4. Define the Limiting factors and estimated population  		    ####
####             denisty given the limiting factor                              ####
####################################################################################

#..................................................................................#
#    Estimate the Min population Size and Limiting factor for the 50-Percentile    #  
#..................................................................................#
rm(list = ls());gc()
require(raster)
# Estimate population density using the 50-percentile model
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the PaleoPopDenEst50 estimate
#PaleoPopDenEst50 <- readRDS("./Data/50Percent/PaleoPopDenEst50.RData")
PaleoPopDenEst50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/PaleoPopDenEst50.RData")
## Estimat6e the Min PopDensity
MinPopPopDen50 <- lapply(names(PaleoPopDenEst50), function(y){
  PopDen <- PaleoPopDenEst50[[y]] # Load pop density estimates
  # Remove MWM -  Might not be necesary latter on
  if(sum(names(PopDen)%in%"MWM")!=0){
    PopDen <- PopDen[[-which(names(PopDen)%in%"MWM")]]}
  Ice <- IceList[[y]]==0 # load Ice sheets
  PopDenCrop <- PopDen*Ice
  PopDenCrop[is.infinite(PopDenCrop[])] <- NA
  # calculate the Mim Popultion size the Limming factor Variable
  calc(PopDenCrop,function(x){min(x)})
})
## Plot the Min Population density
dev.new()
rasterVis::levelplot(MinPopPopDen50[[1]],
                     margin=F,
                     col.regions = rev(hcl.colors(100, palette = "RdYlBu")),
                     main = paste0(gsub("BP.","",names(PaleoPopDenEst50)[[1]]),"0 BP"),
                     colorkey = list(title= "Pop Density",cex=2)) +
  rasterVis::levelplot(IceList[[1]], col.regions =c(NA,"grey"),add=T, title="NA")
names(MinPopPopDen50) <- names(PaleoPopDenEst50)
## Save the Min Population density Output
saveRDS(MinPopPopDen50,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/MinPopPopDen50_NoIceMask.RData")

# Determine the limiting factors
LimFact50 <- lapply(names(PaleoPopDenEst50), function(y){#(y <- "BP.2100")
  PopDen <- PaleoPopDenEst50[[y]] # Load pop density estimates
  # Remove MWM -  Might not be necesary latter on
  if(sum(names(PopDen)%in%"MWM")!=0){
    PopDen <- PopDen[[-which(names(PopDen)%in%"MWM")]]}
  # Remove Locations covered by Ice
  Ice <- IceList[[y]]==0 # load Ice sheets
  PopDenCrop <- PopDen*Ice
  PopDenCrop[is.infinite(PopDenCrop[])] <- NA
  # Define the Limming factor Variable
  LimFact<-calc(PopDenCrop,
                function(x){
                  if(is.na(x[1]) | x[1] == Inf){out <- NA}
                  else {out <- order(x)[1]}
                  return(out)
                })
  return(LimFact)})

## Plot the limiting factor
i<-1
rasterVis::levelplot(LimFact50[[i]],
                     margin=F,
                     main = paste0(gsub("BP.","",names(PaleoPopDenEst50)[[i]]),"0 BP"),
                     col.regions = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf', '#999999'),
                     at = 1:10,
                     colorkey = list(at = 1:10,
                                     labels = list(labels = names(PaleoPopDenEst50[[1]])[-5], at = 1:10 + 0.125)))
## Save the Limiting fctors
#saveRDS(LimFact50,file = "./Data/50Percent/LimFact50_NoIceMask.RData")
names(LimFact50) <- names(PaleoPopDenEst50)

saveRDS(LimFact50,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/LimFact50_NoIceMask.RData")
#..................................................................................#
#    Estimate the Min population Size and Limiting factor for the 90-Percentile    #  
#..................................................................................#
rm(list = ls());gc()
require(raster)
# Estimate population density using the 90-percentile model
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the PaleoPopDenEst90 estimate
#PaleoPopDenEst90 <- readRDS("./Data/90Percent/PaleoPopDenEst90.RData")
PaleoPopDenEst90 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/PaleoPopDenEst90.RData")
## Estimat6e the Min PopDensity
MinPopPopDen90 <- lapply(names(PaleoPopDenEst90), function(y){
  PopDen <- PaleoPopDenEst90[[y]] # Load pop density estimates
  # Remove MWM -  Might not be necesary latter on
  if(sum(names(PopDen)%in%"MWM")!=0){
    PopDen <- PopDen[[-which(names(PopDen)%in%"MWM")]]}
  Ice <- IceList[[y]]==0 # load Ice sheets
  PopDenCrop <- PopDen*Ice
  PopDenCrop[is.infinite(PopDenCrop[])] <- NA
  # calculate the Mim Popultion size the Limming factor Variable
  calc(PopDenCrop,function(x){min(x)})
})
## Plot the Min Population density
dev.new()
rasterVis::levelplot(MinPopPopDen90[[1]],
                     margin=F,
                     col.regions = rev(hcl.colors(100, palette = "RdYlBu")),
                     main = paste0(gsub("BP.","",names(PaleoPopDenEst90)[[1]]),"0 BP"),
                     colorkey = list(title= "Pop Density",cex=2)) +
  rasterVis::levelplot(IceList[[1]], col.regions =c(NA,"grey"),add=T, title="NA")
## Save the Min Population density Output
#saveRDS(MinPopPopDen90,file = "./Data/90Percent/MinPopPopDen90_NoIceMask.RData")
names(MinPopPopDen90) <- names(PaleoPopDenEst90)
saveRDS(MinPopPopDen90,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/MinPopPopDen90_NoIceMask.RData")

# Determine the limiting factors
LimFact90 <- lapply(names(PaleoPopDenEst90), function(y){#(y <- "BP.2100")
  PopDen <- PaleoPopDenEst90[[y]] # Load pop density estimates
  # Remove MWM -  Might not be necesary latter on
  if(sum(names(PopDen)%in%"MWM")!=0){
    PopDen <- PopDen[[-which(names(PopDen)%in%"MWM")]]}
  # Remove Locations covered by Ice
  Ice <- IceList[[y]]==0 # load Ice sheets
  PopDenCrop <- PopDen*Ice
  PopDenCrop[is.infinite(PopDenCrop[])] <- NA
  # Define the Limming factor Variable
  LimFact<-calc(PopDenCrop,
                function(x){
                  if(is.na(x[1]) | x[1] == Inf){out <- NA}
                  else {out <- order(x)[1]}
                  return(out)
                })
  return(LimFact)})

## Plot the limiting factor
i<-1
rasterVis::levelplot(LimFact90[[i]],
                     margin=F,
                     main = paste0(gsub("BP.","",names(PaleoPopDenEst90)[[i]]),"0 BP"),
                     col.regions = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf', '#999999'),
                     at = 1:10,
                     colorkey = list(at = 1:10,
                                     labels = list(labels = names(PaleoPopDenEst90[[1]])[-5], at = 1:10 + 0.125)))
## Save the Limiting fctors
#saveRDS(LimFact90,file = "./Data/90Percent/LimFact90_NoIceMask.RData")
names(LimFact90) <- names(PaleoPopDenEst90)
saveRDS(LimFact90,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/LimFact90_NoIceMask.RData")

#..................................................................................#
#    Estimate the Min population Size and Limiting factor for the 10-Percentile    #  
#..................................................................................#
rm(list = ls());gc()
require(raster)
# Estimate population density using the 10-percentile model
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the PaleoPopDenEst10 estimate
#PaleoPopDenEst10 <- readRDS("./Data/10Percent/PaleoPopDenEst10.RData")
PaleoPopDenEst10 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/PaleoPopDenEst10.RData")
## Estimat6e the Min PopDensity
MinPopPopDen10 <- lapply(names(PaleoPopDenEst10), function(y){
  PopDen <- PaleoPopDenEst10[[y]] # Load pop density estimates
  # Remove MWM -  Might not be necesary latter on
  if(sum(names(PopDen)%in%"MWM")!=0){
    PopDen <- PopDen[[-which(names(PopDen)%in%"MWM")]]}
  Ice <- IceList[[y]]==0 # load Ice sheets
  PopDenCrop <- PopDen*Ice
  PopDenCrop[is.infinite(PopDenCrop[])] <- NA
  # calculate the Mim Popultion size the Limming factor Variable
  calc(PopDenCrop,function(x){min(x)})
})
## Plot the Min Population density
dev.new()
rasterVis::levelplot(MinPopPopDen10[[1]],
                     margin=F,
                     col.regions = rev(hcl.colors(100, palette = "RdYlBu")),
                     main = paste0(gsub("BP.","",names(PaleoPopDenEst10)[[1]]),"0 BP"),
                     colorkey = list(title= "Pop Density",cex=2)) +
  rasterVis::levelplot(IceList[[1]], col.regions =c(NA,"grey"),add=T, title="NA")
## Save the Min Population density Output
#saveRDS(MinPopPopDen10,file = "./Data/10Percent/MinPopPopDen10_NoIceMask.RData")
names(MinPopPopDen10) <- names(PaleoPopDenEst10)
saveRDS(MinPopPopDen10,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/MinPopPopDen10_NoIceMask.RData")

# Determine the limiting factors
LimFact10 <- lapply(names(PaleoPopDenEst10), function(y){#(y <- "BP.2100")
  PopDen <- PaleoPopDenEst10[[y]] # Load pop density estimates
  # Remove MWM -  Might not be necesary latter on
  if(sum(names(PopDen)%in%"MWM")!=0){
    PopDen <- PopDen[[-which(names(PopDen)%in%"MWM")]]}
  # Remove Locations covered by Ice
  Ice <- IceList[[y]]==0 # load Ice sheets
  PopDenCrop <- PopDen*Ice
  PopDenCrop[is.infinite(PopDenCrop[])] <- NA
  # Define the Limming factor Variable
  LimFact<-calc(PopDenCrop,
                function(x){
                  if(is.na(x[1]) | x[1] == Inf){out <- NA}
                  else {out <- order(x)[1]}
                  return(out)
                })
  return(LimFact)})

## Plot the limiting factor
i<-1
rasterVis::levelplot(LimFact10[[i]],
                     margin=F,
                     main = paste0(gsub("BP.","",names(PaleoPopDenEst10)[[i]]),"0 BP"),
                     col.regions = c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#ffff33', '#a65628', '#f781bf', '#999999'),
                     at = 1:10,
                     colorkey = list(at = 1:10,
                                     labels = list(labels = names(PaleoPopDenEst10[[1]])[-5], at = 1:10 + 0.125)))
## Save the Limiting fctors
#saveRDS(LimFact10,file = "./Data/10Percent/LimFact10_NoIceMask.RData")
names(LimFact10) <- names(PaleoPopDenEst10)
saveRDS(LimFact10,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/LimFact10_NoIceMask.RData")