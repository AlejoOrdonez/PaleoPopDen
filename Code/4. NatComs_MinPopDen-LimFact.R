####################################################################################
####     Step 4. Define the Limiting factors and estimated population  		    ####
####             denisty given the limiting factor                              ####
####################################################################################
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
							Ice <- IceList[[y]]==0 # load Ice sheets
							PopDenCrop <- PopDen/Ice
							calc(PopDenCrop,function(x){min(x)})})
## Plot the Min Population density
dev.new()
image(MinPopPopDen50[[1]],
     col = rev(hcl.colors(100, palette = "RdYlBu")),
     main = paste0(gsub("BP.","",names(PaleoPopDenEst50)[[1]]),"0 BP"))
image(IceList[[1]],col=c(NA,"grey"),add=T)
## Save the Min Population density Output
#saveRDS(MinPopPopDen50,file = "./Data/50Percent/MinPopPopDen50.RData")
saveRDS(MinPopPopDen50,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/MinPopPopDen50.RData")

# Determine the limiting factors
LimFact50 <- lapply(names(PaleoPopDenEst50), function(y){#(y <- "BP.2100")
							PopDen <- PaleoPopDenEst50[[y]] # Load pop density estimates
							Ice <- IceList[[y]]==0 # load Ice sheets
							PopDenCrop <- PopDen/Ice
							PopDenCrop[is.infinite(PopDenCrop[])] <- NA
							LimFact<-calc(PopDenCrop,
										 function(x){
										 	if(is.na(x[1]) | x[1] == Inf){out <- NA}
										 	else {out <- order(x)[1]}
										 	return(out)
										 	})
							return(LimFact)})

## Plot the limiting factor
plot(LimFact50[[1]])
image(IceList[[1]],col=c(NA,"grey"),add=T)

## Save the Limiting fctors
#saveRDS(LimFact50,file = "./Data/50Percent/LimFact50.RData")
saveRDS(LimFact50,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/LimFact50.RData")
#-----------------------------------------------------------------------------------
rm(list = ls());gc()
require(raster)
# Load Ice Layers from ICE-6G-C
IceList <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")
#IceList <- readRDS(file = "https://www.dropbox.com/s/34jy6jpb6usbqju/Ice0_5DD.RData?dl=1")
#IceList <- readRDS(file = "./Data/Paleo_Predictors/Ice0_5DD.RData")

## load the PaleoPopDenEst90 estimate
#PaleoPopDenEst90 <- readRDS("./Data/90Percent/PaleoPopDenEst90.RData")
PaleoPopDenEst90 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/PaleoPopDenEst90.RData")

## Estimate the Min PopDensity
MinPopPopDen90 <- lapply(names(PaleoPopDenEst90), function(y){
							PopDen <- PaleoPopDenEst90[[y]] # Load pop density estimates
							Ice <- IceList[[y]]==0 # load Ice sheets
							PopDenCrop <- PopDen/Ice
							calc(PopDenCrop,function(x){min(x)})})

## Plot the Min Population density
plot(MinPopPopDen90[[1]])
image(IceList[[1]],col=c(NA,"grey"),add=T)

## Save the Min Population density Output
#saveRDS(MinPopPopDen90,file = "./Data/90Percent/MinPopPopDen90.RData")
saveRDS(MinPopPopDen90,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/MinPopPopDen90.RData")

# Determine the limiting factors
LimFact90 <- lapply(names(PaleoPopDenEst90), function(y){#(y <- "BP.2100")
							PopDen <- PaleoPopDenEst90[[y]] # Load pop density estimates
							Ice <- IceList[[y]]==0 # load Ice sheets
							PopDenCrop <- PopDen/Ice
							LimFact<-calc(PopDenCrop,
										 function(x){
										 	if(is.na(x[1]) | x[1] == Inf){out <- NA}
										 	else {out <- order(x)[1]}
										 	return(out)
										 	})
							return(LimFact)})
## Plot the limiting factor
plot(LimFact90[[1]])
image(IceList[[1]],col=c(NA,"grey"),add=T)

## Save the Limiting fctors
#saveRDS(LimFact90,file = "./Data/90Percent/LimFact90.RData")
saveRDS(LimFact90,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/LimFact90.RData")
#-----------------------------------------------------------------------------------
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

## Estimate the Min PopDensity
MinPopPopDen10 <- lapply(names(PaleoPopDenEst10), function(y){
							PopDen <- PaleoPopDenEst10[[y]] # Load pop density estimates
							Ice <- IceList[[y]]==0 # load Ice sheets
							PopDenCrop <- PopDen/Ice
							calc(PopDenCrop,function(x){min(x)})})
## Plot the Min Population density
plot(MinPopPopDen10[[1]])
image(IceList[[1]],col=c(NA,"grey"),add=T)

## Save the Min Population density Output
#saveRDS(MinPopPopDen10,file = "./Data/10Percent/MinPopPopDen10.RData")
saveRDS(MinPopPopDen10,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/MinPopPopDen10.RData")

# Determine the limiting factors
LimFact10 <- lapply(names(PaleoPopDenEst10), function(y){#(y <- "BP.2100")
							PopDen <- PaleoPopDenEst10[[y]] # Load pop density estimates
							Ice <- IceList[[y]]==0 # load Ice sheets
							PopDenCrop <- PopDen/Ice
							LimFact<-calc(PopDenCrop,
										 function(x){
										 	if(is.na(x[1]) | x[1] == Inf){out <- NA}
										 	else {out <- order(x)[1]}
										 	return(out)
										 	})
							return(LimFact)})
## Plot the limiting factor
plot(LimFact10[[1]])
image(IceList[[1]],col=c(NA,"grey"),add=T)

## Save the Limiting fctors
#saveRDS(LimFact10,file = "./Data/10Percent/LimFact10.RData")
saveRDS(LimFact10,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/LimFact10.RData")