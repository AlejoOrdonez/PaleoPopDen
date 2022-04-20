rm(list=ls());gc()
require(raster)
require(mgcv)


# Define the Population size
#Load the estimate of Pop density for the 50-percentile model
PopDenFinalRast50Pect <- readRDS('~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/MinPopPopDen50.RData')
#Define the Population size for the 50-percentile model
PopSize50Pect <- lapply(PopDenFinalRast50Pect,
                           function(x){
                             #x[x<0.2] <- NA
                             out <- x*25
                             sum(values(out),na.rm=T)})

Out <- do.call("c",PopSize50Pect)
names(Out) <- paste0("BP.",seq(-21,-8,by=0.500))
Out
# Define the Population size
#Load the estimate of Pop density for the 90-percentile model
PopDenFinalRast90Pect <- readRDS('~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/MinPopPopDen90.RData')
#Define the Population size for the 90-percentile model
PopSize90Pect <- lapply(PopDenFinalRast90Pect,
                        function(x){
                          #x[x<0.2] <- NA
                          out <- x*25
                          sum(values(out),na.rm=T)})

Out <- do.call("c",PopSize90Pect)
names(Out) <- paste0("BP.",seq(-21,-8,by=0.500))
Out
# Define the Population size
#Load the estimate of Pop density for the 90-percentile model
PopDenFinalRast10Pect <- readRDS('~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/MinPopPopDen10.RData')
#Define the Population size for the 90-percentile model
PopSize10Pect <- lapply(PopDenFinalRast10Pect,
                        function(x){
                          #x[x<0.2] <- NA
                          out <- x*25
                          sum(values(out),na.rm=T)})
Out <- do.call("c",PopSize10Pect)
names(Out) <- paste0("BP.",seq(-21,-8,by=0.500))
Out

# Define the Area Occupied
AreaOccFinalList <- lapply(PopDenFinalRast50Pect,
						function(x){
              out <- na.omit(values(x))
							sum(c(out>0))/4705})
AreaOcc <- data.frame(Size = do.call("c", AreaOccFinalList),
					  Time = seq(-21,-8,by=0.500))
fit50 <- gam(Size ~ s(Time),
			 data = AreaOcc)
#### Mean occupied area during the LGM termination
round(mean(predict(fit50,newdata=list(Time = seq(-22,-20,0.5))))*100)
#### Mean occupied area during the Bølling–Allerød
round(mean(predict(fit50,newdata=list(Time = seq(-14.69,-12.89,0.5))))*100)
#### Mean occupied area during the Younger Dryas
round(mean(predict(fit50,newdata=list(Time = seq(-12.9,-11.7,0.5))))*100)
#### Mean occupied area during the Holocene
round(mean(predict(fit50,newdata=list(Time = seq(-11.65,-8,0.5))))*100)


# Define the mean population density
MeanPopSize <- lapply(PopDenFinalRast50Pect,
                           function(x){
                             out <- na.omit(values(x))
                             mean(out)})
MeanPopSize <- data.frame(Size = do.call("c", MeanPopSize),
                      Time = seq(-21,-8,by=0.500))
fit50 <- gam(Size ~ s(Time),
             data = MeanPopSize)
#### Mean pop density during the LGM termination
mean(predict(fit50,newdata=list(Time = seq(-22,-20,0.5))))
#### Mean pop density during the Bølling–Allerød
mean(predict(fit50,newdata=list(Time = seq(-14.69,-12.89,0.5))))
#### Mean pop density during the Younger Dryas
mean(predict(fit50,newdata=list(Time = seq(-12.9,-11.7,0.5))))
#### Mean pop density during the Holocene
mean(predict(fit50,newdata=list(Time = seq(-11.65,-8,0.5))))