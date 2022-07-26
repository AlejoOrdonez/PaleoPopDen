###################################################################################
####     Step 2a. Generate the RasterStacks of enviornmnetal predictions        ####
####             between 21kaBP and 8kaBP at 500yrs intervals                  ####
###################################################################################
rm(list = ls());gc()
require(raster)
# Define the variables names to use to load the rasteres 
EnvVarUse <- c("ET", "PET","NPP",
               "MCM", "MWM",
               "TS",
               "TAP",
               "PDM", "PWM",
               "PS"
               )

# Which of these rasters need to be log transformed
LogTransVars <- c("TAP","PDM", "PWM","MAM.Prec", "JJA.Prec", "SON.Prec", "DJF.Prec")
# Define the location of paleoclimate rasters
setwd("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/CCSM/Derived")
EnvVarUseList <- list(NA)
for(Yr.Use in seq(-2100,-800,by=50)){#(Yr.Use <- -2100) # 500 yrs increments 
# Get a summary of the env variable for a given period
# Set a progress bar
	pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
	                     max = length(EnvVarUse), # Maximum value of the progress bar
	                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
	                     width = 50,   # Progress bar width. Defaults to getOption("width")
	                     char = "=")   # Character used to create the bar
# These are 100 year averages centered on the year of reference
	EnvVarRast <- lapply(EnvVarUse, function(x){#(x<-"PWM")
# Sets the progress bar to the current state
					 setTxtProgressBar(pb, which(EnvVarUse%in%x)) 
# Define the time period to average (50 yr before and 50Yr after the central time period)
					 TimRng <- which(c(-2200:3)%in%c(Yr.Use-5)):which(c(-2200:3)%in%c(Yr.Use+5))
# Generate a raster for the selected period 
					 RastOut <- lapply(TimRng,
					  					function(y){
# Load the Raster
					  					  out <- raster(paste0(x,".tif"), band = y)
#					  					  out <- raster(paste0("https://www.dropbox.com/s/wg6rps7fsi7df8x/",x,".tif?dl=1"), band = y)
# for selected varaibles, log transform
					  								if(x%in%LogTransVars){out <- log10(out+1)}
					  								return(out)
					  								})
					  return(mean(do.call("stack", RastOut)))
					  })
# Make the map the Env  Raster Stack
	EnvVarRast <- do.call("stack", EnvVarRast)
	names(EnvVarRast) <- c("ET", "PET","NPP", "MCM", "MWM", "TS", "Log10.TAP", "Log10.PDM", "Log10.PWM", "PS")
# make a list
	if(which(seq(-2100,-800,by=50)%in% Yr.Use)==1){EnvVarUseList <- list(EnvVarRast)}
	else{EnvVarUseList[[which(seq(-2100,-800,by=50)%in% Yr.Use)]] <- EnvVarRast}
	rm(EnvVarRast);gc()
}

names(EnvVarUseList) <- paste0("BP.",abs(seq(-2100,-800,by=50)))
# Save the Env vars into an R RDS file
saveRDS(EnvVarUseList, file="~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/EnvVarUse.RData")
####################################################################################
####     Step 2b. Load, crop and reporject Ice Layers from ICE-6G-C				####
####################################################################################
rm(list = ls());gc()
require(raster)

# Load, crop and reporject Ice Layers from ICE-6G-C
IceList <- lapply(paste0("I6_C.VM5a_10min.",as.character(rev(seq(8,21,by =0.5))),".nc"),
                  function(x){Ice <- raster(paste0("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/",x), # Load Ice Layers from ICE-6G-C
                                            varname = "sftgif")
                  Ice <- rotate(Ice) # Reprojecting raster from 0 360 to -180 180
                  IceCrop <- crop(Ice,extent(c(-12,38,34,74))) # crop to Europe extent
                  IceCropResample <- resample(IceCrop,
                                              readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/EnvVarUse.RData")[[1]],
                                              method="ngb")
                  return(IceCropResample)
                  })
names(IceList) <- paste0("BP.",as.character(rev(seq(800,2100,by =50))))
saveRDS(IceList, file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/IceMaps/ICE-6G-C/Ice0_5DD.RData")	
	
	
	
	
	
	
	
	
	
	
	
	
	