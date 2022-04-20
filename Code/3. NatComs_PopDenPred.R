####################################################################################
####     Step 3. Predictions under past conditions for each evaluated 			####
####             variable                                                       ####
####################################################################################
rm(list = ls());gc()
require(raster)
require(qgam)
require(MASS)
# Load Environmental Predictors - Each List slot is a Year
EnvVarUseList <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Paleo Predictors/EU-Lorents dowscale/EnvVarUse.RData")
#EnvVarUseList <- readRDS("./Data/Paleo_Predictors/EnvVarUse.RData")

# New names that match model names
EnvVarUse <- c("ET", "PET","NPP",
               "MCM", "MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS"
               )
# Estimate population density using 50-percentile model
# Load Models - Each List slot is a repetition
CrosValMod50 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod50.RData")
# CrosValMod50 <- readRDS("./Models/CrosValMod50.RData")
# CrosValMod50 <- readRDS("https://www.dropbox.com/s/ihvq1r4i2uc9fg7/CrosValMod50.RData?dl=1")

PaleoPopDenEst50 <- lapply(EnvVarUseList, function(x){
	PopDenEst <- lapply(EnvVarUse,
						function(VarUse){#(VarUse <- "TS")
# Define the Raster to use
							RastUse <- x[[which(names(x)%in%VarUse)]]
							names(RastUse) <- "Var"
# Predicting the estimated Density for each model
							ModPred <- lapply(CrosValMod50,function(i){ ModUse <- i[[VarUse]]
																  	predict(RastUse, ModUse)
																	})
# Estimate the mean Density accorss all models
							10^mean(do.call("stack", ModPred))
						})
	PopDenEstStk <- do.call('stack',PopDenEst)
	names(PopDenEstStk) <- EnvVarUse
	return(PopDenEstStk)
})
# Save the Env vars into an R RDS file
saveRDS(PaleoPopDenEst50, file="~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/50Percent/PaleoPopDenEst50.RData")
#saveRDS(PaleoPopDenEst50, file="./Results/PaleoPopDenEst50.RData")
rm(list=c("CrosValMod50", "PaleoPopDenEst50"));gc()


# Estimate population density using 90-percentile model
# Load Models - Each List slot is a repetition
CrosValMod90 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod90.RData")
# CrosValMod90 <- readRDS("./Models/CrosValMod90.RData")
# CrosValMod90 <- readRDS("https://www.dropbox.com/s/ihvq1r4i2uc9fg7/CrosValMod90.RData?dl=1")

PaleoPopDenEst90 <- lapply(EnvVarUseList, function(x){
	PopDenEst <- lapply(EnvVarUse,
						function(VarUse){#(VarUse <- EnvVarUse[1])
# Define the Raster to use
							RastUse <- x[[which(names(x)%in%VarUse)]]
							names(RastUse) <- "Var"
# Predicting the estimated Density for each model
							ModPred <- lapply(CrosValMod90,function(i){ ModUse <- i[[VarUse]]
																  	predict(RastUse, ModUse)
																	})
# Estimate the mean Density accords all models
							10^mean(do.call("stack", ModPred))
						})
	PopDenEstStk <- do.call('stack',PopDenEst)
	names(PopDenEstStk) <- EnvVarUse
	return(PopDenEstStk)
})
# Save the Env vars into an R RDS file
saveRDS(PaleoPopDenEst90, file="~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/90Percent/PaleoPopDenEst90.RData")
#saveRDS(PaleoPopDenEst90, file="./Results/PaleoPopDenEst90.RData")
rm(list = c("CrosValMod90", "PaleoPopDenEst90"));gc()

# Estimate population density using 10-percentile model
# Load Models - Each List slot is a repetition
CrosValMod10 <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod10.RData")
# CrosValMod10 <- readRDS("./Models/CrosValMod10.RData")
# CrosValMod10 <- readRDS("https://www.dropbox.com/s/ihvq1r4i2uc9fg7/CrosValMod10.RData?dl=1")

PaleoPopDenEst10 <- lapply(EnvVarUseList, function(x){
	PopDenEst <- lapply(EnvVarUse,
						function(VarUse){#(VarUse <- EnvVarUse[1])
# Define the Raster to use
							RastUse <- x[[which(names(x)%in%VarUse)]]
							names(RastUse) <- "Var"
# Predicting the estimated Density for each model
							ModPred <- lapply(CrosValMod10,function(i){ ModUse <- i[[VarUse]]
																  	predict(RastUse, ModUse)
																	})
# Estimate the mean Density accorss all models
							10^mean(do.call("stack", ModPred))
						})
	PopDenEstStk <- do.call('stack',PopDenEst)
	names(PopDenEstStk) <- EnvVarUse
	return(PopDenEstStk)
})
# Save the Env vars into an R RDS file
saveRDS(PaleoPopDenEst10, file="~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/10Percent/PaleoPopDenEst10.RData")
#saveRDS(PaleoPopDenEst10, file="./Results/PaleoPopDenEst10.RData")
rm(list = c("CrosValMod10", "PaleoPopDenEst10"));gc()
