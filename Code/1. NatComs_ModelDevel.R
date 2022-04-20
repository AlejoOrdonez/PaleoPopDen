rm(list = ls());gc()
# Load the Environmental data
#Binford_EnvDta_WorldClim <-read.csv("https://raw.githubusercontent.com/AlejoOrdonez/PaleoPopDen/main/Data/BindfordData/BinfordNewEnvDta_WorldClim.csv")
#Binford_EnvDta_WorldClim <-read.csv("https://www.dropbox.com/s/llg5ym8lw6e2hlt/BinfordNewEnvDta_WorldClim.csv?dl=1")
Binford_EnvDta_WorldClim <-read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Contemp Predictors/BinfordNewEnvDta_WorldClim.csv")

#calculating distances among the sampled points 
Binford_SpceData <- fields::rdist.earth(Binford_EnvDta_WorldClim[, c("LONG", "LAT")], miles = FALSE)
# Defining the Distance for the h-block design - the one where the difference in density between sites is maximum
popDist <- as.matrix(dist(log10(Binford_EnvDta_WorldClim$density)))
GeoD <- as.dist(Binford_SpceData)
DenD <- as.dist(popDist)
plot(x= GeoD,
     y= DenD)
a <- loess(DenD ~ GeoD)
plot(y=predict(a, newdata = data.frame(GeoD = seq(0,20000,length.out=1000))),
     x= seq(0,20000,length.out=1000),col="red",lwd=2,type="b")
abline(v=4500)

#####################################################################################
####             Step 1a. Model the changes in Population density                ####
#####################################################################################
rm(list=ls());gc()
require(qgam)
require(MASS)
# Load the Environmental data
#Binford_EnvDta_WorldClim <-read.csv("https://raw.githubusercontent.com/AlejoOrdonez/PaleoPopDen/main/Data/BindfordData/BinfordNewEnvDta_WorldClim.csv")
#Binford_EnvDta_WorldClim <-read.csv("https://www.dropbox.com/s/llg5ym8lw6e2hlt/BinfordNewEnvDta_WorldClim.csv?dl=1")
Binford_EnvDta_WorldClim <-read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Contemp Predictors/BinfordNewEnvDta_WorldClim.csv")

#calculating distances among the sampled points 
Binford_SpceData <- fields::rdist.earth(Binford_EnvDta_WorldClim[, c("LONG", "LAT")], miles = FALSE)
names(Binford_EnvDta_WorldClim)
# Define the variables names to use 
EnvVarUse <- c("ET", "PET","NPP",
               "MCM", "MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS")

# Generate 10/50/90-percentile qGAM models for population density using a 100-Fold H-block cross validation procedure.
# Off code I ran a subset with 1000 iteration named CorssValMod_Sml.RData the ones used in the Paper
CrosValMod <- lapply(1:1000,
                    function(x,
                    		 EnvVar = EnvVarUse,
                    		 SpceData2 = Binford_SpceData){
# Do the H-block selection
# Cropping the distance to the maximum
                    SpceData2[SpceData2[]<=4500] <- NA
                    rownames(SpceData2) <- 1:dim(SpceData2)[1]
                    colnames(SpceData2) <- 1:dim(SpceData2)[2]
                    for(i in 1:round((dim(Binford_SpceData)[1]*0.7)/2)){ # the number of samples is 70% of the original database [divided by 2 as on each iteration two numberes are selected]
                      a <- sample(rownames(SpceData2),1)
                      b <- sample(names(SpceData2[a,])[!is.na(SpceData2[a,])],1)
                      SpceData2 <- SpceData2[-which(rownames(SpceData2)%in%c(a,b)),-which(rownames(SpceData2)%in%c(a,b))]
                      if(i==1){H.BloqSmpl <- c(a,b)}
                      else{H.BloqSmpl <- c(H.BloqSmpl,a,b)}
                    }

# Generate a (q)GAM model to describe the changes in population density as a function of selected environmental variables
                    GAM.Out <- lapply(EnvVar, function(Var.Use){#(Var.Use <- "NPP")
# Select the density-environmental var combination for those location defined using a H-block design
                                      BinfordNewtmp <- Binford_EnvDta_WorldClim[as.numeric(H.BloqSmpl),c("density", Var.Use)]
                                      names(BinfordNewtmp)[2]<-"Var" # Rename the Env Variable to Var - makes modelling easy 
# Build a (q)GAM models to describe the change in density as a function of the predictor
## 50-percentile
                                      fit50 <- gam(log10(density)~s(Var),
                                                   data = BinfordNewtmp)
## 90-percentile
                                      fit90 <- qgam(log10(density)~s(Var),
                                                    data = BinfordNewtmp,
                                                    qu = 0.90)
## 10-percentile
                                      fit10 <- qgam(log10(density)~s(Var),
                                                    data = BinfordNewtmp,
                                                    qu = 0.10)
# Final compilation
                                      list(sample = H.BloqSmpl,
                                           fit10 = fit10,
                                           fit90 = fit90,
                                           fit50 = fit50
										   )
                                  })
                      names(GAM.Out) <- EnvVar
# Summary of GAM models for the H-block sample 
                   return(GAM.Out)
                })
saveRDS(CrosValMod, file="~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CorssValMod.RData")
#saveRDS(CrosValMod, file="~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CorssValMod_Sml.RData")

#####################################################################################
####             Step 1b. Summary of modes per quantile                 ####
#####################################################################################

# New names that match model names
EnvVarUse <- c("ET", "PET","NPP",
               "MCM", "MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS")
# Load Models - Each List slot is a repetition
#CrosValMod <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CorssValMod.RData")
#CrosValMod <- readRDS("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CorssValMod_Sml.RData")

CrosValMod10 <- lapply(CrosValMod,
                       function(x){
                         b<-lapply(EnvVarUse,
                                   function(y){
                                     x[[y]][["fit10"]]
                                   })
                         names(b) <- EnvVarUse
                         b[["sample"]] = x[[1]]$sample
                         return(b)
                       })
saveRDS(CrosValMod10, file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod10.RData")
#saveRDS(CrosValMod10, file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod10_Sml.RData")

CrosValMod50 <- lapply(CrosValMod,
                       function(x){
                         b<-lapply(EnvVarUse,
                                   function(y){
                                     x[[y]][["fit50"]]
                                   })
                         names(b) <- EnvVarUse
                         b[["sample"]] = x[[1]]$sample
                         return(b)
                       })
saveRDS(CrosValMod50, file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod50.RData")
#saveRDS(CrosValMod50, file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod50_Sml.RData")

CrosValMod90 <- lapply(CrosValMod,
                       function(x){
                         b<-lapply(EnvVarUse,
                                   function(y){
                                     x[[y]][["fit90"]]
                                   })
                         names(b) <- EnvVarUse
                         b[["sample"]] = x[[1]]$sample
                         return(b)
                       })
saveRDS(CrosValMod90, file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod90.RData")
#saveRDS(CrosValMod90, file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod90_Sml.RData")

####################################################################################
####          Step 1c. Assess the cross validated prediction performance        ####
####################################################################################
rm(list = ls());gc()
# Load the Environmental data
#Binford_EnvDta_WorldClim <-read.csv("https://raw.githubusercontent.com/AlejoOrdonez/PaleoPopDen/main/Data/BindfordData/BinfordNewEnvDta_WorldClim.csv")
Binford_EnvDta_WorldClim <-read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Contemp Predictors/BinfordNewEnvDta_WorldClim.csv")
# Define the variables names to use 
EnvVarUse <- c("ET", "PET","NPP",
               "MCM", "MWM",
               "TS",
               "Log10.TAP",
               "Log10.PDM", "Log10.PWM",
               "PS")
# See the variation in estimates based on the validation sample for the 10-percentile
CrosValMod10 <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod10.RData")
#CrosValMod10 <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod10_Sml.RData")
CorssValPrfmce10 <- lapply(EnvVarUse,
                           function(Var.Use){#(Var.Use <- "ET")
                             # make model estimates for the test section of the Main Data frame
                             ## 10-percentile
                             CrossFoldSumm10 <- lapply(CrosValMod10,function(x){
                               # Extract the Training sample
                               Var.predict <- Binford_EnvDta_WorldClim[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample),
                                                                       Var.Use]
                               # Extract the models 
                               Model.fit10 <- x[[Var.Use]]                
                               # Predict  GAM model
                               Predict.fit10 <- predict(Model.fit10,
                                                        newdata = list(Var = Var.predict),
                                                        se.fit = F)
                               # Estimate the mean squared error MSE =  sum((y_k-yhat)^2)/nk (where k is the sample)
                               MSE = mean(c(10^Predict.fit10 - Binford_EnvDta_WorldClim$density[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample)])^2)
                               # Summaries
                               data.frame(NullDev = abs(Model.fit10 $ deviance - Model.fit10 $ null.deviance)/Model.fit10 $ null.deviance,
                                          MSE = MSE,
                                          Nk = length(Var.predict),
                                          R_Sqr = cor(10^Predict.fit10,
                                                      Binford_EnvDta_WorldClim$density[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample)]))
                             })
                             # Summary of the estimates
                             data.frame(NullDev = mean(sapply(CrossFoldSumm10, function(x){x$NullDev})), # Estimate the Null deviance
                                        CV = sum(sapply(CrossFoldSumm10, function(x){x$MSE*(x$Nk/nrow(Binford_EnvDta_WorldClim))})), # Estimate the Cross cross-validation error
                                        rho = mean(sapply(CrossFoldSumm10, function(x){x$R_Sqr}))) # Estimate the average R2
                           })

# See the variation in estimates based on the validation sample for the 50-percentile
CrosValMod50 <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod50.RData")
#CrosValMod50 <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod50_Sml.RData")
CorssValPrfmce50 <- lapply(EnvVarUse,
                           function(Var.Use){#(Var.Use <- "ET")
                             # make model estimates for the test section of the Main Data frame
                             ## 50-percentile
                             CrossFoldSumm50 <- lapply(CrosValMod50,function(x){
                               # Extract the Training sample
                               Var.predict <- Binford_EnvDta_WorldClim[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample),
                                                                       Var.Use]
                               # Extract the models 
                               Model.fit50 <- x[[Var.Use]]                
                               # Predict  GAM model
                               Predict.fit50 <- predict(Model.fit50,
                                                        newdata = list(Var = Var.predict),
                                                        se.fit = F)
                               # Estimate the mean squared error MSE =  sum((y_k-yhat)^2)/nk (where k is the sample)
                               MSE = mean(c(10^Predict.fit50 - Binford_EnvDta_WorldClim$density[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample)])^2)
                               # Summaries
                               data.frame(NullDev = abs(Model.fit50 $ deviance - Model.fit50 $ null.deviance)/Model.fit50 $ null.deviance,
                                          MSE = MSE,
                                          Nk = length(Var.predict),
                                          R_Sqr = cor(10^Predict.fit50,
                                                      Binford_EnvDta_WorldClim$density[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample)]))
                             })
                             # Summary of the estimates
                             data.frame(NullDev = mean(sapply(CrossFoldSumm50, function(x){x$NullDev})), # Estimate the Null deviance
                                        CV = sum(sapply(CrossFoldSumm50, function(x){x$MSE*(x$Nk/nrow(Binford_EnvDta_WorldClim))})), # Estimate the Cross cross-validation error
                                        rho = mean(sapply(CrossFoldSumm50, function(x){x$R_Sqr}))) # Estimate the average R2
                           })
# See the variation in estimates based on the validation sample for the 90-percentile
CrosValMod90 <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod90.RData")
#CrosValMod90 <- readRDS(file = "~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Models/CrosValMod90_Sml.RData")
CorssValPrfmce90 <- lapply(EnvVarUse,
                           function(Var.Use){#(Var.Use <- "ET")
                             # make model estimates for the test section of the Main Data frame
                             ## 90-percentile
                             CrossFoldSumm90 <- lapply(CrosValMod90,function(x){
                               # Extract the Training sample
                               Var.predict <- Binford_EnvDta_WorldClim[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample),
                                                                       Var.Use]
                               # Extract the models 
                               Model.fit90 <- x[[Var.Use]]                
                               # Predict  GAM model
                               Predict.fit90 <- predict(Model.fit90,
                                                        newdata = list(Var = Var.predict),
                                                        se.fit = F)
                               # Estimate the mean squared error MSE =  sum((y_k-yhat)^2)/nk (where k is the sample)
                               MSE = mean(c(10^Predict.fit90 - Binford_EnvDta_WorldClim$density[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample)])^2)
                               # Summaries
                               data.frame(NullDev = abs(Model.fit90 $ deviance - Model.fit90 $ null.deviance)/Model.fit90 $ null.deviance,
                                          MSE = MSE,
                                          Nk = length(Var.predict),
                                          R_Sqr = cor(10^Predict.fit90,
                                                      Binford_EnvDta_WorldClim$density[!c(1:dim(Binford_EnvDta_WorldClim)[1])%in%as.numeric(x[[Var.Use]]$sample)]))
                             })
                             # Summary of the estimates
                             data.frame(NullDev = mean(sapply(CrossFoldSumm90, function(x){x$NullDev})), # Estimate the Null deviance
                                        CV = sum(sapply(CrossFoldSumm90, function(x){x$MSE*(x$Nk/nrow(Binford_EnvDta_WorldClim))})), # Estimate the Cross cross-validation error
                                        rho = mean(sapply(CrossFoldSumm90, function(x){x$R_Sqr}))) # Estimate the average R2
                           })

# Final estimate of Model accuracy - based on the test dataset
CorssValPrfmceSumm <- rbind(
## Summary 90-Percentie
    data.frame(EnvVar=EnvVarUse,
               Percentile = "90-Perc",
               do.call("rbind",CorssValPrfmce90)),
## Summary 50-Percentie
    data.frame(EnvVar=EnvVarUse,
               Percentile = "50-Perc",
               do.call("rbind",CorssValPrfmce50)),
## Summary 10-Percentie
    data.frame(EnvVar=EnvVarUse,
               Percentile = "10-Perc",
               do.call("rbind",CorssValPrfmce10)))
write.csv(CorssValPrfmceSumm,"~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/CrossValidation/CorssValPrfmceSumm") 

####################################################################################
####     Step 1d. Illustrate the modelled changes in Population density         ####
####              Using all the cross validated models					            		####
####################################################################################

CorssValSummPlt <- lapply(EnvVarUse,
                          function(Var.Use){#(Var.Use <- "ET")
# generate a predictor matrix
                          Var.predict <- seq(min(Binford_EnvDta_WorldClim[, Var.Use]),
                                             max(Binford_EnvDta_WorldClim[, Var.Use]),
                                             length.out = 100)
# make model estimates for the range of environmental values in the Main Data frame
## 50-percentile
                          CrossFoldSumm50 <- sapply(CrosValMod50,function(x){
# Extract the models 
                            Model.fit50 <- x[[Var.Use]]
# Predict and plot GAM model
                            Predict.fit50 <- predict(Model.fit50,
                                                     newdata = list(Var = Var.predict),
                                                     se.fit = F)
                            Predict.fit50
                          
                            })
# ## 10-percentile
                            CrossFoldSumm10 <- sapply(CrosValMod10,function(x){
# Extract the models 
                              Model.fit10 <- x[[Var.Use]]
# Predict and plot GAM model
                              Predict.fit10 <- predict(Model.fit10,
                                                       newdata = list(Var = Var.predict),
                                                       se.fit = F)
                            })
## 90-percentile
                              CrossFoldSumm90 <- sapply(CrosValMod90,function(x){
# Extract the models 
                                Model.fit90 <- x[[Var.Use]]
# Predict and plot GAM model
                                Predict.fit90 <- predict(Model.fit90,
                                                         newdata = list(Var = Var.predict),
                                                         se.fit = F)
                                Predict.fit90
                              })
# Summary of the estimates
                              list(Mean = data.frame(fit10 = apply(CrossFoldSumm10,1,mean),
                                                     fit50 = apply(CrossFoldSumm50,1,mean),
                                                     fit90 = apply(CrossFoldSumm90,1,mean)),
                                   SD = data.frame(fit10 = apply(CrossFoldSumm10,1,sd),
                                                   fit50 = apply(CrossFoldSumm50,1,sd),
                                                   fit90 = apply(CrossFoldSumm90,1,sd)))
                          })
                              
names(CorssValSummPlt) <- EnvVarUse

for (Var.Use in EnvVarUse){#(Var.Use<-"ET")
  # Select the density-environmental var combination for those location defined using a H-block design
  BinfordNewtmp <- Binford_EnvDta_WorldClim[,c("density", Var.Use)]
  names(BinfordNewtmp)[2]<-"Var" # Rename the Env Variable to Var - makes modelling easy 
  
# Plot the relation of between a predictor a Population data
	pdf(paste0("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Results/qGAM models/ModelPred_",Var.Use,".pdf"))
	plot(log10(density)~ Var,data = BinfordNewtmp,
	     pch = 19,
	     xlab = Var.Use,
	     main =  Var.Use, cex.main = 2,
	     axes = F,ylim = c(-1,3),
	     ylab = "density [#people/100km2] - log scaled")
	box()
	axis(1)
	axis(2,
	     at = -1:3,
	     labels = 10^c(-1:3),
	     las = 2)

# plot 50-percentile predictions	
  Var.predict <- seq(min(BinfordNewtmp$Var),
	                   max(BinfordNewtmp$Var),
	                   length.out = 100)
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit50,
	      col = "red",lwd = 2,lty=1)
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit50+(2*CorssValSummPlt[[Var.Use]]$SD$fit50),
	      col = "red", lty = 2)
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit50-(2*CorssValSummPlt[[Var.Use]]$SD$fit50),
	      col = "red", lty = 2)

# plot 90-percentile predictions	
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit90,
	      col = "blue",lwd = 2,lty=1)
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit90+(2*CorssValSummPlt[[Var.Use]]$SD$fit90),
	      col = "blue", lty = 2)
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit90-(2*CorssValSummPlt[[Var.Use]]$SD$fit90),
	      col = "blue", lty = 2)
	
# plot 10-percentile predictions	
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit10,
	      col = "purple",lwd = 2,lty=1)
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit10+(2*CorssValSummPlt[[Var.Use]]$SD$fit10),
	      col = "purple", lty = 2)
	lines(x = Var.predict,
	      y = CorssValSummPlt[[Var.Use]]$Mean$fit10-(2*CorssValSummPlt[[Var.Use]]$SD$fit10),
	      col = "purple", lty = 2)
  legend("bottomright",lty=1,col=c("purple","red","blue"),
         legend = c("10-Percentile", "50-Percentile", "90-Percentile"))
	dev.off()
	}

####################################################################################
####     Step 1e. Illustrate the modelled changes in Population density         ####
####              Using all the data											####
####################################################################################
rm(list = ls());gc()
require(qgam)
# Load the Environmental data
#Binford_EnvDta_WorldClim <-read.csv("https://raw.githubusercontent.com/AlejoOrdonez/PaleoPopDen/main/Data/BindfordData/BinfordNewEnvDta_WorldClim.csv")
Binford_EnvDta_WorldClim <-read.csv("~/Dropbox/Aarhus Assistant Professor/Projects/4. PopulationDensity-LGMtoNow/Data/Contemp Predictors/BinfordNewEnvDta_WorldClim.csv")


# Define the variables names to use 
EnvVarUse <- c("ET", "PET", #"MAT",
                "MCM", "MWM",
                "TS",
                #"MAM.Tavg", "JJA.Tavg", "SON.Tavg", "DJF.Tavg",
                "Log10.TAP",
                "Log10.PDM", "Log10.PWM",
                "PS"#,
                #"Log10.MAM.Prec", "Log10.JJA.Prec", "Log10.SON.Prec", "Log10.DJF.Prec",
                #"Log10.TRI"
               )

for (Var.Use in EnvVarUse){#(Var.Use <- "MAM.Tavg")
	BinfordNewtmp <- Binford_EnvDta_WorldClim[,c("density", Var.Use)]
	names(BinfordNewtmp)[2]<-"Var"

## Build a quantile-GAM model to describe the change in density as a function of the predictor
### 90-percentile
	fit90 <- qgam(log10(density)~s(Var),
					 data = BinfordNewtmp,
					 qu = 0.90)
### 50-percentile
	fit50 <- gam(log10(density)~s(Var),
				 data = BinfordNewtmp)
### 10-percentile
	fit10 <- qgam(log10(density)~s(Var),
					 data = BinfordNewtmp,
					 qu = 0.10)

# PLot the relation of between a predictor a Population data
	dev.new()
	plot(log10(density)~ Var,data = BinfordNewtmp,
		 pch = 19,
		 xlab = Var.Use,
		 main =  Var.Use, cex.main = 2,
		 axes = F,ylim = c(-1,3),
		 ylab = "density [#people/100km2] - log scaled")
	box()
	axis(1)
	axis(2,
		 at = -1:3,
		 labels = 10^c(-1:3),
		 las = 2)
	Var.predict <- seq(min(BinfordNewtmp$Var),
					max(BinfordNewtmp$Var),
					length.out = 100)
# Predict and plot GAM model
# MEan model
	fit50.Predict <- predict(fit50,
							 newdata = list(Var = Var.predict),
							 				se.fit = T) 
	lines(x = Var.predict,
		  y = fit50.Predict$fit,
		  col = "red",lwd = 2)
	lines(x = Var.predict,
		  y = fit50.Predict$fit+(qnorm(0.975) * fit50.Predict$se.fit),
		  col = "red",lwd = 1,lty=2)
	lines(x = Var.predict,
		  y = fit50.Predict$fit-(qnorm(0.975) * fit50.Predict$se.fit),
		  col = "red",lwd = 1,lty=2)
# Max model		 	
	fit90.Predict <- predict(fit90,
							 newdata = list(Var = Var.predict),
								 			se.fit = T) 
	lines(x = Var.predict,
		  y = fit90.Predict$fit,
		  col = "blue",lwd = 2)
	lines(x = Var.predict,
		  y = fit90.Predict$fit+(qnorm(0.975) * fit90.Predict$se.fit),
		  col = "blue",lwd = 1,lty=2)
	lines(x = Var.predict,
		  y = fit90.Predict$fit-(qnorm(0.975) * fit90.Predict$se.fit),
		  col = "blue",lwd = 1,lty=2)

#Min model
	fit10.Predict <- predict(fit10,
							 newdata = list(Var = Var.predict),
							 				se.fit = T) 
	lines(x = Var.predict,
		  y = fit10.Predict$fit,
		  col = "purple",lwd = 2)
	lines(x = Var.predict,
		  y = fit10.Predict$fit+(qnorm(0.975) * fit10.Predict$se.fit),
		  col = "purple",lwd = 1,lty=2)
	lines(x = Var.predict,
		  y = fit10.Predict$fit-(qnorm(0.975) * fit10.Predict$se.fit),
		  col = "purple",lwd = 1,lty=2)
}