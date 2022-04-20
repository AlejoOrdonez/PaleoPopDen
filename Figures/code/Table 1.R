####################################################################################
####          Step 1c. Assess the cross validated prediction performance        ####
####################################################################################
rm(list = ls());gc()
require(qgam)
require(MASS)
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
                                        NullDevSD = sd(sapply(CrossFoldSumm50, function(x){x$NullDev})), # Estimate the Null deviance SD
                                        CV = sum(sapply(CrossFoldSumm50, function(x){x$MSE*(x$Nk/nrow(Binford_EnvDta_WorldClim))})), # Estimate the Cross cross-validation error
                                        rho = mean(sapply(CrossFoldSumm50, function(x){x$R_Sqr})), # Estimate the average R2
                                        rhoSD = sd(sapply(CrossFoldSumm50, function(x){x$R_Sqr}))) # Estimate the average R2
                           })
Table1 <- data.frame(EnvVar=EnvVarUse,
           NullDev = paste0(round(do.call("rbind",CorssValPrfmce50)[,"NullDev"],3)," [",
                            round(do.call("rbind",CorssValPrfmce50)[,"NullDev"]-(2*do.call("rbind",CorssValPrfmce50)[,"NullDevSD"]),3)," ",
                            round(do.call("rbind",CorssValPrfmce50)[,"NullDev"]+(2*do.call("rbind",CorssValPrfmce50)[,"NullDevSD"]),3),"]"),
           rho = paste0(round(do.call("rbind",CorssValPrfmce50)[,"rho"],3)," [",
                        round(do.call("rbind",CorssValPrfmce50)[,"rho"]-(2*do.call("rbind",CorssValPrfmce50)[,"rhoSD"]),3)," ",
                        round(do.call("rbind",CorssValPrfmce50)[,"rho"]+(2*do.call("rbind",CorssValPrfmce50)[,"rhoSD"]),3),"]")
           )
write.csv(Table1,"/Users/alejandroordonez/Desktop/Figures/Table1.csv")