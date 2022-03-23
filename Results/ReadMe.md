# PaleoPopDen - RESULTS
Here you will find the *raw* results to the study Changes in limiting factors for forager population dynamics in Europe across the Last Glacial-Interglacial Transition.

The folders **10Percent**, **50Percent*** and **90Percent** include three `.RData` files that have the predicted population density for each evaluated variable `PaleoPopDenEst*.RData`, the minimum populations density predicted for any given grid `MinPopPopDen*.RData`, and the environmental variable that predicted the lowest populations density `LimFact*.RData`. While predicted population density `.RData` files include the values for the full region, minimum populations density and Limiting factors `.RData` files mask-out all the areas where covered by Ice as defined by the ICE-6G gridded data product ^1^.

The **CrossValidation** folder contains the summaries of the cross-validation model assessment as `csv` table 

The **qGAM models** folder contains a series of PDFs showing the relation for three aGAMs models (10-, 50-, and 90-percentile) between Binford's Population density estimates and evaluated environmental variables.

The **PaleoPopDenEst** folder contains a series of maps as PDFs showing the predicted minimum population density from 10- (`PaleoPopDenEst-10.PDF`), 50-(`PaleoPopDenEst-50.PDF`), and 90-percentiles (`PaleoPopDenEst-90.PDF`) gGAMs. 

The **LimFact** folder contains a series of maps as PDFs showing the limiting factor estimated based on gGAMs using 10- (`LimFact-10p_*.PDF`), 50-(`LimFact-50p_*.PDF`), and 90-percentiles (`LimFact-90p_*.PDF`) . 





1. Peltier WR, Argus DF, Drummond R. 2015 Space geodesy constrains ice age terminal deglaciation: the global ICE-6G_C (VM5a) model. J. Geophys. Res. Solid Earth 120, 450–487. (doi:10.1002/ 2014JB011176)
