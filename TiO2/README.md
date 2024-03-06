
## Data
All R scripts are self-contained and need two xlsx files to run, found in the Data folder. The first one, "Rat physiological parameters.xlsx", contains the physiological parameters of the rat, while the second one, "Kreyling-IV-data.xlsx" includes the rat biodistribution data used for calibrating the model parameters, drawn from Kreyling et al. (2017).The 'data' folder also contains .RData files that reproduce the respective models. The 'validation' folder contains data from the studies of Fabian et al. (2008), Garaets et al. (2014) and Disdier et al. (2016), as well as R scripts, and both are used for generating the validation plots. In each script, the user should search for the command `setwd()` and insert the appropriate directory. 

## R packages
Users should downladed the xlsx files and then use the Nevertheless, The R packages that users needs to install are: 
* *openxlsx*, for reading the xlsx files
* *nloptr*, for solving the optimization problem
* *GA*, for running the genetic algorithm 
* *ggplot2* for making the plots, and
* *deSolve*, for solving the ODE system
* 
## Script explanation
Once these packages are installed, all R scripts can be excecuded. The utility of each script is explained below:
* "Metric_selection.R"  --> Contains R code for selecting the most appropriate metric to be used as objective function in the optimization problem. Note that the MANG estimation approach is used and the models developed using the various metrics use the same initialization.
* "FPG_estimation_scheme.R"  --> Contains R code for the FPG grouping scheme.
* "PNG_estimation_scheme.R"  --> Contains R code for the PNG grouping scheme.
* "SPPCG_estimation_scheme.R"  --> Contains R code for the SPPCG grouping scheme.
* "Model_benchmarking.R"  -->  Contains R code for model benchmarking, i.e. produces the evaluation metrics and plots presented in Tsiros et al. (2023). For this script to run, users should also download the three .RData files found in the data file, which contain the solutions of the "FPG_estimation_scheme.R", "PNG_estimation_scheme.R" and "SPPCG_estimation_scheme.R" files.
* "Jaqpot_deploy.R"  -->  Contains R code for deploying the model on the Jaqpot platform, thus exposing it as a ready-so-use web service

## References
- Disdier, C., Devoy, J., Cosnefroy, A., Chalansonnet, M., Herlin-Boime, N., Brun, E., Lund, A. and Mabondzo, A. (2015). Tissue biodistribution of intravenously administrated titanium dioxide nanoparticles revealed blood-brain barrier clearance and brain inflammation in rat. Part. Fibre Toxicol. 12, 27.
- Fabian, E., Landsiedel, R., Ma-Hock, L., Wiench, K., Wohlleben, W. and van Ravenzwaay, B. (2008). Tissue distribution and toxicity of intravenously administered titanium dioxide nanoparticles in rats. Arch. Toxicol. 82, 151–157.
- Geraets, L., Oomen, A.G., Krystek, P., Jacobsen, N.R., Wallin, H., Laurentie, M., Verharen, H.W., Brandon, E.F. and de Jong, W.H. (2014). Tissue distribution and elimination after oral and intravenous administration of different titanium dioxide nanoparticles in rats. Part. Fibre. Toxicol., 11, 30. https://doi.org/10.1186/1743-8977-11-30
- Kreyling, W. G., Holzwarth, U., Haberl, N., Kozempel, J., Hirn, S., Wenk, A., Schleh, C., Schäffler, M., Lipka, J., Semmler-Behnke, M., & Gibson, N. (2017). Quantitative biokinetics of titanium dioxide nanoparticles after intravenous injection in rats: Part 1. Nanotoxicology, 11(4), 434–442. https://doi.org/10.1080/17435390.2017.1306892
- Tsiros, P., Minadakis, V., Li, D. and Sarimveis, H. Parameter Grouping and co-estimation in Physiologically-Based Kinetic models using genetic algorithms. Toxicol Sci. (In review)

