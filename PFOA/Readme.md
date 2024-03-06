
## Data
All R scripts are self-contained and draw necessary data from the 'Data' folder. The latter contains data from the studies of Kudo et al. (2007) and Dzierlenga et al. (2020) used for recalibrating model parameters. The 'data' folder also contains .RData files that reproduce the model. The 'validation' folder contains data from the studies of Kudo et al. (2007), Cui et al. (2009) and Kim et al. (2016)., as well as R scripts, and both are used for generating the validation plots. In each script, the user should search for the command `setwd()` and insert the appropriate directory. 

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
* "goodness-of-fit-plot.R"  --> Contains R code for generating the goodness-of-fit plots
* "PFOA_GA_male_rat.R"  --> Contains R code for the GA grouping approach.
* "Jaqpot_deploy_PFOA.R"  -->  Contains R code for deploying the model on the Jaqpot platform, thus exposing it as a ready-so-use web service

## References
- Cui, L., Zhou, Q. F., Liao, C. Y., Fu, J. J., and Jiang, G. B. (2009). Studies on the toxicological effects of PFOA and PFOS on rats using histological observation and chemical analysis. Arch. Environ. Contam. Toxicol. 56, 338–349.
- Dzierlenga, A. L., Robinson, V. G., Waidyanatha, S., DeVito, M. J., Eifrid, M. A., Gibbs, S. T., Granville, C. A., and Blystone, C. R. (2020). Toxicokinetics of perfluorohexanoic acid (PFHxA), perfluorooctanoic acid (PFOA) and perfluorodecanoic acid (PFDA) in male and female Hsd:Sprague dawley SD rats following intravenous or gavage administration. Xenobiotica. 50, 722–732.
- Kudo, N., Sakai, A., Mitsumoto, A., Hibino, Y., Tsuda, T., & Kawashima, Y. (2007). Tissue distribution and hepatic subcellular distribution of perfluorooctanoic acid at low dose are different from those at high dose in rats. Biol. Pharm. Bull. 30, 1535–1540.
- Kim, S. J., Heo, S. H., Lee, D. S., Hwang, I. G., Lee, Y. B., and Cho, H. Y. (2016). Gender differences in pharmacokinetics and tissue distribution of 3 perfluoroalkyl and polyfluoroalkyl substances in rats. Food. Chem. Toxicol. 97, 243–255.
- Tsiros, P., Minadakis, V., Li, D. and Sarimveis, H. Parameter Grouping and co-estimation in Physiologically-Based Kinetic models using genetic algorithms. Toxicol Sci. (In review)

