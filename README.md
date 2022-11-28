# PBPK_Grouping
This repository contains the code used for producing the results publicated in "Title". All R scripts are self-contained and need two xlsx files to run. The first one, "Rat physiological parameters.xlsx", contains the physiological parameters of the rat, while the second one, "Kreyling-IV-data.xlsx" includes the rat biodistribution data used for calibrating the model parameters, drawn from Kreyling et al. (2017). Users should downladed the xlsx files and then use the Nevertheless, there are some R packages that the user needs to install, these are: 
* "openxlsx", for reading the xlsx files
* "nloptr", for solving the optimization problem
* "GA", for running the genetic algorithm 
* "ggplot2" for making the plots, and
* "deSolve", for solving the ODE system
Once these packages are installed, all R scripts can be excecuded. The utility of each script is explained below:

References
- Tsiros et al
- Kreyling, W. G., Holzwarth, U., Haberl, N., Kozempel, J., Hirn, S., Wenk, A., Schleh, C., Schäffler, M., Lipka, J., Semmler-Behnke, M., & Gibson, N. (2017). Quantitative biokinetics of titanium dioxide nanoparticles after intravenous injection in rats: Part 1. Nanotoxicology, 11(4), 434–442. https://doi.org/10.1080/17435390.2017.1306892
