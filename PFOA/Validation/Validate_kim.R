library(deSolve)
setwd("/Users/ptsir/Documents/GitHub/PBK_Grouping/PFOA/Validation")

#===============
# Generate predictions
#===============
load("model_params.RData")
# Body weight Kim 2016
BW <- 0.25#kg, from Kim et al. 2018
#iv dose
dose <- 1 #mg/kg
admin.dose <- dose * BW #mg
parameters <- create.params( list("BW" = BW  , sex = "M", 
                                  "admin.type" = "iv", "admin.time" = 0.01,
                                  "admin.dose" = admin.dose, "fitted_pars" = fitted_pars, 
                                  "group" = group, "N_pars" = N_pars ))
events <- create.events(parameters)
sample_time <- seq(0,12*24,0.1)
solution_iv <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                        y = inits, parms = parameters, events = events,
                                        method="lsodes",rtol = 1e-7, atol = 1e-7))

pred_comps <- c( "Cheart", "Clung", "Ckidneys", "Cliver",  "Cspleen")
solution_iv <- solution_iv[solution_iv$time ==12*24,pred_comps]


#oral dose
parameters <- create.params( list("BW" = BW  , sex = "M", 
                                  "admin.type" = "oral", "admin.time" = 0.01,
                                  "admin.dose" = admin.dose, "fitted_pars" = fitted_pars, 
                                  "group" = group, "N_pars" = N_pars ))
events <- create.events(parameters)
sample_time <- seq(0,12*24,0.1)
solution_oral <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                       y = inits, parms = parameters, events = events,
                                       method="lsodes",rtol = 1e-7, atol = 1e-7))

pred_comps <- c( "Cheart", "Clung", "Ckidneys", "Cliver",  "Cspleen")
solution_oral <- solution_oral[solution_oral$time ==12*24,pred_comps]



##############################
### Load experimental data ###
##############################

# Load the mean values 
tissues <- openxlsx::read.xlsx("Raw_Data/Kim_2016/PFOA_male-tissues_Kim_2016.xlsx")
tissues_iv <- tissues[1:5,]
tissues_iv$Concentration_nanog_per_g <- tissues_iv$Concentration_nanog_per_g/1000 #convert ng/g to mg/L
tissues_oral <- tissues[6:10,]
tissues_oral$Concentration_nanog_per_g <- tissues_oral$Concentration_nanog_per_g/1000 #convert ng/g to mg/L

# Gather the required data for the x-y plot

results_df_iv<- data.frame("Study" = "Kim", "Dose" =  tissues_iv$Dose_mg_per_kg,
                             "Tissue" = tissues_iv$Tissue,
                           "Type" = "iv",
                             "Observed" = tissues_iv$Concentration_nanog_per_g,
                             "Predicted" = unname(t(solution_iv)))


results_df_oral<- data.frame("Study" = "Kim", "Dose" =  tissues_oral$Dose_mg_per_kg,
                           "Tissue" = tissues_oral$Tissue,
                           "Type" = "oral",
                           "Observed" = tissues_oral$Concentration_nanog_per_g,
                           "Predicted" = unname(t(solution_oral)))



results_df <- rbind(results_df_iv, results_df_oral)

write.csv(results_df,
          "/Users/ptsir/Documents/GitHub/PBK_Grouping/PFOA/Validation/Validation_results/Kim_results.csv",
          row.names =F)
