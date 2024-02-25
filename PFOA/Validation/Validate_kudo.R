library(deSolve)
setwd("/Users/user/Documents/GitHub/PBK_Grouping/PFOA/Validation")

#===============
# Generate predictions
#===============
load("fitted_model.RData")
# Body weight Kudo 2007
BW <- 0.29 #kg
#low dose
dose <- 0.041 #mg/kg
admin.dose <- dose * BW #mg
parameters <- create.params( list("BW" = BW  , sex = "M", 
                    "admin.type" = "iv", "admin.time" = 0.01,
                "admin.dose" = admin.dose, "fitted_pars" = fitted_pars, 
                "group" = group, "N_pars" = N_pars ))
events <- create.events(parameters)
sample_time <- seq(0,2,0.01)
solution_low <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                      y = inits, parms = parameters, events = events,
                      method="lsodes",rtol = 1e-7, atol = 1e-7))

pred_comps <- c( "Cplasma_ven", "Cliver", "Ckidneys","Cintestine", 
            "Ctestis", "Cspleen", "Cheart", "Clung", "Cbrain","Cstomach",  "Ccarcass")
solution_low <- solution_low[solution_low$time ==2,pred_comps]


#High dose
#low dose
dose <- 16.56 #mg/kg
admin.dose <- dose * BW #mg
parameters <- create.params( list("BW" = BW  , sex = "M", 
                                  "admin.type" = "iv", "admin.time" = 0.01,
                                  "admin.dose" = admin.dose, "fitted_pars" = fitted_pars, 
                                  "group" = group, "N_pars" = N_pars ))
events <- create.events(parameters)
solution_high <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                        y = inits, parms = parameters, events = events,
                                        method="lsodes",rtol = 1e-7, atol = 1e-7))

solution_high <- solution_high[solution_high$time ==2,pred_comps]
##############################
### Load experimental data ###
##############################

# Load the mean values 
tissues_low <- openxlsx::read.xlsx("Raw_Data/Kudo_2007/IV_male_rats_tissues_low_kudo_2007.xlsx")
tissues_high <-  openxlsx::read.xlsx("Raw_Data/Kudo_2007/IV_male_rats_tissues_high_kudo_2007.xlsx")

tissues_names <-  c("Serum" , "Liver", "Kidney", "Intestine", "Testis", "Spleen",
                    "Heart",  "Lung", "Brain","Stomach", "Carcass")

tissues_low <- tissues_low[tissues_low$Tissue %in% tissues_names,]
tissues_high <- tissues_high[tissues_high$Tissue %in% tissues_names,]

# Gather the required data for the x-y plot

results_df_low <- data.frame("Study" = "Kudo", "Dose" =  tissues_low$Dose_mg_per_kg,
                             "Tissue" = tissues_low$Tissue,
                             "Type" = "iv",
                             "Observed" = tissues_low$`Concentration_microg_per_g_organ)`,
                             "Predicted" = unname(t(solution_low)))

results_df_high <- data.frame("Study" = "Kudo", "Dose" =  tissues_high$Dose_mg_per_kg,
                              "Tissue" = tissues_low$Tissue,
                              "Type" = "iv",
                             "Observed" = tissues_high$`Concentration_microg_per_g_organ)`,
                             "Predicted" = unname(t(solution_high)))



results_df <- rbind(results_df_low, results_df_high)

write.csv(results_df,
          "/Users/user/Documents/GitHub/PBK_Grouping/PFOA/Validation/Validation_results/Kudo_results.csv",
          row.names =F)
