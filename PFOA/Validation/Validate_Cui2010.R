library(deSolve)
setwd("/Users/ptsir/Documents/GitHub/PBK_Grouping/PFOA/Validation")

#===============
# Generate predictions
#===============
load("model_params.RData")
# Body weight Kim 2016
BW <- 0.2 #kg
#5mg/kg dose
admin.time <- seq(0.01,27*24.01,24)
dose <- rep(5,28) #mg/kg
admin.dose <- dose * BW #mg
parameters <- create.params( list("BW" = BW  , sex = "M", 
                                  "admin.type" = "oral", "admin.time" = admin.time,
                                  "admin.dose" = admin.dose, "fitted_pars" = fitted_pars, 
                                  "group" = group, "N_pars" = N_pars ))
events <- create.events(parameters)
sample_time <- seq(0,28*24,0.1)
solution_5 <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                       y = inits, parms = parameters, events = events,
                                       method="lsodes",rtol = 1e-7, atol = 1e-7))

pred_comps <- c( "Aurine", "Afeces")
exp_time <- c(1,3,5,7,10,14,18,21,24,28)*24
solution_5 <- solution_5[solution_5$time %in% exp_time,pred_comps]


#20mg/kg dose
dose <- rep(20,28) #mg/kg
admin.dose <- dose * BW #mg
parameters <- create.params( list("BW" = BW  , sex = "M", 
                                  "admin.type" = "oral", "admin.time" = admin.time,
                                  "admin.dose" = admin.dose, "fitted_pars" = fitted_pars, 
                                  "group" = group, "N_pars" = N_pars ))
events <- create.events(parameters)
solution_20 <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                         y = inits, parms = parameters, events = events,
                                         method="lsodes",rtol = 1e-7, atol = 1e-7))

solution_20 <- solution_20[solution_20$time %in% exp_time,pred_comps]




##############################
### Load experimental data ###
##############################

# Load the mean values 
tissues <- openxlsx::read.xlsx("Raw_Data/Cui_2010/Cui_excreta.xlsx")
#Analysis for 5mg/kg
tissues_5_daily <- tissues[tissues$Dose_mg_per_kg == 5,]
approx_times <- 1:28
# Daily excretion based on daily excretion on experimental time points
urine5_approximate <- approxfun(exp_time/24, tissues_5_daily[tissues_5_daily$Excreta  == "Urine","Mass_mg"])
total_daily_urine5 <- urine5_approximate(approx_times)
# Cumulative excretion on experimental days
urine_5_cum <- cumsum(total_daily_urine5)[c(1,3,5,7,10,14,18,21,24,28)]

# Daily excretion based on daily excretion on experimental time points
feces5_approximate <- approxfun(exp_time/24, tissues_5_daily[tissues_5_daily$Excreta  == "Feces","Mass_mg"])
total_daily_feces5 <- feces5_approximate(approx_times)
# Cumulative excretion on experimental days
feces_5_cum <- cumsum(total_daily_feces5)[c(1,3,5,7,10,14,18,21,24,28)]


  
#Analysis for 5mg/kg
tissues_20_daily <- tissues[tissues$Dose_mg_per_kg == 20,]
approx_times <- 1:28
# Daily excretion based on daily excretion on experimental time points
urine20_approximate <- approxfun(exp_time/24, tissues_20_daily[tissues_20_daily$Excreta  == "Urine","Mass_mg"])
total_daily_urine20 <- urine20_approximate(approx_times)
# Cumulative excretion on experimental days
urine_20_cum <- cumsum(total_daily_urine20)[c(1,3,5,7,10,14,18,21,24,28)]

# Daily excretion based on daily excretion on experimental time points
feces20_approximate <- approxfun(exp_time/24, tissues_20_daily[tissues_20_daily$Excreta  == "Feces","Mass_mg"])
total_daily_feces20 <- feces20_approximate(approx_times)
# Cumulative excretion on experimental days
feces_20_cum <- cumsum(total_daily_feces20)[c(1,3,5,7,10,14,18,21,24,28)]

# Gather the required data for the x-y plot

results_df_5<- data.frame("Study" = "Cui_2010", "Dose" =  tissues_5$Dose_mg_per_kg,
                           "Tissue" = tissues_5$Excreta,
                           "Type" = "oral",
                           "Observed" = c(urine_5_cum, feces_5_cum),
                           "Predicted" = c(solution_5$Aurine,solution_5$Afeces))


results_df_20<- data.frame("Study" = "Cui_2010", "Dose" =  tissues_20$Dose_mg_per_kg,
                          "Tissue" = tissues_20$Excreta,
                          "Type" = "oral",
                          "Observed" =c(urine_20_cum, feces_20_cum),
                          "Predicted" = c(solution_20$Aurine,solution_20$Afeces))


results_df <- rbind(results_df_5, results_df_20)

write.csv(results_df,
          "/Users/ptsir/Documents/GitHub/PBK_Grouping/PFOA/Validation/Validation_results/Cui_2010_results.csv",
          row.names =F)
