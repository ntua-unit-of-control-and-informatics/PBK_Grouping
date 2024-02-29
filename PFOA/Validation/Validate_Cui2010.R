library(deSolve)
library(ggplot2)
setwd("/Users/ptsir/Documents/GitHub/PBK_Grouping/PFOA/Validation")

#===============
# Generate predictions
#===============
load("fitted_model.RData")
# Body weight 
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
solution_5_all <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                       y = inits, parms = parameters, events = events,
                                       method="lsodes",rtol = 1e-7, atol = 1e-7))

pred_comps <- c( "Aurine", "Afeces")
exp_time <- c(1,3,5,7,10,14,18,21,24,28)*24
solution_5 <- solution_5_all[solution_5_all$time %in% exp_time,pred_comps]


#20mg/kg dose
dose <- rep(20,28) #mg/kg
admin.dose <- dose * BW #mg
parameters <- create.params( list("BW" = BW  , sex = "M", 
                                  "admin.type" = "oral", "admin.time" = admin.time,
                                  "admin.dose" = admin.dose, "fitted_pars" = fitted_pars, 
                                  "group" = group, "N_pars" = N_pars ))
events <- create.events(parameters)
solution_20_all <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                         y = inits, parms = parameters, events = events,
                                         method="lsodes",rtol = 1e-7, atol = 1e-7))

solution_20 <- solution_20_all[solution_20_all$time %in% exp_time,pred_comps]

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
tissues_5 <- tissues_5_daily
tissues_5$Mass_mg <- c(urine_5_cum,feces_5_cum)
tissues_5$Time_day <- tissues_5$Time_day*24

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
tissues_20 <- tissues_20_daily
tissues_20$Mass_mg <- c(urine_20_cum,feces_20_cum)
tissues_20$Time_day <- tissues_20$Time_day*24

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


# Defining the linetype and colour of each curve
cls <-  c("urine prediction (5mg/kg)" = "#56B4E9", "feces prediction (5mg/kg)" = "#CC79A7",
          "urine prediction (20mg/kg)" = "#009E73", "feces prediction (20mg/kg)" ="#D55E00",
          "urine observation (5mg/kg)" = "#56B4E9", "feces observation (5mg/kg)" = "#CC79A7",
          "urine observation (20mg/kg)" = "#009E73", "feces observation (20mg/kg)" ="#D55E00") 
        

final_plot  <- ggplot(data = solution_5_all)+
    geom_line( aes(x= time, y= Aurine,color = 'urine prediction (5mg/kg)'),  size=1.5,alpha = 0.7) +
    geom_line( aes(x= time, y= Afeces,color = 'feces prediction (5mg/kg)'),  size=1.5,alpha = 0.7) +
    geom_line( data = solution_20_all, aes(x= time, y= Aurine,
                                                  color = 'urine prediction (20mg/kg)'),  size=1.5,alpha = 0.7) +
    geom_line( data = solution_20_all, aes(x= time, y= Afeces,
                                                  color = 'feces prediction (20mg/kg)'),  size=1.5,alpha = 0.7) +
    
      geom_point(data=tissues_5[tissues_5$Excreta == "Urine",], 
               aes(x= Time_day, y= Mass_mg ,color = 'urine observation (5mg/kg)'), size=4)+
    
    geom_point(data=tissues_5[tissues_5$Excreta == "Feces",], 
               aes(x= Time_day, y= Mass_mg ,color = 'feces observation (5mg/kg)'), size=4)+
    
    geom_point(data=tissues_20[tissues_20$Excreta == "Urine",], 
               aes(x= Time_day, y= Mass_mg ,color = 'urine observation (20mg/kg)'), size=4)+
    geom_point(data=tissues_20[tissues_20$Excreta == "Feces",], 
               aes(x= Time_day, y= Mass_mg ,color = 'feces observation (20mg/kg)'), size=4)+
    
    labs( y = "PFOA (mg)" ,x = "Time (hours)")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme_light() + 
    scale_color_manual("Excreta", values=cls,
                       guide = guide_legend(override.aes =
                                              list(shape = c(16,16,  NA, NA, 16, 16,  NA, NA),
                                                   linetype = c(0, 0, 1,1, 0,0, 1,1))))+
    theme(  legend.justification = "top" ,
            legend.key.size = unit(1.5, 'cm'),  
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.margin=grid::unit(c(0.25,0.25,0.25,0.25), "cm"))
  
  
# Save the plot with dynamically adjusted dimensions
ggsave("excreta_plot.png", plot = final_plot,
       device = 'png', dpi = 300,
       width = 11,
       height = 7,
       units = "in")





