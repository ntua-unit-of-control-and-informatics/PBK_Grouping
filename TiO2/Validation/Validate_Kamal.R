library(deSolve)
setwd("/Users/vassilis/Documents/GitHub/PBK_Grouping/Validation")


#####################################
### Function to create Parameters ###
#####################################
create.params <- function(user_input){
  with( as.list(user_input),{
    ### Important!!! each compartment has a specific index vectors Tissue_fractions, Regional_flow_fractions, Capillary_fractions and cannot be changed
    # The index of each compartment:
    #Rest of Body (rob) --> 1
    #Heart (ht) --> 2
    #Kidneys (ki) --> 3
    #Brain (br) --> 4
    #Spleen (spl) --> 5
    #Lungs (lu) --> 6
    #Liver (li) --> 7
    #Uterus (ut) --> 8
    #Bone (bone) --> 9
    #Adipose (ad) --> 10
    #Skin (skin) --> 11
    #Muscles (mu) --> 12
    #Gastrointestinal track (GIT) --> 13
    
    
    comp_names <- list( "RoB"="RoB","Heart"="Heart", "Kidneys"="Kidneys", "Brain"= NA, "Spleen"="Spleen",
                        "Lungs"="Lungs", "Liver"="Liver", "Uterus"=NA, "Bone"="Bone", "Adipose"=NA, "Skin"=NA,
                        "Muscles"=NA, "GIT"="GIT") #used as input in function, compartments that are used in pbpk
    
    
    # List with names of all possible compartments
    all_comps <- list("RoB"="RoB","Heart"="Heart", "Kidneys"="Kidneys", "Brain"="Brain", "Spleen"="Spleen",
                      "Lungs"="Lungs", "Liver"="Liver", "Uterus"="Uterus", "Bone"="Bone", "Adipose"="Adipose", "Skin"="Skin", "Muscles"="Muscles",
                      "GIT"="GIT") # List with names of all possible compartments
    
    ### Density of tissues/organs
    d_tissue <- 1 #g/ml
    d_skeleton <- 1.92 #g/ml
    d_adipose <- 0.940 #g/ml
    
    Q_total <- (1.54*mass^0.75)*60 # Total Cardiac Output (ml/h)
    
    Total_Blood <- 0.06*mass+0.77 # Total blood volume (ml)
    
    fr_ad <- 0.0199*mass + 1.644 # w in g,  Brown et al.1997 p.420. This equation gives the  adipose % of body weight 
    
    
    # Physiological parameters units
    # V_blood, V_ven, V_art (ml): Volume of total blood, venous blood and arterial blood
    # w_i (g):                    mass of tissue or organ "i"
    # V_tis_i (ml):                volume of tissue or organ "i"
    # V_cap_i (ml):                volume of capillary blood in tissue "i"
    # Q_i, Q_total (ml/h):        regional blood flow of tissue or organ "i"
    
    fractions <- matrix(rep(NA, 4*13), ncol = 4)
    colnames(fractions) <- c("Tissue.weight.fraction.(%.of.BW)", "Regional.flow.fractions.(%.of.total.cardiac.output)",
                             "Capillary.fractions.(fraction.of.tissue.volume)", "Macrophage.fractions.(fraction.of.tissue.volume)" )
    fractions[,1] <- c(NA, 0.33, 0.730, 0.570, 0.200, 0.500, 3.660, 0.011, 10.000, NA, 19.030, 40.000, 2.700)
    fractions[,2] <- c(NA, 4.90, 14.10, 2.00, 1.22, 100.00, 17.40, 1.11, 12.20, 7.00, 5.80, 27.80, 14.00)
    fractions[,3] <- c(0.0400, 0.2600, 0.1600, 0.0300, 0.2200, 0.3600, 0.2100, 0.0770, 0.0400, 0.0055, 0.0200, 0.0143, 0.0500)
    fractions[,4] <- c(0.02, 0.02, 0.02, 0.04, 0.30, 0.04, 0.10, 0.02, 0.04, 0.04, 0.02, 0.02, NA)
    rownames(fractions) <- all_comps
    
    #Tissue weight fraction 
    Tissue_fractions <- fractions[,1]/100 # % of BW. Na values refers to the volume of the rest organs(RoB)
    Tissue_fractions[10] <- fr_ad/100
    #Regional blood flow fraction
    Regional_flow_fractions <- fractions[,2]/100 # % of total cardiac output
    #Capillary volume fractions (fractions of tissue volume)
    Capillary_fractions <- fractions[,3] # of tissue volume
    
    
    W_tis <- rep(0,length(comp_names))
    V_tis <- rep(0,length(comp_names))
    V_cap <- rep(0,length(comp_names))
    W_macro <- rep(0,length(comp_names))  #one more for blood compartment
    Q <- rep(0,length(comp_names))
    
    # The following values were calculated by dividing the %ID/ g tissue with the %ID w/o free 48 from Table 2 of Kreyling et al. (2017)
    # Thus, they represent the average mass, in grams, of the respective tissues in each time group.
    liver_expw <- mean(c(8.57, 8.92, 9.30, 8.61, 9.20))
    spleen_expw <- mean(c(0.93, 0.75, 0.97, 0.68, 0.71))
    kidneys_expw <- mean(c(2.27, 2.36, 2.44, 2.11, 2.26))
    lungs_expw <- mean(c(1.87, 1.60, 1.80, 1.48, 1.31))
    heart_expw <- mean(c(0.89, 1.00, 1.00, 1.00, 0.88))
    blood_expw <- mean(c(16.52, 17.45, 15.33, 18.50, 18.00))
    carcass_expw <- mean(c(206.00, 203.33, 184.00, 202.00, 203.75))
    skeleton_expw <- mean(c(26.15, 27.50, 25.56, 25.79, 25.26))
    soft_tissues <- mean(c(228.57, 253.85, 214.29, 225.93, 231.04))
    
    ### Calculation of tissue weights  
    W_tis[2] <- heart_expw
    W_tis[3] <- kidneys_expw
    W_tis[5] <- spleen_expw
    W_tis[6] <- lungs_expw
    W_tis[7] <- liver_expw
    W_tis[9] <- skeleton_expw
    W_tis[13] <- Tissue_fractions[13]*mass
    
    
    for (i in 1:length(comp_names)) {
      control <- comp_names[i]
      
      Regional_flow_fractions[i] <- ifelse(is.na(control), NA, Regional_flow_fractions[i])
      Capillary_fractions[i] <- ifelse(is.na(control), NA, Capillary_fractions[i])
      
      ###Calculation of tissue volumes
      
      if (i==9){
        V_tis[i] <- W_tis[i]/d_skeleton
      } else if(i==10){
        V_tis[i] <- W_tis[i]/d_adipose
      } else{
        V_tis[i] <- W_tis[i]/d_tissue 
      }
      
      ###Calculation of capillary volumes
      V_cap[i] <- V_tis[i]*Capillary_fractions[i]
      
      ###Calculation of regional blood flows
      Q[i] <- Q_total*Regional_flow_fractions[i]
    }
    
    
    ### Calculations for "Soft tissue" compartment
    W_tis[1] <- mass - sum(W_tis[2:length(W_tis)], na.rm = TRUE)-Total_Blood
    V_tis[1] <- W_tis[1]/d_adipose     
    Q[1] <- Q_total - sum(Q[2:length(Q)],na.rm = TRUE) + Q[6]
    V_cap[1] <- V_tis[1]*Capillary_fractions[1] #Total_Blood - Vven - Vart - sum(V_cap[2:length(V_cap)], na.rm = TRUE)
    
    parameters <- matrix(c(W_tis[],V_tis[],V_cap[],Q[],W_macro[]), ncol = 5)
    colnames(parameters) <- c("W_tis", "V_tis", "V_cap", "Q", "W_macro")
    rownames(parameters) <- all_comps
    
    Vven=0.64*Total_Blood
    Vart=0.15*Total_Blood
    Wm_ven=0.01*Vven
    Wm_art=0.01*Vart
    
    return(list(
      "Q_total"=Q_total, "V_blood"=Total_Blood, "V_ven"=Vven, "V_art"=Vart,
      
      "w_rob"=parameters[1,1], "w_ht"=parameters[2,1], "w_ki"=parameters[3,1], "w_spl"=parameters[5,1], "w_lu"=parameters[6,1], "w_li"=parameters[7,1], "w_bone"=parameters[9,1], "w_git"=parameters[13,1],
      
      "V_tis_rob"=parameters[1,2], "V_tis_ht"=parameters[2,2], "V_tis_ki"=parameters[3,2], "V_tis_spl"=parameters[5,2], "V_tis_lu"=parameters[6,2], "V_tis_li"=parameters[7,2], "V_tis_bone"=parameters[9,2], "V_tis_git"=parameters[13,2], 
      
      "V_cap_rob"=parameters[1,3], "V_cap_ht"=parameters[2,3], "V_cap_ki"=parameters[3,3], "V_cap_spl"=parameters[5,3], "V_cap_lu"=parameters[6,3], "V_cap_li"=parameters[7,3], "V_cap_bone"=parameters[9,3], "V_cap_git"=parameters[13,3],
      
      "Q_rob"=parameters[1,4], "Q_ht"=parameters[2,4], "Q_ki"=parameters[3,4], "Q_spl"=parameters[5,4], "Q_lu"=parameters[6,4], "Q_li"=parameters[7,4], "Q_bone"=parameters[9,4], "Q_git"=parameters[13,4],
      "dose" = dose, "administration" = administration_time
      
    ))
  })
}


#===============================================
#2. Function to create initial values for ODEs 
#===============================================

create.inits <- function(parameters){
  with( as.list(parameters),{
    M_ht<-0; M_lu<-0; M_li<-0; M_spl<-0; M_ki<-0; M_git<-0; M_bone<-0; M_rob<-0;
    
    M_cap_ht<-0; M_cap_lu<-0; M_cap_li<-0; M_cap_spl<-0; M_cap_ki<-0; M_cap_git<-0; M_cap_bone<-0; M_cap_rob<-0;
    
    M_lumen <- 0;
    M_ven<-0; M_art<-0
    M_feces<-0; M_urine<-0 
    
    return(c("M_ht" = M_ht, "M_lu" = M_lu, 
             "M_li" = M_li, "M_spl" = M_spl, 
             "M_ki" = M_ki, "M_git" = M_git, 
             "M_bone" = M_bone,"M_rob"=M_rob,
             
             "M_cap_ht" = M_cap_ht, "M_cap_lu" = M_cap_lu, 
             "M_cap_li" = M_cap_li, "M_cap_spl" = M_cap_spl, 
             "M_cap_ki" = M_cap_ki, "M_cap_git" = M_cap_git, 
             "M_cap_bone" = M_cap_bone,"M_cap_rob"=M_cap_rob,
             
             "M_lumen" = M_lumen,
             "M_ven" = M_ven, "M_art" = M_art, "M_feces" = M_feces, "M_urine" = M_urine))
    
  })
}

#==============
#3.Function for creating events #
#==============
create.events<- function(parameters){
  with( as.list(parameters),{
    
    ldose <- length(dose)
    ltimes <- length(administration)
    
    addition <- dose
    if (ltimes == ldose){
      events <- list(data = rbind(data.frame(var = "M_ven",  time = administration, 
                                             value = addition, method = c("add")) ))
    }else{
      stop("The times when the drug is injected should be equal in number to the doses")
    }
    
    
    return(events)
  }) 
}

###################
# 4.Custom function 
###################

custom.func <- function(){
  return()
}


#==============
#5. ODEs System
#==============
ode.func <- function(time, Initial.values, Parameters, custom.func){
  with( as.list(c(Initial.values, Parameters)),{
    
    position <- c(1,2,3,4,3,1,5,1,6,7,6,8,7,6,9,10)
    fit_pars <-exp(c(-0.133735, 2.122784, 7.684528, 6.6723, 1.761389, -2.18345, 
                     -9.489032, -2.726946, -6.120888, -8.849567, -2.124971, -9.946329)) 
    
    P_ht <- fit_pars[position[1]]
    P_lu <- fit_pars[position[2]]
    P_li <- fit_pars[position[3]]
    P_spl <- fit_pars[position[4]]
    P_ki <- fit_pars[position[5]]
    P_git <- fit_pars[position[6]]
    P_bone <- fit_pars[position[7]]
    P_rob <- fit_pars[position[8]]
    
    x_ht <- fit_pars[position[9]]
    x_lu <- fit_pars[position[10]]
    x_li <- fit_pars[position[11]]
    x_spl <- fit_pars[position[12]]
    x_ki <- fit_pars[position[13]]
    x_git <- fit_pars[position[14]]
    x_bone <- fit_pars[position[15]]
    x_rob <- fit_pars[position[16]]
    
    CLE_f <- fit_pars[length(fit_pars)-1]
    CLE_h <- fit_pars[length(fit_pars)]
    CLE_u <- 0
    
    
    # Concentrations (micro grams of NPs)/(g tissue)
    C_ht <- M_ht/w_ht
    C_cap_ht <- M_cap_ht/V_cap_ht
    C_lu <- M_lu/w_lu
    C_cap_lu <- M_cap_lu/V_cap_lu
    C_li <- M_li/w_li
    C_cap_li <- M_cap_li/V_cap_li
    C_spl <- M_spl/w_spl
    C_cap_spl <- M_cap_spl/V_cap_spl
    C_ki <- M_ki/w_ki
    C_cap_ki <- M_cap_ki/V_cap_ki
    C_git <- M_git/w_git
    C_cap_git <- M_cap_git/V_cap_git
    C_bone <- M_bone/w_bone
    C_cap_bone <- M_cap_bone/V_cap_bone
    C_rob <- M_rob/w_rob
    C_cap_rob <- M_cap_rob/V_cap_rob
    
    C_ven <- M_ven/V_ven
    C_art <- M_art/V_art
    
    # Heart
    dM_cap_ht <- Q_ht*(C_art - C_cap_ht) - x_ht*Q_ht*(C_cap_ht - C_ht/P_ht)
    dM_ht <- x_ht*Q_ht*(C_cap_ht - C_ht/P_ht) 
    
    # Lungs
    dM_cap_lu <- Q_total*(C_ven - C_cap_lu) - x_lu*Q_total*(C_cap_lu - C_lu/P_lu)
    dM_lu <-  x_lu*Q_total*(C_cap_lu - C_lu/P_lu)
    
    # Liver 
    dM_cap_li <- Q_li*(C_art - C_cap_li) + Q_spl*(C_cap_spl - C_cap_li) + Q_git*(C_cap_git - C_cap_li) -
      x_li*(Q_li+Q_git+Q_spl)*(C_cap_li - C_li/P_li)
    dM_li <- x_li*(Q_li+Q_git+Q_spl)*(C_cap_li - C_li/P_li) - CLE_h*M_li
    
    # Spleen
    dM_cap_spl <- Q_spl*(C_art - C_cap_spl) - x_spl*Q_spl*(C_cap_spl - C_spl/P_spl)
    dM_spl <- x_spl*Q_spl*(C_cap_spl - C_spl/P_spl) 
    
    # Kidneys
    dM_cap_ki <- Q_ki*(C_art - C_cap_ki) - x_ki*Q_ki*(C_cap_ki - C_ki/P_ki)- CLE_u*M_cap_ki
    dM_ki <- x_ki*Q_ki*(C_cap_ki - C_ki/P_ki) 
    
    # GIT - Gastrointestinal Track
    dM_cap_git <- Q_git*(C_art - C_cap_git) - x_git*Q_git*(C_cap_git - C_git/P_git)
    dM_git <- x_git*Q_git*(C_cap_git - C_git/P_git) 
    dM_lumen <- CLE_h*M_li - CLE_f *M_lumen 
    
    # Bone
    dM_cap_bone <- Q_bone*(C_art - C_cap_bone) - x_bone*Q_bone*(C_cap_bone - C_bone/P_bone)
    dM_bone <- x_bone*Q_bone*(C_cap_bone - C_bone/P_bone) 
    
    
    # RoB - Rest of Body
    dM_cap_rob <- Q_rob*(C_art - C_cap_rob) - x_rob*Q_rob*(C_cap_rob - C_rob/P_rob)
    dM_rob <- x_rob*Q_rob*(C_cap_rob - C_rob/P_rob) 
    
    # Urine
    dM_urine <- CLE_u*M_cap_ki
    
    # Feces
    dM_feces <- CLE_f*M_lumen
    
    # Venous Blood
    dM_ven <- Q_ht*C_cap_ht + (Q_li + Q_spl+Q_git)*C_cap_li + Q_ki*C_cap_ki +
      Q_bone*C_cap_bone + Q_rob*C_cap_rob - Q_total*C_ven
    
    # Arterial Blood
    dM_art <-  Q_total*C_cap_lu - Q_total*C_art
    
    Blood_total <- M_ven + M_art + M_cap_ht + M_cap_lu +M_cap_li+M_cap_spl+
      M_cap_ki+ M_cap_git+M_cap_bone+M_cap_rob
    Blood <- Blood_total/(V_blood)
    
    C_git_total <- (M_git+M_lumen)/w_git
    
    list(c("dM_ht" = dM_ht, "dM_lu" = dM_lu, 
           "dM_li" = dM_li, "dM_spl" = dM_spl, 
           "dM_ki" = dM_ki, "dM_git" = dM_git, 
           "dM_bone" = dM_bone,"dM_rob"=dM_rob,
           
           "dM_cap_ht" = dM_cap_ht, "dM_cap_lu" = dM_cap_lu, 
           "dM_cap_li" = dM_cap_li, "dM_cap_spl" = dM_cap_spl, 
           "dM_cap_ki" = dM_cap_ki, "dM_cap_git" = dM_cap_git, 
           "dM_cap_bone" = dM_cap_bone,"dM_cap_rob"=dM_cap_rob,
           
           "dM_lumen" = dM_lumen,
           "dM_ven" = dM_ven, "dM_art" = dM_art, "dM_feces" = dM_feces, "dM_urine" = dM_urine),
         
         "Heart" = M_ht+M_cap_ht, "Lungs" = M_lu + M_cap_lu, "Liver" = M_li + M_cap_li,
         "Spleen" = M_spl + M_cap_spl, "Kidneys" = M_ki + M_cap_ki,
         "Git" = M_git+M_lumen + M_cap_git, "Bone" = M_bone + M_cap_bone,
         "RoB" = M_rob + M_cap_rob, "Blood" = M_ven + M_art,
         
         
         "C_Blood"=Blood,
         "C_Heart"=C_ht, "C_Lungs"=C_lu, "C_Liver"=C_li, "C_Spleen"=C_spl,
         "C_Kidneys"=C_ki, "C_Git"=C_git_total, "C_Bone"=C_bone, "C_RoB"=C_rob,
         "Feces"=M_feces, "Urine"=M_urine)
  })
}
##############################
### Load experimental data ###
##############################
# Concentrations are reported in ppm = 1ug/g
# Dose = 5 mg/kg
# Single dose data refer to single IV and measured concentrations 48 h post IV
single_dose_data <- read.csv("/Users/vassilis/Documents/GitHub/PBK_Grouping/Validation/Data/Kamal_data/single_dose_data.csv")
names(single_dose_data)[3:4] <- c("Time", "Concentration") 
single_dose_data$Time <- 48
# Repeated dose data refer to six times IV (IV interval = 1 week) and measured concentrations at the beginning of 7th week (42 days after 1st IV)
repeated_dose_data <- read.csv("/Users/vassilis/Documents/GitHub/PBK_Grouping/Validation/Data/Kamal_data/repeated_dose_data.csv")
names(repeated_dose_data)[3:4] <- c("Time", "Concentration")
repeated_dose_data$Time <- 42*24

Body_mass <- 250 # g  (200 - 300 g)
Dose_per_rat <- 5 # ug/g

# Organise data by NM type 

A_data <- data.frame(matrix(NA, ncol = 4, nrow = 2))
colnames(A_data) <- c("Time", "Liver", "Spleen", "Heart")
A_data$Time <- c(24, 1008) 

B_data <- A_data
C_data <- A_data

data_list <- list("A_data"=A_data, "B_data"=B_data, "C_data"=C_data)

Mr_Ti <- 47.867
Mr_TiO2 <- 47.867 + 32

for (i in 1:length(data_list)) {
  if(i==1){
    nm_type <- "A"
  }else if(i==2){
    nm_type <- "B"
  }else{nm_type <- "C"}
  
  sub_single <- single_dose_data[which(single_dose_data$Type == nm_type),"Concentration"]
  sub_repeated <- repeated_dose_data[which(repeated_dose_data$Type == nm_type),"Concentration"]
  for (j in 2:dim(data_list[[i]])[2]) { #loop over columns (compartments)
    data_list[[i]][,j]  <- c(sub_single[j-1], sub_repeated[j-1])
  }
  # Transform the Ti mass to TiO2 mass
  data_list[[i]][,-1] <- data_list[[i]][,-1]*Mr_TiO2/Mr_Ti
}


####################
### User's INPUT ###
####################
mass <- Body_mass # g  
dose <- Dose_per_rat*Body_mass # ug TiO2
administration_time = c(0,1,2,3,4,5)*7*24
user_input <-list("dose" = rep(dose, length(administration_time)), "mass" = mass, "administration_time" = administration_time)

params<-create.params(user_input)
inits <- create.inits(params)
events <- create.events(params)


sample_time <-seq(0, 1010, 0.5)
solution <- data.frame(ode(times = sample_time,  func = ode.func, y = inits, parms = params, 
                           custom.func = custom.func, method="lsodes",  events = events))

predictions <- solution[,c('time', 'C_Liver', 'C_Spleen', 'C_Heart')]
names(predictions)[2:4] <- c("Liver", "Spleen", "Heart")

library(ggplot2)

# Defining the linetype and colour of each curve
cls <-  c("Predictions" = "#000000", "Observations" = "#E69F00")
shapes <- c("A" = 16, "B" = 17, "C" = 18)  # You can choose different shapes for each layer

create.plots <- function(data_list, predictions, compartment, y_max =NULL){  
  
  # if(is.null(y_max)){
  #   y_max = max(c(max(observations[compartment]),  max(predictions[compartment])))
  # }
  
  test_plot = ggplot()+
    geom_line(data=predictions, aes(x= time/24, y= !!as.name(compartment), color = "Predictions"), size=1.5,alpha = 0.7) +
    geom_point(data=data_list[[1]], aes(x=Time/24, y= !!as.name(compartment), color='Observations', shape = 'A'), size=4)+
    geom_point(data=data_list[[2]], aes(x=Time/24, y= !!as.name(compartment), color='Observations', shape = 'B'), size=4)+
    geom_point(data=data_list[[3]], aes(x=Time/24, y= !!as.name(compartment), color='Observations', shape = 'C'), size=4)+
    #scale_y_continuous(trans='log10')+
    #ylim(0, 1.1*y_max)+
    labs(title = compartment, 
         y = "TiO2 (ug TiO2/g tissue)",
         x = "Time (d)")+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_color_manual("Data", values=cls)+
    scale_shape_manual("Marker", values = shapes) +
    theme(legend.key.size = unit(1.5, 'cm'),  
          legend.title = element_text(size=14),
          legend.text = element_text(size=14),
          axis.text = element_text(size = 14))
  
  print(test_plot)
  
}

plot_compartment <- c("Liver", "Spleen", "Heart")

p1 <-  create.plots(data_list, predictions, plot_compartment[1])
p2 <-  create.plots(data_list, predictions, plot_compartment[2])
p3 <-  create.plots(data_list, predictions, plot_compartment[3])

ggpubr::ggarrange(p1, p2, p3, 
                  ncol=3, nrow=1, 
                  common.legend = TRUE, legend="right")
