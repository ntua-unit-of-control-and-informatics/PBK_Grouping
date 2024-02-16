setwd("C:/Users/user/Documents/GitHub/PBK_Grouping/PFOA")

  
  library(deSolve)
  library(ggplot2)
  
  #=========================
  #1. Parameters of the model
  #=========================
  create.params  <- function(user_input){
    with( as.list(user_input),{
      # Initialise a vector for correction factors
      CF <- rep(NA, length(group))
      # Create the parameter values, with the first value being 1, i.e. no correction needed
      parameter_values <- c(1, exp(par[1]), exp(par[2]), exp(par[3]))
      #Pass grouping information from GA to the correction factors
      for (i in 1:length(group)){
        CF[i] <- parameter_values[group[i]]
      }
      # User input: BW(kg), sex(F/M)
      #Physiological Parameters
      MKC = 0.0073	#fraction mass of kidney (percent of BW); Brown 1997
      MLC = 0.0366	#fraction mass of liver (percent of BW); Brown 1997
      
      #Cardiac Output and Bloodflow (as fraction of cardiac output)
      QCC = 14.0 #cardiac output in L/h/kg^0.75; Brown 1997		 
      QL_hepatic_arteryC = 0.021 #fraction blood flow to liver; Brown 1997	
      QKC = 0.141 #fraction blood flow to kidney; Brown 1997.
      QgonadsC = 0.500/53  	#fraction blood flow to gonads; from doi:10.1124/dmd.107.015644
      QintestineC = 0.451/2.58	#fraction blood flow to intestine; from doi:10.1007/bf02353860
      QspleenC = 0.63/74	#fraction blood flow to spleen; davies 1993
      QheartC = 0.051	#fraction blood flow to heart; Brown 1997
      QlungC = 	1#fraction blood flow to lung; Brown 1997
      QbrainC = 0.02	#fraction blood flow to brain; Brown 1997
      QstomachC = 0.068 /2.58	#fraction blood flow to stomach; from doi:10.1007/bf02353860
      Htc = 0.46 #hematocrit for the rat; Davies 1993
      
      #Tissue Volumes 
      VplasC = 0.0312 #fraction vol. of plasma (L/kg BW); Davies 1993
      VLC = 0.0366 #fraction vol. of liver (L/kg BW); Brown 1997
      VkidneyC = 0.0073 #fraction vol. of kidney (L/kg BW); Brown 1997
      VgonadsC = 2.50/250	#fraction vol. of gonads (L/kg BW); from doi:10.1124/dmd.107.015644
      VintestineC = 0.0140+0.0084	#fraction vol. of tointestine (L/kg BW); Brown 1997
      VspleenC = 0.0020 #fraction vol. of spleen (L/kg BW); Brown 1997
      VheartC = 0.0033	#fraction vol. of heart (L/kg BW); Brown 1997
      VlungC = 	0.0050#fraction vol. of lung (L/kg BW); Brown 1997
      VbrainC = 0.0057	#fraction vol. of brain (L/kg BW); Brown 1997
      VstomachC = 0.0046	#fraction vol. of stomach (L/kg BW); Brown 1997
      VfilC = 8.4e-4	#fraction vol. of filtrate (L/kg BW)
      VPTCC = 1.35e-4 #vol. of proximal tubule cells (L/g kidney) (60 million PTC cells/gram kidney, 1 PTC = 2250 um3)
      
      #Chemical Specific Parameters
      MW = 414.07	#PFOA molecular mass (g/mol)
      
      #Kidney Transport Parameters
      Vmax_baso_invitro = 393.45 #Vmax of basolateral transporter (pmol/mg protein/min); averaged in vitro value of rOAT1 and rOAT3 from Nakagawa, 2007
      Km_baso = 27.2 #Km of basolateral transporter (mg/L) Average of rOAT1 and rOAT3 from Nakagawa et. al, 2007
      Vmax_apical_invitro = 9300 #Vmax of apical transporter (pmol/mg protein/min); invitro value for Oatp1a1 from Weaver, 2010
      Km_apical = 52.3#Km of apical transporter (mg/L), in vitro value for Oatp1a1 from Weaver, 2010.
      protein = 2.0e-6	#amount of protein in proximal tubule cells (mg protein/proximal tubule cell)
      
      #Partition Coefficients (from rat tissue data, Kudo et al, 2007)
      Pliver = 2.2 #liver:blood
      Pkidney = 1.05 #kidney:blood
      Prest = 0.030/0.254 #rest of body:blood
      Pintestine = 0.017/0.254 
      Pgonads = 0.018/0.254 
      Pspleen = 0.015/0.254 
      Pheart = 0.029/0.254 
      Plung = 0.039/0.254
      Pbrain = 0.003/0.254
      Pstomach = 0.009/0.254 
      
      #rate constants
      kdif = 0.001	#diffusion rate from proximal tubule cells (L/h)
      kabsc = 2.12	#rate of absorption of chemical from small intestine to liver (1/(h*BW^0.25))(fit to data)
      kunabsc = 7.06e-5	#rate of unabsorbed dose to appear in feces (1/(h*BW^0.25))(fit to data)
      GEC = 0.54 #gastric emptying time (1/(h*BW^0.25)); from Yang, 2013
      k0C = 1.0	#rate of uptake from the stomach into the liver (1/(h*BW^0.25)) (fit to data)
      
      keffluxc = 2.49 #rate of clearance of PFOA from proximal tubule cells into blood (1/(h*BW^0.25))
      kbilec = 0.004 #biliary elimination rate ((male); liver to feces storage (1/(h*BW^0.25)) (fit to data)
      kurinec = 1.6 #rate of urine elimination from urine storage (male) (1/(h*BW^0.25))(fit to data)
      Free = 0.09#0.006 #free fraction in plasma (male)
      
      if(sex == "M"){
        GFRC = 62.1	#glomerular filtration rate (L/hr/kg kidney) (male); Corley, 2005
        RAFbaso = 4.07	#relative activity factor, basolateral transporters (male) (fit to data)	
        RAFapi = 35	#relative activity factor, apical transporters (male) (fit to data)	
      }else if(sex == "F"){
        GFRC = 41.04	#glomerular filtration rate (L/hr/kg kidney) (female); Corley, 2005
        RAFapi = 0.001356	#relative activity factor, apical transporters (female) (fit to data)
        RAFbaso = 0.01356	#relative activity factor, basolateral transporters (female) (fit to data)
      }
      
      
      #Scaled Parameters
      #Cardiac output and blood flows
      QC <- QCC*(BW^0.75)*(1-Htc)	#cardiac output in L/h; adjusted for plasma
      Qlung <- QC
      QK <- (QKC*QC)	#plasma flow to kidney (L/h)
      QL_hepatic_artery <- (QL_hepatic_arteryC*QC)	#plasma flow to liver (L/h)
      Qspleen <- (QspleenC*QC)	#plasma flow to spleen (L/h)
      Qgonads <- (QgonadsC*QC)	#plasma flow to gonads (L/h)
      Qintestine <- (QintestineC*QC)	#plasma flow to intestine (L/h)
      Qstomach <- (QstomachC*QC)	#plasma flow to stomach (L/h)
      Qheart <- (QheartC*QC)	#plasma flow to heart (L/h)
      Qbrain <- (QbrainC*QC)	#plasma flow to liver (L/h)
      Qrest <- QC - QK - QL_hepatic_artery - Qspleen - Qgonads - Qintestine	- Qstomach -
        Qheart - Qbrain#plasma flow to rest of body (L/h)
      QBal <- QC - (QK + QL_hepatic_artery + Qspleen + Qgonads + Qintestine	+ Qstomach +
                      Qheart + Qbrain+Qrest) #Balance check of blood flows; should equal zero
      
      #Tissue Volumes
      MK = MKC*BW*1000	#mass of the kidney (g)
      PTC = MKC*6e7	#number of PTC (cells/kg BW) (based on 60 million PTC/gram kidney)
      VPTC = MK*VPTCC	#volume of proximal tubule cells (L)	
      VPlas = VplasC*BW 	#volume of plasma (L) 
      V_venous_blood <- BW*11.3/250 	#volume of venous plasma (L); from doi:10.1007/bf02353860
      V_arterial_blood <- BW*5.6/250	#volume of arterial plasma (L); from doi:10.1007/bf02353860
      V_blood <-   V_venous_blood + V_arterial_blood
      Vven_plasma = VPlas*V_venous_blood/V_blood	#volume of venous plasma (L)
      Vart_plasma = VPlas*V_arterial_blood/V_blood  	#volume of arterial plasma (L)
      Vgonads = VgonadsC*BW 	#volume of gonads (L)
      Vspleen = VspleenC*BW 	#volume of spleen (L)
      Vheart = VheartC*BW 	#volume of heart (L)
      Vstomach = VstomachC*BW 	#volume of stomach (L)
      Vintestine = VintestineC*BW 	#volume of intestine (L)
      Vlung = VlungC*BW 	#volume of lung (L)
      Vbrain = VbrainC*BW 	#volume of brain (L)
      Vkidney = VkidneyC*BW 	#volume of kidney (L)
      
      Vkidneyb = Vkidney*0.16	#volume of blood in the kidney (L); fraction blood volume of kidney (0.16) from Brown, 1997
      Vfil = VfilC*BW	#volume of filtrate (L)
      VL = VLC*BW	#volume of liver (L)
      VR = BW - V_venous_blood -V_arterial_blood -Vfil  - VL - Vkidney - Vbrain - Vlung-
        Vintestine - Vstomach - Vheart - Vspleen -  Vgonads #volume of remaining tissue (L); 
      VBal = BW - (VR + VL + VPTC + Vfil + VPlas)	#Balance check of tissue volumes; should equal zero 
      ML = MLC*BW*1000	#mass of the liver (g)
      
      #Kidney Parameters
      MK = MKC*BW*1000	#mass of the kidney (g)
      PTC = MKC*6e7	#number of PTC (cells/kg BW) (based on 60 million PTC/gram kidney)
      VPTC = MK*VPTCC	#volume of proximal tubule cells (L)	
      MPTC = VPTC*1000 #mass of the proximal tubule cells (g) (assuming density 1 kg/L)	
      Vmax_basoC = (Vmax_baso_invitro*RAFbaso*PTC*protein*60*(MW/1e12)*1000)#Vmax of basolateral transporters (mg/h/kg BW)
      Vmax_apicalC = (Vmax_apical_invitro*RAFapi*PTC*protein*60*(MW/1e12)*1000) #Vmax of basolateral transporters (mg/h/kg BW)
      Vmax_baso = Vmax_basoC*BW^0.75	#(mg/h)
      Vmax_apical = Vmax_apicalC*BW^0.75	#(mg/h)
      kbile = kbilec*BW^(-0.25)	#biliary elimination; liver to feces storage (/h)
      kurine = kurinec*BW^(-0.25)	#urinary elimination, from filtrate (/h)
      kefflux = keffluxc*BW^(-0.25)	#efflux clearance rate, from PTC to blood (/h)
      GFR = GFRC*(MK/1000)	#glomerular filtration rate, scaled to mass of kidney(in kg)(L/h)
      
      #GI Tract Parameters
      kabs = kabsc*BW^(-0.25)	#rate of absorption of chemical from small intestine to liver (/h)
      kunabs = kunabsc*BW^(-0.25)	#rate of unabsorbed dose to appear in feces (/h)
      GE = GEC/BW^0.25	#gastric emptying time (/h)
      k0 = k0C/BW^0.25 	#rate of uptake from the stomach into the liver (/h)
      
      return(list( "QC" = QC, "QK" = QK, "QL_hepatic_artery" = QL_hepatic_artery, "Qrest" = Qrest, 
                   "Qgonads" = Qgonads, "Qstomach" = Qstomach, "Qintestine" = Qintestine,
                   "Qbrain" = Qbrain, "Qlung" = Qlung,  "Qheart" = Qheart,
                   "Qspleen" = Qspleen,
                   
                   "VPlas" = VPlas, "Vart_plasma" = Vart_plasma, "Vven_plasma" = Vven_plasma,
                   "Vkidneyb" = Vkidneyb, "Vfil" = Vfil, "VL" = VL, "VR" = VR, 
                   "Vgonads" = Vgonads, "Vstomach" = Vstomach, "Vintestine" = Vintestine,
                   "Vbrain" = Vbrain, "Vlung" = Vlung,  "Vheart" = Vheart,
                   "Vspleen" = Vspleen, "Vkidney" = Vkidney,
                   
                   "GFR" = GFR, "VPTC" = VPTC,"Km_baso" = Km_baso, "Km_apical" = Km_apical,
                   
                   "Pliver" = Pliver*CF[1],  "Prest" = Prest*CF[2], 
                   "Pintestine" = Pintestine*CF[3], "Pgonads" = Pgonads*CF[4],
                   "Pspleen" = Pspleen*CF[5], "Pheart" = Pheart*CF[6],
                   "Plung" = Plung*CF[7], "Pbrain" = Pbrain*CF[8], "Pstomach" =Pstomach*CF[9], 
                   
                   "Vmax_baso" = Vmax_baso*CF[10], "Vmax_apical" = Vmax_apical*CF[11],
                   'kdif' = kdif*CF[12], 
                   "kbile" = kbile*CF[13], "kurine" = kurine*CF[14], "kefflux" = kefflux*CF[15],
                   "kabs" = kabs*CF[16], "kunabs" = kunabs*CF[17], "GE" = GE*CF[18], "k0" = k0*CF[19],
                   "Free" = Free*CF[20], 
                   
                   
                   "admin.type" = admin.type,
                   "admin.time" = admin.time, "admin.dose" = admin.dose))
      
    })
  }
  
  #===============================================
  #2. Function to create initial values for ODEs 
  #===============================================
  
  create.inits <- function(parameters){
    with( as.list(parameters),{
      "AR" <- 0; "Adif" <- 0; "A_baso" <- 0; "AKidneyblood" <- 0;
      "ACl" <- 0; "Aefflux" <- 0;
      "A_apical" <- 0; "APTC" <- 0; "Afil" <- 0;
      "Aurine" <- 0; "Astomach_lumen" <- 0;
      "Astomach" <- 0; "Aintestine_lumen" <- 0; "Aintestine" <- 0; "Afeces" <- 0;
      "ALiver" <- 0; "Abile" <- 0;
      "Agonads" <- 0; "Aspleen" <- 0; "Aheart" <- 0;
      "Alung" <- 0; "Abrain" <- 0; "Aven_free" <- 0; "Aart_free" <- 0;
      
      return(c("AR" = AR, "Agonads" = Agonads, "Aspleen" = Aspleen, "Aheart" = Aheart,
               "Alung" = Alung, "Abrain" = Abrain, "Adif" = Adif, "A_baso" = A_baso, 
               "AKidneyblood" = AKidneyblood,
               "ACl" = ACl, "Aefflux" = Aefflux,
               "A_apical" = A_apical, "APTC" = APTC, "Afil" = Afil,
               "Aurine" = Aurine, "Astomach_lumen" =Astomach_lumen,
               "Astomach" = Astomach, "Aintestine_lumen" = Aintestine_lumen, "Aintestine" = Aintestine,
               "Afeces" = Afeces, 
               "ALiver" = ALiver, "Abile" = Abile, "Aven_free" = Aven_free, "Aart_free" = Aart_free))
    })
  }
  
  #===================
  #3. Events function
  #===================
  
  create.events <- function(parameters){
    with(as.list(parameters), {
      
      # Calculate number of administrated doses and corresponding administration time
      ldose <- length(admin.dose)
      ltimes <- length(admin.time)
      # If not equal, then stop 
      if (ltimes != ldose){
        stop("The times of administration should be equal in number to the doses")
      }else{
        if (admin.type == "iv"){
          events <- list(data = rbind(data.frame(var = c("Aven_free"),  time = admin.time, 
                                                 value = admin.dose, method = c("add")) ))
        }else if (admin.type == "oral"){
          events <- list(data = rbind(data.frame(var = c("Astomach_lumen"),  time = admin.time, 
                                                 value = c( admin.dose), method = c("add")) ))
        }
      }
      return(events)
    })
  }
  
  #==================
  #4. Custom function 
  #==================
  custom.func <- function(){
    return()
  }
  
  #==============
  #5. ODEs System
  #==============
  
  ode.func <- function(time, inits, params, custom.func){
    with(as.list(c(inits,params)),{
      CR = AR/VR #concentration in rest of body (mg/L)
      CVR = CR/Prest	#concentration in venous blood leaving the rest of the body (mg/L)
      CKidneyblood = AKidneyblood/Vkidneyb	#concentration in kidney blodd (mg/L) 
      CVkidney = CKidneyblood #/Pkidney	#concentration in venous blood leaving kidney (mg/L)
      CPTC = APTC/VPTC	#concentration in PTC (mg/L)
      Cfil = Afil/Vfil	#concentration in filtrate (mg/L)
      Cliver = ALiver/VL	#concentration in the liver (mg/L)
      CVliver = Cliver/Pliver	#concentration in the venous blood leaving the liver (mg/L)
      Cgonads = Agonads/Vgonads	#concentration in the gonads (mg/L)
      CVgonads = Cgonads/Pgonads	#concentration in the venous blood leaving the gonads (mg/L)
      Cspleen = Aspleen/Vspleen	#concentration in the spleen (mg/L)
      CVspleen = Cspleen/Pspleen	#concentration in the venous blood leaving the spleen (mg/L)
      Cheart = Aheart/Vheart	#concentration in the heart (mg/L)
      CVheart = Cheart/Pheart	#concentration in the venous blood leaving the heart (mg/L)
      Clung = Alung/Vlung	#concentration in the lung (mg/L)
      CVlung = Clung/Plung	#concentration in the venous blood leaving the lung (mg/L)
      Cbrain = Abrain/Vbrain	#concentration in the brain (mg/L)
      CVbrain = Cbrain/Pbrain	#concentration in the venous blood leaving the brain (mg/L)
      Cintestine = Aintestine/Vintestine	#concentration in the liver (mg/L)
      CVintestine = Cintestine/Pintestine	#concentration in the venous blood leaving the liver (mg/L)
      Cstomach = Astomach/Vstomach	#concentration in the liver (mg/L)
      CVstomach = Cstomach/Pstomach	#concentration in the venous blood leaving the liver (mg/L)
      
      Cart_free = Aart_free/Vart_plasma		#concentration in arterial  plasma (mg)
      Cart = Cart_free/Free	#concentration of total PFOA in arterial plasma (mg/L)
      Cven_free = Aven_free/Vven_plasma		#concentration in venous plasma (mg)
      Cven = Cven_free/Free	#concentration of total PFOA in venous plasma (mg/L)
      
      # Rest of Body (Tis)
      dAR = Qrest*(Cart-CVR)*Free	#rate of change in rest of body (mg/h)
      dAgonads = Qgonads*(Cart-CVgonads)*Free	#rate of change in rest of body (mg/h)
      dAspleen = Qspleen*(Cart-CVspleen)*Free	#rate of change in rest of body (mg/h)
      dAheart = Qheart*(Cart-CVheart)*Free	#rate of change in rest of body (mg/h)
      dAlung = Qlung*(Cven-CVlung)*Free	#rate of change in rest of body (mg/h)
      dAbrain = Qbrain*(Cart-CVbrain)*Free	#rate of change in rest of body (mg/h)
      
      #Kidney 
      #Kidney Blood (Kb)
      dAdif <- kdif*(CKidneyblood - CPTC)	#rate of diffusion from into the PTC (mg/hr)
      dA_baso <- (Vmax_baso*CKidneyblood)/(Km_baso + CKidneyblood)	
      dAKidneyblood <- QK*(Cart-CVkidney)*Free- Cven*GFR*Free - dAdif - dA_baso #rate of change in kidney blood (mg/h).
      dACl <- Cven*GFR*Free	#rate of clearance via glormerular filtration (mg/h)
      
      #Proximal Tubule Cells (PTC)
      dAefflux <- kefflux*APTC
      dA_apical <- (Vmax_apical*Cfil)/(Km_apical + Cfil)
      dAPTC <-  dAdif + dA_apical + dA_baso - dAefflux #rate of change in PTC(mg/h)
      
      #Filtrate (Fil)
      dAfil = Cven*GFR*Free - dA_apical - Afil*kurine	#rate of change in filtrate (mg/h)
      
      #Urinary elimination 
      dAurine = kurine*Afil	#rate of change in urine (mg/h)
      
      #GI Tract (Absorption site of oral dose)
      #Stomach
      dAstomach_lumen=  - k0*Astomach_lumen - GE*Astomach_lumen 	#rate of change in the stomach lumen (mg/h)
      dAstomach = k0*Astomach_lumen + Qstomach*(Cart-CVstomach)*Free	#rate change in the stomach (mg/h)
      
      #Small Intestine
      dAintestine_lumen = GE*Astomach_lumen - kabs*Aintestine_lumen - kunabs*Aintestine_lumen	#rate of change in the  intestine lumen (mg/hr)
      dAintestine = kabs*Aintestine_lumen +  Qintestine*(Cart-CVintestine)*Free	#rate change in the  intestine (mg/hr)
      
      #Feces compartment
      dAfeces = kbile*ALiver + kunabs*Aintestine_lumen #rate of change in the feces compartment (mg/h)
      
      #Liver
      dALiver = QL_hepatic_artery*Cart*Free - kbile*ALiver + kabs*Aintestine_lumen + k0*Astomach_lumen +
        Qspleen*CVspleen*Free +Qstomach*CVstomach*Free+ Qintestine*CVintestine*Free-
        (QL_hepatic_artery+Qspleen+Qstomach+Qintestine)*CVliver*Free#rate of change in the liver (mg/h)
      dAbile = kbile*ALiver  
      
      #Venous Plasma compartment
      dAven_free = Qrest*CVR*Free + Qgonads*CVgonads*Free +  Qheart*CVheart*Free + Qbrain*CVbrain*Free +
        (QK*CVkidney*Free) + ((QL_hepatic_artery+Qspleen+Qstomach+Qintestine) *CVliver*Free) - 
        (Qlung*Cven*Free) + dAefflux  #rate of change in the plasma (mg/h) 
      
      #Arterial Plasma compartment
      dAart_free =  Qlung*CVlung*Free - Cart*Free*(Qrest+Qgonads+Qspleen+Qheart+Qbrain+QK+
                                                     Qstomach+Qintestine+QL_hepatic_artery)
      
      #Mass Balance Check
      Atissue = Aart_free +Aven_free+ AR + AKidneyblood + Afil + APTC + ALiver + 
        Astomach + Astomach_lumen + Aintestine+ Aintestine_lumen+
        Agonads + Aspleen + Aheart + Alung + Abrain #sum of mass in all compartments (mg)
      Aloss = Aurine + Afeces #sum of mass lost through urinary and fecal excretion (mg)
      Atotal = Atissue + Aloss 	#total mass; should equal total dose
      
      list(c("dAR" = dAR, "dAgonads" = dAgonads, "dAspleen" = dAspleen, "dAheart" = dAheart,
             "dAlung" = dAlung, "dAbrain" = dAbrain, 
             "dAdif" = dAdif, "dA_baso" = dA_baso, "dAKidneyblood" = dAKidneyblood,
             "dACl" = dACl, "dAefflux" = dAefflux,
             "dA_apical" = dA_apical, "dAPTC" = dAPTC, "dAfil" = dAfil,
             "dAurine" = dAurine, "dAstomach_lumen" =dAstomach_lumen,
             "dAstomach" = dAstomach, "dAintestine_lumen" = dAintestine_lumen,
             "dAintestine" = dAintestine, "dAfeces" = dAfeces, 
             "dALiver" = dALiver, "dAbile" = dAbile, "dAven_free" = dAven_free,
             "dAart_free" = dAart_free), 
           "Atissue" = Atissue, "Aloss" = Aloss, "Atotal" = Atotal, "CR" =CR,
           "CVR" = CVR, "CKidneyblood" = CKidneyblood, 
           "CVkidney" = CVkidney, "CPTC" = CPTC,
           "Cfil" = Cfil, "Cliver" = Cliver, "CVliver" = CVliver, "Cart_free" = Cart_free,
           "Cart" = Cart, Cplasma = (Aart_free +Aven_free)/VPlas/Free,
           "Ckidneys" = (APTC+AKidneyblood)/Vkidney,
           "Cbrain" = Cbrain)
      
    })
  }
  
  #======================
  #3. Objective function  
  #======================
  
  obj.func <- function(par, group, serum, serum_indices, tissue,tissue_indices, inits, sex, BW){
    
    solve_odes <- function(admin.type, admin.dose, indices){
      # Calculate PBK parameters
      parameters <- create.params( list("BW" = BW  , sex = sex, 
                                        "admin.type" = admin.type,
                                        "admin.time" = 0.0, "admin.dose" = admin.dose,
                                        "par" = par, "group" = group ))
      events <- create.events(parameters)
      # Structure the in silico time vector in a way that the sampling time points are included
      if(sex == "M"){
        sample_time <- c(0,1e-10,1e-9,1e-8,1e-7,1e-6,1e-5, 1e-4, 1e-3,
                         seq(0.01,0.08, 0.001), 0.083, 0.1, 0.2, 0.25, 
                         seq(0.3,  0.9, 0.1), seq(1,23,1),
                         seq(24, 192, 4), seq(196, 516,16), 528, seq(540, 860,16),
                         864, seq(880, 1200,32))
      }else{
        sample_time 
      }
      solution <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                          y = inits, parms = parameters, events = events,
                                          method="lsodes",rtol = 1e-7, atol = 1e-7))
      
      concentrations <- data.frame("time" = solution$time, "Cplasma" = solution$Cplasma)
      experimental_time_points <- serum$Time[indices[1]:indices[2]]
      concentrations <- concentrations[concentrations$time %in% experimental_time_points, "Cplasma"]
      
      observed <- list("Cplasma" = serum$Mass[indices[1]:indices[2]])
      predicted <- list("Cplasma" = concentrations)
      #Calculate goodness-of-fit
      discrepancy <- SODI(observed, predicted)
      return(list("discrepancy" = discrepancy,"solution" = solution))
    }
    discrepancy_iv_6 <- solve_odes(admin.type = unique(serum$Type)[1],
                                   admin.dose = BW*unique(serum$Dose)[1], 
                                   indices = c(1,serum_indices[1]-1))$discrepancy
    discrepancy_oral_6 <- solve_odes(admin.type = unique(serum$Type)[2],
                                     admin.dose = BW*unique(serum$Dose)[1], 
                                     indices = c(serum_indices[1],serum_indices[2]-1))$discrepancy
    solution_oral_12 <- solve_odes(admin.type = unique(serum$Type)[2],
                                   admin.dose = BW*unique(serum$Dose)[2], 
                                   indices = c(serum_indices[2],serum_indices[3]-1))
    discrepancy_oral_12 <- solution_oral_12$discrepancy
    discrepancy_oral_48 <- solve_odes(admin.type = unique(serum$Type)[2],
                                      admin.dose = BW*unique(serum$Dose)[3], 
                                      indices = c(serum_indices[3],dim(serum)[1]))$discrepancy
    
    # Estimate the discrepancy with the tissues
    concentrations <- data.frame("time" = solution_oral_12$solution$time, 
                                 "Ckidneys" = solution_oral_12$solution$Ckidneys,
                                 "Cliver" =solution_oral_12$solution$Cliver,
                                 "Cbrain" = solution_oral_12$solution$Cbrain)
    
    experimental_time_points_liver <- tissue$Time[1:(tissue_indices[1]-1)]
    concentration_liver <- concentrations[concentrations$time %in% experimental_time_points_liver, "Cliver"]
    
    experimental_time_points_kidneys <- tissue$Time[tissue_indices[1]:(tissue_indices[2]-1)]
    concentration_kidneys <- concentrations[concentrations$time %in% experimental_time_points_kidneys, "Ckidneys"]
    
    experimental_time_points_brain<- tissue$Time[tissue_indices[2]:dim(tissue)[1]]
    concentration_brain <- concentrations[concentrations$time %in% experimental_time_points_brain, "Ckidneys"]
    
    observed <- list("Cliver" = tissue[tissue$Tissue == "Liver","Mass"],
                     "Ckidneys" = tissue[tissue$Tissue == "Kidneys","Mass"],
                     "Ckidneys" = tissue[tissue$Tissue == "Brain","Mass"])
    
    predicted <- list("Cliver" = concentration_liver, "Ckidneys" = concentration_kidneys,
                      "Cbrain" = concentration_brain)
    #Calculate goodness-of-fit
    discrepancy_tissues <- SODI(observed, predicted)
    total_discrepancy <- discrepancy_iv_6 + discrepancy_oral_6 + discrepancy_oral_12 + 
      discrepancy_oral_48+discrepancy_tissues
    return(total_discrepancy)
  }
  
  # SODI function the returns the SODI index described in Tsiros et al.2024
  # predictions: list of vectors containing the predicted data
  # names of the compartments
  
  SODI <- function(observed, predicted, comp.names =NULL){
    # Check if the user provided the correct input format
    if (!is.list(observed) || !is.list(predicted)){
      stop(" The observations and predictions must be lists")
    }
    # Check if the user provided equal length lists
    if (length(observed) != length(predicted)){
      stop(" The observations and predictions must have the same compartments")
    }
    Ncomp <- length(observed) # Number of compartments
    I <- rep(NA, Ncomp) # Compartment discrepancy index
    N_obs <- rep(NA, Ncomp) #Number of observations per compartment
    #loop over the compartments
    for (i in 1:Ncomp){
      Et <- 0 #relative error with observations
      St <- 0  #relative error with simulations
      N <- length(observed[[i]]) # number of observations for compartment i
      # Check if observations and predictions have equal length
      if(N != length(predicted[[i]])){
        stop(paste0("Compartment ",i," had different length in the observations and predictions"))
      }
      N_obs[i] <- N # populate the N_obs vector
      for (j in 1:N){
        # sum of relative squared errors (error = observed - predicted)
        Et <- Et + ( abs(observed[[i]][j] - predicted[[i]][j])  / observed[[i]][j] )  ^2
        St <- St + ( abs(observed[[i]][j] - predicted[[i]][j])  / predicted[[i]][j] )  ^2
      }
      
      # root mean of the square of observed values
      RMEt <- sqrt(Et/N)
      # root mean of the square of simulated values
      RMSt <- sqrt( St/N)
      
      I[i] <- (RMEt + RMSt)/2   
    }
    # Total number of observations
    Ntot <- sum(N_obs)
    # Initialise the consolidated discrepancy index
    Ic <-0
    for (i in 1:Ncomp){
      # Give weight to compartments with more observations (more information)
      Ic <- Ic +  I[i]* N_obs[i]/Ntot
    }
    # Name the list of compartment discrepancy indices
    if ( !is.null(comp.names)){
      names(I) <- comp.names
    }else if (!is.null(names(observed))){
      names(I) <- names(observed)
    } else if (!is.null(names(predicted)) && is.null(comp.names) ){
      names(I) <- names(predicted)
    }
    return(Ic)
    #return(list(Total_index = Ic, Compartment_index= I))
  }
  #==============================
  #5. Decode chromosomes  
  #==============================
  # Function for decoding the GA output. Simply, we take the floor of the continuous number
  decode_ga_real <- function(real_num){ 
    # Partition coefficient grouping
    CF1<- floor(real_num[1])
    CF2 <- floor(real_num[2])
    CF3 <- floor(real_num[3])
    CF4 <- floor(real_num[4])
    CF5 <- floor(real_num[5])
    CF6 <- floor(real_num[6])
    CF7 <- floor(real_num[7])
    CF8 <- floor(real_num[8])
    CF9 <- floor(real_num[9])
    CF10 <- floor(real_num[10])
    CF11 <- floor(real_num[11])
    CF12 <- floor(real_num[12])
    CF13 <- floor(real_num[13])
    CF14 <- floor(real_num[14])
    CF15 <- floor(real_num[15])
    CF16 <- floor(real_num[16])
    CF17 <- floor(real_num[17])
    CF18 <- floor(real_num[18])
    CF19 <- floor(real_num[19])
    CF20 <- floor(real_num[20])
    
    out <- structure(c(CF1,CF2,CF3,CF4,CF5,CF6,CF7,CF8, CF9,CF10,CF11,CF12,CF13,
                       CF14,CF15,CF16, CF17, CF18, CF19, CF20),
                     names = c("CF1","CF2","CF3","CF4","CF5","CF6","CF7","CF8", "CF9",
                               "CF10","CF11","CF12","CF13",
                               "CF14","CF15","CF16", "CF17", "CF18", "CF19", "CF20"))
    return(out)
  }
  
  
  
  #===============
  # Load data  
  #===============
  MW = 414.07	#PFOA molecular mass (g/mol)
  # Load raw data from paper Kreyling et al.2017, which are given in %ID/g tissue
  df_serum <- openxlsx::read.xlsx("serum_male.xlsx",  colNames = T, rowNames = F)
  df_tissue <- openxlsx::read.xlsx("tissue_male.xlsx", colNames = T, rowNames = F)
  #Rename columns for easier handling
  colnames(df_serum) <- c("Time", "Mass", "Dose", "Type")
  colnames(df_tissue) <- c("Time", "Mass", "Dose", "Tissue")
  
  #Transform microMolar to mg/L
  df_serum$Mass <- df_serum$Mass*MW/1000
  df_tissue$Mass <- df_tissue$Mass*MW/1000
  
  #Load male results
  load("PFOA.RData")  
  chromosome <- GA_results@solution
  group <- decode_ga_real(chromosome)

  opts <- list( "algorithm" = "NLOPT_LN_NEWUOA",
                "xtol_rel" = 1e-09,
                "ftol_rel" = 0.0,
                "ftol_abs" = 0.0,
                "xtol_abs" = 0.0 ,
                "maxeval" = 1000,
                "print_level"=1)
  # Create initial conditions (zero initialisation)
  inits <- create.inits(list(NULL))
  fit <- log(c(1,1, 1))
  try(
    # Run the optimization algorithmm to estimate the parameter values
    optimizer <- nloptr::nloptr( x0= fit,
                                 eval_f = obj.func,
                                 lb	= rep(log(1e-10), length(fit)),
                                 ub = rep(log(100), length(fit)),
                                 opts = opts,
                                 group = group,
                                 serum = df_serum,
                                 serum_indices = c(13,23,33),# index where dose changes
                                 tissue = df_tissue,
                                 tissue_indices = c(8,14),# index where tissue changes
                                 inits = inits, 
                                 sex = "M", 
                                 BW = 0.3), # Weight from https://animal.ncku.edu.tw/p/412-1130-16363.php?Lang=en based on SD rats 8 weeks old
    
    silent = TRUE
  )
  # fitted parameter values
  par = optimizer$solution
  
  #Function for solving the ODEs given the solution of the GA and the optimisation problem
  solve_odes <- function(admin.type, admin.dose, BW, sex, par, group ){
    # Calculate PBK parameters
    parameters <- create.params( list("BW" = BW  , sex = sex, 
                                      "admin.type" = admin.type,
                                      "admin.time" = 0.01, "admin.dose" = admin.dose,
                                      "par" = par, "group" = group ))
    events <- create.events(parameters)
    # Structure the in silico time vector in a way that the sampling time points are included
    if(sex == "M"){
      sample_time <- c(0, seq(0.01,0.08, 0.001), 0.083, 0.1, 0.2, 0.25, 
                       seq(0.3,  0.9, 0.1), seq(1,23,1),
                       seq(24, 192, 4), seq(196, 516,16), 528, seq(540, 860,16),
                       864, seq(880, 1200,32))
    }else{
      sample_time 
    }
    solution <- data.frame(deSolve::ode(times = sample_time,  func = ode.func,
                                        y = inits, parms = parameters, events = events,
                                        method="lsodes",rtol = 1e-7, atol = 1e-7))
    return(solution)
  }
  #Acquire solutions for the different exposure scenarios
  solution_iv_6_male <- solve_odes(admin.type = unique(df_serum$Type)[1],
                                 admin.dose = BW*unique(df_serum$Dose)[1], 
                                 BW = 0.3, sex = "M", par = par, group = group)
  solution_oral_6_male <- solve_odes(admin.type = unique(df_serum$Type)[2],
                                   admin.dose = BW*unique(df_serum$Dose)[1], 
                                BW = 0.3, sex = "M", par = par, group = group)
  solution_oral_12_male <- solve_odes(admin.type = unique(df_serum$Type)[2],
                                 admin.dose = BW*unique(df_serum$Dose)[2], 
                                 BW = 0.3, sex = "M", par = par, group = group)
  solution_oral_48_male <- solve_odes(admin.type = unique(df_serum$Type)[2],
                                    admin.dose = BW*unique(df_serum$Dose)[3], 
                                 BW = 0.3, sex = "M", par = par, group = group)
  
  serum_predictions <- data.frame(Time = rep(solution_iv_6_male$time,4), value = c(solution_iv_6_male$Cplasma,
                      solution_oral_6_male$Cplasma, solution_oral_12_male$Cplasma, 
                      solution_oral_48_male$Cplasma), dose = c(rep("dose1-iv", dim(solution_iv_6_male)[1]),
                                                       rep("dose1-oral", dim(solution_iv_6_male)[1]),
                                                       rep("dose2-oral", dim(solution_iv_6_male)[1]),
                                                       rep("dose3-oral", dim(solution_iv_6_male)[1])))
  serum_predictions <- serum_predictions[serum_predictions$value != 0 ,]
  df_serum$dose_to_plot <- rep(NA, dim(df_serum)[1])
  for (i in 1:dim(df_serum)[1]){
      if (df_serum[i,"Type"] == "iv"){
        df_serum[i,"dose_to_plot"] <- "dose1-iv"
      }else if(df_serum[i,"Dose"] == 6){
        df_serum[i,"dose_to_plot"] <- "dose1-oral"
      }else if(df_serum[i,"Dose"] == 12){
        df_serum[i,"dose_to_plot"] <- "dose2-oral"
      }else if(df_serum[i,"Dose"] == 48){
        df_serum[i,"dose_to_plot"] <- "dose3-oral"
      }
  }
    
  library(ggplot2)
  
  # Defining the linetype and colour of each curve
  ltp <- c("dose1-iv" = "twodash", "dose1-oral" = "solid", "dose2-oral" = "dotted","dose3-oral" = "dashed",
           "Liver" = "twodash", "Kidneys" = "solid","Brain" = "dotted" )
  cls <-  c("dose1-iv" = "#56B4E9",  "dose1-oral" ="#000000", "dose2-oral" = "#009E73", 
            "dose3-oral" ="#CC79A7",
            "Liver" = "#CC79A7", "Kidneys" = "#000000","Brain" = "#56B4E9")

    ggplot(data = serum_predictions)+
      geom_line(aes(x = Time, y = value, color = dose, linetype = dose), size = 1.5, alpha = 0.7) +
      geom_point(data = df_serum, aes(x = Time, y = Mass, color = dose_to_plot), size = 4) +
      coord_cartesian(ylim = c(1, 1000))+
      labs(y = "Concentration (mg/L)",x = "Time (hours)")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_log10()+
      scale_color_manual("", values=cls)+
      scale_linetype_manual("", values=ltp) +
      theme(legend.key.size = unit(1.5, 'cm'),  
            legend.title = element_text(size=14),
            axis.title=element_text(size=14),
            legend.text = element_text(size=14))
    
    tissue_predictions <- data.frame(Time = rep(solution_oral_12_male$time,3), 
                                     value = c(solution_oral_12_male$Cliver,
                                               solution_oral_12_male$Ckidneys, 
                                               solution_oral_12_male$Cbrain),
                                     Tissue = c(rep("Liver", dim(solution_oral_12_male)[1]),
                                      rep("Kidneys", dim(solution_oral_12_male)[1]),
                                      rep("Brain", dim(solution_oral_12_male)[1])))  
    
    tissue_predictions <- tissue_predictions[tissue_predictions$value != 0 ,]
    ggplot(data = tissue_predictions)+
      geom_line(aes(x = Time, y = value, color = Tissue, linetype = Tissue), size = 1.5, alpha = 0.7) +
      geom_point(data = df_tissue, aes(x = Time, y = Mass, color = Tissue), size = 4) +
      labs(y = "Concentration (mg/L)",x = "Time (hours)")+
      theme(plot.title = element_text(hjust = 0.5))+
      scale_y_log10()+
      coord_cartesian(ylim = c(0.1, 100))+
      scale_color_manual("", values=cls)+
      scale_linetype_manual("", values=ltp) +
      theme(legend.key.size = unit(1.5, 'cm'),  
            legend.title = element_text(size=14),
            axis.title=element_text(size=14),
            legend.text = element_text(size=14))
    
    