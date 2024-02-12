library(ggplot2)

setwd("/Users/vassilis/Documents/GitHub/PBK_Grouping/Validation/Validation_results_data")
column_names <- c('Study', 'NM.Type', 'Anatase %', 'Rutile %', 'Dose',
                  'Injections',  'Tissue', 'Observed',   'Predicted')

# Load Results 
Disdier_results <- read.csv("Disdier_results.csv", header = T)
colnames(Disdier_results) <- column_names
Fabian_results <- read.csv("Fabian_results.csv", header = T)
colnames(Fabian_results) <- column_names
Garaets_results <- read.csv("Garaets_results.csv", header = T)
colnames(Garaets_results) <- column_names

# Tissue_colors <- scales::hue_pal()(6)
# names(Tissue_colors) <- c( "Lungs",   "Spleen",  "Liver",   "Kidneys", "Blood", "Heart")
# 
# Study_markers <- c(15,16,17)
# names(Study_markers) <- unique(c(Disdier_results$Study, Fabian_results$Study, Garaets_results$Study))

Tissue_markers <-  c(0,1,2,5,13, 8)
names(Tissue_markers) <- c( "Lungs",   "Spleen",  "Liver",   "Kidneys", "Blood", "Heart")

Study_colors <- scales::hue_pal()(3)
names(Study_colors) <- unique(c(Disdier_results$Study, Fabian_results$Study, Garaets_results$Study))

scatter_plot <- ggplot()+
  geom_abline(intercept = log10(1), slope = 1, linetype = "dashed", color = "black", size=1.5,alpha = 0.7) +  # Identity line in log10 scale
  geom_point(data = Disdier_results, aes(x=Observed, y=Predicted, color = Study, shape = Tissue), size=4)+
  geom_point(data = Fabian_results, aes(x=Observed, y=Predicted, color = Study, shape = Tissue), size=4)+
  geom_point(data = Garaets_results, aes(x=Observed, y=Predicted, color = Study, shape = Tissue), size=4)+
  
  
  scale_y_log10(limits=c(1e-03,NA))+
  scale_x_log10(limits=c(1e-03,NA))+

  
  scale_color_manual(values = Study_colors)+
  scale_shape_manual(values = Tissue_markers)+
  
  labs(title = "Predictions vs Observations", 
       y = "Log10 Predicted TiO2 concentrtions (ug/g)",
       x = "Log10 Observed TiO2 concentrtions (ug/g)")+
  theme(plot.title = element_text(hjust = 0.5))+

  theme(legend.key.size = unit(1.5, 'cm'),  
        legend.title = element_text(size=14),
        legend.text = element_text(size=14),
        axis.text = element_text(size = 14))

print(scatter_plot)