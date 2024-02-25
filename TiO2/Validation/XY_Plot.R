library(ggplot2)

setwd("/Users/vassilis/Documents/GitHub/PBK_Grouping/TiO2/Validation/Validation_results_data")
column_names <- c('Study', 'NM Size', 'Anatase %', 'Rutile %', 'Dose',
                  'Injections',  'Tissue', 'Observed',   'Predicted')

# Load Results 
Disdier_results <- read.csv("Disdier_results.csv", header = T)
colnames(Disdier_results) <- column_names


for (i in 1:dim(Disdier_results)[1]) {
  Disdier_results[i,10] <- paste0(Disdier_results$Study[i], 
                                ", Dose (ug) = ",Disdier_results$Dose[i])
}
colnames(Disdier_results)[10] <- "Experiment"
###############################################

Fabian_results <- read.csv("Fabian_results.csv", header = T)
colnames(Fabian_results) <- column_names


for (i in 1:dim(Fabian_results)[1]) {
  Fabian_results[i,10] <- paste0(Fabian_results$Study[i], 
                                 ", Dose (ug) = ",Fabian_results$Dose[i])
}
colnames(Fabian_results)[10] <- "Experiment"

###############################################

Garaets_results <- read.csv("Garaets_results.csv", header = T)
colnames(Garaets_results) <- column_names

for (i in 1:dim(Garaets_results)[1]) {
  Garaets_results[i,10] <- paste0(Garaets_results$Study[i], 
                                  ", Dose (ug) = ",Garaets_results$Dose[i])
}

colnames(Garaets_results)[10] <- "Experiment"

# names(Experiment) <- unique(c(expression("Disdier et al. (2015), Dose (" * mu* "g) = 215"),
#                               expression("Fabian et al. (2008), Dose (" * mu* "g) = 1250"),
#                               expression("Garaets et al. (2014), Dose (" * mu*  "g) = 2300"),
#                               expression("Garaets et al. (2014), Dose (" * mu*  "g) = 11500")))

Tissue_markers <-  c(0,1,2,5,3, 8)
names(Tissue_markers) <- c( "Lungs",   "Spleen",  "Liver",   "Kidneys", "Blood", "Heart")

Experiment <- scales::hue_pal()(4)
Experiment <- c("#009E73", "#E69F00", "#56B4E9", "#CC79A7")
names(Experiment) <- unique(c(Disdier_results$Experiment, Fabian_results$Experiment, Garaets_results$Experiment))

mu <-expression(mu)
test_label <- paste("Dose =", 215, mu, "g")

scatter_plot <- ggplot()+
  geom_abline(intercept = log10(1), slope = 1, linetype = "dashed", color = "black", linewidth = 1.5,alpha = 0.7) +  # Identity line in log10 scale
  geom_point(data = Disdier_results, aes(x=Observed, y=Predicted, color = Experiment, shape = Tissue), size=4, stroke = 1.5)+
  geom_point(data = Fabian_results, aes(x=Observed, y=Predicted, color = Experiment, shape = Tissue), size=4, stroke = 1.5)+
  geom_point(data = Garaets_results, aes(x=Observed, y=Predicted, color = Experiment, shape = Tissue), size=4, stroke = 1.5)+
  
  
  scale_y_log10(limits=c(1e-03,1e+04))+
  scale_x_log10(limits=c(1e-03,1e+04))+
  
  
  scale_color_manual(values = Experiment,
                    labels = c(#expression("Disdier et al. (2015), Dose = 215 " * mu* "g"),
                               str_wrap("Disdier et al. (2015): Dose = 215 \u03BCg", 25),
                               #expression("Fabian et al. (2008), Dose = 1250 " * mu* "g"),
                               str_wrap("Fabian et al. (2008): Dose = 1250 \u03BCg", 25),
                               
                               #expression(paste("Dose = \n215 " , mu, "g")),
                               str_wrap("Garaets et al. (2014): Dose = 2300 \u03BCg x 5 Doses", 25),
                               #expression("Garaets et al. (2014), Dose = 2300 " * mu* "g x 1 Dose")
                               str_wrap("Garaets et al. (2014): Dose = 2300 \u03BCg x 1 Dose", 25)
                             )
                     )+
  
  theme(legend.spacing.y = unit(1, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE))+
  
  scale_shape_manual(values = Tissue_markers)+
  theme_light()+
  labs(y = expression("Predicted TiO2 (" * mu* "g/g tissue)"),
       x = expression("Observed TiO2 (" * mu* "g/g tissue)"))+
  theme(plot.title = element_text(hjust = 0.5),
        legend.key.size = unit(1.0, 'cm'),  
        legend.title = element_text(size=14),
        legend.text = element_text(size=12,  hjust = 0),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        )

print(scatter_plot)

ggsave("validation_plot.png", scatter_plot, width = 11, height = 7, units = "in", dpi = 400)


