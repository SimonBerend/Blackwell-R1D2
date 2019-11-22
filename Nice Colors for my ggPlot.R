if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "arules",
               "arulesViz", "graphics",
               "RColorBrewer", "wesanderson")



setwd("C:/Users/Gebruiker/Downloads/Product_Analysis_Data/Product_Analysis_Data")
prod <- read.csv2("existingProductAttributes.csv")



ggplot(prod, aes(x = ProductType, y = Volume, fill = ProductType)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(prod$Volume, c(0.1, 0.9))) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_brewer(palette = "Set3")

  
  
  scale_color_manual(palette = wes.palette(4, "GrandBudapest")
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  fill_brewer(palette="Pastel2")
             ?
