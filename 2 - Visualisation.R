
## Classification in R
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(caret)
library(tidyr)


# Data visualisation

# Load in the processed data
cancer_data <- readr::read_csv("data/processed/processed_data.csv")

# Plot 1 - Visualising the distribution of each variable individually
(plot1 <- cancer_data %>% select(-id, -class) %>%
  gather(key = "Variable", value = "Value") %>% 
  mutate(Value = as.numeric(Value)) %>% 
  ggplot(aes(x=Value, group=Variable, fill = Variable)) + 
  geom_density(size = 1, alpha = 0.6, show.legend = FALSE) + 
  facet_wrap(~ Variable, ncol = 3) + 
  theme_bw() +
  labs(y = "Variable Density", title = "Variable Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(minor_breaks = NULL))

# Save the plot as a png
ggsave("plots/Plot1_Variable_Distribution.png", plot1)



# Plot 2 - Comparing the distribution of each variable within the two classes
class_labelled_data <- cancer_data %>% mutate(class = factor(case_when(class == 0 ~ "Benign", TRUE ~ "Malignant")))

(plot2 <- featurePlot(x = class_labelled_data[, 2:10], 
                      y = class_labelled_data$class, 
                      plot = "box", 
                      scales = list(y = list(relation = "free")),  
                      labels = c("Clump Classification", "Variable Distribution"), 
                      main = "Distribution of Variables in Benign vs Malignant Clumps",
                      par.settings=list(box.rectangle = list(col = c("blue", "red"), fill=c("blue", "red"), alpha=0.3, lwd = 2),
                                        box.umbrella  = list(col = c("blue", "red"), lwd = 2),
                                        box.symbol    = list(col = c("blue", "red"), fill=c("blue", "red"), lwd = 2))
))

# Save the plot as a png
trellis.device(device="png", filename="plots/Plot2_Class_Comparison.png")
print(plot2)
dev.off()



# Plot 3 - Create a correlation plot for the predictors

# Create a correlation matrix of all the independent variables
cancer_correlation <- cor(cancer_data[,2:10])

# Use ggcorrplot to plot the correlations of each variable against the others
(plot3 <- ggcorrplot(cancer_correlation, hc.order = TRUE, type = "lower",
           ggtheme = ggplot2::theme_classic, 
           method = "square", lab = TRUE, 
           legend.title = "Correlation", title = "Variable Correlation") + 
  theme(plot.title = element_text(hjust = 0.5)))

# Save the plot as a png
ggsave("plots/Plot3_Variable_Correlation.png", plot3)

