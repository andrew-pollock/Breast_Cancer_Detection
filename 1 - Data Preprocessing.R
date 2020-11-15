
## Preparation of the Dataset
library(dplyr)

# Load the unprocessed data
breast_cancer_raw <- readr::read_csv("data/raw/breast-cancer-wisconsin.data", col_names = FALSE)

# Rename the variables
names(breast_cancer_raw) <- c("id", "clump_thickness",  "uniform_size",  "uniform_shape", "marginal_adhesion", "epithelial_size", 
                              "bare_nuclei", "bland_chromatin", "normal_nucleoli", "mitoses", "class")

# Check the variable types
str(breast_cancer_raw)

# Investigate why bare_nuclei is a character
breast_cancer_raw[is.na(as.integer(breast_cancer_raw$bare_nuclei)),]

# bare_nuclei uses "?" for values that are NA
# Replace any "?" with NA
breast_cancer_raw[is.na(as.integer(breast_cancer_raw$bare_nuclei)),7] <- NA

# 16 observations out of 699 have NA bare_nuclei
breast_cancer_raw %>% group_by(class) %>% summarise(num_obs = n()) %>% mutate(freq = prop.table(num_obs))
breast_cancer_raw %>% filter(is.na(bare_nuclei)) %>% group_by(class) %>% summarise(num_obs = n()) %>% mutate(freq = prop.table(num_obs))

# Only 2 of the 16 observations with NA bare_nuclei are malignant
# This suggests that the data is Missing Not at Random (MNAR) and is safest to exclude it
breast_cancer_raw <- breast_cancer_raw %>% filter(!is.na(bare_nuclei)) 

# Reformat bare_nuclei as a numeric
breast_cancer_raw <- breast_cancer_raw %>% mutate(bare_nuclei = as.numeric(bare_nuclei))

# To make interpretation easier, change class to 1 if they have cancer (malignant), 0 if the don't (benign)
breast_cancer_raw <- breast_cancer_raw %>% mutate(class = case_when(class == 2 ~ 0, class == 4 ~ 1))

# Now lets check the data
summary(breast_cancer_raw)

# Save the formatted data
write.csv(breast_cancer_raw, "data/processed/preprocessed_data.csv", row.names = FALSE)


