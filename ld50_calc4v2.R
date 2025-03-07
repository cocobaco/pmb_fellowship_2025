# plant mutation breeding data analysis
# for use in IAEA fellowship training, Feb 2025
# R. Picha, TINT RD
# for class given on 28 Feb 2025 at TINT


# ==============================
# LD50 calculation (actual data)
# ==============================


# Load necessary libraries
library(ggplot2)  # for plotting
library(drc)  # for dose-response model analysis
library(readxl)  # for reading excel file
library(dplyr)  # for data manipulation


# --------------------------------------
# Setup data
# --------------------------------------

# read from file (actual data)
file_path <- 'data/lethal dose-v2.xlsx'
# data <- read_excel(file_path, sheet='all', range='A1:F73')
data <- read_excel(file_path, sheet='all')

# rename columns
# data %>% 
#   rename(SurvivalRate = `Survival%Rel`)
colnames(data)[colnames(data) == "Survival%Rel"] <- "SurvivalRate"

data <- data %>% 
  mutate(Sample = paste(Type, Name, sep = '_'))

colnames(data)


# --------------------------------------
# Basic visualization
# --------------------------------------

# Scatter plot of Survival Rate vs. Dose
ggplot(data, aes(x = Dose, y = SurvivalRate)) +
  geom_point(size = 3, shape = 16) +
  geom_line() +
  labs(title = "Survival Rate after Gamma Irradiation",
       x = "Dose (kGy)",
       y = "Relative Survival Rate (%)") +
  facet_wrap(~Sample, ncol=3) +
  theme_minimal()


# --------------------------------
# Calculate LD50 using LL.3 model
# --------------------------------

# Function to fit LL.3 model and check validity
fit_model <- function(df) {
  if (nrow(df) < 3) {  # Ensure enough data points
    return(data.frame(Sample = df$Sample[1], LD50 = NA))
  }
  
  model <- tryCatch(
    drm(SurvivalRate ~ Dose, data = df, fct = LL.3()),
    error = function(e) NULL  # If model fitting fails, return NULL
  )
  
  if (is.null(model)) {
    return(data.frame(Sample = df$Sample[1], LD50 = NA))  # Handle model failure
  }

  LD50 <- tryCatch(
    ED(model, 50)[1],
    error = function(e) NA  # If LD50 calculation fails, return NA
  )
  
  if (!is.finite(LD50) || LD50 < 0 || LD50 > max(df$Dose)) {
    return(data.frame(Sample = df$Sample[1], LD50 = NA))  # Avoid extreme LD50 values
  }
  
  return(data.frame(Sample = df$Sample[1], LD50 = LD50))
}

# Group by Name (species) and apply model fitting
results <- data %>%
  group_by(Sample) %>%
  do(fit_model(.)) %>%
  ungroup()

# Print LD50 results for each species
print(results)


# Generate predictions for plotting
dose_seq <- seq(min(data$Dose), max(data$Dose), length.out = 100)

predictions <- data %>%
  group_by(Sample) %>%
  do({
    model <- drm(SurvivalRate ~ Dose, data = ., fct = LL.3())  # Fit model
    pred <- data.frame(Dose = dose_seq, 
                       Predicted_Surv = predict(model, 
                                                newdata = data.frame(Dose = dose_seq)), 
                       Name = unique(.$Sample))
  })

# Plot data and fitted curves for each species
ggplot(data, aes(x = Dose, y = SurvivalRate)) +
  geom_point(size = 3, aes(color = Sample)) +  # Data points
  geom_line(data = predictions, 
            aes(x = Dose, y = Predicted_Surv, color = Sample), 
            linewidth = 1) +  # Fitted curves
  facet_wrap(~ Sample, ncol = 3, scales = "free") +  # Separate plots for each species
  labs(title = "Dose-Response Curves for Each Species",
       x = "Dose",
       y = "Survival Rate") +
  theme_minimal()
