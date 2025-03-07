# plant mutation breeding data analysis
# for use in IAEA fellowship training, Feb 2025
# R. Picha, TINT RD
# for class given on 28 Feb 2025 at TINT


# ======================================
# LD50 calculation (fitting comparison)
# ======================================


# Load necessary libraries
library(drc)  # for drm model    
library(ggplot2)  # for plotting


# *** You can choose data to analyze here (data1 or data2) *****
# use_data = 'data1'
use_data = 'data2'
# **************************************************************


# Sample plant survival data (nonlinear trend)
data1 <- data.frame(
  Dose = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4),
  SurvivalRate = c(99, 95, 90, 55, 25, 10, 2, 0, 0)
)

# extreme case to show why linear fit is not appropriate
data2 <- data.frame(
  Dose = c(0, 0.1, 0.2, 0.3, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6, 7),
  SurvivalRate = c(99, 100, 99, 97, 95, 90, 55, 25, 10, 2, 0, 0, 0, 0, 0)
)


if (use_data == 'data1') {
  data <- data1
} else if (use_data == 'data2') {
  data <- data2
}


# calculate relative survival rate (adjusted for control)
ctrl_surv = as.numeric(data[1, 'SurvivalRate'])
data$SurvivalRateRel <- data$SurvivalRate / ctrl_surv * 100

head(data)


# basic plot

plot(data$Dose, data$SurvivalRateRel)

ggplot(data, aes(x = Dose, y = SurvivalRateRel)) +
  geom_point(size = 4) +
  labs(title = "Plant Survival",
       x = "Dose",
       y = "Survival Rate (%)")


# fit models

# Fit a linear model
model_linear <- lm(SurvivalRateRel ~ Dose, data = data)

# Fit a log-logistic (LL.3) dose-response model
model_LL3 <- drm(SurvivalRateRel ~ Dose, data = data, fct = LL.3())

# view models' coefficients
print(coef(model_linear))
print(coef(model_LL3))

# Calculate LD50 for both models
# for linear model, solve for dose at SurvivalRate = 50 using slope and intercept
intercept = coef(model_linear)[1]
slope = coef(model_linear)[2]
LD50_linear <- (50 - intercept) / slope  
# for drm model, use ED to get LD50
LD50_LL3 <- ED(model_LL3, 50)[1]  

# Print LD50 values
cat("LD50 (Linear Fit) =", round(LD50_linear, 2), "\n")
cat("LD50 (LL.3 Model) =", round(LD50_LL3, 2), "\n")


# Generate predictions for plotting

# generate x (Dose) values (sequence from min to max)
dose_seq <- seq(0, max(data$Dose), length.out = 100)

# calculate predictions based on the models
pred_linear <- data.frame(
  Dose = dose_seq,
  Predicted_Surv = predict(model_linear, newdata = data.frame(Dose = dose_seq)),
  Model = "Linear"
)
pred_LL3 <- data.frame(
  Dose = dose_seq,
  Predicted_Surv = predict(model_LL3, newdata = data.frame(Dose = dose_seq)),
  Model = "LL.3"
)

# combine predictions by rows
predictions <- rbind(pred_linear, pred_LL3)


# Plot the results (data and fitted curves)
ggplot(data, aes(x = Dose, y = SurvivalRateRel)) +
  # data points
  geom_point(size = 4, color = "black") +  
  # model predictions
  geom_line(data = predictions, 
            aes(x = Dose, y = Predicted_Surv, color = Model), 
            linewidth = 2, alpha=0.5) +
  # LD50 lines using geom_segment (from 0% to 50%)
  geom_segment(x = LD50_linear, xend = LD50_linear, y = 0, yend = 50, 
               color = "blue", linetype = "dashed", linewidth = 1) +
  geom_segment(x = LD50_LL3, xend = LD50_LL3, y = 0, yend = 50, 
               color = "red", linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("Linear" = "blue", "LL.3" = "red")) +
  # LD50 text annotations
  annotate("text", x = LD50_linear, y = 55, 
           label = paste("LD50 (Linear) =", round(LD50_linear, 2)), 
           color = "blue", hjust = 0, vjust = -0.5, alpha = 0.6) +
  annotate("text", x = LD50_LL3 + 0.1, y = 40, 
           label = paste("LD50 (LL.3) =", round(LD50_LL3, 2)), 
           color = "red", hjust = 0, vjust = 1.5, alpha = 0.6) +
  labs(title = paste0("Linear vs. LL.3 Fit (", use_data, ")"),
       x = "Dose",
       y = "Survival Rate (%)",
       color = "Model")