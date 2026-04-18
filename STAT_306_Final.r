# ---
# jupyter:
#   jupytext:
#     text_representation:
#       extension: .r
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.19.1
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %% [markdown]
# ## STAT 306 Project: Vancouver Land Value Associated with Prior Year Values and Age

# %% vscode={"languageId": "r"}
library(tidyverse)
library(broom)
library(ggplot2)
library(car)
library(GGally)
library(broom)

# %% [markdown]
# --------------------------
# #### Data cleaning

# %% vscode={"languageId": "r"}
unique(property_sample$ZONING_CLASSIFICATION)

# %% vscode={"languageId": "r"}
set.seed(306)

property_sample <- read.csv(file = "data\\sample_data.csv")

property_cleaned <- property_sample |>  
                    select(
                        LEGAL_TYPE, 
                        ZONING_CLASSIFICATION, 
                        CURRENT_LAND_VALUE,
                        CURRENT_IMPROVEMENT_VALUE, 
                        PREVIOUS_LAND_VALUE, 
                        PREVIOUS_IMPROVEMENT_VALUE, 
                        YEAR_BUILT,
                        ) |>
                    na.omit() |>
                    filter(
                        CURRENT_LAND_VALUE > 1 &
                        CURRENT_IMPROVEMENT_VALUE > 1 & 
                        PREVIOUS_IMPROVEMENT_VALUE > 1 &
                        PREVIOUS_LAND_VALUE > 1 &
                        LEGAL_TYPE != "OTHER"
                        ) |>
                    mutate(
                        LEGAL_TYPE = as.factor(LEGAL_TYPE),
                        ZONING_CLASSIFICATION = as.factor(ZONING_CLASSIFICATION),
                        ZONING_CLASSIFICATION = case_when(
                        ZONING_CLASSIFICATION %in% c(
                            "Residential",
                            "Residential Inclusive",
                            "Residential Rental",
                            "One-Family Dwelling",
                            "Two-Family Dwelling",
                            "Multiple Dwelling"
                        ) ~ "Residential",

                        ZONING_CLASSIFICATION %in% c(
                            "Commercial",
                            "Comprehensive Development"
                        ) ~ "Commercial/Mixed",

                        ZONING_CLASSIFICATION == "Industrial" ~ "Industrial",

                        ZONING_CLASSIFICATION %in% c(
                            "Historical Area",
                            "Other",
                            "Limited Agriculture",
                            ""
                        ) ~ "Special/Other",

                        TRUE ~ as.character(ZONING_CLASSIFICATION)
                        
                        ),
                        ZONING_CLASSIFICATION = as.factor(ZONING_CLASSIFICATION)
                    )



# Check how many rows were dropped
nrow(property_sample) - nrow(property_cleaned) 
nrow(property_sample)
head(property_cleaned)

# %% [markdown]
# --------------------------
# #### EDA

# %% vscode={"languageId": "r"}
# Distribution Check
par(mfrow = c(3,2))
hist(property_cleaned$YEAR_BUILT,
     main = "Year Built",
     xlab = "Year")
hist(x = (property_cleaned$CURRENT_IMPROVEMENT_VALUE), main = "Current Improvement Value", xlab = "Current Improvement Value")
hist(x = (property_cleaned$PREVIOUS_LAND_VALUE), main = "Previous Land Value", xlab = "Previous Land Value")
hist(x = (property_cleaned$PREVIOUS_IMPROVEMENT_VALUE), main = "Previous Improvement Value", xlab = "Previous Improvement Value")
hist(x = (property_cleaned$CURRENT_LAND_VALUE), main = "Current Land Value", xlab = "Current Land Value")

# %% vscode={"languageId": "r"}
# Log-transformation Justification
par(mfrow = c(4,2))
hist(x = (property_cleaned$CURRENT_IMPROVEMENT_VALUE), main = "Raw", xlab = "Current Improvement Value")
hist(x = log(property_cleaned$CURRENT_IMPROVEMENT_VALUE), main = "Log Transformed", xlab = "Log Current Improvement Value")
hist(x = (property_cleaned$PREVIOUS_LAND_VALUE), main = "", xlab = "Previous Land Value")
hist(x = log(property_cleaned$PREVIOUS_LAND_VALUE), main = "", xlab = "Log Previous Land Value")
hist(x = (property_cleaned$PREVIOUS_IMPROVEMENT_VALUE), main = "", xlab = "Previous Improvement Value")
hist(x = log(property_cleaned$PREVIOUS_IMPROVEMENT_VALUE), main = "", xlab = "Log Previous Improvement Value")
hist(x = (property_cleaned$CURRENT_LAND_VALUE), main = "", xlab = "Current Land Value")
hist(x = log(property_cleaned$CURRENT_LAND_VALUE), main = "", xlab = "Log Current Land Value")

# %% vscode={"languageId": "r"}
# Class Imbalance Check
counts1 <- table(property_cleaned$LEGAL_TYPE)
counts1

round(100 * prop.table(table(property_cleaned$LEGAL_TYPE)), 2)

counts2 <- table(property_cleaned$ZONING_CLASSIFICATION)
counts2

round(100 * prop.table(table(property_cleaned$ZONING_CLASSIFICATION)), 2)

# %% vscode={"languageId": "r"}
# Response vs Covariates
#png("scatterplots.png", width = 700, height = 400)

par(mfrow = c(1, 3))

plot(x = log(property_cleaned$PREVIOUS_LAND_VALUE),
     y = log(property_cleaned$CURRENT_LAND_VALUE),
     pch = 16, cex = 0.5,
     xlab = "log(PREVIOUS_LAND_VALUE)",
     ylab = "log(CURRENT_LAND_VALUE)",
     main = "CLV vs PLV")


plot(x = log(property_cleaned$PREVIOUS_IMPROVEMENT_VALUE),
     y = log(property_cleaned$CURRENT_LAND_VALUE),
     pch = 16, cex = 0.5,
     xlab = "log(PREVIOUS_IMPROVEMENT_VALUE)",
     ylab = "log(CURRENT_LAND_VALUE)",
     main = "CLV vs PIV")


plot(x = property_cleaned$YEAR_BUILT,
     y = log(property_cleaned$CURRENT_LAND_VALUE),
     pch = 16, cex = 0.5,
     xlab = "YEAR_BUILT",
     ylab = "log(CURRENT_LAND_VALUE)",
     main = "CLV vs Year Built")

#dev.off()

 # %% vscode={"languageId": "r"}
 par(mfrow = c(1, 1), mar = c(5,4,4,2))

boxplot(log(CURRENT_LAND_VALUE) ~ LEGAL_TYPE,
        data = property_cleaned,
        main = "log(CURRENT_LAND_VALUE) by LEGAL_TYPE",
        xlab = "LEGAL_TYPE",
        ylab = "log(CURRENT_LAND_VALUE)",
        col = "lightgray")

boxplot(log(CURRENT_LAND_VALUE) ~ ZONING_CLASSIFICATION,
        data = property_cleaned,
        main = "log(CURRENT_LAND_VALUE) by ZONING",
        xlab = "ZONING_CLASSIFICATION",
        ylab = "log(CURRENT_LAND_VALUE)",
        col = "lightgray")

par(mfrow = c(1, 1))

# %% vscode={"languageId": "r"}
numeric_data <- data.frame(
  log_CURRENT_LAND_VALUE = log(property_cleaned$CURRENT_LAND_VALUE),
  log_PREVIOUS_LAND_VALUE = log(property_cleaned$PREVIOUS_LAND_VALUE),
  log_PREVIOUS_IMPROVEMENT_VALUE = log(property_cleaned$PREVIOUS_IMPROVEMENT_VALUE),
  YEAR_BUILT = property_cleaned$YEAR_BUILT
)

round(cor(numeric_data), 3)

# %% [markdown] vscode={"languageId": "r"}
# --------------------------
# #### Model Selection

# %% vscode={"languageId": "r"}
# Define null and full model
fit_null <- lm(log(CURRENT_LAND_VALUE) ~ 1, data = property_cleaned)

fit_full <- lm(
                log(CURRENT_LAND_VALUE) ~ 
                    log(PREVIOUS_LAND_VALUE) +
                    log(PREVIOUS_IMPROVEMENT_VALUE) +
                    YEAR_BUILT +
                    LEGAL_TYPE +
                    ZONING_CLASSIFICATION,
                data = property_cleaned
                )

# Full stepwise (both directions)
fit_step <- step(fit_null, 
                 direction = "both", 
                 scope = formula(fit_full))

# Start from full model, remove predictors one by one
fit_backward <- step(fit_full, direction = "backward")

# Start from null, add predictors one by one
fit_forward <- step(fit_null, direction = "forward", scope = formula(fit_full))

# Summary of chosen model
cat("Stepwise Selection (both directions):")
summary(fit_step)
AIC(fit_step)

cat("Backward Selection:")
summary(fit_backward)
AIC(fit_backward)

cat("Forward Selection:")
summary(fit_forward)
AIC(fit_forward)

# %% [markdown]
# --------------------------
# #### Fitting Models

# %% vscode={"languageId": "r"}
fit_add <- lm(
  log(CURRENT_LAND_VALUE) ~
    log(PREVIOUS_LAND_VALUE) +
    log(PREVIOUS_IMPROVEMENT_VALUE) +
    YEAR_BUILT +
    LEGAL_TYPE +
    ZONING_CLASSIFICATION,
  data = property_cleaned
)

fit_int_zone <- lm(
  log(CURRENT_LAND_VALUE) ~
    log(PREVIOUS_LAND_VALUE) * ZONING_CLASSIFICATION +
    log(PREVIOUS_IMPROVEMENT_VALUE) +
    YEAR_BUILT +
    LEGAL_TYPE,
  data = property_cleaned
)

fit_int_legal <- lm(
  log(CURRENT_LAND_VALUE) ~
    log(PREVIOUS_LAND_VALUE) * LEGAL_TYPE +
    log(PREVIOUS_IMPROVEMENT_VALUE) +
    YEAR_BUILT +
    ZONING_CLASSIFICATION,
  data = property_cleaned
)
anova_zone <- anova(fit_add, fit_int_zone)
anova_legal <- anova(fit_add, fit_int_legal)

anova_legal

data.frame(
  Model = c("Additive", "Zone interaction", "Legal interaction"),
  AIC = c(AIC(fit_add), AIC(fit_int_zone), AIC(fit_int_legal)),
  Adj_R2 = c(
    summary(fit_add)$adj.r.squared,
    summary(fit_int_zone)$adj.r.squared,
    summary(fit_int_legal)$adj.r.squared
  ),
  Residual_SE = c(
    summary(fit_add)$sigma,
    summary(fit_int_zone)$sigma,
    summary(fit_int_legal)$sigma
  ),
   F_value = c(
    NA,
    anova_zone$F[2],
    anova_legal$F[2]
  ),
  P_value = c(
    NA,
    anova_zone$`Pr(>F)`[2],
    anova_legal$`Pr(>F)`[2]
  )
)

# %% vscode={"languageId": "r"}
summary(fit_int_legal)

# %% [markdown]
# --------------------------
# #### Analysis Plots

# %% vscode={"languageId": "r"}
vif_add <- vif(fit_add)
vif_add

# %% vscode={"languageId": "r"}
ggplot(data.frame(fitted = fit$fitted.values, residuals = fit$residuals),
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals",
       title = "Residuals vs Fitted") +
  theme_minimal()

# %% vscode={"languageId": "r"}
ggplot(data.frame(residuals = fit$residuals), aes(sample = residuals)) +
  stat_qq(alpha = 0.3) +
  stat_qq_line(color = "red", linetype = "dashed") +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
       title = "Normal Q-Q Plot") +
  theme_minimal()
