library(tidyverse)
library(dplyr)
library(broom)
library(ggplot2)
library(car)
library(GGally)
library(broom)

property <- read.csv(
  file = "./data/sample_data.csv"
) |>
  subset(select = c(CURRENT_LAND_VALUE,
    PREVIOUS_LAND_VALUE,
    PREVIOUS_IMPROVEMENT_VALUE,
    YEAR_BUILT, ZONING_CLASSIFICATION,
    LEGAL_TYPE
  )) |>
  na.omit()

property2 <- subset(property,
  is.finite(CURRENT_LAND_VALUE) & CURRENT_LAND_VALUE > 0 &
  is.finite(PREVIOUS_LAND_VALUE) & PREVIOUS_LAND_VALUE > 0 &
  is.finite(PREVIOUS_IMPROVEMENT_VALUE) & PREVIOUS_IMPROVEMENT_VALUE > 0 &
  is.finite(YEAR_BUILT)
)


nrow(property) - nrow(property2) ## we check how many rows were dropped
sample_n(property, 10) ## looking at our data


# See how many variables in ZONING CLASSIFICATION

unique(property$ZONING_CLASSIFICATION)
length(unique(property$ZONING_CLASSIFICATION))

# See what missing values in ZONING CLASSIFICATION

property[property$ZONING_CLASSIFICATION == '',]

# Simplify ZONING CLASSIFICATION into 5 categories: Residential, Commercial, Industrial, Agriculture, Special

property2$ZONE_SIMPLE <- dplyr::case_when(
  property2$ZONING_CLASSIFICATION %in% c(
    "Residential Inclusive",
    "One-Family Dwelling",
    "Two-Family Dwelling",
    "Multiple Dwelling",
    "Residential"
  ) ~ "Residential",

  property2$ZONING_CLASSIFICATION == "Commercial" ~ "Commercial",

  property2$ZONING_CLASSIFICATION == "Industrial" ~ "Industrial",

  property2$ZONING_CLASSIFICATION == "Limited Agriculture" ~ "Agriculture",
  property2$ZONING_CLASSIFICATION %in% c(
    "Comprehensive Development",
    "Historical Area",
    "",
    "Other"
  ) ~ "Special"
)

unique(property$ZONE_SIMPLE)
length(unique(property$ZONE_SIMPLE))

colSums(is.na(property))

table(property2$LEGAL_TYPE)
## there is only one OTHER legal type so we decicded to drop it as outlier
nrow(property2)  

property2 <- property2[property2$LEGAL_TYPE != "OTHER", ]
nrow(property2)  # should be 1 less than before

fit <- lm(log(CURRENT_LAND_VALUE) ~ log(PREVIOUS_LAND_VALUE) + log(PREVIOUS_IMPROVEMENT_VALUE) + YEAR_BUILT + ZONE_SIMPLE, data = property2)
bb <- coef(fit)
# bb
fit_both <- lm(log(CURRENT_LAND_VALUE) ~ log(PREVIOUS_LAND_VALUE) + log(PREVIOUS_IMPROVEMENT_VALUE) + YEAR_BUILT + ZONE_SIMPLE + LEGAL_TYPE, data = property2)
fit_legal <- lm(log(CURRENT_LAND_VALUE) ~ log(PREVIOUS_LAND_VALUE) + log(PREVIOUS_IMPROVEMENT_VALUE) + YEAR_BUILT + LEGAL_TYPE, data = property2)
fit_inter <- lm(log(CURRENT_LAND_VALUE) ~ log(PREVIOUS_LAND_VALUE)*LEGAL_TYPE + log(PREVIOUS_IMPROVEMENT_VALUE) + YEAR_BUILT, data = property2)
cat("Additive Model with ZONE_SIMPLE")
summary(fit)
cat(" \n \n Additive Model with LEGAL_TYPE")
summary(fit_legal)
cat("\n \n Additive Model with both ZONE_SIMPLE and LEGAL_TYPE")
summary(fit_both)
cat("\n \n Interaction Model with log(PREVIOUS_LAND_VALUE) and LEGAL_TYPE")
summary(fit_inter)

cat("VIF of model with ZONE_SIMPLE")
vif(fit)
cat("\n \n VIF of model with LEGAL_TYPE")
vif(fit_legal)
cat("\n \n VIF of model with both ZONE_SIMPLE and LEGAL_TYPE")
vif(fit_both)

fit_int_legal <- lm(log(CURRENT_LAND_VALUE) ~ log(PREVIOUS_LAND_VALUE) * LEGAL_TYPE + 
                    log(PREVIOUS_IMPROVEMENT_VALUE) + YEAR_BUILT, data = property2)
anova(fit_legal, fit_int_legal)

summary(fit_int_legal)

property_log <- property2 |>
                mutate(log_curr = log(CURRENT_LAND_VALUE),
                log_prev = log(PREVIOUS_LAND_VALUE),
                log_improve = log(PREVIOUS_IMPROVEMENT_VALUE),
                year = as.numeric(YEAR_BUILT)) |>
                select(log_curr, log_prev, log_improve, year, ZONE_SIMPLE, LEGAL_TYPE) 
ggpairs(property_log)

scatterplot(
    log(CURRENT_LAND_VALUE) ~ log(PREVIOUS_LAND_VALUE)|LEGAL_TYPE, 
    smooth=FALSE, 
    by.groups=TRUE,
    xlab = "Previous Land Value (log)", 
    ylab = "Current Land Value (log)",
    main = "Current vs Previous Land Value by Zone",
    legend = list(title = "Zone", coords = "bottomright"),
    data = property2
    )

#ggplot2 for the red line
ggplot(data.frame(fitted = fit_int_legal$fitted.values, residuals = fit_int_legal$residuals),
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(x = "Fitted Values", 
       y = "Residuals",
       title = "Residuals vs Fitted Values") +
  theme_minimal()

# ggplot2
ggplot(data.frame(resid = residuals(fit_int_legal)), aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(
    title = "Normal Q-Q Plot of Residuals",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
