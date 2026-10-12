# Download data from the OECD website.
https://www.oecd.org/en/data/datasets/piaac-2nd-cycle-database.html

packages <- c("haven", "summarytools", "modelsummary", "dplyr", "ggplot2",  "car", "tidyverse", "boot", "mediation", "data.table") 

# Set working directory 
setwd("YOUR/FILE/PATH/HERE")  # <-- Replace with your directory

# Function to load and preprocess data
load_and_prepare_data <- function(filepath) {
  data <- read_dta(filepath)
  
  # Convert relevant variables to factors
  data$gender_r.f <- factor(data$gender_r)
  data$ageg10lfs.f <- factor(data$ageg10lfs)
  data$paredc2.f <- factor(data$paredc2)
  data$j2_q04c.f <- factor(data$j2_q04c)
  data$j2_q05c.f <- factor(data$j2_q05c)
  data$iscoskil4_moth.f <- factor(data$iscoskil4_moth)
  data$iscoskil4_fath.f <- factor(data$iscoskil4_fath)
  data$edcat6_tc1.f <- factor(data$edcat6_tc1)
  data$edfield.f <- factor(data$edfield)
  
  # For pooled data, also convert country ID to factor
  if ("cntryid" %in% names(data)) {
    data$cntryid.f <- factor(data$cntryid)
  }
  
  return(data)
}

# Load datasets
piaac <- load_and_prepare_data("PATH/TO/file1.dta")
piaac_happy <- load_and_prepare_data("PATH/TO/file2.dta")
piaac_pooled <- load_and_prepare_data("PATH/TO/file3.dta")

# Define a function to run mediation analysis
run_mediation_analysis <- function(data, treat_var, mediator_var, outcome_var, covariates, output_prefix) {
  # Create formula strings
  formula_m <- as.formula(paste(mediator_var, "~", treat_var, "+", paste(covariates, collapse = "+")))
  formula_y <- as.formula(paste(outcome_var, "~", treat_var, "+", mediator_var, "+", paste(covariates, collapse = "+")))
  
  # Fit models
  med_model <- lm(formula_m, data = data)
  out_model <- lm(formula_y, data = data)
  
  # Mediation analysis
  med_out <- mediate(med_model, out_model, treat = treat_var, mediator = mediator_var, boot.ci.bca = TRUE, sims = 1000)
  
  # Output summaries
  print(paste("Results for", output_prefix))
  print(summary(med_out))
  
  # Sensitivity analysis
  sens <- medsens(med_out, rho.by = 0.1, effect.type = "both", sims = 1000)
  print(sens)
  
  # Save plots
  pdf(file = paste0(output_prefix, ".pdf"))
  plot(med_out)
  plot(sens, sens.par = "rho", main = "Sensitivity to rho", ylim = c(-0.05, 0.05))
  plot(sens, sens.par = "R2", r.type = "total", sign.prod = "positive")
  plot(sens, sens.par = "R2", r.type = "total", sign.prod = "negative")
  dev.off()
}

# Example usage
# For dataset 'piaac'
run_mediation_analysis(
  data = piaac,
  treat_var = "pvlit1",  
  mediator_var = "earnhrbonusdclc2",
  outcome_var = "i2_q05",
  covariates = c("gender_r.f", "ageg10lfs.f", "paredc2.f", "iscoskil4_fath.f", "edcat6_tc1.f", "edfield.f"),
  output_prefix = "Analysis_Literacy"
)

# Repeat for other variables and datasets as needed
# For example:
run_mediation_analysis(
  data = piaac,
  treat_var = "pvnum1",
  mediator_var = "earnhrbonusdclc2",
  outcome_var = "i2_q05",
  covariates = c("gender_r.f", "ageg10lfs.f", "paredc2.f", "iscoskil4_fath.f", "edcat6_tc1.f", "edfield.f"),
  output_prefix = "Analysis_Numeracy"
)

# For emotional stability
run_mediation_analysis(
  data = piaac,
  treat_var = "emos",
  mediator_var = "earnhrbonusdclc2",
  outcome_var = "i2_q05",
  covariates = c("gender_r.f", "ageg10lfs.f", "paredc2.f", "iscoskil4_fath.f", "edcat6_tc1.f", "edfield.f"),
  output_prefix = "Analysis_EmotionalStability"
)

# For pooled data including country dummies, include 'cntryid.f' in covariates
# e.g.,
run_mediation_analysis(
  data = piaac_pooled,
  treat_var = "zpvlit1",
  mediator_var = "earnhrbonusdclc2",
  outcome_var = "i2_q05",
  covariates = c("gender_r.f", "ageg10lfs.f", "paredc2.f", "iscoskil4_fath.f", "edcat6_tc1.f", "edfield.f", "cntryid.f"),
  output_prefix = "Pooled_Literacy"
)

# End of script