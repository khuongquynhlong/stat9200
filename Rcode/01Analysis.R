library(haven)
library(tidyverse)
library(survey)
library(gtsummary)
library(flextable)


#----- Read data
#===============================================================================
df <- read_dta("Clean data/SSA_25countries_pooled.dta") |> as.data.frame()

glimpse(df)

#----- Label variables
binvar <- c(
  "neonatal", "infant", "under5", "ch_lbw", "everbf", "ebreast5", "polio123", 
  "dpt123", "measles", "bcg", "tetanus", "full_immun", "anc_any", "anc_firsttri", 
  "anc4", "skilled", "inst_birth", "inst_skilled", "early_preg", "early_marr", 
  "water", "sanit", "clean_fuel"
)

# Binary variables
df <- df |> mutate_at(binvar, function(x){factor(x, labels = c("No", "Yes"))})

# Remaining variables
df <- df |>
  mutate(
    b0 = factor(b0, labels = c("Single", "Multiple 2", "Multiple 3", "Multiple 4")),
    state = as.factor(state),
    ch_sex = factor(ch_sex, labels = c("Male", "Female")),
    birthorder = factor(birthorder, labels = c("1st", "2ndor3rd", "4thor5th", "6thabove")),
    interval = factor(interval, labels = c("1st", "<24months", "24_47months", "48months")),
    ch_size_birth = factor(ch_size_birth, labels = c("very small", "smaller",
                                                     "average", "larger", "very large")),
    anc_cat3 = factor(anc_cat3, labels = c("ANCless4", "ANC4-7", "ANC8+")),
    anc_cat3_cut15 = factor(anc_cat3_cut15, labels = c("ANCless4", "ANC4-7", "ANC8-15")),
    anc_cat3_cut12 = factor(anc_cat3_cut12, labels = c("ANCless4", "ANC4-7", "ANC8-12")),
    mom_age_cat = factor(mom_age_cat, labels = c("15-19", "20-24", "25-29", "30-34",
                                                 "35-39", "40-44", "45-49")),
    marital = factor(marital, labels = c("Married", "Other")),
    mom_edu_year = as.numeric(mom_edu_year),
    mom_educ = factor(mom_educ, labels = c("No education", "Primary", "Secondary",
                                           "High school", "College")),
    wealth = factor(wealth, labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")),
    placeofres = factor(placeofres, labels = c("Urban", "Rural")),
    delivery = factor(delivery, labels = c("Normal", "C-Section")),
    birthorder = relevel(birthorder, "1st"),
    interval = relevel(interval, "1st"),
    ch_size_birth = relevel(ch_size_birth, "very small"),
    anc_cat3 = relevel(anc_cat3, "ANCless4"),
    anc_cat3_cut15 = relevel(anc_cat3_cut15, "ANCless4"),
    anc_cat3_cut12 = relevel(anc_cat3_cut12, "ANCless4"),
    mom_age_cat = relevel(mom_age_cat, "15-19"),
    mom_educ = relevel(mom_educ, "No education"),
    wealth = relevel(wealth, "Poorest")
  )



#----- Survey design
dhs_design <- svydesign(
  ids = ~id_com, 
  weights = ~wt, 
  data = df          
)


#----- Table 1: participant characteristics
outlist <- c("neonatal", "infant", "under5")

varlist <- c(
  "ch_sex", "birthorder", "interval",
  "delivery", "mom_age_cat", "marital", "mom_educ", "early_preg", "tetanus",
  "everbf", "wealth", "water", "sanit", "placeofres", "inst_skilled", "anc4"
)

varlab <- list(
  neonatal ~ "Neonatal mortality",
  infant ~ "Infant mortality",
  under5 ~ "Child under 5 mortality",
  ch_sex ~ "Child sex",
  birthorder ~ "Birth order",
  interval ~ "Birth interval",
  delivery ~ "Delivery mode",
  mom_age_cat ~ "Mother age",
  marital ~ "Maternal marital status",
  mom_educ ~ "Maternal education level",
  early_preg ~ "Early pregnancy",
  tetanus ~ "Tetanus vaccine",
  everbf ~ "Children who were ever breastfed",
  wealth ~ "Wealth index",
  water ~ "Improved source of drinking water",
  sanit ~ "Improved ssanitary facility",
  placeofres ~ "Place of residence",
  inst_skilled ~ "Institutional and skilled birth attendance",
  anc4 ~ "ANC visits at least 4 times"
)


table1_overall <- tbl_svysummary(
  dhs_design,
  include = c(outlist, varlist), 
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} ({p}%)"   
  ),
  label = varlab,
  missing = "ifany" 
)


table1_overall

# Save to doc
table1_overall |>
  as_flex_table() |>
  save_as_docx(path = "Results/Table1_overall.docx")




#----- Table 2: % of outcomes across characteristics
results_list <- list()

# Loop through each outcome variable
for (outcome in outlist) {
  for (var in varlist) {
    # Create a weighted table of proportions
    table <- svyby(
      as.formula(paste("~", outcome)), 
      as.formula(paste("~", var)), 
      dhs_design, 
      svymean, 
      na.rm = TRUE,
      vartype = "ci" 
    )
    
    # Add identifiers for the variable and outcome
    table <- table |>
      mutate(
        group_var = var,
        outcome = outcome
      )
    
    # Store the results
    results_list[[paste(outcome, var, sep = "_")]] <- table
  }
}

# Combine all results
results_df <- bind_rows(results_list)

# Clean a bit
results_df <- results_df |>
  mutate(across(where(is.numeric), ~ round(., 5))) |>
  select(group_var, outcome, neonatalYes, ci_l.neonatalYes, ci_u.neonatalYes,
         infantYes, ci_l.infantYes, ci_u.infantYes, 
         under5Yes, ci_l.under5Yes, ci_u.under5Yes)

# Save to excel
write.csv(results_df, "Results/percentages_by_varlist.csv", row.names = TRUE, na = "")




#----- Table 3: Logistic
logi_neo <- svyglm(neonatal ~ ch_sex + birthorder + delivery + 
                     mom_age_cat + marital + mom_educ + early_preg + 
                     wealth + water + sanit + placeofres + 
                     inst_skilled + anc4, design = dhs_design, family = quasibinomial)

logi_inf <- svyglm(infant ~ ch_sex + birthorder + delivery + 
                     mom_age_cat + marital + mom_educ + early_preg + 
                     wealth + water + sanit + placeofres + 
                     inst_skilled + anc4, design = dhs_design, family = quasibinomial)

logi_u5 <- svyglm(under5 ~ ch_sex + birthorder + delivery + 
                     mom_age_cat + marital + mom_educ + early_preg + 
                     wealth + water + sanit + placeofres + 
                     inst_skilled + anc4, design = dhs_design, family = quasibinomial)


summary(logi_neo)
summary(logi_inf)
summary(logi_u5)

coef_tab1 <- summary(logi_neo)$coefficients |>
  as.tibble(rownames = "Parameter")|>
  filter(Parameter != "(Intercept)")

coef_tab2 <- summary(logi_inf)$coefficients|>
  as.tibble(rownames = "Parameter")|>
  filter(Parameter != "(Intercept)")

coef_tab3 <- summary(logi_u5)$coefficients|>
  as.tibble(rownames = "Parameter")|>
  filter(Parameter != "(Intercept)")

coef_tab_all <- rbind(coef_tab1, coef_tab2, coef_tab3) 

coef_tab_all$model <- rep(c("NM", "IM", "U5M"), each = nrow(coef_tab1))

# Calculate OR and 95%CI
coef_tab_all <- coef_tab_all |>
  mutate(
    OR = exp(Estimate),
    lb = exp(Estimate - 1.96*`Std. Error`),
    ub = exp(Estimate + 1.96*`Std. Error`)
  )


# Forest plot
dodge <- position_dodge(width = 0.5)

coef_tab_all |>
  ggplot(aes(x = Parameter, y = OR, ymin = lb, ymax = ub, color = model)) + 
  geom_errorbar(width = 0.5, size = 1, position = dodge) +
  geom_point(size = 2, shape = 21, fill="white", position = dodge) +
  geom_hline(yintercept = 1, linetype = 2, col = "red", size = 1) +
  scale_color_brewer(palette = "Set1") +
  coord_flip() +
  theme_bw() + 
  theme(legend.position = "top") +
  labs(
    x = NULL,
    y = "OR (95% CI)",
    color = "Model"
  )

