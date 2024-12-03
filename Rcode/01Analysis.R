library(haven)
library(tidyverse)
library(survey)
library(gtsummary)
library(flextable)
library(forestplot)

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
#===============================================================================
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
#===============================================================================
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
#===============================================================================
#--- Neonatal
logi_neo_unadj <- svyglm(neonatal ~ wealth + placeofres + inst_skilled + anc4,
                         design = dhs_design, family = quasibinomial)

logi_neo_adj <- svyglm(neonatal ~  wealth + placeofres +  inst_skilled + anc4 + 
                         birthorder + interval + delivery + ch_sex +
                         mom_age_cat + marital + mom_educ + early_preg + 
                         water + sanit + country_name, design = dhs_design, family = quasibinomial)


summary(logi_neo_unadj)
summary(logi_neo_adj)


#--- Infant
logi_inf_unadj <- svyglm(infant ~ wealth + placeofres + inst_skilled + anc4,
                         design = dhs_design, family = quasibinomial)

logi_inf_adj <- svyglm(infant ~  wealth + placeofres +  inst_skilled + anc4 + 
                         birthorder + interval + delivery + ch_sex +
                         mom_age_cat + marital + mom_educ + early_preg + 
                         water + sanit + country_name, design = dhs_design, family = quasibinomial)


summary(logi_inf_unadj)
summary(logi_inf_adj)



#--- U5MR
logi_u5_unadj <- svyglm(under5 ~ wealth + placeofres + inst_skilled + anc4,
                         design = dhs_design, family = quasibinomial)

logi_u5_adj <- svyglm(under5 ~  wealth + placeofres +  inst_skilled + anc4 + 
                         birthorder + interval + delivery + ch_sex +
                         mom_age_cat + marital + mom_educ + early_preg + 
                         water + sanit + country_name, design = dhs_design, family = quasibinomial)


summary(logi_u5_unadj)
summary(logi_u5_adj)



#----- Function to extract results
myresultExtract <- function(modelA, modelB, name) {
  modelA <- summary(modelA)$coefficients |>
    as.tibble(rownames = "Parameter") |>
    filter(Parameter != "(Intercept)") |>
    mutate(model = "Reduced")
  
  modelB <- summary(modelB)$coefficients |>
    as.tibble(rownames = "Parameter") |>
    filter(Parameter != "(Intercept)") |>
    mutate(model = "Fully adjusted")
  
  coef_all <- rbind(modelA, modelB) |>
    mutate(
      OR = round(exp(Estimate), 2),
      lb = round(exp(Estimate - 1.96*`Std. Error`), 2),
      ub = round(exp(Estimate + 1.96*`Std. Error`), 2),
      display = paste0(OR, " (", lb, "-", ub, ")")
    )
  # Save to excel
  path <- paste0("Results/coef_all_", name, ".csv")
  write.csv(coef_all, path, row.names = FALSE, na = "")
}


myresultExtract(modelA = logi_neo_unadj, modelB = logi_neo_adj, name = "neo")
myresultExtract(modelA = logi_inf_unadj, modelB = logi_inf_adj, name = "inf")
myresultExtract(modelA = logi_u5_unadj, modelB = logi_u5_adj, name = "u5")


#----- Function to create forest plot
myforestPlot <- function(data) {
  coef_unadj <- data |> filter(model == "Reduced")
  coef_adj <- data |> filter(model == "Fully adjusted")
  
  tabletext <- cbind(c("",coef_unadj$Parameter), 
                     c("Reduced",coef_unadj$display),
                     c("Fully adjusted",coef_adj$display))
  
  forestplot(labeltext=tabletext, 
             graph.pos=2, 
             clip=c(0.6, 1.5), 
             xticks = c(0.6, 0.8, 1, 1.2, 1.5), 
             xlog=T, 
             mean=cbind(c(NA, coef_unadj$OR),c(NA, coef_adj$OR)), 
             lower=cbind(c(NA, coef_unadj$lb),c(NA, coef_adj$lb)), 
             upper=cbind(c(NA, coef_unadj$ub),c(NA, coef_adj$ub)), 
             is.summary=c(T, 
                          T,rep(FALSE,1),
                          T,rep(FALSE,1),
                          T,rep(FALSE,1),
                          T,rep(FALSE,4)),
             fn.ci_norm = c(fpDrawNormalCI, fpDrawCircleCI), 
             txt_gp=fpTxtGp(label=gpar(cex = 1), 
                            summary = list(gpar(cex=1,fontface=2), gpar(cex=1,fontface=1), gpar(cex=1,fontface=1)), 
                            ticks=gpar(cex = 1),
                            xlab=gpar(cex = 1), 
                            title=gpar(cex = 1)), 
             col=fpColors(box=c("#225ea8", "#d7301f"), lines=c("#225ea8", "#d7301f"), zero = "gray50"),
             lty.ci = c(2,1), 
             zero=1, 
             cex=1, 
             lineheight = "auto", 
             line.margin = 0.15,
             colgap=unit(5,"mm"),
             legend = c("Reduced", "Fully adjusted"),
             legend_args = fpLegend(title = NULL,
                                    pos = "top"),
             boxsize=0.3, 
             lwd.ci=1.5, 
             ci.vertices=TRUE,
             ci.vertices.height = 0.1, 
             xlab = "OR (95% CI)")
  
}


coef_neo_all <- read_csv("Results/coef_all_neo1.csv")
coef_inf_all <- read_csv("Results/coef_all_inf1.csv")
coef_u5_all <- read_csv("Results/coef_all_u51.csv")


png("Results/Figure3a_neo.png", units="in", width = 12, height = 6, res = 300)
myforestPlot(data = coef_neo_all)
dev.off() 


png("Results/Figure3b_inf.png", units="in", width = 12, height = 6, res = 300)
myforestPlot(data = coef_inf_all)
dev.off() 

png("Results/Figure3c_u5.png", units="in", width = 12, height = 6, res = 300)
myforestPlot(data = coef_u5_all)
dev.off() 

