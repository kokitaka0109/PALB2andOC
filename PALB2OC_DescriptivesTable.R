# Load necessary packages
library(dplyr)
library(knitr)
library(kableExtra)

# === Compute summary statistics from your data ===
load("/Users/nicolaskubista/Partners HealthCare Dropbox/BayesMendel/Projects/Active/NK_PenetranceEstimation/3_Code/CCGCRN Data Application/PALB2/PALB2_carrier_families.RData")
dat <- bind_rows(carrier_families_list, .id="id")

# Subset the data to include only Hispanic individuals
dat_hispanic <- dat %>% filter(isHispanic == 1)

### Compute Overall Statistics ###

# Number of probands (using distinct PedigreeID for probands)
num_probands_overall <- dat %>% 
  filter(isProband == 1) %>% 
  distinct(PedigreeID) %>% 
  nrow()

# Family sizes: mean and range (min -- max)
family_sizes_overall <- dat %>% 
  group_by(PedigreeID) %>% 
  summarise(n = n())
mean_family_size_overall <- round(mean(family_sizes_overall$n), 0)
min_family_size_overall <- min(family_sizes_overall$n)
max_family_size_overall <- max(family_sizes_overall$n)

# Total number of individuals
total_individuals_overall <- nrow(dat)

# Count of females and males (assume Sex: 0 = Female, 1 = Male)
num_females_overall <- sum(dat$Sex == 0, na.rm = TRUE)
num_males_overall   <- sum(dat$Sex == 1, na.rm = TRUE)

# Genotyping for PALB2 (assume genotyped if PALB2 is 0 or 1)
total_genotyped_overall <- dat %>% 
  filter(PALB2 %in% c(0,1)) %>% 
  nrow()

# For probands (overall)
probands_genotyped_overall <- dat %>% 
  filter(isProband == 1, PALB2 %in% c(0,1)) %>% 
  nrow()
probands_carriers_overall <- dat %>% 
  filter(isProband == 1, PALB2 == 1)
num_probands_carriers_overall       <- nrow(probands_carriers_overall)
num_probands_carriers_female_overall <- sum(probands_carriers_overall$Sex == 0, na.rm = TRUE)
num_probands_carriers_male_overall   <- sum(probands_carriers_overall$Sex == 1, na.rm = TRUE)

# For relatives (non-probands, overall)
relatives_genotyped_overall <- dat %>% 
  filter(isProband == 0, PALB2 %in% c(0,1)) %>% 
  nrow()
relatives_carriers_overall <- dat %>% 
  filter(isProband == 0, PALB2 == 1)
num_relatives_carriers_female_overall <- sum(relatives_carriers_overall$Sex == 0, na.rm = TRUE)
num_relatives_carriers_male_overall   <- sum(relatives_carriers_overall$Sex == 1, na.rm = TRUE)

# Total PALB2 PGV carriers (overall)
total_carriers_overall <- dat %>% 
  filter(PALB2 == 1, PALB2 %in% c(0,1)) %>% 
  nrow()

# Ovarian Cancer Cases (overall)
ovarian_cases_total_overall    <- dat %>% filter(isAffOC == 1) %>% nrow()
ovarian_cases_probands_overall <- dat %>% filter(isAffOC == 1, isProband == 1, Sex == 0) %>% nrow()
ovarian_cases_relatives_overall<- dat %>% filter(isAffOC == 1, isProband == 0, Sex == 0) %>% nrow()

# New: Denominators for OC percentages (only females can have OC)
num_probands_female_overall <- dat %>% filter(isProband == 1, Sex == 0) %>% nrow()
num_relatives_female_overall <- dat %>% filter(isProband == 0, Sex == 0) %>% nrow()

# Age at Diagnosis for ovarian cancer (overall)
oc_ages_overall    <- dat %>% filter(isAffOC == 1) %>% pull(AgeOC)
oc_age_median_overall <- median(oc_ages_overall, na.rm = TRUE)
oc_age_min_overall    <- min(oc_ages_overall, na.rm = TRUE)
oc_age_max_overall    <- max(oc_ages_overall, na.rm = TRUE)


### Compute Hispanic-Only Statistics ###

# Number of probands among Hispanics
num_probands_hisp <- dat_hispanic %>% 
  filter(isProband == 1) %>% 
  distinct(PedigreeID) %>% 
  nrow()

# Family sizes among Hispanics: mean and range
family_sizes_hisp <- dat_hispanic %>% 
  group_by(PedigreeID) %>% 
  summarise(n = n())
mean_family_size_hisp <- if(nrow(family_sizes_hisp) > 0) round(mean(family_sizes_hisp$n), 0) else NA
min_family_size_hisp <- if(nrow(family_sizes_hisp) > 0) min(family_sizes_hisp$n) else NA
max_family_size_hisp <- if(nrow(family_sizes_hisp) > 0) max(family_sizes_hisp$n) else NA

# Total number of individuals among Hispanics
total_individuals_hisp <- nrow(dat_hispanic)

# Count of females and males among Hispanics
num_females_hisp <- sum(dat_hispanic$Sex == 0, na.rm = TRUE)
num_males_hisp   <- sum(dat_hispanic$Sex == 1, na.rm = TRUE)

# Genotyping for PALB2 (Hispanics)
total_genotyped_hisp <- dat_hispanic %>% 
  filter(PALB2 %in% c(0,1)) %>% 
  nrow()

# For probands (Hispanics)
probands_genotyped_hisp <- dat_hispanic %>% 
  filter(isProband == 1, PALB2 %in% c(0,1)) %>% 
  nrow()
probands_carriers_hisp <- dat_hispanic %>% 
  filter(isProband == 1, PALB2 == 1)
num_probands_carriers_hisp       <- nrow(probands_carriers_hisp)
num_probands_carriers_female_hisp <- sum(probands_carriers_hisp$Sex == 0, na.rm = TRUE)
num_probands_carriers_male_hisp   <- sum(probands_carriers_hisp$Sex == 1, na.rm = TRUE)

# For relatives (Hispanics)
relatives_genotyped_hisp <- dat_hispanic %>% 
  filter(isProband == 0, PALB2 %in% c(0,1)) %>% 
  nrow()
relatives_carriers_hisp <- dat_hispanic %>% 
  filter(isProband == 0, PALB2 == 1)
num_relatives_carriers_female_hisp <- sum(relatives_carriers_hisp$Sex == 0, na.rm = TRUE)
num_relatives_carriers_male_hisp   <- sum(relatives_carriers_hisp$Sex == 1, na.rm = TRUE)

# Total PALB2 PGV carriers (Hispanics)
total_carriers_hisp <- dat_hispanic %>% 
  filter(PALB2 == 1, PALB2 %in% c(0,1)) %>% 
  nrow()

# Ovarian Cancer Cases (Hispanics)
ovarian_cases_total_hisp    <- dat_hispanic %>% filter(isAffOC == 1) %>% nrow()
ovarian_cases_probands_hisp <- dat_hispanic %>% filter(isAffOC == 1, isProband == 1, Sex == 0) %>% nrow()
ovarian_cases_relatives_hisp<- dat_hispanic %>% filter(isAffOC == 1, isProband == 0, Sex == 0) %>% nrow()

# New: Denominators for OC percentages (Hispanic)
num_probands_female_hisp <- dat_hispanic %>% filter(isProband == 1, Sex == 0) %>% nrow()
num_relatives_female_hisp <- dat_hispanic %>% filter(isProband == 0, Sex == 0) %>% nrow()

# Age at Diagnosis for ovarian cancer (Hispanics)
oc_ages_hisp    <- dat_hispanic %>% filter(isAffOC == 1) %>% pull(AgeOC)
oc_age_median_hisp <- if(length(oc_ages_hisp) > 0) median(oc_ages_hisp, na.rm = TRUE) else NA
oc_age_min_hisp    <- if(length(oc_ages_hisp) > 0) min(oc_ages_hisp, na.rm = TRUE) else NA
oc_age_max_hisp    <- if(length(oc_ages_hisp) > 0) max(oc_ages_hisp, na.rm = TRUE) else NA


### Create the Table Data Frame with an Additional Hispanic Column ###

table_data <- data.frame(
  Characteristic = c(
    "Number of Probands/Pedigrees",
    "Family Size, Mean (Range)",
    "Total Number of Individuals",
    " Females",
    " Males",
    "Total Genotyped for \\textit{PALB2}",
    " Probands",
    "  Females",
    "  Males",
    " Relatives",
    "  Females",
    "  Males",
    " \\textit{PALB2} PGV Carriers",
    "Ovarian Cancer Cases Total",
    " Probands (female)$^a$",
    " Relatives (female)$^b$",
    "Age at Diagnosis, Median (Range)"
  ),
  Overall = c(
    num_probands_overall,
    paste0(mean_family_size_overall, " (", min_family_size_overall, "--", max_family_size_overall, ")"),
    paste0(total_individuals_overall, " (100.0)"),
    paste0(num_females_overall, " (", round(num_females_overall / total_individuals_overall * 100, 1), ")"),
    paste0(num_males_overall, " (", round(num_males_overall / total_individuals_overall * 100, 1), ")"),
    paste0(total_genotyped_overall, " (", round(total_genotyped_overall / total_individuals_overall * 100, 1), ")"),
    paste0(probands_genotyped_overall, " (", round(probands_genotyped_overall / total_genotyped_overall * 100, 1), ")"),
    paste0(num_probands_carriers_female_overall, " (", round(num_probands_carriers_female_overall / probands_genotyped_overall * 100, 1), ")"),
    paste0(num_probands_carriers_male_overall, " (", round(num_probands_carriers_male_overall / probands_genotyped_overall * 100, 1), ")"),
    paste0(relatives_genotyped_overall, " (", round(relatives_genotyped_overall / total_genotyped_overall * 100, 1), ")"),
    paste0(num_relatives_carriers_female_overall, " (", round(num_relatives_carriers_female_overall / relatives_genotyped_overall * 100, 1), ")"),
    paste0(num_relatives_carriers_male_overall, " (", round(num_relatives_carriers_male_overall / relatives_genotyped_overall * 100, 1), ")"),
    paste0(total_carriers_overall, " (", round(total_carriers_overall / total_genotyped_overall * 100, 1), ")"),
    ovarian_cases_total_overall,
    paste0(ovarian_cases_probands_overall, " (", round(ovarian_cases_probands_overall / num_probands_female_overall * 100, 1), ")"),
    paste0(ovarian_cases_relatives_overall, " (", round(ovarian_cases_relatives_overall / num_relatives_female_overall * 100, 1), ")"),
    paste0(oc_age_median_overall, " (", oc_age_min_overall, "--", oc_age_max_overall, ")")
  ),
  Hispanic = c(
    num_probands_hisp,
    paste0(mean_family_size_hisp, " (", min_family_size_hisp, "--", max_family_size_hisp, ")"),
    paste0(total_individuals_hisp, " (100.0)"),
    paste0(num_females_hisp, " (", round(num_females_hisp / total_individuals_hisp * 100, 1), ")"),
    paste0(num_males_hisp, " (", round(num_males_hisp / total_individuals_hisp * 100, 1), ")"),
    paste0(total_genotyped_hisp, " (", round(total_genotyped_hisp / total_individuals_hisp * 100, 1), ")"),
    paste0(probands_genotyped_hisp, " (", round(probands_genotyped_hisp / total_genotyped_hisp * 100, 1), ")"),
    paste0(num_probands_carriers_female_hisp, " (", round(num_probands_carriers_female_hisp / probands_genotyped_hisp * 100, 1), ")"),
    paste0(num_probands_carriers_male_hisp, " (", round(num_probands_carriers_male_hisp / probands_genotyped_hisp * 100, 1), ")"),
    paste0(relatives_genotyped_hisp, " (", round(relatives_genotyped_hisp / total_genotyped_hisp * 100, 1), ")"),
    paste0(num_relatives_carriers_female_hisp, " (", round(num_relatives_carriers_female_hisp / relatives_genotyped_hisp * 100, 1), ")"),
    paste0(num_relatives_carriers_male_hisp, " (", round(num_relatives_carriers_male_hisp / relatives_genotyped_hisp * 100, 1), ")"),
    paste0(total_carriers_hisp, " (", round(total_carriers_hisp / total_genotyped_hisp * 100, 1), ")"),
    ovarian_cases_total_hisp,
    paste0(ovarian_cases_probands_hisp, " (", round(ovarian_cases_probands_hisp / num_probands_female_hisp * 100, 1), ")"),
    paste0(ovarian_cases_relatives_hisp, " (", round(ovarian_cases_relatives_hisp / num_relatives_female_hisp * 100, 1), ")"),
    paste0(oc_age_median_hisp, " (", oc_age_min_hisp, "--", oc_age_max_hisp, ")")
  ),
  stringsAsFactors = FALSE
)

# Print the table in LaTeX format with two columns: Overall and Hispanic
kable(table_data,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      col.names = c("Characteristic", "Overall Count (\\%)", "Hispanic Count (\\%)"),
      caption = "Demographic and Clinical Characteristics of the CCGCRN Dataset (Overall and Hispanic)") %>%
  kable_styling(latex_options = c("hold_position"))
