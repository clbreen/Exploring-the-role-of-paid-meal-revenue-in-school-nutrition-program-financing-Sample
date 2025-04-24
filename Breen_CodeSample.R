### Claire Breen
### April 23, 2025
### Code Sample for DSHS RDA Researcher Position

### Note: READ_ME File contains more detailed explanation of data files, code, and output

### BEGIN CODE SAMPLE ------------------------------------------------------------------
## Load libraries ----------------------------------------------------------------------
library(tidyverse) 
library(ineq.2d)
library(did)

# Create output directories -----------------------------------------------------------
ifelse(!dir.exists("Cleaned Data"), dir.create("Cleaned Data"), "Folder exists already")
ifelse(!dir.exists("Plots"), dir.create("Plots"), "Folder exists already")

## Load 12 years of LEA fiscal data ---------------------------------------------------
file_names <- list.files(path = "Data/", pattern = "*LEA_Sample.csv")
files <- lapply(paste0("Data/",file_names), read.csv)

## Clean fiscal data based on Murray et al. methods -----------------------------------
# Find basic descriptive stats and frequencies for each df
lapply(files, 
       function(x) {x %>% 
           summary}) # Note: There are missing observations in some variables of interest

# Remove LEAs with missing IDs or missing data on student membership, # operational schools, revenues of interest, and HI
# Remove LEAs with zero student enrollment, non-unified LEAs, no sources of school nutrition revenue
all_yrs_data <- lapply(files, 
       function(x) {x %>% 
           filter(!is.na(LEAID)) %>% 
           filter(!is.na(V33)) %>%
           filter(OPERATIONAL_SCHOOLS >= 0) %>% # Note: Zero operational schools are supervisory units that report membership 
           filter(C25 >= 0) %>% #
           filter(C10 >= 0) %>% 
           filter(A09 >= 0) %>% 
           filter(V33 > 0) %>%
           filter(FIPST != 15) %>% # Note: HI has only one statewide LEA, no variation within state
           filter(as.character(SCHLEV) == "03" | as.character(SCHLEV) == "3") %>% # Note: Unified LEAs include upper and lower grades
           filter(C25 + C10 + A09 != 0)}) # Note: Must have at lease one source of child nutrition revenue

# Create new variables for per-pupil revenues of interest
all_yrs_data <- lapply(all_yrs_data,
                       function(x) {x %>% 
                           mutate (A09_PUPIL = A09/V33,
                                   C10_PUPIL = C10/V33,
                                   C25_PUPIL = C25/V33,
                                   ALL_PUPIL = A09/V33 + C10/V33 + C25/V33)})

# Convert to 2012 dollars
all_yrs_data <- lapply(all_yrs_data,
                       function(x) {data.frame(x) %>% 
                           mutate(PRICE_INDEX = case_when(YEAR == 7 ~ 0.874956422, 
                                                          YEAR == 8 ~ 0.857670688, 
                                                          YEAR == 9 ~ 0.891490646,
                                                          YEAR == 10 ~ 0.924117348,
                                                          YEAR == 11 ~ 0.962873955,
                                                          YEAR == 12 ~ 1,
                                                          YEAR == 13 ~ 1.043093932,
                                                          YEAR == 14 ~ 1.083784288,
                                                          YEAR == 15 ~ 1.113990056,
                                                          YEAR == 16 ~ 1.161807375,
                                                          YEAR == 17 ~ 1.223677739,
                                                          YEAR == 18 ~ 1.274912573))%>%
                           mutate(A09 = A09/PRICE_INDEX,
                                  C10 = C10/PRICE_INDEX,
                                  C25 = C25/PRICE_INDEX,
                                  A09_PUPIL = A09_PUPIL/PRICE_INDEX,
                                  C10_PUPIL = C10_PUPIL/PRICE_INDEX,
                                  C25_PUPIL = C25_PUPIL/PRICE_INDEX,
                                  ALL_PUPIL)})

# Remove extreme values based on Murray et al. methods
all_yrs_data <- lapply(all_yrs_data,
                       function(x) {x %>%
                           filter(C25_PUPIL <= quantile(C25_PUPIL, 0.95) * 1.50) %>%
                           filter(C25_PUPIL >= quantile(C25_PUPIL, 0.05) * 0.50) %>%
                           filter(C10_PUPIL <= quantile(C10_PUPIL, 0.95) * 1.50) %>%
                           filter(C10_PUPIL >= quantile(C10_PUPIL, 0.05) * 0.50) %>%
                           filter(A09_PUPIL <= quantile(A09_PUPIL, 0.95) * 1.50) %>%
                           filter(A09_PUPIL >= quantile(A09_PUPIL, 0.05) * 0.50)})

# Save cleaned dfs
mapply(write.csv, all_yrs_data, paste0("Cleaned Data/Cleaned_", file_names))
       
## Create table of descriptive statistics ----------------------------------------------
# Create summary table by fiscal year
all_yrs_df <-as.data.frame(do.call (rbind, all_yrs_data))
descriptives <- all_yrs_df %>%
  group_by(YEAR) %>%
  summarize(mean_A09 = mean(A09_PUPIL, na.rm =TRUE),
            median_A09 = median(A09_PUPIL, na.rm=TRUE),
            mean_C10 = mean(C10_PUPIL, na.rm=TRUE),
            median_C10 = median(C10_PUPIL, na.rm=TRUE),
            mean_C25 = mean(C25_PUPIL, na.rm=TRUE),
            median_C25 = median(C25_PUPIL, na.rm=TRUE),
            unique_LEAID = length(unique(LEAID)),
            mean_V33 = mean(V33, na.rm =TRUE),
            mean_OPERATIONALSCHOOLS = mean(OPERATIONAL_SCHOOLS, na.rm =TRUE)
  )
descriptives

## View distribution of main outcome of interest ---------------------------------------
# Create histogram of paid meal revenue per pupil
ggplot(all_yrs_df, aes(A09_PUPIL)) +
  geom_histogram(color = "#51C3CC", fill = "#91cfd4" , binwidth = 50)+
  labs(title="Distribution of Paid Meal Revenue per Pupil",x="Paid Meal Revenue per Pupil", y = "Count")

# Save histogram
ggsave("Plots/Histogram_PaidMeal.png", plot=last_plot())

# Create histogram of paid meal revenue per pupil by urbanicity
ggplot(data=subset(all_yrs_df, !is.na(URBANICITY)), aes(x=A09_PUPIL, color = URBANICITY, fill=URBANICITY)) +
  geom_histogram(position="dodge") +
  theme_minimal()+theme_bw()+
  labs(title="Distribution of Paid Meal Revenue per Pupil by Urbanicity",x="Paid Meal Revenue per Pupil", y = "Count")+
  scale_color_manual(values=c("#51C3CC", "#F76D5E", "palegreen4"))+
  scale_fill_manual(values=c("#91cfd4", "#f3bdb7", "palegreen3"))

# Save histogram
ggsave("Plots/Histogram_PaidMeal_ByUrbanicity.png", plot=last_plot())

## Calculate Paid Meal Revenue Theil Index ---------------------------------------------
# Create df with only selected variables
all_yrs_A09_data <- lapply(all_yrs_data,
                                function(x) {x %>%
                                    select(LEAID,
                                           A09_PUPIL,
                                           FIPST,
                                           V33)})

# Calculate decomposed Theil Index for Paid Meal Revenue by state
decomp_theil_A09 <- lapply(all_yrs_A09_data,
                           function(x) {theil.2d(x,
                                                 "A09_PUPIL",
                                                 "FIPST",
                                                 "A09_PUPIL",
                                                 "V33")})

# Aggregate state Theil Indices into total between and within state inequality
decomp_theil_A09 <- lapply(decomp_theil_A09,
                           function(x) {x %>%
                               mutate(SUM_W = across(ends_with(".W")) %>% 
                                        rowSums) %>% 
                               mutate(SUM_B = across(ends_with(".B")) %>% 
                                        rowSums)})

## Create Paid Meal Revenue Inequality Figure ------------------------------------------
# Create df of Paid Meal Revenue Theil Index Decomposition, including between and within state inequality
all_years_theil_A09 <- lapply(decomp_theil_A09,
                           function(x) {x %>%
                              select(SUM_B,
                                     SUM_W)})
all_years_theil_A09 <-as.data.frame(do.call (rbind, all_years_theil_A09))
all_years_theil_A09 <-all_years_theil_A09 %>% add_column(YEAR = c(7:18))

ggplot(all_years_theil_A09, aes(as.numeric(YEAR))) + 
  geom_line(aes(y = as.numeric(SUM_B), colour = "Between States Theil")) + 
  geom_point(aes(y=as.numeric(SUM_B))) +
  geom_line(aes(y = as.numeric(SUM_W), colour = "Within States Theil")) + 
  geom_point(aes(y=as.numeric(SUM_W))) +
  ylab("Paid Meal Revenue Theil Index") +
  xlab("School Year") +
  scale_color_manual(values = c("Between States Theil"="#F76D5E", "Within States Theil" = "#51C3CC")) +
  theme(legend.position="bottom") +
  theme_bw()

# Save Inequality Graph
ggsave("Plots/Graph_PaidMeal_TheilIndex.png", plot=last_plot())

## Begin Causal Inference --------------------------------------------------------------
# Load Community Eligibility Provision Treatment Data
cep_data <- read_csv("Data/Data_LEA_CEP_Sample.csv")

## Create table of descriptive statistics ----------------------------------------------
descriptives <- cep_data %>%
  group_by(YEAR) %>% # Note: Staggered adoption, so all groups treated. Descriptive stats by yr treated
  summarize(mean_A09 = mean(A09_PUPIL, na.rm =TRUE),
            mean_C25 = mean(C25_PUPIL, na.rm =TRUE),
            mean_C10 = mean(C10_PUPIL, na.rm =TRUE),
            mean_Total = mean(ALL_PUPIL, na.rm =TRUE),
            mean_V33 = mean(V33, na.rm = TRUE),
            mean_BL = mean (BL_PCT, na.rm = TRUE),
            mean_WH = mean (WH_PCT, na.rm = TRUE),
            mean_HI = mean (HI_PCT, na.rm = TRUE),
            mean_CHARTER = mean (CHARTER_PCT, na.rm = TRUE),
            mean_TOTFRL = mean (TOTFRL_PCT, na.rm = TRUE),
            mean_TITLEI = mean (TITLEI_PCT, na.rm = TRUE),
            mean_SCHOOLS = mean (OPERATIONAL_SCHOOLS, na.rm = TRUE))
descriptives

## Callaway & Sant'Anna Staggered Adoption Event Study ---------------------------------
model <- att_gt(yname = "A09_PUPIL",
                gname = "TREATED",
                idname = "LEAID",
                tname = "YEAR",
                xformla = ~ 
                  TITLEI_PCT +
                  TOTFRL_PCT +
                  CHARTER_PCT +
                  BL_PCT +
                  HI_PCT,
                control_group = "notyettreated",
                data = cep_data,
                est_method = "reg",
                clustervars = "LEAID")
es_full <- aggte(model, type = "dynamic",  max_e = 2, na.rm = TRUE)
ggdid(es_full,
      xlab = "Years since CEP adoption",
      ylab = "Estimate and 95% Conf. Int.",
      title = "Event Study: Effect of CEP adoption on Paid Meal Revenue Per Pupil") +
      ylim (-30, 70)
summary(es_full)

# Save Inequality Graph
ggsave("Plots/EventStudy_PaidMeal_Full.png", plot=last_plot())

## Explore effects by urbanicity -------------------------------------------------------
# Urban LEAs (Panel A)
cep_data_urban <-  filter(cep_data, URBANICITY == "URBAN")
model <- att_gt(yname = "A09_PUPIL",
                gname = "TREATED",
                idname = "LEAID",
                tname = "YEAR",
                xformla = ~ 
                  TITLEI_PCT +
                  TOTFRL_PCT +
                  CHARTER_PCT +
                  BL_PCT +
                  HI_PCT,
                control_group = "notyettreated",
                data = cep_data_urban,
                est_method = "reg",
                clustervars = "LEAID")
es_urban <- aggte(model, type = "dynamic",  max_e = 2, na.rm = TRUE)
ggdid(es_urban,
      xlab = "Years since CEP adoption",
      ylab = "Estimate and 95% Conf. Int.",
      title = "Event Study: Effect of CEP adoption on Paid Meal Revenue Per Pupil among Urban LEAs") + 
      ylim (-100, 125)
summary(es_urban)

# Save Inequality Graph
ggsave("Plots/EventStudy_PaidMeal_Urban.png", plot=last_plot())

# Town LEAs (Panel B)
cep_data_town <-  filter(cep_data, URBANICITY == "TOWN")
model <- att_gt(yname = "A09_PUPIL",
                gname = "TREATED",
                idname = "LEAID",
                tname = "YEAR",
                xformla = ~ 
                  TITLEI_PCT +
                  TOTFRL_PCT +
                  CHARTER_PCT +
                  BL_PCT +
                  HI_PCT,
                control_group = "notyettreated",
                data = cep_data_town,
                est_method = "reg",
                clustervars = "LEAID")
es_town <- aggte(model, type = "dynamic",  max_e = 2, na.rm = TRUE)
ggdid(es_town,
      xlab = "Years since CEP adoption",
      ylab = "Estimate and 95% Conf. Int.",
      title = "Event Study: Effect of CEP adoption on Paid Meal Revenue Per Pupil among Town LEAs") + 
      ylim (-100, 125)
summary(es_town)

# Save Inequality Graph
ggsave("Plots/EventStudy_PaidMeal_Town.png", plot=last_plot())

# Rural LEAs (Panel C)
cep_data_rural <-  filter(cep_data, URBANICITY == "RURAL")
model <- att_gt(yname = "A09_PUPIL",
                gname = "TREATED",
                idname = "LEAID",
                tname = "YEAR",
                xformla = ~ 
                  TITLEI_PCT +
                  TOTFRL_PCT +
                  CHARTER_PCT +
                  BL_PCT +
                  HI_PCT,
                control_group = "notyettreated",
                data = cep_data_rural,
                est_method = "reg",
                clustervars = "LEAID"
)
es_rural <- aggte(model, type = "dynamic",  max_e = 2, na.rm = TRUE)
ggdid(es_rural,
      xlab = "Years since CEP adoption",
      ylab = "Estimate and 95% Conf. Int.",
      title = "Event Study: Effect of CEP adoption on Paid Meal Revenue Per Pupil among Rural LEAs") + 
      ylim (-100, 125)
summary(es_rural)

# Save Inequality Graph
ggsave("Plots/EventStudy_PaidMeal_Rural.png", plot=last_plot())

### END CODE SAMPLE --------------------------------------------------------------------