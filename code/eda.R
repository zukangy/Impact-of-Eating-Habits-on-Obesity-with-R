# install.packages(c("caret"))

library(here)
library(tidyverse)
library(caret)
library(ggplot2)
library(ggpubr) #requires tidyr 1.3.0

# Uncomment the next two lines for Windows system
setwd('/Users/Chris/Desktop/lab 2/lab-2-lab2-demaio-huang-yang')
getwd()

raw_df <- read.csv("./data/raw/ObesityDataSet_raw_and_data_sinthetic.csv")

# adding in BMI for Y value
raw_df <- raw_df %>%
  mutate(BMI = Weight / Height^2)

raw_df$NObeyesdad <- NULL

# renaming columns
new_col_names <- c(high_cal_consum = 'FAVC', freq_veg='FCVC',
                   num_main_meals = 'NCP', food_between_meals = 'CAEC',
                   water_consum = 'CH2O', alcohol_consum = 'CALC',
                   cal_consum = 'SCC', phy_act = 'FAF', tech_use = 'TUE',
                   transpo_use = 'MTRANS')

raw_df <- rename(raw_df, all_of(new_col_names))

# View the data
glimpse(raw_df)

# Rounding via mutate
raw_df <- raw_df %>%
  mutate(freq_veg = round(freq_veg))

raw_df <- raw_df %>%
  mutate(num_main_meals = round(num_main_meals))

raw_df <- raw_df %>%
  mutate(water_consum = round(water_consum))

raw_df <- raw_df %>%
  mutate(phy_act = round(phy_act))

raw_df <- raw_df %>%
  mutate(tech_use = round(tech_use))

# Convert alcohol consumption into binary
raw_df$alcohol_consum <- ifelse(raw_df$alcohol_consum %in% c("Sometimes", "Frequently", "Always"), 1, 0)

# Convert water consumption into binary 
raw_df$water_consum <- ifelse(raw_df$water_consum %in% c(1, 2), 1, 0)

# Combine always and frequently as one level in food_between_meal
raw_df <- raw_df %>% mutate(food_between_meals=case_when(
    food_between_meals %in% c("Frequently", "Always") ~ "Frequently",
    food_between_meals %in% c("Sometimes") ~ "Sometimes",
    food_between_meals %in% c("no") ~ "No"
  ))

# set likert values to factors
raw_df$Gender <- as.factor(raw_df$Gender)
raw_df$family_history_with_overweight <- as.factor(raw_df$family_history_with_overweight)
raw_df$high_cal_consum <- as.factor(raw_df$high_cal_consum)
raw_df$SMOKE <- as.factor(raw_df$SMOKE)
raw_df$cal_consum <- as.factor(raw_df$cal_consum)
raw_df$freq_veg <- as.factor(raw_df$freq_veg)
raw_df$num_main_meals <- as.factor(raw_df$num_main_meals)
raw_df$water_consum <- as.factor(raw_df$water_consum)
raw_df$phy_act <- as.factor(raw_df$phy_act)
raw_df$tech_use <- as.factor(raw_df$tech_use)
raw_df$transpo_use <- as.factor(raw_df$transpo_use)
raw_df$alcohol_consum <- as.factor(raw_df$alcohol_consum)
raw_df$food_between_meals <- as.factor(raw_df$food_between_meals)

# Remove height and weight
raw_df$Height <- NULL
raw_df$Weight <- NULL

raw_df$cal_consum <- NULL

colnames(raw_df)
glimpse(raw_df)

# Set the seed for reproducibility
set.seed(1)

split_idx <- createDataPartition(raw_df$BMI, p=0.7, list=FALSE)
confirm_set <- raw_df[split_idx, ]
explore_set <- raw_df[-split_idx, ]

# Exploration
weight_class <- explore_set$weight_class

full_mod <- lm(BMI ~ ., data=explore_set)
summary(full_mod)

#---
#full_mod_1 <- lm(BMI ~ . + I(Age**2), data=explore_set)
#summary(full_mod_1)
#---

main_mod <- lm(BMI ~ high_cal_consum + freq_veg + num_main_meals + food_between_meals +
                 alcohol_consum + water_consum, data=explore_set)
summary(main_mod)

anova(full_mod, main_mod)

mod_no_physical_act <- lm(BMI ~ . - phy_act, data=explore_set)
anova(mod_no_physical_act, full_mod) # remove physical act

mod_no_water_consum <- lm(BMI ~ . - water_consum, data=explore_set)
anova(mod_no_water_consum, full_mod) # keep water consumption

mod_no_veg <- lm(BMI ~ . - freq_veg, data=explore_set)
anova(mod_no_veg, full_mod) # keep veg

mod_no_food_bt <- lm(BMI ~ . - food_between_meals, data=explore_set)
anova(mod_no_food_bt, full_mod) # keep food between meals

mod_no_smoke <- lm(BMI ~ . - SMOKE, data=explore_set)
anova(mod_no_smoke, full_mod) # remove smoke

mod_no_tech <- lm(BMI ~ . - tech_use, data=explore_set)
anova(mod_no_tech, full_mod) # keep tech use

mod_no_transpo <- lm(BMI ~ . - transpo_use, data=explore_set)
anova(mod_no_transpo, full_mod)  # remove transpo_use

confirm_set$transpo_use <- NULL
confirm_set$phy_act <- NULL
confirm_set$SMOKE <- NULL
confirm_set$transpo_use <- NULL

#split sets by gender for plots
confirm_set_female <- confirm_set %>%
  filter(Gender == 'Female')

confirm_set_male <- confirm_set %>%
  filter(Gender == 'Male')


female_plt <- ggplot(confirm_set_female, aes(y=BMI, x=Age, colour=high_cal_consum)) +
  geom_point(shape=16, size=1.2) +
  geom_smooth(se=FALSE) +
  ggtitle("Female") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c('#FFBC79', '#1170AA'),
                     name = "Frequently Consumes High Caloric Food", 
                     guide = guide_legend(reverse=TRUE),
                     labels = c("No", "Yes"))
female_plt

male_plt <- ggplot(confirm_set_male, aes(y=BMI, x=Age, colour=high_cal_consum)) +
  geom_point(shape=16, size=1.2) +
  geom_smooth(se=FALSE) +
  ggtitle("Male") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values=c('#FFBC79', '#1170AA'),
                     name = "Frequently Consumes High Caloric Food", 
                     guide = guide_legend(reverse=TRUE),
                     labels = c("No", "Yes"))
male_plt 

ggarrange(male_plt, female_plt, ncol=2, common.legend=TRUE, legend="bottom")

# confirm_set_female %>% 
#   ggplot() + 
#   aes(y=BMI, x = Age, col = high_cal_consum) + 
#   geom_point() +
#   labs(
#     x = 'Age', 
#     y = 'BMI') +
#   geom_smooth(se=FALSE) +
#   scale_colour_manual(name = "", 
#                       values=c("pink","red","#ADD8E6","blue"), 
#                       labels=c("Female Non-High Cal Consumption",
#                                "Female High Cal Consumption",
#                                "Male Non-High Cal Consumption",
#                                "Male High Cal Consumption")
#   ) +
#   theme(legend.title=element_blank(), legend.position = c(0.8, .85))

# Save dataset
write.csv(confirm_set, file="./data/final/obesity_confirm.csv", row.names=F)
write.csv(explore_set, file="./data/final/obesity_explore.csv", row.names=F)