# Load packages
library(tidytuesdayR)
library(tidyverse)

# Identify TidyTuesday data sets in 2022
tidytuesdayR::tt_datasets("2022")

# Download data set. Note: As list
ttdata <- tidytuesdayR::tt_load(x = 2022, week = 13)

# Select data set of interest
sportdt <- ttdata[[1]]

# Alternative
# sportdt <- ttdata$sports


# Data Exploration --------------------------------------------------------

# Explore data set
glimpse(sportdt)

# Select variables of interest & chr variables as fct
ttdt_selection <- sportdt %>% 
  dplyr::select(year, institution_name, classification_name, partic_men, partic_women,
         ef_male_count, ef_female_count, ef_total_count, rev_men,
         rev_women,total_rev_menwomen, exp_men, exp_women,
         total_exp_menwomen, sports) %>% 
  mutate(year = as.factor(year),
         institution_name = as.factor(institution_name),
         classification_name = as.factor(classification_name),
         sports = as.factor(sports),
         total_par = partic_men + partic_women) 

# How many years?
sum(table(unique(ttdt_selection$year)))

# sum(table(fct_unique(ttdt_selection$year)))

# How many divisions? 
sum(table(unique(ttdt_selection$classification_name)))

# How may institutions? 
sum(table(unique(ttdt_selection$institution_name)))

# How many sports?
sum(table(unique(ttdt_selection$sports)))

# How many cases per wave?
ttdt_selection %>% 
  count(year)

# How many cases per sport?
ttdt_selection %>% 
  count(sports) 

# Visualizations ----------------------------------------------------------

# Plot measures per sport
ggplot(data = ttdt_selection) +
  geom_bar(mapping = aes(x = sports, color = sports))  +
  theme(legend.position = "none")

# Plot measures per sport (y axis)
ggplot(data = ttdt_selection) + 
  geom_bar(mapping = aes(y = sports, color = sports))

# Plot measures per sport (y axis ordered infrequent)
ggplot(data = ttdt_selection) + 
  geom_bar(mapping = aes(y = fct_infreq(sports), color = sports))

# Plot measures per sport (y)
ggplot(data = ttdt_selection) + 
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(sports)), color = sports))

# Plot measures per sport (y)
ggplot(data = ttdt_selection) + 
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(sports)), color = sports)) +
  ylab("Sports") 

# Plot measures per sport (per year)
ggplot(data = ttdt_selection) + 
  geom_bar(mapping = aes(y = fct_rev(fct_infreq(sports)), color = sports)) +
  ylab("Sports") +
  facet_wrap(vars(year)) +
  theme(legend.position = "none")

# Is any NA in any of my variables?
summary(ttdt_selection)

# Remove NAs from revenues in men and women
myselection <- ttdt_selection %>% 
  filter(!rev_men %in% NA & !rev_women %in% NA)

# Check if NAs
summary(myselection)

# Alternative way
table(is.na(myselection))

# Calculate revenues & expenditure per participant & add new variables
myselection <- myselection %>% 
  mutate(exp_per_men = exp_men / partic_men,
         exp_per_women = exp_women / partic_women,
         exp_per_total = total_exp_menwomen / total_par, 
         rev_per_men = rev_men / partic_men,
         rev_per_women = rev_women / partic_women,
         rev_per_total = total_rev_menwomen / total_par)

# Revenue in Sports -------------------------------------------------------
# Mean revenues per sport
rev_mean <- myselection %>% 
  group_by(sports) %>% 
  summarise(mean_rev_total = mean(total_rev_menwomen)) 

# Plot mean revenues per sport 
myselection %>% 
  group_by(sports) %>% 
  summarise(mean_rev_total = mean(total_rev_menwomen)) %>% 
  ggplot(aes(x = mean_rev_total, y = sports, color = sports)) +
  geom_bar() +
  labs(x = "Mean Revenues", y = "Sports") 

# Get rid of scientific notation
options(scipen = 999)

# Activate scientific notation
# options(scipen = 0)

# Solution change to stat = "identity" in geom_bar()
myselection %>% 
  group_by(sports) %>% 
  summarise(mean_rev_total = mean(total_rev_menwomen)) %>% 
  ggplot(aes(x = mean_rev_total, y = sports, color = sports)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Revenues", y = "Sports") 

# Ordering bars
myselection %>% 
  group_by(sports) %>% 
  summarise(mean_rev_total = mean(total_rev_menwomen)) %>% 
  ggplot(aes(x = mean_rev_total, y = fct_rev(fct_infreq(sports)), color = sports)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Revenues", y = "Sports") 

# Bars reordered
myselection %>% 
  group_by(year, sports) %>% 
  summarise(mean_rev_total = mean(total_rev_menwomen)) %>% 
  ggplot(aes(x = mean_rev_total, y = reorder(sports, mean_rev_total),
             color = sports)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Revenues", y = "Sports") + 
  theme(legend.position = "none") + 
  facet_wrap(vars(year))

# Plot mean revenues per sport and sex
myselection %>% 
  group_by(sports) %>% 
  summarise(mean_rev_men = mean(rev_men),
            mean_rev_women = mean(rev_women)) %>% 
  pivot_longer(cols = c(mean_rev_men,mean_rev_women), names_to = "sex",
               values_to = "mean_rev") %>% 
  ggplot(aes(x = mean_rev, y = reorder(sports, mean_rev), fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Revenues", y = "Sports", fill = "Sex") +
  scale_fill_discrete(labels = c("Men", "Women"))

myselection %>% 
  group_by(sports) %>% 
  summarise(mean_rev_men = mean(rev_men),
            mean_rev_women = mean(rev_women)) %>% 
  mutate(mean_dif = sqrt((mean_rev_men - mean_rev_women) ^ 2)) %>% 
  ggplot(aes(x = mean_dif, y = reorder(sports, mean_dif), fill = mean_dif)) +
  geom_bar(stat = "identity") +
  # facet_wrap(vars(year)) +
  labs(x = "Mean Sex Differences in Revenues (USD)",  y = "Sports", fill = "USD")  

# Expenditures in Sport ---------------------------------------------------
# Plot mean expenditure
myselection %>% 
  group_by(sports) %>% 
  summarise(mean_exp_men = mean(exp_men),
            mean_exp_women = mean(exp_women)) %>% 
  pivot_longer(cols = c(mean_exp_men,mean_exp_women), names_to = "sex",
               values_to = "mean_exp") %>% 
  ggplot(aes(x = mean_exp, y = reorder(sports, mean_exp), fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Mean Expenditure", y = "Sports", fill = "Sex") +
  scale_fill_discrete(labels = c("Men", "Women"))

# Plotting mean differences by sex
myselection %>% 
  group_by(sports) %>% # if facet_wrap, add year
  summarise(mean_exp_men = mean(exp_men),
            mean_exp_women = mean(exp_women)) %>% 
  mutate(mean_dif = sqrt((mean_exp_men - mean_exp_women) ^ 2)) %>% 
  ggplot(aes(x = mean_dif, y = reorder(sports, mean_dif), fill = mean_dif)) +
  geom_bar(stat = "identity") +
  # facet_wrap(vars(year)) +
  labs(x = "Mean Sex Differences (USD)",  y = "Sports", fill = "USD")  

## If necessary install RColorBrewer package 
# install.packages(RColorBrewer) 
# Set palettes (display.brewer.all())
discrete_palettes <- list(
  c("orange", "skyblue"),
  RColorBrewer::brewer.pal(6, "Accent"),
  RColorBrewer::brewer.pal(3, "Set2")
)

# Calculate mean expenditure per participant & plot
myselection %>% 
  group_by(sports) %>% 
  summarise(mean_exp_pamen = mean(exp_per_men),
            mean_exp_pawomen = mean(exp_per_women)) %>%  
  pivot_longer(cols = c(mean_exp_pamen,mean_exp_pawomen), names_to = "sex",
               values_to = "mean_exp_pa") %>% 
  ggplot(aes(x = mean_exp_pa, y = reorder(sports, mean_exp_pa), fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Year and Institution Mean Expenditure per Participant",
       y = "Sports", fill = "Sex") +
  scale_fill_discrete(labels = c("Men", "Women"), type = discrete_palettes)

# Calculate mean expenditure per participant differences & plot

myselection %>% 
  group_by(sports) %>% 
  summarise(mean_exp_pamen = mean(exp_per_men),
            mean_exp_pawomen = mean(exp_per_women)) %>% 
  mutate(mean_pa_dif = sqrt((mean_exp_pamen - mean_exp_pawomen) ^ 2)) %>% 
  ggplot(aes(x = mean_pa_dif, y = reorder(sports, mean_pa_dif), 
             fill = mean_pa_dif)) +
  geom_bar(stat = "identity") +
  # facet_wrap(vars(year)) +
  labs(x = "Mean Sex Differences Expenditures per Participant (USD)", 
       y = "Sports", fill = "USD") +
  scale_fill_continuous( type = "viridis")

# Compare plots with means: Expenditure "Gross" & per participant
plotmeanexp <- myselection %>% 
  group_by(sports) %>% 
  summarise(mean_exp_men = mean(exp_men),
            mean_exp_women = mean(exp_women)) %>% 
  pivot_longer(cols = c(mean_exp_men,mean_exp_women), names_to = "sex",
               values_to = "mean_exp") %>% 
  ggplot(aes(x = mean_exp, y = reorder(sports, mean_exp), fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Year and Institution Mean Expenditure", y = "Sports", fill = "Sex") +
  scale_fill_discrete(labels = c("Men", "Women"))

plotmeanexp_pa <- myselection %>% 
  group_by(sports) %>% 
  summarise(mean_exp_pamen = mean(exp_per_men),
            mean_exp_pawomen = mean(exp_per_women)) %>%  
  pivot_longer(cols = c(mean_exp_pamen,mean_exp_pawomen), names_to = "sex",
               values_to = "mean_exp_pa") %>% 
  ggplot(aes(x = mean_exp_pa, y = reorder(sports, mean_exp_pa), fill = sex)) +
  geom_bar(stat = "identity") +
  labs(x = "Year and Institution Mean Expenditure per Participant",
       y = "Sports", fill = "Sex") +
  scale_fill_discrete(labels = c("Men", "Women"), type = discrete_palettes)


plotmeandifexp <- myselection %>% 
  group_by(sports) %>% # if facet_wrap, add year
  summarise(mean_exp_men = mean(exp_men),
            mean_exp_women = mean(exp_women)) %>% 
  mutate(mean_dif = sqrt((mean_exp_men - mean_exp_women) ^ 2)) %>% 
  ggplot(aes(x = mean_dif, y = reorder(sports, mean_dif), fill = mean_dif)) +
  geom_bar(stat = "identity") +
  # facet_wrap(vars(year)) +
  labs(x = "Mean Sex Differences in Expenditures (USD)",  y = "Sports", fill = "USD")  

plotmeandifexp_pa <- myselection %>% 
  group_by(sports) %>% 
  summarise(mean_exp_pamen = mean(exp_per_men),
            mean_exp_pawomen = mean(exp_per_women)) %>% 
  mutate(mean_pa_dif = sqrt((mean_exp_pamen - mean_exp_pawomen) ^ 2)) %>% 
  ggplot(aes(x = mean_pa_dif, y = reorder(sports, mean_pa_dif), 
             fill = mean_pa_dif)) +
  geom_bar(stat = "identity") +
  # facet_wrap(vars(year)) +
  labs(x = "Mean Sex Differences Expenditures per Participant (USD)", 
       y = "Sports", fill = "USD") +
  scale_fill_continuous( type = "viridis")

# If necessary install package
# install.packages("gridExtra")

# Load package
library(gridExtra)

# Plots together to compare
gridExtra::grid.arrange(plotmeanexp, plotmeanexp_pa)

gridExtra::grid.arrange(plotmeandifexp, plotmeandifexp_pa)

# Relationship between expenditure and revenue ---------------------------

plotmeandifexp_pa <- myselection %>% 
  group_by(sports) %>% 
  summarise(mean_exp_pamen = mean(exp_per_men),
            mean_exp_pawomen = mean(exp_per_women)) %>% 
  mutate(mean_pa_dif = sqrt((mean_exp_pamen - mean_exp_pawomen) ^ 2)) %>% 
  ggplot(aes(x = mean_pa_dif, y = reorder(sports, mean_pa_dif), 
             fill = mean_pa_dif)) +
  geom_bar(stat = "identity") +
  # facet_wrap(vars(year)) +
  labs(x = "Mean Sex Differences Expenditures per Participant (USD)", 
       y = "Sports", fill = "USD") +
  scale_fill_continuous( type = "viridis")

plotmeandifrev_pa <- myselection %>% 
  group_by(sports) %>% 
  summarise(mean_rev_pamen = mean(rev_per_men),
            mean_rev_pawomen = mean(rev_per_women)) %>% 
  mutate(mean_parev_dif = sqrt((mean_rev_pamen - mean_rev_pawomen) ^ 2)) %>% 
  ggplot(aes(x = mean_parev_dif, y = reorder(sports, mean_parev_dif), 
             fill = mean_parev_dif)) +
  geom_bar(stat = "identity") +
  # facet_wrap(vars(year)) +
  labs(x = "Mean Sex Differences Revenues per Participant (USD)", 
       y = "Sports", fill = "USD") 

# Grid plot
gridExtra::grid.arrange(plotmeandifrev_pa, plotmeandifexp_pa)

# Correlation between Expenditures and Revenues
cor(myselection$exp_men, myselection$rev_men, method = "spearman")

# Correlation between exp. and rev. per sport
myselection %>% 
  group_by(sports) %>%
  summarise(assoc_exp_rev_men = cor(exp_men, rev_men, method = "spearman"))
  

# Plot association
myselection %>% 
  group_by(sports) %>%
  ggplot(mapping = aes(x = exp_men, y = rev_men)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Men Expenditure", 
       y = "Men Revenue", fill = "USD") +
  facet_wrap(vars(sports), scales = "free_y")
  
myselection %>% 
  group_by(sports) %>%
  ggplot(mapping = aes(x = exp_women, y = rev_women)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Women Expenditure", 
       y = "Women Revenue", fill = "USD") +
  facet_wrap(vars(sports), scales = "free_y")




