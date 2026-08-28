# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - #   Beetle Count (Discrete) -- Analyses & Figures           # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

#### SET-UP #### ---------------------------------------------------------------

# Load packages 
library(dplyr)
library(ggplot2)
library(cowplot)
library(ggpubr)
library (car) 
library(ARTool)
library(emmeans)
library(lme4)

# Set the plotting theme 
theme_tess <- function (){
  theme_cowplot()+ #cowplot is an existing nice looking plot type thing
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 20),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 20),
          axis.text.x=element_text(size=20), 
          axis.text.y=element_text(size=20),
          plot.title = element_text(hjust = 0.5,size=20))}

#### DATA IMPORT & SET-UP #### ----------------------------------------------------------

# Import the data
data <- read.csv("./Data/beetlecounts.csv")
                                                          
# Check the data was imported correctly
# View(data)

# Check the structure of the data 
str(data)

# Convert categorical variables into factors
data$hist_temp <- as.factor(data$hist_temp)
data$competition <-as.factor(data$competition)
data$rep <- as.factor(data$rep)

# Create summary data of T.castaneum data (mean, n, sd, and se)
data_sum <- data %>%
  group_by(hist_temp, competition, week) %>% # Group by 'hist_temp', 'competition', and 'week'
  summarize(N = sum(!is.na(cast)), # counts the number of non-missing (i.e., non-NA) values in the variable inside the brackets
            mean_cast = mean(cast, na.rm = TRUE), #na.rm = TRUE removes NAs (i.e., missing values)
            sd = sd(cast, na.rm = TRUE),
            se = sd(cast, na.rm = TRUE)/sqrt(N))

#View(data_sum)

# Create summary data of T.confusum data (mean, n, sd, and se)
conf_sum <- data %>%
  filter(competition == "w") %>% # Filter for replicates with confusum present
  group_by(hist_temp, competition, week) %>% # Group data by 'hist_temp','competition', and 'week'
  summarize(N = sum(!is.na(conf)),
            mean_conf = mean(conf, na.rm = TRUE),
            sd = sd(conf, na.rm = TRUE),
            se = sd(conf, na.rm = TRUE)/sqrt(N))

#View(conf_sum)

#### GLOBAL ANOVA #### ---------------------------------------------------------

## T. castaneum mixed-effects model ## ------------------------------------------
# Make a new column with unique population ids (no repeated replicate numbers across hist_temp x competition)
data <- data %>%
  mutate(pop_id = paste(hist_temp,competition,rep, sep = "_"))

# Check that n = 60 for pop_id
n_distinct(data$pop_id)

# Construct global linear mixed-effects model
lmmglobal <- lmer(cast ~ hist_temp * competition * week + (1|pop_id), data = data)

Anova(lmmglobal, type = 2) # Significant interaction between hist_temp*competition*week

# Check residuals 
# plot(lmmglobal)

# Check residual normality 
# qqnorm(residuals(lmmglobal))


## T. confusum mixed-effects model ## --------------------------------------------------------
# Construct global linear mixed-effects model
lmmglobal_conf <- lmer(conf ~ hist_temp * week + (1|pop_id), data = data)
Anova(lmmglobal_conf) # Significant main effect of time

summary(lmmglobal_conf) 
emtrends(lmmglobal_conf, ~ 1, var = "week") # T.confusum abundance decreased over time

#### PLOTS FOR MANUSCRIPT #### -----------------------------------------------

# Convert week variable to a factor for plotting
data$week <- as.factor(data$week)
data_sum$week <- as.factor(data_sum$week)
conf_sum$week <- as.factor(conf_sum$week)

# T. castaneum plot
data_sum2 <- data %>%
  group_by(hist_temp, competition, week) %>%
  summarize(N = sum(!is.na(cast)),
            mean_cast = mean(cast, na.rm = TRUE),
            sd = sd(cast, na.rm = TRUE),
            se = sd(cast, na.rm = TRUE)/sqrt(N)) %>%
  mutate(t_value = qt(0.975, df = N - 1), # Construct 95% confidence intervals
         CI_lower = mean_cast - t_value * se,
         CI_upper = mean_cast + t_value * se) 

data_sum2$week <- as.factor(data_sum2$week)

weeks_plot_v4 <-ggplot(data_sum, aes(x = week, y = mean_cast, color = hist_temp, shape = competition)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_point(data = data, aes(x = week, y = cast, color = hist_temp), position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.15, jitter.height = 0), size = 3, alpha = 0.2) +
  scale_x_discrete(breaks = c(6, 12, 18, 24, 30), labels = c("6", "12", "18", "24", "30")) +
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  scale_shape_manual(values = c(17, 16), name = "Interspecific competition", labels = c("With", "Without")) +
  geom_errorbar(data = data_sum2, aes(x = week, ymin = CI_lower, ymax = CI_upper), position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Weeks", y = expression(italic("T.castaneum") ~ "abundance at 30°C")) + 
  theme_tess() +
  theme(legend.text = element_text(size = 14),legend.title = element_text(size = 15))

# ggsave(file="Output/Cast_abundance_figure.pdf", weeks_plot_v4 , width = 12, 
#       height = 7, units = "in")

# T. confusum plot
conf_sum2 <- data %>%
  filter(competition == "w") %>%
  group_by(hist_temp, competition, week) %>%
  summarize(N = sum(!is.na(conf)),
            mean_conf = mean(conf, na.rm = TRUE),
            sd = sd(conf, na.rm = TRUE),
            se = sd(conf, na.rm = TRUE)/sqrt(N)) %>%
  mutate(t_value = qt(0.975, df = N - 1),
         CI_lower = mean_conf - t_value * se,
         CI_upper = mean_conf + t_value * se)

conf_sum2$week <- as.factor(conf_sum2$week)

conf_weeks_plot_v2 <-ggplot(conf_sum, aes(x = week, y = mean_conf, color = hist_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_point(data = data, aes(x = week, y = conf, color = hist_temp), position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.15, jitter.height = 0), size = 3, alpha = 0.2) +
  scale_x_discrete(breaks = c(6, 12, 18, 24, 30), labels = c("6", "12", "18", "24", "30")) +
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  geom_errorbar(data = conf_sum2, aes(x = week, ymin = CI_lower, ymax = CI_upper), position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Weeks", y = expression(italic("T.confusum") ~ "abundance at 30°C")) + 
  theme_tess() +
  theme(legend.text = element_text(size = 14),legend.title = element_text(size = 15))

# ggsave(file="Output/Conf_abundance_figure.pdf", conf_weeks_plot_v2 , width = 12, 
#       height = 7, units = "in")

#### WEEK BY WEEK T. CASTANEUM ANALSYES ####

#### ANALYSIS OF FIRST COUNT (Week 6) #### -------------------------------------

# Create a subset of the data and the summary data that only include T.castaneum data from Count 1 (week 6)
data_sum_1 <- data_sum[data_sum$week == '6',]
data_1 <- data[data$week == '6',]

# Two-way ANOVA
lm1 <- lm(cast ~ hist_temp*competition, data=data_1)
Anova(lm1,type=2) # Significant interaction between hist_temp*competition (P = 0.021475)

## Two-way ANOVAs assume normality and homogeneity of variances - Check these assumptions ##

# Check histogram for skewness
# hist(data_1$cast)

# Check diagnostic plots
# plot(lm1)

# Shapiro-Wilk test for checking normality of variances
aov_residuals <- residuals(object = lm1) # Extract the residuals
shapiro.test(x = aov_residuals) # Residuals are non-normal
                                # Not too worried about slight skew in histogram as diagnostic plots look normal and P is close to 0.05. ANOVAs are robust to slight departures from normality

# Levene's test for checking homogeneity of variances 
leveneTest(cast ~ hist_temp * competition, data = data_1) # Variances across groups are not significantly different (i.e., are homogeneous)

## Significant interaction, run post-hoc test ##
emm1 <- emmeans(lm1, ~ hist_temp | competition)
pairs(emm1, adjust = "tukey") # 25C abundance is greater than 30C and 35C abundance in populations without interspecific competition 


#### ANALYSIS OF SECOND COUNT (Week 12) #### -------------------------------------

# Create a subset of the data and the summary data that only include castaneum data from Count 2 (week 12)
data_sum_2 <- data_sum[data_sum$week == '12',] 
data_2 <- data[data$week == '12',]

# Two-way ANOVA 
lm2 <- lm(cast ~hist_temp*competition, data=data_2)
Anova(lm2, type=2) # Significant interaction between hist_temp*competition 

## Two-way ANOVA assumes normality and homogeneity of variances - Check these assumptions ##

# Check histogram for skewness
# hist(data_2$cast)

# Check diagnostic plots
# plot(lm2)

# Shapiro-Wilk test for checking normality of residuals
aov_residuals <- residuals(object = lm2) 
shapiro.test(x = aov_residuals) # Residuals are non-normal

# Levene's test for checking homogeneity of variances 
leveneTest(cast ~ hist_temp * competition, data = data_2) # Variances across groups are significantly different (i.e., not homogeneous)

## Less normal and not homogeneous variances, conduct non-parametric test ##
## ART (Aligned Rank Transformation) model using the "ARTool" package ##

# Fit ART model (ART = Aligned Rank Transformation)
lm2_nonpara <- art(cast ~ hist_temp*competition, data = data_2)

# Run ANOVA on ART model
anova(lm2_nonpara) # Significant interaction between hist_temp*competition


#### ANALYSIS OF THIRD COUNT (Week 18) #### -------------------------------------

# Create a subset of the data and the summary data that only include castaneum data from Count 3 (week 18)
data_sum_3 <- data_sum[data_sum$week == '18',] 
data_3 <- data[data$week == '18',]

# Two-way ANOVA 
lm3 <- lm(cast ~hist_temp*competition, data=data_3)
Anova(lm3, type=2) # Significant interaction between hist_temp*competition

## Two-way ANOVA assumes normality and homogeneity of variances - Check these assumptions ##

# Check histogram for skewness
# hist(data_3$cast)

# Check diagnostic plots
# plot(lm3)

# Shapiro-Wilk test for checking normality of residuals
aov_residuals <- residuals(object = lm3) 
shapiro.test(x = aov_residuals) # Residuals are non-normal

# Levene's test for checking homogeneity of variances 
leveneTest(cast ~ hist_temp * competition, data = data_3) # Variances across groups are significantly different (i.e., not homogeneous)

## Less normal and not homogeneous variances, conduct non-parametric test ##
## ART (Aligned Rank Transformation) model using the "ARTool" package ##

# Fit ART model (ART = Aligned Rank Transformation)
lm3_nonpara <- art(cast ~ hist_temp*competition, data = data_3)

# Run ANOVA on ART model
anova(lm3_nonpara) # Significant interaction between hist_temp*competition


#### ANALYSIS OF FOURTH COUNT (Week 24) #### -------------------------------------

# Create a subset of the data and the summary data that only include castaneum data from Count 4 (week 24)
data_sum_4 <- data_sum[data_sum$week == '24',] 
data_4 <- data[data$week == '24',]

# Two-way ANOVA 
lm4 <- lm(cast ~hist_temp*competition, data=data_4)
Anova(lm4, type=2) #significant interaction between hist_temp*competition

## Two-way ANOVA assumes normality and homogeneity of variances - Check these assumptions ##

# Check histogram for skewness
# hist(data_4$cast)

# Check diagnostic plots
# plot(lm4)

# Shapiro-Wilk test for checking normality of residuals
aov_residuals <- residuals(object = lm4) 
shapiro.test(x = aov_residuals) # Residuals are non-normal

# Levene's test for checking homogeneity of variances 
leveneTest(cast ~ hist_temp * competition, data = data_4) # Variances across groups are significantly different (i.e., not homogeneous)

## Less normal and not homogeneous variances, conduct non-parametric test ##
## ART (Aligned Rank Transformation) model using the "ARTool" package ##

# Fit ART model (ART = Aligned Rank Transformation)
lm4_nonpara <- art(cast ~ hist_temp*competition, data = data_4)

# Run ANOVA on ART model
anova(lm4_nonpara) # Significant interaction between hist_temp*competition


#### ANALYSIS OF FIFTH COUNT (Week 30) #### --------------------------------------

# Create a subset of the data and the summary data that only include castaneum data from Count 5 (week 30)
data_sum_5 <- data_sum[data_sum$week == '30',] 
data_5 <- data[data$week == '30',]

# Two-way ANOVA 
lm5 <- lm(cast ~hist_temp*competition, data=data_5)
Anova(lm5, type=2) # Significant interaction between hist_temp*competition

## Two-way ANOVA assumes normality and homogeneity of variances - Check these assumptions ##

# Check histogram for skewness
# hist(data_5$cast)

# Check diagnostic plots
# plot(lm5)

# Shapiro-Wilk test for checking normality of residuals
aov_residuals <- residuals(object = lm5) # Extract the residuals
shapiro.test(x = aov_residuals) # Residuals are non-normal

# Levene's test for checking homogeneity of variances 
leveneTest(cast ~ hist_temp * competition, data = data_5) # Variances across groups are significantly different (i.e., not homogeneous)

## Less normal and not homogeneous variances, conduct non-parametric test ##
## ART (Aligned Rank Transformation) model using the "ARTool" package ##

# Fit ART model (ART = Aligned Rank Transformation)
lm5_nonpara <- art(cast ~ hist_temp*competition, data = data_5)

# Run ANOVA on ART model
anova(lm5_nonpara) # Significant interaction between hist_temp*competition 

## Bonferroni correction for date-specific models  ## ------------------------------------------

# Extract the p-values of the hist_temp x competition interaction
p_values <- c(
  anova(lm1)["hist_temp:competition", "Pr(>F)"],
  anova(lm2_nonpara)["hist_temp:competition", "Pr(>F)"],
  anova(lm3_nonpara)["hist_temp:competition", "Pr(>F)"],
  anova(lm4_nonpara)["hist_temp:competition", "Pr(>F)"],
  anova(lm5_nonpara)["hist_temp:competition", "Pr(>F)"])

# Bonferroni correction
p.adjust(p_values, method = "bonferroni")