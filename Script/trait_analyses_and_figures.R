# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #
# - # - #   Life-history Traits -- Analyses & Figures               # - # - #
# - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

#### SET-UP #### ---------------------------------------------------------------

# Load packages
library(dplyr)
library(ggplot2)
library(car)
library(ggpubr)
library(cowplot)
library(emmeans)
library(multcompView)
library(multcomp)
library(stringr)
library(lme4)
library(patchwork)

# Import ggplot theme for graphs
theme_tess <- function (){
  theme_cowplot()+ 
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 20),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 20),
          axis.text.x=element_text(size=20), 
          axis.text.y=element_text(size=20),
          plot.title = element_text(hjust = 0.5,size=20))}

#### BODY SIZE BEFORE THE EXPERIMENT ####

# Import data 

bodysize_cg <- read.csv("./Data/bodysize_cg_before.csv")

# View data 
# View(bodysize_cg)

# Convert categorical variables into factors and specify order of levels
bodysize_cg$hist_temp <- factor(bodysize_cg$hist_temp, levels = c("25", "30", "35"))
bodysize_cg$indiv_num <- as.factor(bodysize_cg$indiv_num)
bodysize_cg$sex <- factor(bodysize_cg$sex, levels = c("f", "m"))

# Check factor levels and their order 
levels(bodysize_cg$hist_temp)
levels(bodysize_cg$indiv_num)
levels(bodysize_cg$sex)

# View structure of data set 
str(bodysize_cg)

# Create an updated data set, adding a new column converting weight in g to mg 
bodysize_cg2 <- bodysize_cg %>%
  mutate(weight_mg = weight*1000)

# View(bodysize_cg2)
str(bodysize_cg2)

# Calculate summary data (n, mean, sd, se)
bodysize_cg_sum <- bodysize_cg2 %>%
  group_by(hist_temp, sex) %>% # Group by 'hist_temp' and 'sex'
  summarize(N = sum(!is.na(weight_mg)),
            mean_weight = mean(weight_mg, na.rm = TRUE),
            sd = sd(weight_mg, na.rm = TRUE),
            se = sd(weight_mg, na.rm = TRUE)/sqrt(N))

# View(bodysize_cg_sum)

# Conduct one-way ANOVA
# Need to create subsets of the original data set
# One with the male level dropped from the sex factor and another with the female level dropped from the sex factor

## Males ## --------------------------------------------------------------------
bodysize_cg2_m <- bodysize_cg2[bodysize_cg2$sex == 'm',]
bodysize_cg_sum_m <- bodysize_cg_sum[bodysize_cg_sum$sex == 'm',]

# View(bodysize_cg2_m)
# View(bodysize_cg_sum_m)

# Construct linear model and run ANOVA
bodysize_m <- lm(weight_mg ~ hist_temp, data = bodysize_cg2_m)
Anova(bodysize_m, type=2) # Not significant 

# Check ANOVA assumptions 
# plot(bodysize_m)

leveneTest(weight_mg ~ hist_temp, data = bodysize_cg2_m) # Variances are homogeneous

bodysize_m_residuals <- residuals(object = bodysize_m) # Extract the residuals
shapiro.test(x = bodysize_m_residuals) # Residuals are normally distributed

## Females ## --------------------------------------------------------------

bodysize_cg2_f <- bodysize_cg2[bodysize_cg2$sex == 'f',]
bodysize_cg_sum_f <- bodysize_cg_sum[bodysize_cg_sum$sex == 'f',]

# View(bodysize_cg2_f)
# View(bodysize_cg_sum_f)

# Construct linear model and run ANOVA
bodysize_f <- lm(weight_mg ~ hist_temp, data = bodysize_cg2_f)
Anova(bodysize_f, type = 2) # Not significant

# Check ANOVA assumptions 
# plot(bodysize_f)

leveneTest(weight_mg ~ hist_temp, data = bodysize_cg2_f) # Variances are homogeneous

bodysize_f_residuals <- residuals(object = bodysize_f) # Extract the residuals
shapiro.test(x = bodysize_f_residuals) # Residuals are normally distributed

## Plot for manuscript ## ------------------------------------------------------

# Male and female on the same plot with sex on the x-axis 
cg_bodysize_plot_v2 <- ggplot(bodysize_cg_sum, aes(x = sex, y = mean_weight, color = hist_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_point(data = bodysize_cg2, aes(x = sex, y = weight_mg, color = hist_temp), position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.15, jitter.height = 0), size = 3, alpha = 0.2) +
  scale_y_continuous(breaks = seq(0.8, 1.8, by = 0.2), limits = c(0.8, 1.8)) + 
  scale_x_discrete(labels = c("Female", "Male")) +
  geom_errorbar(data = bodysize_cg_sum, aes(x = sex, ymin = mean_weight-se, ymax = mean_weight+se), position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Sex", y = "Weight (mg)") + 
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  theme_tess() +
  theme(legend.position = "none") +
  annotate("text", label = expression(italic("P") ~ "= 0.12"), x = 2, y = 1.8, size = 5.5, fontface = 2) +
  annotate("text", label = expression(italic("P") ~ "= 0.48"), x = 1, y = 1.8, size = 5.5, fontface = 2) +
  ggtitle("Body size before experiment") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

#windows();cg_bodysize_plot_v2

#### BODY SIZE AT THE END OF THE EXPERIMENT ####

# Import data 

bodysize_end <- read.csv("./Data/bodysize_cg_end.csv")

# View data and structure
# View(bodysize_end)
str(bodysize_end)

# Convert categorical variables into a factor 
bodysize_end$hist_temp <- as.factor(bodysize_end$hist_temp)
bodysize_end$indiv_num <- as.factor(bodysize_end$indiv_num)
bodysize_end$sex <- as.factor(bodysize_end$sex)
bodysize_end$competition <- as.factor(bodysize_end$competition)
bodysize_end$rep <- as.factor(bodysize_end$rep)

# Check factor levels and their order
levels(bodysize_end$hist_temp)
levels(bodysize_end$indiv_num)
levels(bodysize_end$sex)
levels(bodysize_end$competition)
levels(bodysize_end$rep)

# Create an updated data set, adding a new column converting weight in g to mg 
bodysize_end <- bodysize_end %>%
  mutate(weight_mg = weight*1000)

# View(bodysize_end)

# Check for potential outliers/errors
bodysize_end %>% 
  filter(weight_mg > 1.75) # One observation that had a weight of 3.3 mg (definitely an error)

# Remove the error
bodysize_end <- bodysize_end %>%
  filter(!(hist_temp == 30 &
             competition == "wo" &
             rep == 6 &
             sex == "f" &
             indiv_num == 1))

# Check to see it was removed successfully 
bodysize_end %>%
  filter(hist_temp == 30,
         competition == "wo",
         rep == 6,
         sex == "f",
         indiv_num == 1) # Returns 0 rows so it was successfully removed

# Calculate mean body size for each replicate population/community for each sex
bodysize_means <- bodysize_end %>% 
  group_by(hist_temp, competition, rep, sex) %>% # Group by 'hist_temp', 'competition', 'rep', and 'sex'
  summarize(mean_bodysize = mean(weight_mg, na.rm = TRUE))

# View(bodysize_means)

## Calculate summary stats for each treatment combination (mean, n, sd, and se)
bodysize_sum <- bodysize_means %>%
  group_by(hist_temp, competition, sex) %>% # Group by 'hist_temp', 'competition', and 'sex'
  summarize(N = sum(!is.na(mean_bodysize)),
            avg_bodysize = mean(mean_bodysize, na.rm = TRUE),
            sd = sd(mean_bodysize, na.rm = TRUE),
            se = sd(mean_bodysize, na.rm = TRUE)/sqrt(N))

# View(bodysize_sum)

## Males ## --------------------------------------------------------------------

# Conduct one-way ANOVA

# Subset of data for just males 
bodysize_m_end <- bodysize_means[bodysize_means$sex == 'm',]
bodysize_sum_m <- bodysize_sum[bodysize_sum$sex == 'm',]

# View(bodysize_m_end)
# View(bodysize_sum_m)

# Construct linear model and conduct one-way ANOVA
bodysize_lm_m <- lm(mean_bodysize ~ hist_temp*competition, data = bodysize_m_end)
Anova(bodysize_lm_m, type=2) # Significant main effect of hist_temp on male body size 

# Check ANOVA assumptions 
leveneTest(mean_bodysize ~ hist_temp*competition, data = bodysize_m_end) # Variances are homogeneous

bodysize_m_residuals <- residuals(object = bodysize_lm_m) # Extract the residuals
shapiro.test(x = bodysize_m_residuals) # Residuals are normally distributed

# Use emmeans() to conduct pairwise comparisons
emmeans(bodysize_lm_m, pairwise ~ hist_temp, adjust = "tukey") # No difference between 25C and 30C males 
                                                          # 25C males are bigger than 35C males 
                                                          # 30C males are bigger than 35C males 

## Females ## ------------------------------------------------------------------

# Subset of data for just females 
bodysize_f_end <- bodysize_means[bodysize_means$sex == 'f',]
bodysize_sum_f <- bodysize_sum[bodysize_sum$sex == 'f',]

# View(bodysize_sum_f)
# View(bodysize_f_end)

# Construct linear model and conduct one-way ANOVA 
bodysize_lm_f <- lm(mean_bodysize ~ hist_temp*competition, data = bodysize_f_end)
Anova(bodysize_lm_f, type=2) # Significant main effect of hist_temp on female body size 

# Check ANOVA assumptions 
leveneTest(mean_bodysize ~ hist_temp*competition, data = bodysize_f_end) # Variances are homogeneous

bodysize_f_residuals <- residuals(object = bodysize_lm_f) # Extract the residuals
shapiro.test(x = bodysize_f_residuals) # Residuals are normally distributed

# Use emmeans() to conduct pairwise comparisons
emmeans(bodysize_lm_f, pairwise ~ hist_temp, adjust = "tukey") # 25C females are bigger than 30C females and 35C females 
                                                          # 30C females are bigger than 35C females 

## Plot for manuscript ## ------------------------------------------------------

# Create data frames for both males and females that includes the compact letter display
cld_m_df_bodysize <- cld(emmeans(bodysize_lm_m, ~hist_temp), Letters = letters, adjust = "tukey", sort = FALSE)
cld_f_df_bodysize <- cld(emmeans(bodysize_lm_f, ~hist_temp), Letters = letters, adjust = "tukey", sort = FALSE)

# Remove all spaces from the compact letter display
cld_m_df_bodysize$.group <- str_remove_all(cld_m_df_bodysize$.group, " ")
cld_f_df_bodysize$.group <- str_remove_all(cld_f_df_bodysize$.group, " ")

# Make sure the factor level order in cld_df is the same as in the summary data set
cld_m_df_bodysize$hist_temp <- factor(cld_m_df_bodysize$hist_temp, levels = levels(bodysize_sum_m$hist_temp))
cld_f_df_bodysize$hist_temp <- factor(cld_f_df_bodysize$hist_temp, levels = levels(bodysize_sum_f$hist_temp))

# Add a column for 'sex' in each cld data frame so the clds can be mapped onto the plot
cld_m_df_bodysize$sex <- "m"
cld_f_df_bodysize$sex <- "f"

# Add se from the summary data to each cld data frame
# This will ensure that letters are equal distances above error bars since the SE of emmeans() are the same for each hist_temp
cld_m_df_bodysize <- left_join(cld_m_df_bodysize, bodysize_sum_m[, c("hist_temp", "se")], by = c("hist_temp"))
cld_f_df_bodysize <- left_join(cld_f_df_bodysize, bodysize_sum_f[, c("hist_temp", "se")], by = c("hist_temp"))

# Combine the cld data frames for males and females
cld_df_bodysize_end <- rbind(cld_f_df_bodysize, cld_m_df_bodysize)

# Male and female on the same plot with sex on the x-axis
bodysize_end_plot <- ggplot(bodysize_sum, aes(x = sex, y = avg_bodysize, color = hist_temp, shape = competition)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_point(data = bodysize_means, aes(x = sex, y = mean_bodysize, color = hist_temp, shape = competition), position = position_jitterdodge(dodge.width = 0.5, jitter.width = 0.15, jitter.height = 0), size = 3, alpha = 0.2) +
  scale_x_discrete(labels = c("Female", "Male")) +
  geom_errorbar(data = bodysize_sum, aes(x = sex, ymin = avg_bodysize-se, ymax = avg_bodysize+se), position = position_dodge(width = 0.5), width = 0) +
  geom_text(data = cld_df_bodysize_end, aes(x = sex, y = emmean + SE + 0.1, label = .group, group = hist_temp), inherit.aes = FALSE, color = "black", position = position_dodge(width = 0.5), size = 5.5, fontface = "bold", vjust = 0) +
  labs(x = "Sex", y = "Weight (mg)") + 
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  scale_shape_manual(values = c(17, 16), name = "Interspecific competition", labels = c("With", "Without")) +
  theme_tess() +
  theme(legend.position = "none") +
  annotate("text", label = expression(bolditalic("P") ~ bold("< 0.001")), x = 2, y = 1.45, size = 5.5, fontface = 2) +
  annotate("text", label = expression(bolditalic("P") ~ bold("< 0.001")), x = 1, y = 1.45, size = 5.5, fontface = 2) +
  ggtitle("Body size end of experiment") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

#windows();bodysize_end_plot

#### FECUNDITY ####

# Import data 
fecundity <- read.csv("./Data/fecundity.csv")

# View dataset 
# View(fecundity)

# Convert categorical variables into a factor 
fecundity$hist_temp <- as.factor(fecundity$hist_temp)
fecundity$indiv_num <- as.factor(fecundity$indiv_num)

# Check factor levels and their order
levels(fecundity$hist_temp)
levels(fecundity$indiv_num)

# View structure of fecundity dataset 
str(fecundity)

# Calculate summary data for each temperature
fecundity_sum <- fecundity %>%
  group_by(hist_temp) %>%
  summarize(N = sum(!is.na(egg_count)),
            mean_fecundity = mean(egg_count, na.rm = TRUE),
            sd = sd(egg_count, na.rm = TRUE),
            se = sd(egg_count, na.rm = TRUE)/sqrt(N))

# View(fecundity_sum)

# Construct linear model and conduct ANOVA
fecundity_lm <- lm(egg_count ~ hist_temp, data = fecundity)
Anova(fecundity_lm,type=2) # Significant effect of hist_temp on fecundity

# Check ANOVA assumptions 
# plot(fecundity_lm)

leveneTest(egg_count ~ hist_temp, data = fecundity) # Variances are homogeneous

fecundity_residuals <- residuals(object = fecundity_lm) # Extract the residuals
shapiro.test(x = fecundity_residuals) # Residuals are normally distributed

# Use emmeans() to conduct pairwise comparisons instead 
emmeans(fecundity_lm, pairwise ~ hist_temp, adjust = "tukey") # No difference between 30C and 35C
                                                         # Fecundity is higher in 25C than 30C and 35C

## Plot for manuscript ## ------------------------------------------------------

# Create data frame that includes the compact letter display
cld_df_fecundity <- cld(emmeans(fecundity_lm, ~hist_temp), Letters = letters, adjust = "tukey", sort = FALSE)

# Remove all spaces from the compact letter display
cld_df_fecundity$.group <- str_remove_all(cld_df_fecundity$.group, " ")

# Make sure the factor level order in cld_df is the same as in the summary data set
cld_df_fecundity$hist_temp <- factor(cld_df_fecundity$hist_temp, levels = levels(fecundity_sum$hist_temp))

# Fecundity plot
fecundity_plot_v3 <- ggplot(fecundity_sum, aes(x = hist_temp, y = mean_fecundity, color = hist_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_jitter(data = fecundity, aes(x = hist_temp, y = egg_count, color = hist_temp), width = 0.05, height = 0, size = 3, alpha = 0.2) +
  scale_y_continuous(breaks = seq(0, 16, by = 4)) + 
  scale_x_discrete(breaks = c(25, 30, 35), labels = c("25", "30", "35")) +
  geom_errorbar(data = fecundity_sum, aes(x = hist_temp, ymin = mean_fecundity-se, ymax = mean_fecundity+se), position = position_dodge(width = 0.5), width = 0) +
  geom_text(data = cld_df_fecundity, aes(x = hist_temp, y = emmean + SE + 1.5, label = .group), color = "black", size = 5.5, fontface = "bold", vjust = 0) +
  labs(x = "Historical temperature (°C)", y = "Eggs laid in 48 hrs at 30°C") + 
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  theme_tess() +
  theme(legend.position = "none") + 
  annotate("text", label = expression(bolditalic("P") ~ bold("< 0.001")), x = 2, y = 18, size = 5.5, fontface = 2) +
  ggtitle("Fecundity") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

#windows();fecundity_plot_v3

#### EGG SIZE ####

# Import data 
eggdata <- read.csv("./Data/eggsize.csv")

# View data 
# View(eggdata)

# Convert categorical variables into a factor 
eggdata$hist_temp <- as.factor(eggdata$hist_temp)
eggdata$indiv_num <- as.factor(eggdata$indiv_num)
eggdata$egg_num <- as.factor(eggdata$egg_num)

# Check factor levels and their order
levels(eggdata$hist_temp)
levels(eggdata$indiv_num)

# View structure of data set 
str(eggdata)

# Calculate mean egg size for each female per temperature
egg_means <- eggdata %>%
  group_by(hist_temp, indiv_num) %>%
  summarize(mean_egg = mean(length, na.rm = TRUE))

# View(egg_means)

## Calculate summary stats for each temperature (mean, n, sd, and se)
egg_sum <- egg_means %>%
  group_by(hist_temp) %>%
  summarize(N = sum(!is.na(mean_egg)),
            avg_egg = mean(mean_egg, na.rm = TRUE),
            sd = sd(mean_egg, na.rm = TRUE),
            se = sd(mean_egg, na.rm = TRUE)/sqrt(N))

# View(egg_sum)

# Construct linear model and conduct ANOVA
egg_lm <- lm(mean_egg ~ hist_temp, data = egg_means)
Anova(egg_lm,type=2) # Not significant

# Check ANOVA assumptions 
# plot(egg_lm)

leveneTest(mean_egg ~ hist_temp, data = egg_means) # Variances are homogeneous

eggsize_residuals <- residuals(object = egg_lm) # Extract the residuals
shapiro.test(x = eggsize_residuals) # Residuals are normally distributed

## Plot for manuscript ## ------------------------------------------------------

# Egg size plot 
eggsize_plot_v3 <- ggplot(egg_sum, aes(x = hist_temp, y = avg_egg, color = hist_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_jitter(data = egg_means, aes(x = hist_temp, y = mean_egg, color = hist_temp), width = 0.05, height = 0, size = 3, alpha = 0.2) +
  scale_x_discrete(breaks = c(25, 30, 35), labels = c("25", "30", "35")) +
  geom_errorbar(data = egg_sum, aes(x = hist_temp, ymin = avg_egg-se, ymax = avg_egg+se), position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Historical temperature (°C)", y = "Egg length (µm)") + 
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  theme_tess() +
  theme(legend.position = "none") +
  annotate("text", label = expression(italic("P") ~ "= 0.22"), x = 2, y = 720, size = 5.5, fontface = 2) +
  ggtitle("Egg size") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

#windows();eggsize_plot_v3

#### DEVELOPMENT RATE ####

# Import data
develop <- read.csv("./Data/development.csv")

# View data 
# View(develop)

# Convert categorical variables into a factor 
develop$hist_temp <- as.factor(develop$hist_temp)
develop$indiv_num <- as.factor(develop$indiv_num)

# Check factor levels and their order
levels(develop$hist_temp)
levels(develop$indiv_num)

# View structure of data set 
str(develop)

## Development Rate instead of Time ## -----------------------------------------

# Create an updated data set, adding a new column converting days to pupation to a development rate 
develop2 <- develop %>%
  mutate(develop_rate = 1/days_to_pupation)

# Calculate summary stats for each beetle per temperature
develop_rate_means <- develop2 %>%
  group_by(hist_temp, indiv_num) %>%
  summarize(mean_develop = mean(develop_rate, na.rm = TRUE),
            mean_pupa = mean(days_to_pupation, na.rm = TRUE))

# View(develop_rate_means)

# Calculate the summary stats for each temperature (mean, n, sd, and se)
develop_rate_sum <- develop_rate_means %>%
  group_by(hist_temp) %>%
  summarize(N = sum(!is.na(mean_develop)),
            mean_develop_rate = mean(mean_develop, na.rm = TRUE),
            sd = sd(mean_develop, na.rm = TRUE),
            se = sd(mean_develop, na.rm = TRUE)/sqrt(N))

# View(develop_rate_sum)

# Construct linear model and conduct ANOVA
develop_rate_lm <- lm(mean_develop ~ hist_temp, data = develop_rate_means)
Anova(develop_rate_lm,type=2) # Significant effect of hist_temp on development rate

# Check ANOVA assumptions 
# plot(develop_rate_lm)

leveneTest(mean_develop ~ hist_temp, data = develop_rate_means) # Variances are homogeneous

develop_residuals <- residuals(object = develop_rate_lm) # Extract the residuals
shapiro.test(x = develop_residuals) # Residuals are normally distributed

# Use emmeans() to conduct pairwise comparisons
emmeans(develop_rate_lm, pairwise ~ hist_temp, adjust = "tukey") # No difference between 25C and 30C
                                                            # Marginally significant between 25C and 35C 
                                                            # 35C develop slower than 30C 

## Plot for manuscript ## ------------------------------------------------------

# Create data frame that includes the compact letter display
cld_df_develop <- cld(emmeans(develop_rate_lm, ~hist_temp), Letters = letters, adjust = "tukey", sort = FALSE)

# Remove all spaces from the compact letter display
cld_df_develop$.group <- str_remove_all(cld_df_develop$.group, " ")

# Make sure the factor level order in cld_df is the same as in the summary data set
cld_df_develop$hist_temp <- factor(cld_df_develop$hist_temp, levels = levels(develop_rate_sum$hist_temp))

# Development rate plot
development_rate_plot_v2 <- ggplot(develop_rate_sum, aes(x = hist_temp, y = mean_develop_rate, color = hist_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_jitter(data = develop_rate_means, aes(x = hist_temp, y = mean_develop, color = hist_temp), width = 0.05, height = 0, size = 3, alpha = 0.2) +
  #scale_y_continuous(breaks = seq(0, 0.05, by = 0.01)) + 
  scale_x_discrete(breaks = c(25, 30, 35), labels = c("25", "30", "35")) +
  geom_errorbar(data = develop_rate_sum, aes(x = hist_temp, ymin = mean_develop_rate-se, ymax = mean_develop_rate+se), position = position_dodge(width = 0.5), width = 0) +
  geom_text(data = cld_df_develop, aes(x = hist_temp, y = emmean + SE + 0.0015, label = .group), color = "black", size = 5.5, fontface = "bold", vjust = 0) +
  labs(x = "Historical temperature (°C)", y = "1/days to pupation at 30°C") + 
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  theme_tess() +
  theme(legend.position = "none") +
  annotate("text", label = expression(bolditalic("P") ~ bold("= 0.033")), x = 2, y =0.05, size = 5.5, fontface = 2) +
  ggtitle("Development rate") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

#windows();development_rate_plot_v2

#### EGG SURVIVAL ####

# Import data 
survival <- read.csv("./Data/survival.csv")

# View data 
# View(survival)

# Convert categorical variables into factors
survival$indiv_num <- as.factor(survival$indiv_num)
survival$hist_temp <- as.factor(survival$hist_temp)
survival$egg_num <- as.factor(survival$egg_num)

# Check factor levels and their order
levels(survival$hist_temp)
levels(survival$indiv_num)

# Convert survival y/n to 1/0
survival <- survival %>%
  mutate(survived = ifelse(survival == "y", 1, 0))

# View structure of data set 
str(survival)
# View(survival)

## Using the cbind method (USE QUASIBINOMIAL GLM METHOD FOR ANALYSIS) ## ----

# Calculate success (survived) and failures (died) for each individual per temperature
df_cbind <- survival %>%
  group_by(hist_temp,indiv_num) %>%
  summarise(
    successes = sum(survived == 1),
    failures  = sum(survived == 0),
    .groups = "drop"
  )

# Make a new column with the "true" individuals (no repeats across hist_temps)
df_cbind <- df_cbind %>%
  mutate(indiv_true = paste(hist_temp,indiv_num, sep = "_"))

# Run generalized linear model
glm_binom<- glm(cbind(successes, failures) ~hist_temp, data = df_cbind, 
                family = binomial) 
Anova(glm_binom, type = "II") # Marginally significant
summary(glm_binom)

# Check for overdispersion
summary(glm_binom)$deviance / summary(glm_binom)$df.residual
# >> 1.5, so that's evidence of overdispersion

# And a formal test for overdispersion
deviance <- deviance(glm_binom)
df_resid <- df.residual(glm_binom)
p_value <- pchisq(deviance, df_resid, lower.tail = FALSE)
print(p_value) # < 0.05 so data are overdispersed

# Re-run as quasibinomial to deal with overdispersion 
glm_quasibinom<- glm(cbind(successes, failures) ~hist_temp, data = df_cbind, 
                     family = quasibinomial) 
Anova(glm_quasibinom, type = "II") # Not significant

# ---------- Plot mean proportion of survival per temperature --------#

## Plot for manuscript ## ------------------------------------------------------

# Make new column calculating the proportion survival from each individual
proportion_survival <- survival %>%
  group_by(hist_temp, indiv_num) %>%
  summarize(eggs_total = n(),
            eggs_survived = sum(survived, na.rm = TRUE),
            proportion_survived = eggs_survived/eggs_total)

# View(proportion_survival)

# Calculate summary stats for each temperature (mean, n, sd, and se)
survival_sum <- proportion_survival %>%
  group_by(hist_temp) %>%
  summarize(N = n(),
            mean_survival = mean(proportion_survived),
            sd = sd(proportion_survived),
            se = sd(proportion_survived)/sqrt(N))

# View(survival_sum)

# Proportion of survival plot
proportion_survival_plot_v3 <- ggplot(survival_sum, aes(x = hist_temp, y = mean_survival, color = hist_temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_jitter(data = proportion_survival, aes(x = hist_temp, y = proportion_survived, color = hist_temp), width = 0.1, height = 0, size = 3, alpha = 0.2) +
  scale_y_continuous(breaks = seq(0,1, by = 0.5)) + 
  scale_x_discrete(breaks = c(25, 30, 35), labels = c("25", "30", "35")) +
  geom_errorbar(data = survival_sum, aes(x = hist_temp, ymin = mean_survival-se, ymax = mean_survival+se), position = position_dodge(width = 0.5), width = 0) +
  labs(x = "Historical temperature (°C)", y = "Proportion of eggs\nsurviving to adulthood") + 
  scale_color_manual(values = c("blue", "orange", "red"), name = "Historical temperature", labels = c("25°C", "30°C", "35°C")) +
  theme_tess() +
  theme(legend.position = "none") +
  annotate("text", label = expression(italic("P") ~ "= 0.29"), x = 2, y = 1.1, size = 5.5, fontface = 2) +
  ggtitle("Egg survival") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

#windows();proportion_survival_plot_v3

#### COMPOSITE FIGURE FOR MANUSCRIPT ####

composite_trait_plot_v2 <- ((cg_bodysize_plot_v2|bodysize_end_plot)/
                              (plot_spacer()|plot_spacer())/
                              (fecundity_plot_v3|eggsize_plot_v3)/
                              (plot_spacer()|plot_spacer())/
                              (development_rate_plot_v2|proportion_survival_plot_v3)/
                              guide_area()) +
  plot_layout(heights = c(1, 0.15, 1, 0.15, 1, 0.1), widths = c(1, 1), guides = "collect") +
  plot_annotation(tag_levels = c('A')) & 
  theme(plot.tag = element_text(size = 25, face = "bold", margin = margin(t = -40)), # margin = margin(t = -40) moves the plot tag (i.e., 'A') upwards
        plot.tag.position = c(0.035, 1), plot.margin = margin(t = 20, r = 5.5, b = 20, l = 5.5) # changing the plot margins ensures that no labels are cut off
        , legend.position = "bottom", legend.title = element_text(size = 20), legend.text = element_text(size = 20)) 

# ggsave(file="Output/Composite_trait_figure.pdf", composite_trait_plot_v2 , width = 15, 
#        height = 27, units = "in")
