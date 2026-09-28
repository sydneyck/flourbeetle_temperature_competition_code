# Load packages
library(dplyr)
library(ggplot2)
library(car)
library(ggpubr)
library(cowplot)
library(emmeans)
library(assertr)
library(multcompView)
library(multcomp)
library(stringr)
library(lme4)
library(ggeffects)
library(glmmTMB)
library(patchwork)


# ggplot theme
theme_tess <- function (){
  theme_cowplot()+ 
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 20),
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 20),
          axis.text.x=element_text(size=20), 
          axis.text.y=element_text(size=20),
          plot.title = element_text(hjust = 0.5,size=20))}

# Import the data 

bodysize <- read.csv("./Data/bodysize_founder.csv")

# Convert categorical variables into factors and specify order of levels
bodysize$temp <- factor(bodysize$temp, levels = c("founder","25", "30", "35"))
bodysize$indiv_num <- as.factor(bodysize$indiv_num)
bodysize$sex <- factor(bodysize$sex, levels = c("female", "male"))
bodysize$year <- factor(bodysize$year, levels = c("2021", "2022", "2023", "2025"))


# Check factor levels and their order 
levels(bodysize$temp)
levels(bodysize$indiv_num)
levels(bodysize$sex)
levels(bodysize$year)

## PLOT

# Calculate mean body size for each replicate population for each sex
bodysize_means <- bodysize %>% 
  group_by(year, temp, rep, sex) %>% 
  summarize(mean_bodysize = mean(weightinmg, na.rm = TRUE))

## Calculate summary stats for each temp treatment
bodysize_sum <- bodysize_means %>%
  group_by(year, temp, sex) %>% 
  summarize(N = sum(!is.na(mean_bodysize)),
            avg_bodysize = mean(mean_bodysize, na.rm = TRUE),
            sd = sd(mean_bodysize, na.rm = TRUE),
            se = sd(mean_bodysize, na.rm = TRUE)/sqrt(N))

# Plot females 

female_popmeans<-bodysize_means%>%
  filter(sex=="female")

female_treatmeans<-bodysize_sum%>%
  filter(sex=="female")

f <- ggplot(female_treatmeans, aes(x = year, y = avg_bodysize, color = temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_point(data = female_popmeans, aes(x = year, y = mean_bodysize,
                                         color = temp),size = 3, shape=16, alpha = 0.2,
             position = position_jitterdodge(dodge.width = 0.5, 
                                             jitter.width = 0.3, jitter.height = 0)) +
  scale_x_discrete(labels = c("2021", "2022","2023","2025\n(Current)")) +
  geom_errorbar(data = female_treatmeans, aes(x = year, ymin = avg_bodysize-se, 
                                         ymax = avg_bodysize+se),width = 0,
                position = position_dodge(width = 0.5)) +
  labs(x = "Year", y = "Weight (mg)") + 
  scale_color_manual(values = c("black","blue", "orange", "red"), 
                     name = "Temperature", 
                     labels = c("Founder","25°C", "30°C", "35°C")) +
  scale_y_continuous(limits=c(0.87,1.6))+
  ggtitle("Females") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  theme_tess()+
  geom_vline(
    xintercept = 3.5,
    color = "black",
    linewidth = 1.1
  )+
  theme(legend.position = "none")
  
# windows();f

# Plot males

male_popmeans<-bodysize_means%>%
  filter(sex=="male")

male_treatmeans<-bodysize_sum%>%
  filter(sex=="male")

m <- ggplot(male_treatmeans, aes(x = year, y = avg_bodysize, color = temp)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_point(data = male_popmeans, aes(x = year, y = mean_bodysize,
                                         color = temp),size = 3, shape=16, alpha = 0.2,
             position = position_jitterdodge(dodge.width = 0.5, 
                                             jitter.width = 0.3, jitter.height = 0)) +
  scale_x_discrete(labels = c("2021", "2022","2023","2025\n(Current)")) +
  geom_errorbar(data = male_treatmeans, aes(x = year, ymin = avg_bodysize-se, 
                                              ymax = avg_bodysize+se),width = 0,
                position = position_dodge(width = 0.5)) +
  labs(x = "Year", y = "Weight (mg)") + 
  scale_color_manual(values = c("black","blue", "orange", "red"), 
                     name = "Temperature", 
                     labels = c("Founder","25°C", "30°C", "35°C")) +
  scale_y_continuous(limits=c(0.87,1.6))+
  ggtitle("Males") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) +
  geom_vline(
    xintercept = 3.5,
    color = "black",
    linewidth = 1.1
  )+
  theme_tess() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 16))
  
# windows();m


# Combine the two panels into one plot

#bsize<-plot_grid(f,m,align="h",nrow=1,rel_widths=c(1,1.2))

bsize <- plot_grid(
  f, m,
  align = "h",
  nrow = 1,
  rel_widths = c(1, 1.2),
  labels = c("A", "B"),
  label_fontface = "bold",
  label_size = 20,
  label_x = 0.02,   
  label_y = 0.98)

#windows();bsize

ggsave(file="./Output/Bodysize_founder.pdf", bsize , width = 15, 
      height = 8, units = "in")

##### Analysis of founder body size ####

# Construct linear model and conduct two-way anova: year X temp

#females
twoyears_f<-female_popmeans %>%
  filter(year%in%c(2023,2025))

bodysize_f <- lm(mean_bodysize ~ year*temp, 
                    data = twoyears_f)
Anova(bodysize_f, type=2) 
#effect of temp, no effect of year and no interaction

#males
twoyears_m<-male_popmeans %>%
  filter(year%in%c(2023,2025))

bodysize_m <- lm(mean_bodysize ~ year*temp, 
                 data = twoyears_m)
Anova(bodysize_m, type=2) 

#effect of temp, weak effect of year, no interaction
