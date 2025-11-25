blue_tit <- read.csv("R/blue_tit_data_updated_2020-04-18.csv")
library(lme4)
library(tidyverse)
blue_tit$manipulation <- ifelse(blue_tit$net_rearing_manipulation %in%
                                  as.character(-4:-1), "Reduced", 
                                ifelse(blue_tit$net_rearing_manipulation %in% 
                                         as.character(1:4), "Enlarged","Unchanged"))
mod1 <- lmer(day_14_weight ~ manipulation + Date_of_day14 + 
               (1|day14_measurer) + (1|rear_area/rear_Box), data = blue_tit)
pairs(emmeans::emmeans(mod1, ~manipulation))
blue_tit$change_brood_count <- replace_na(as.numeric(
  blue_tit$net_rearing_manipulation), 0)
mod2 <- lmer(day_14_weight ~ change_brood_count + Date_of_day14 + 
               (1|day14_measurer) + (1|rear_area/rear_Box), data = blue_tit)
summary(mod2)
mean(abs(blue_tit$change_brood_count))
blue_tit$brood_size <- as.numeric(blue_tit$rear_Cs_at_start_of_rearing)
mod3 <- lmer(day_14_weight ~ brood_size + Date_of_day14 + 
               (1|day14_measurer) + (1|rear_area/rear_Box), data = blue_tit)
summary(mod3)
