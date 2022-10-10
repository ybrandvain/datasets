# Applied Biostats 
# Correlation 
# Oct 10 2022

library(tidyverse)
library(conflicted)
library(magrittr)
library(broom)
library(janitor)
# library(ggrepel) optional


######### EXPLORATION GET STARTED
# Load in the data
tv_link <- "https://raw.githubusercontent.com/ybrandvain/datasets/master/tvs.csv"
tv_data <- read_csv(tv_link) %>%
  clean_names()

# Check it out
ggplot(tv_data,aes(x = people_per_tv, y = life_expectancy))+
  geom_point()




### transform 
tv_data <- mutate(tv_data, log_10_people_per_tv = log10(people_per_tv)) %>%
  dplyr::filter(!is.na(people_per_tv))

ggplot(tv_data,aes(x =  log_10_people_per_tv, y = life_expectancy))+
  #geom_text_repel(aes(label = country))+
  geom_point()


######### Summarize

tv_data %>%
  mutate(dev_tv = (log_10_people_per_tv - mean(log_10_people_per_tv)) ,
         dev_lifespan = (life_expectancy - mean(life_expectancy)),
         shared_dev = dev_tv*dev_lifespan )%>%
  dplyr::summarise(math_cov = sum(shared_dev)/ (n()-1),
                   math_cor = (math_cov  / (sd(life_expectancy) * sd(log_10_people_per_tv))),
                   r_cov    = cov(life_expectancy,log_10_people_per_tv),
                   cor      = cor(life_expectancy,log_10_people_per_tv))



### Uncertainty
n_boots <- 1000
tv_boots <- replicate(n = n_boots , simplify = FALSE,
  slice_sample(tv_data, prop = 1, replace = TRUE) %>%
  summarise(boot_cor = cor(life_expectancy,log_10_people_per_tv))) %>%
  bind_rows()

tv_boots %>%
  dplyr::summarise(se = sd(boot_cor),
                   lower_95 = quantile(boot_cor, prob = 0.025),
                   upper_95 = quantile(boot_cor, prob = 0.975))


## Hypothesis testing
### Uncertainty
n_perm <- 10000
tv_perm <- replicate(n = n_perm , simplify = FALSE,
                      tv_data %>%
                        mutate(perm_tvs = sample(log_10_people_per_tv, replace = FALSE))%>%
                        summarise(permtv_cor = cor(life_expectancy,perm_tvs ))) %>%
  bind_rows()


# p-value 

observed <-  tv_data %>%
  summarise(cor(life_expectancy,log_10_people_per_tv)) %>%
  pull()

tv_perm %>%
  mutate(as_or_more = abs(permtv_cor) >= abs(observed))%>%
  summarise(p_val = mean(as_or_more))


### with maths
# without magrittr

cor.test(pull(tv_data, log_10_people_per_tv), 
         pull(tv_data,life_expectancy))

# with magrittr
tv_data %$%
  cor.test(log_10_people_per_tv, life_expectancy)


# with magrittr and broom
tv_data %$%
  cor.test(log_10_people_per_tv, life_expectancy) %>%
  glance()


# non parameteric rank based tst

tv_data %$%
  cor.test(log_10_people_per_tv, life_expectancy, method = "spearman")
