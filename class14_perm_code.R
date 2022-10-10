# Applied Biostats
# Permutation Code
# Oct 7 2022

library(tidyverse)
library(janitor)
library(Hmisc)
newts <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter13/chap13q08Newts.csv")%>%
  clean_names()


#################
## PLOTS
#################
# plot the data [not just a box and whiskers plots]   
ggplot(data = newts, aes(x = locality , 
                         y = whole_animal_resistance))  + # set up the plot
  geom_jitter(width = .3, height =0, 
              size = 3, alpha = .5)+                     # show the data
  stat_summary(fun.data = "mean_cl_boot", color = "red") # show means and error bars

# plot the data as two histograms   
ggplot(data = newts, aes(___ =  ____________))  +
  ____(bins = 6, color = "white")+ # make a histogram 
  ____(~___, ncol =1)              # split the data in strips by locality


#################
## DATA SUMMARIES
#################

news_summary <- newts %>%
  group_by(locality)  %>% # get different data summaries for each locality
  dplyr::summarise(sd_resist   = sd(whole_animal_resistance), # find standard deviations 
                   mean_resist = mean(whole_animal_resistance)) # find means


observed_diff_mean <- newts_summary %>%
  summarise( diff_in_means = diff(mean_resist)) %>%
  mutate(abs_diff_in_means = abs(diff_in_means)) %>%
  pull(abs_diff_in_means)  # the absolate value of the difference in means is



### permuting_dist 


### permuting_dist 
n_perms <- 5000
permuted_dist <- replicate(n = n_perms, simplify = FALSE,
  newts %>%
  mutate(perm_locality = sample(locality, replace =  FALSE)) %>%
  group_by(perm_locality)%>%
  summarise(mean_perm_resist = mean(whole_animal_resistance)) %>%
  summarise(mean_perm_diff = diff(mean_perm_resist)) ) %>% 
  bind_rows()


### boot_dist 
n_boots <- 1000
boot_dist <- replicate(n = n_boots, simplify = FALSE,
                       group_by(newts, locality) %>%
                         slice_sample( prop = 1, replace = TRUE) %>%
                          #mutate(perm_locality = sample(locality, replace =  FALSE)) %>%
                          group_by(locality)%>%
                          summarise(mean_boot_resist = mean(whole_animal_resistance)) %>%
                          summarise(mean_boot_diff = diff(mean_boot_resist)) ) %>% 
  bind_rows()

ggplot(boot_dist, aes(x = abs(mean_boot_diff)))+
  geom_histogram()+ 
  geom_vline(xintercept = c(.426))
#
# p-value 
permuted_dist %>% 
  mutate(abs_diff_means = abs(mean_perm_diff),
         as_or_more_extreme = abs_diff_means >= observed_diff_mean)%>%
  summarise(p_value = mean( as_or_more_extreme))
