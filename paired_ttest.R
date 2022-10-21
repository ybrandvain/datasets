library(tidyverse)
library(Hmisc)
library(broom)
library(janitor)

#################################################################
# Load the data and do some light processing
#################################################################

goat <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12q29goatCalls.csv") %>%
  clean_names()

# glimpse(goat)
# View(goat)
goat <- goat %>%
  mutate(kid_minus_other = score_former_kid - score_unrelated_kid)


#################################################################
# Look at the data
#################################################################
# Lets look at the data
# Does it look normalish? 
ggplot(goat, aes(sample = kid_minus_other)) +
  geom_qq()+
  geom_qq_line()
# Does it seem interesting?
ggplot(goat, aes(x = kid_minus_other)) +
  geom_histogram(binwidth = .5, color = "white")+
  geom_vline(xintercept = 0, color = "red")



#################################################################
# Conduct one sample t-test "by hand"
#################################################################
goat %>%
  dplyr::summarise(mean_difference     = mean(kid_minus_other),
                   sd_difference       = sd(kid_minus_other),
                   cohensD_difference  = mean_difference / sd_difference,
                   null_difference     = 0 ,
                   sample_size         = n(),
                   df_difference       = sample_size -1, 
                   se_difference       = sd_difference / sqrt(sample_size),
                   crit_t              = qt(p =  0.975, df = df_difference),
                   lower_95_difference = mean_difference - se_difference *crit_t,
                   upper_95_difference = mean_difference + se_difference *crit_t,
                   t_difference        = abs(mean_difference) / se_difference,
                   reject_null         = t_difference < crit_t,
                   p_value             = 2 * pt(t_difference, df = df_difference, lower.tail = FALSE)
                   ) %>%
  view()


#################################################################
# Conduct one sample t-test with t.test() 
#################################################################
# With both members of the pair.  with paired = TRUE
t.test(pull(goat , score_former_kid) ,   # group 1
       pull(goat , score_unrelated_kid), # matched with pair from group 2
       paired = TRUE)                    # be sure to tesll the t.test function that the data are paired


# With the difference between members against the null of zero
t.test(pull(goat , kid_minus_other) ,    # the difference between each pair
       mu = 0)                           # our null for the difference is 0 
