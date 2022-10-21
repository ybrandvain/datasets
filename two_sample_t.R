library(tidyverse)
library(Hmisc)
library(broom)
library(janitor)

#################################################################
# Load the data and do some light processing
#################################################################

exercise_ataxia <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter12/chap12q28SpinocerebellarAtaxia.csv")

# view(exercise_ataxia)
# glimpse(exercise_ataxia)
# Why can't we pair them?


#################################################################
# Look at the data
#################################################################
# Lets look at the data
# Does it look normalish within groups?
exercise_ataxia %>%
  ggplot(aes(sample = lifespan))+
  geom_qq()+
  geom_qq_line()+
  facet_wrap(~treatment, scales = "free_y")

# make a histogram
exercise_ataxia %>%
  ggplot(aes(x = lifespan)) +
  geom_histogram(bins = 7, color = "white")+
  facet_wrap(~treatment, ncol = 1)

# make a jitterplot
exercise_ataxia %>%
  ggplot(aes(x = treatment, y = lifespan, color = treatment)) +
  geom_jitter(,height = 0, width = .1, size = 3, alpha  = .6, show.legend = FALSE)+
  stat_summary(fun.data = "mean_cl_normal", color = "black")

# chec out the reordered data
exercise_ataxia_reordered %>%
  ggplot(aes(x = treatment, y = lifespan, color = treatment)) +
  geom_jitter(,height = 0, width = .1, size = 3, alpha  = .6, show.legend = FALSE)+
  stat_summary(fun.data = "mean_cl_normal", color = "black")
  


#################################################################
# Conduct one sample t-test "by hand"
#################################################################

# get summaries for each group
ataxia_group_summaries <- exercise_ataxia_reordered %>%
  group_by(treatment)    %>%
  dplyr::summarise(mean_lifespan = mean(lifespan),
                   sd_lifespan   = sd(lifespan),
                   var_lifespan  = var(lifespan),
                   sample_size   = n())

ataxia_group_summaries

# get summaries for differece between groups
ataxia_group_summaries  %>%
  dplyr::summarise(mean_difference     = diff(mean_lifespan),                    # 1. data summaries
                   pooled_var          = sum(var_lifespan * (sample_size - 1)) / sum(sample_size-1),
                   cohensD_difference  = mean_difference / sqrt(pooled_var),
                   se_difference       = sqrt(pooled_var * sum(1/sample_size)), # 2. uncertainty
                   df_difference       = sum(sample_size - 1), 
                   crit_t              = qt(p =  0.975, df = df_difference),
                   lower_95_difference = mean_difference - se_difference *crit_t,
                   upper_95_difference = mean_difference + se_difference *crit_t,
                   t_difference        = abs(mean_difference) / se_difference,    # 3. NHST
                   reject_null         = t_difference < crit_t,
                   p_value             = 2 * pt(t_difference, df = df_difference, lower.tail = FALSE)) %>%
  view()



#################################################################
# Conduct one sample t-test with t.test() 
#################################################################

#  OPTION ONE --- two vectors
noex_treatment  <- exercise_ataxia %>%
  dplyr::filter(treatment == "NoExcercise") %>%
  pull(lifespan)
ex_treatment  <- exercise_ataxia %>%
  dplyr::filter(treatment == "Excercise") %>%
  pull(lifespan)

t.test(noex_treatment,    # group 1
       ex_treatment,      # matched with pair from group 2
       var.equal = TRUE)  # be sure to tell the t.test function that the variance is equal among groups.. unless you dont want to make that assumption

# Option 2 Formula

t.test(lifespan ~ treatment, data = exercise_ataxia, var.equal = TRUE) # Formula syntax

# or
exercise_ataxia %>% 
  t.test(lifespan ~ treatment, data = ., var.equal = TRUE) # Formula syntax



# try looking at this with the tidy()  function from broom
exercise_ataxia %>% 
  t.test(lifespan ~ treatment, data = ., var.equal = TRUE)%>%
  tidy()
