library(tidyverse)

knee_link <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15e1KneesWhoSayNight.csv"
knee_data <- read_csv(knee_link) 
# glimpse(knee_data) # lets see what type of data we're dealing with
# view(knee_data)    # lets look at out data

knee_data <- knee_data  %>% # reorder factors to put eyes last
  mutate(treatment = fct_relevel(treatment, "control", "knee","eyes"))

################################################
### Make  a plot! Be sure to add x and y
################################################
ggplot(knee_data, aes(x = , y = ))+    # fill in x and y
  geom_jitter() +
  stat_summary(fun.data = "mean_cl_normal", color = "black")


################################################
### run all pairwise t.tests [this is WRONG]
################################################

# test with control and knee only
no_eyes <- dplyr::filter(knee_data, treatment  != "eyes")   # filter the data
t.test(shift ~ treatment, data = no_eyes, var.equal = TRUE) # run test

# test with control and eye only
no_knees <- dplyr::filter(knee_data, ___ )                  # filter the data
t.test(shift ~ treatment, data = ___, var.equal = ___)      # run test

# test with knee and eye only
no_control <- dplyr::filter(knee_data, treatment  != "control") # filter the data
t.test(__)                                                      # run test





################################################
### lets do an ANOVA [this is right!]
################################################

# evaluate assumptions


# calculate all squared deviations
knee_all2_devs <- knee_data %>% 
  mutate(grand_mean = mean(shift)) %>% # Find the overall mean
  group_by(treatment)              %>%
  mutate(group_mean = mean(shift)) %>%
  ungroup( )                       %>%
  dplyr::mutate(total2_dev = (shift      - grand_mean)^2,
                group2_dev = (___        - grand_mean)^2,
                error2_dev = (____      - ____)^2) 

# calculate sums of square, mean saures, F and p-values
knee_all2_devs %>%
  summarise(n_group = n_distinct(treatment),
            n_total = n(),
            df_group = n_group - 1,
            df_error = n_total - n_group,
            ss2_total = sum(total2_dev),
            ss2_group = sum(group2_dev),
            ss2_error = sum(error2_dev),
            mean2_group = ss2_group / df_group,
            mean2_error = ss2_error / df_error,
            f_val       = ___ /  ___,
            p_val       = pf(q = f_val, 
                             df1 = df_group, df2 = df_error, 
                             lower.tail = FALSE),
            r2          = __ / ___
            ) 


# or with aov()
knee_anova <- aov(shift ~ treatment, data = knee_data)
knee_anova
summary(knee_anova)    # Lets check out the ANOVA table
TukeyHSD(knee_anova)   # Lets do a posthoc test to see which means differ

# compare to all t-tests above... how different?
# conclude