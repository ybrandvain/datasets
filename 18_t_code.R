library(tidyverse)
library(janitor)
library(broom)
temp_link <- "https://whitlockschluter3e.zoology.ubc.ca/Data/chapter11/chap11e3Temperature.csv"
temp_data <- read_csv(temp_link) %>% # load in data
  clean_names()


#glimpse(temp_data)
############################
#### Plot the data -
############################
# Plot is it normalish? lets make a qqplot
ggplot(temp_data, aes(sample = temperature))+
  geom_qq()+
  geom_qq_line()


# How close is it to the null?
h0_temp <- 98.6
ggplot(temp_data, aes(x = temperature))+
  geom_histogram(bins = 10, color = "white")+ # make a histogram
  geom_vline(xintercept = h0_temp, color = "red")+ # show null
  annotate(geom = "label",x = h0_temp, y = 5, color = "red",hjust = 0, label = "null")

# Data summaries and hypothesis test
temp_data %>%
  dplyr::summarise(mean_temp   = mean(temperature),
                   sd_temp     = sd(temperature),
                   cohens_d    = (mean_temp - h0_temp) / sd_temp,
                   sample_size = n(),
                   se_temp     = sd_temp / sqrt(sample_size),
                   crit_t      = qt(p = 0.025,df = sample_size-1, lower.tail=FALSE),
                   lower_95    = mean_temp - crit_t * se_temp,
                   upper_95    = mean_temp + crit_t * se_temp,
                   abs_t_val   = abs((mean_temp - h0_temp) / se_temp),
                   p_val       = 2*pt(abs_t_val, df = sample_size-1, lower.tail = FALSE))


t.test(pull(temp_data, temperature),mu = 98.6)


t.test(pull(temp_data, temperature),mu = 98.6) %>%
  tidy()
