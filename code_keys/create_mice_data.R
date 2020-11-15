### Generate Mouse Data for Part 1

library(tidyverse)
library(lubridate)
library(xlsx)

sex <- runif(60, 0, 1) %>% 
  round(digits = 0)

mice <- data.frame(sex)

nums <- mice %>% group_by(sex) %>% count() ## 27 female | 33 male

female_weights <- rnorm(nums$n[1], 20, 5) # rnorm draws N numbers from a normal distribution with a set mean and standard deviation (sd). In this case, I got 27 values from a distribution w/ mean 20 and sd 5
male_weights <- rnorm(nums$n[2], 30, 5)

# Now we need to make birthdate as a variable

birthdates <- rep(seq.Date(from = mdy("4-21-20"), to = mdy("10-13-20"), by = 6),2) %>% 
  as.numeric() %>% 
  jitter(amount = 7) %>% 
  round(0) %>% 
  as.Date(origin)

weights_birthdates_sorted_df <-
  cbind(weight = sort(female_weights),
        birthdates = sort(desc(sample(birthdates, 27))) %>% abs()) %>%
  rbind(.,
        cbind(
          weight = sort(male_weights),
          birthdates = sort(desc(sample(birthdates, 33))) %>% abs()
        )) %>%
  as.data.frame()

mice_dates <- mice %>% 
  mutate(sex = if_else(sex == 1, "M", "F")) %>% 
  arrange(sex) %>% 
  cbind(., weights_birthdates_sorted_df) %>% 
  mutate(weight = round(weight, 2),
         birthdates = as.Date(birthdates, origin)) %>% 
  mutate(treatment_date = birthdates+21) %>% 
  rownames_to_column("mouse_id")

# mice_dates %>%
#   group_by(sex) %>% 
#   summarise(ave = mean(weight),
#             med = median(weight),
#             sd = sd(weight))
#   summary()
  
mice_dates

add_weight <- function(df) {
  df %>% 
    #filter(sex == sex) %>% 
    sample_frac(size = 0.33) %>% 
    mutate(weight = weight + rnorm(1, 7, 4),
           treatment = "B")
}

treatment_B <- mice_dates %>% add_weight()

affected <- rnorm(40, 1, 0.25)
unaffected <- rnorm(20, 0.33, 0.15)

mice_final <- mice_dates %>% 
  filter(!mouse_id %in% treatment_B$mouse_id) %>% 
  sample_n(size = nrow(.)) %>% 
  mutate(treatment = c(rep("Placebo", 20), rep("A", 20))) %>% 
  full_join(., treatment_B) %>% 
  mutate(open_field_time = c(unaffected, affected)) %>% 
  arrange(as.integer(mouse_id)) %>% 
  mutate(weight = round(weight, 2),
         open_field_time = round(open_field_time, 2))


write.xlsx(mice_final, file = "data/part1/mice.xlsx")
