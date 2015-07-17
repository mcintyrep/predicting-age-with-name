
library(dplyr)
library(babynames)
library(zoo)
library(Hmisc)
library(ggplot2)
library(scales)


# load life tables & compute survival rates
lifeTables <- lifetables %>%
  group_by(year) %>%
  mutate(px = lead(lx)/100000) %>%
  mutate(age = x) %>%
  select(year, age, sex, px)


# create a new data frame and interpolate survival rates
year <- data.frame("year" = rep(seq(1900, 2010, 1), times = 120)) %>% arrange(year)
age <- data.frame("age" = rep(seq(0, 119, 1), times = 111))

m <- bind_cols(year, age) %>% mutate(sex = "M")
f <- bind_cols(year, age) %>% mutate(sex = "F")

lifeTables2 <- bind_rows(m, f) %>%
  left_join(lifeTables) %>%
  arrange(sex, age, year) %>%
  mutate(px = round(na.approx(px),4)) %>%
  arrange(year, desc(sex), age)


# load baby names & join to life tables
babyNames <- babynames %>%
  filter(year >= 1900 & year <= 2010) %>%
  mutate(age = 2010 - year) %>%
  left_join(lifeTables2) %>%
  mutate(alive = ifelse(n*px >= 1, round(n*px,), 0)) %>%
  mutate(born = n) %>%
  mutate(alive = ifelse(alive >= born, born, alive)) %>%
  select(-n, -prop)


# compute statistics for names of Americans estimated to be alive
statName <- babyNames %>%
  filter(alive > 0) %>%
  group_by(name, sex) %>%
  mutate(lowerQ = wtd.quantile(age, weights=alive, probs=.25)) %>%
  mutate(median = wtd.quantile(age, weights=alive, probs=.50)) %>%
  mutate(upperQ = wtd.quantile(age, weights=alive, probs=.75)) %>%
  mutate(totalAlive = sum(alive)) %>%
  group_by(name, sex, lowerQ, median, upperQ, totalAlive) %>%
  tally() %>%
  select(-n)


# combine name stats 
babyNames2 <- babyNames %>%
  left_join(statName) %>%
  mutate(alive = ifelse(alive >= born, born, alive))


# find age distribution for boys named paul (by year of birth)
paul <- babyNames %>% filter(name == "Paul", sex == "M", alive > 0)

ggplot(paul, aes(year, born)) + 
  geom_line(size = I(1.5)) + 
  geom_area(aes(year, alive), alpha = I(.9)) + 
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(1900, 2010, 10))

statName %>% filter(name == "Paul", sex == "M")
