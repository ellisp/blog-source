library(tidyverse)
library(scales)
library(gridExtra)
library(frs) # for the french death rates

#-----------------birth probabilities-----------------------------
first_ave_child_rate <- ave_child_rate <- 2.2
birth_rate_perturb <- -0.02
death_rate_perturb <- +0.30
mean_starting_age_m <- 35
mean_starting_age_f <- 35
sd_starting_age <- 10
ssr <- 107 # number of boys born for every 100 girls.  global average is 107 accoridng to Wikipedia
starting_n <- 2000 # number of starting couples
mimr <- 5 # male infant mortality per 1000, starting
fimr <- 5 # female infant moratlity per 1000, starting
mle <- 75 # male life expectanye, starting
fle <- 80 # female life expectancy, starting

fertile_ages <- 12:50
n_fertile_ages <- length(fertile_ages)

birth_probs = data_frame(age = fertile_ages,
                         prob_birth = dbeta((fertile_ages - 12) / n_fertile_ages, 2, 4) * ave_child_rate / n_fertile_ages,
                         # mother's sex, for later joining to other tables:
                         sex = "female") 

svg("../img/0122-birth-probability.svg", 8, 4)
ggplot(birth_probs, aes(x = age, y = prob_birth)) +
  geom_hline(yintercept = ave_child_rate / n_fertile_ages, colour = "steelblue") +
  geom_line() +
  annotate("text", x =41, y = ave_child_rate / n_fertile_ages + 0.01, colour = "steelblue",
           label = "Flat rate if it didn't vary by age") +
  labs(y = "Probability of a woman given birth") +
  ggtitle("A simple model for age distribution of births",
          paste("Converting an average births-per-woman of", ave_child_rate, 
                "to different probabilities by womens' age")) +
  scale_x_continuous(breaks = fertile_ages[fertile_ages %% 2 == 1]) +
  theme(panel.grid.minor = element_blank())
dev.off()

#-------------------death probabilities---------------------------
# https://www.ined.fr/en/everything_about_population/data/france/deaths-causes-mortality/mortality-rates-sex-age/
save(french_death_rates_2015, file = "0122-demographics/french_death_rates_2015.rda")
age <- french_death_rates_2015$age

dr <- approx(age, french_death_rates_2015$male, xout = 0:150)
death_probs_m <- data_frame(age = dr$x, 
                          prob_death = dr$y / 1000,
                          sex = "male")

#' calculate the multiplier of a given set of death rates, other than IMR, needed to get a new set of death
#' rates for a target life expectancy
le_opt <- function(x, age, rate, target){
    n <- length(rate)
    rate[-c(1, n)] <- pmax(pmin(1000, rate[-c(1, n)] * x), 0)
    return(abs(life_expectancy(age, rate) - target))
    }

# arbitrarily set a male infant death rate:
death_probs_m[1, "prob_death"] <- mimr / 1000

# modify the rest of the death rates to match an arbitrary life expectancy target:
best_multiplier <- optimize(f = le_opt, 
         lower = 0, upper = 100, 
         age = death_probs_m$age, rate = death_probs_m$prob_death * 1000, target = mle)

death_probs_m[-c(1, nrow(death_probs_m)), "prob_death"] <- best_multiplier$minimum * 
  death_probs_m[-c(1, nrow(death_probs_m)), ]$prob_death



dr <- approx(age, french_death_rates_2015$female, xout = 0:150)
death_probs_f <- data_frame(age = dr$x, 
                            prob_death = dr$y / 1000,
                            sex = "female")

# arbitrarily set a female infant death rate:
death_probs_f[1, "prob_death"] <- fimr / 1000

# modify the rest of the death rates to match an arbitrary life expectancy target:
best_multiplier <- optimize(f = le_opt, 
                            lower = 0, upper = 100, 
                            age = death_probs_f$age, rate = death_probs_f$prob_death * 1000, target = fle)

death_probs_f[-c(1, nrow(death_probs_f)), "prob_death"] <- best_multiplier$minimum * 
  death_probs_f[-c(1, nrow(death_probs_f)), ]$prob_death




death_probs <- rbind(death_probs_m, death_probs_f) 



svg("../img/0122-deathrates.svg", 8, 4)
ggplot(death_probs, aes(x = age, y = prob_death, colour = sex)) +
  geom_line() +
  scale_y_log10(breaks = c(1, 10, 100, 1000)/ 1000) +
  labs(y = "Probability of dying in any given year") +
  ggtitle("A simple model for age distribution of deaths",
          "Based on French death rates by age group in 2015, with linear interpolation")
dev.off()

# compare to http://archive.stats.govt.nz/browse_for_stats/health/life_expectancy/new-zealand-life-tables-2005-07/chapter-2-national-trends-in-longevity-and-mortality.aspx

#--------------------number of couples starting out-------------------------

men <- round(rnorm(starting_n, mean_starting_age_m, sd_starting_age))
men[men < 0] <- 0
women <- round(rnorm(starting_n, mean_starting_age_f, sd_starting_age))
women[women < 0] <- 0

currently_alive = data_frame(age = c(men, women),
                             sex = rep(c("male", "female"), each = starting_n)) %>%
  group_by(age, sex) %>%
  summarise(population = n()) %>%
  mutate(birth_year = 0 - age) %>%
  ungroup()



all_deaths <- data_frame(age = numeric(), 
                         sex = character(), 
                         birth_year=numeric(), 
                         deaths = integer(), 
                         death_year = numeric(),
                         population = numeric())

all_borns <- data_frame(sex = character(),
                        population = numeric(),
                        birth_year = numeric())

alive_at_end_of_year <- currently_alive %>%
  mutate(year = 0)



for(this_year in 1:500){
  #------------------births------------------------------
  ave_child_rate <- ave_child_rate * rnorm(1, 1 + birth_rate_perturb / 100, abs(birth_rate_perturb / 100))
  
  
  # recalculate the birth rates
  birth_probs = data_frame(age = fertile_ages,
                           prob_birth = dbeta((fertile_ages - 12) / n_fertile_ages, 2, 4) * 
                             ave_child_rate / n_fertile_ages,
                           # mother's sex, for later joining to other tables:
                           sex = "female") 
  
  
  new_borns <- currently_alive %>%
    inner_join(birth_probs, by = c("age", "sex")) %>%
    mutate(births = rbinom(n(), population, prob_birth),
           # see https://www.scientificamerican.com/article/is-a-pregnant-womans-chan/:
           male = rbinom(n(), births, ssr / (ssr + 100)),
           female = births - male) %>%
    select(-age, -sex, -population, -birth_year, -prob_birth, -births) %>%
    gather(sex, population) %>%
    group_by(sex) %>%
    summarise(population = sum(population)) %>%
    mutate(age = 0, birth_year = this_year)
  
  all_borns <- new_borns %>%
    select(sex, population, birth_year) %>%
    rbind(all_borns)
  #----------------------deaths----------------------------
  
  death_probs <- death_probs %>%
    mutate(prob_death = pmin(1, prob_death * 
                               rnorm(1, 1 + death_rate_perturb / 100, abs(death_rate_perturb / 100))))
  

  
  
  new_deaths <- currently_alive %>%
    inner_join(death_probs, by = c("age", "sex")) %>%
    mutate(deaths = rbinom(n(), population, prob_death),
           death_year = this_year) 
  # at this point, new_deaths has all the information on every cohort, including a row for those with no deaths
  
  #------------------new state-------------------
  currently_alive <- new_deaths %>%
    mutate(population = population - deaths,
           age = age + 1) %>%
    select(-prob_death, -deaths, -death_year) %>%
    rbind(new_borns) 
  
  all_deaths <- new_deaths %>%
    filter(deaths > 0) %>%
    select(age, sex, birth_year, deaths, death_year, population) %>%
    rbind(all_deaths)
  
  alive_at_end_of_year <- currently_alive %>%
    mutate(year = this_year) %>%
    rbind(alive_at_end_of_year) %>%
    filter(population > 0)
}


#=====================Presenting results================
p1 <- alive_at_end_of_year %>%
  group_by(year, sex) %>%
  summarise(population = sum(population)) %>%
  ggplot(aes(x = year, y = population, colour = sex)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  ggtitle("Population size over time",
          paste0("Starting and finishing children per woman: ", round(first_ave_child_rate, 1),
                 " and ", round(ave_child_rate, 1), ".",
                 "\nDeath rate tendency: ", death_rate_perturb, "%")) +
  labs(x = "Year since simulation began", colour = "")

p2 <- alive_at_end_of_year %>%
  filter(year %in% c(0, 20, 40, 75, 120, 180, 250, 350, 500)) %>%
  mutate(year_lab = paste("Year:", year),
         year_lab = fct_reorder(year_lab, year)) %>%
  ggplot(aes(x = age, y = population, colour = sex)) +
  geom_line() +
  facet_wrap(~year_lab, scales = "free_y") +
  ggtitle("Population age and sex distribution over time") +
  labs(x = "Age of population", colour = "") +
  scale_y_continuous("Number of people\n(note that scale varies)", label = comma)


p2ad <- alive_at_end_of_year %>%
  filter(year %in% c(0, 20, 40, 75, 120, 180, 250, 350, 500)) %>%
  mutate(year_lab = paste("Year:", year),
         year_lab = fct_reorder(year_lab, year)) %>%
  mutate(age_c = fct_drop(cut(age, breaks = 0:31 * 5, right = FALSE)) ) %>%
  group_by(age_c, year_lab, sex) %>%
  summarise(population = sum(population))

p2a <- p2ad %>%
  ggplot(aes(x = as.numeric(age_c), y = population, colour = sex)) +
  geom_line() +
  facet_wrap(~year_lab, scales = "free_y") +
  ggtitle("Population age and sex distribution over time") +
  labs(x = "Age of population", colour = "") +
  scale_y_continuous("Number of people\n(note that scale varies)", 
                     breaks = unique(as.numeric(p2ad$age_c)), 
                     labels = levels(p2ad$age_c))

total_pop <- alive_at_end_of_year %>%
  group_by(year) %>%
  summarise(population = sum(population)) 

total_pop_sex <- alive_at_end_of_year %>%
  group_by(year, sex) %>%
  summarise(population = sum(population)) 


cdr <- all_deaths %>%
  select(deaths, death_year) %>%
  rename(year = death_year) %>%
  group_by(year) %>%
  summarise(deaths = sum(deaths)) %>%
  left_join(total_pop, by = "year") %>%
  group_by(year) %>%
  summarise(crude_death_rate = sum(deaths) / sum(population) * 1000) 


cbr <- all_borns %>%
  rename(born = population, year = birth_year) %>%
  group_by(year) %>%
  summarise(born = sum(born)) %>%
  left_join(total_pop, by = "year") %>%
  mutate(crude_birth_rate = born / population * 1000)

p3 <- cbr %>%
  select(year, crude_birth_rate) %>%
  left_join(cdr, by = "year") %>%
  gather(variable, value, -year) %>%
  ggplot(aes(x = year, y = value, colour = variable)) +
  geom_line() +
  scale_y_continuous("Births and deaths\nper 1,000 population", label = comma) +
  ggtitle("Crude birth and death rates") +
  scale_colour_manual(values = c("darkgreen", "orange")) +
  labs(x = "Year since simulation began", colour = "") +
  theme(legend.position = "right") +
  geom_smooth(method = "loess", span = 0.1)

p4 <- alive_at_end_of_year %>%
  group_by(year, sex) %>%
  summarise(ave_age = sum(age * population) / sum(population)) %>%
  ggplot(aes(x = year, y = ave_age, colour = sex)) +
  geom_line() +
  labs(y = "Average age", x = "Year since simulation began") +
  ggtitle("Average age over time") + guides(colour = guide_legend(override.aes = list(size=100)))


p5 <- all_deaths %>%
  group_by(birth_year, sex) %>%
  summarise(average_observed_death_age = sum(age * population) / sum(population)) %>%
  ggplot(aes(x = birth_year, y = average_observed_death_age, colour = sex)) +
  geom_line() +
  labs( x = "Birth year (relative to Year Zero for the simulation)", colour = "") +
  ggtitle("Average age at death",
          "Note that this is not the same as life expectancy at birth except in the middle of the data,\nas recent observations are biased to young deaths; and many deaths before year zero were not observed.")


print(p2)
grid.arrange(p1, p3, p4, p5)
