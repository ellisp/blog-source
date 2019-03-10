library(ozfedelect)
library(tidyverse)
library(scales)
library(grid)
library(Cairo)

# Some checks.
# -7.5 for Aston in 2013 meant a swing against the Labor govt
# - 10.1 for Bass in 2016 meant a swing against the Lib/Nat govt
results_2pp_div %>%
  select(division_nm, swing_to_govt, election_year) %>%
  spread(election_year, swing_to_govt)


#---------------------------explore distribution of swings---------

# We are interested for future forecasting models in the variance of division-level swings
# that are not explained by the nation-wide swing
model <- lm(swing_to_govt ~ avg_swing, data = d)
confint(model)
coef(model) # of course the slope is 1 and the intercept is 0, basically - by design
# the interesting thing is actually the residual standard error:
summary(model)

residual_sd <- summary(model)$sigma

# Let's try to visualise those historical swings
d <- results_2pp_div  %>%
  group_by(election_year, incumbent) %>%
  mutate(avg_swing = sum(swing_to_govt * total_votes) / sum(total_votes)) %>%
  ungroup() %>%
  filter(abs(swing_to_govt) < 20) %>%
  mutate(year = fct_reorder(as.ordered(election_year), avg_swing))

avgs <- distinct(d, avg_swing, year, incumbent)

annotation_col <- "grey50"
update_geom_defaults("label", list(family = main_font, fill = "white", colour = annotation_col, alpha = 0.8))

p7 <- d %>%
  ggplot(aes(x = avg_swing / 100, y = swing_to_govt / 100)) +
  geom_smooth(se = FALSE, method = "lm") +
  geom_vline(xintercept = 0, colour = annotation_col) +
  geom_hline(yintercept = 0, colour = annotation_col) +
  geom_point(aes(colour = incumbent)) +
  geom_text(data = avgs, y = -0.18, aes(label = year, colour = incumbent)) +
  labs(caption = "Source: Australian Electoral Commission data, analysed by freerangestats.info.") +
  scale_x_continuous("Overall swing towards incumbent government", label = percent_format(accuracy = 1)) +
  scale_y_continuous("Division level swings to government", label = percent_format(accuracy = 1)) +
  annotate("label", x = -0.053, y = 0.06, label = "Strongly change\ngovernment") +
  annotate("label", x = -0.045, y = -0.065, label = "Narrow escape for\nHoward in 1998") +
  annotate("label", x = 0.0055, y = 0.04, label = "Strongly retain\ngovernment") +
  scale_colour_manual("Incumbent government:", 
                      values = c("ALP" = "#e53440", "Lib/Nat" = "#1c4f9c")) +
  ggtitle("Individual seats face more voting uncertainty than Australia as a whole",
          "Each point represents the swing in a single seat; residual standard deviation of about 3.2 percentage points around the nation-wide swing.")

CairoSVG("../img/0146-incumbency.svg", 13, 9)
print(p7)
dev.off()

#--------------------explore spatial aspects of swings---------------------------

CairoSVG("../img/0146-2pp-swing-2016.svg", 8, 6.5)
ozpol_infographic(2016, variable = "swing_to_govt", fontfamily = main_font)
dev.off()

CairoSVG("../img/0146-2pp-votes-2016.svg", 8, 6.5)
ozpol_infographic(2016, fontfamily = main_font)
dev.off()

CairoSVG("../img/0146-2pp-swing-2013.svg", 8, 6.5)
ozpol_infographic(2013, variable = "swing_to_govt", fontfamily = main_font)
dev.off()

CairoSVG("../img/0146-2pp-votes-2013.svg", 8, 6.5)
ozpol_infographic(2013, fontfamily = main_font)
dev.off()

CairoSVG("../img/0146-2pp-swing-2010.svg", 8, 6.5)
ozpol_infographic(2010, variable = "swing_to_govt", fontfamily = main_font)
dev.off()

CairoSVG("../img/0146-2pp-votes-2010.svg", 8, 6.5)
ozpol_infographic(2010, fontfamily = main_font)
dev.off()

#----------------------- explore relationship to census variables------------

d1 <- results_2pp_div %>%
  filter(election_year == 2016) %>%
  left_join(div_census_2016, by = "division_nm") %>%
  select(division_nm, state_ab, lib_nat_percentage, swing_to_govt, young_persons:only_english_spoken_home) %>%
  mutate(state_ab = fct_relevel(state_ab, "NSW"))

d2 <- d1 %>%
  gather(variable, value, -division_nm, -lib_nat_percentage, -swing_to_govt, -state_ab)


p8 <- d2 %>%
  ggplot(aes(x = value, y = lib_nat_percentage / 100)) +
  facet_wrap(~variable, scale = "free_x") +
  scale_y_continuous("Two-party-preferred vote for Liberal/National Coalition") +
  geom_smooth(method = "gam") +
  geom_point(aes(colour = state_ab)) +
  ggtitle("Vote compared to census variables by electoral division",
          "2016 federal election") +
  labs(colour = "",x = "")

p9 <- d2 %>%
  ggplot(aes(x = value, y = swing_to_govt / 100)) +
  facet_wrap(~variable, scale = "free_x") +
  scale_y_continuous("Two-party-preferred swing towards Liberal/National Coalition (incumbent government)") +
  geom_smooth(method = "gam") +
  geom_point(aes(colour = state_ab)) +
  ggtitle("Swing compared to census variables by electoral division",
          "2016 federal election") +
  labs(colour = "",x = "")

d3 <- d1 %>%
  select( -division_nm, - lib_nat_percentage) %>%
  mutate_at(vars(young_persons:only_english_spoken_home), scale)

mod1 <- lm(I(swing_to_govt / 100) ~ ., data = d3)

p10 <- confint(mod1)[-1, ] %>%
  as.data.frame() %>%
  mutate(var = rownames(.),
         var = gsub("state_ab", "", var),
         var = gsub("_", " ", var)) %>%
  rename(lower = `2.5 %`,
         upper = `97.5 %`) %>%
  mutate(mid = (lower + upper) / 2,
         var = fct_reorder(var, mid)) %>%
  ggplot(aes(x = lower, xend = upper, y = var, yend = var)) +
  geom_vline(xintercept = 0, colour = "steelblue") +
  geom_segment(size = 3, colour = "grey") +
  scale_x_continuous("Impact of change in one standard deviation in census variable on swing",
                     label = percent) +
  labs(y = "", 
       caption = "Source: ABS Census data, AES election results, analysis by http://freerangestats.info") +
  ggtitle("Division-level variables related to a swing to the Liberal-National Coalition",
          "Comparing the 2016 results to 2013 by electoral division (or 'seat').
          Conclusions about individual characteristics relating to vote should be drawn only with great caution.")

CairoSVG("../img/0146-census-vote.svg", 8, 6)
print(p8)
dev.off()

CairoSVG("../img/0146-census-swing.svg", 8, 6)
print(p9)
dev.off()

CairoSVG("../img/0146-mod-results.svg", 8, 6)
print(p10)
dev.off()

convert_pngs("0146")