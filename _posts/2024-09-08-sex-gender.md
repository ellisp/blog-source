---
layout: post
title: Gender and sexuality in Australian surveys and census
date: 2024-09-08
tag: 
   - Surveys
   - Australia
   - WorkRelated
   - Health
description: I familiarise myself with the Australian Bureau of Statistics' statistical Standard on sex and gender, and play around with some data from the Australian General Social Survey that has outputs reported by persons' sexual orientation.
image: /img/0278-lollipops.svg
socialimage: https:/freerangestats.info/img/0278-lollipops.png
category: R
---

## The 2021 ABS Standard for 'Sex, Gender, Variations of Sex Characteristics and Sexual Orientation Variables'

Over the past two weeks there has been [quite a controversy](https://www.abc.net.au/news/2024-09-01/anthony-albanese-census-gender-identity-intersex/104296702) relating to questions about sexuality and gender in the next Australian Census of Population and Housing, in 2026. Things hit the news when the testing of "new questions" was withdrawn, then there was an announcement that "the question" will be included; then apparent clarification that one of "the questions" will be but not all of them.

My work includes understanding risks for these sorts of controversies (in other countries, not Australia) so I spent a bit of time getting my head around it. The Australian Broadcasting Corporation reports that the [three originally proposed questions were](https://www.abc.net.au/news/2024-09-06/2026-census-questions-revealed/104321662):

- What is the person's gender?
- How does the person describe their sexual orientation?"
- Has the person been told they were born with a variation of sex characteristics?

The are essentially three of [the standard questions on sex, gender, sexual orientation](https://www.abs.gov.au/statistics/standards/standard-sex-gender-variations-sex-characteristics-and-sexual-orientation-variables/latest-release) in the relevant Australian Bureau of Statistics (ABS) Standard that was released in early 2021 and has had some minor tweaks and corrections since. A fourth question, apparently not up for debate, is:

- What was [your/the person's name/their] sex recorded at birth?

The standard options for this are "Male", "Female", and "Another term (please specify)".

At the time of writing, it looks like the sexual orientation and sex at birth questions *will* be included in the Census but not those on gender and variation of sex characteristics. I guess this reflects a perception that Australian society / politics has a definite widespread acceptance of variation in sexual orientation - it's generally seen that people can sleep with and indeed marry whomever they choose. What hasn't yet become as common apparently is acceptance of a modest degree of complexity in sexual characteristics and gender identity.

The Australian Standard is well worth a read in full for anyone who has to ask or analyse survey questions relating to sex and gender. It's a good piece of work, was very thoroughly researched and consulted on. The concepts are explained clearly, most critically the difference between sex relating to physical characteristics and gender as a social and cultural concept. 

The standard isn't just a list of questions and the wording to use for the answer options (although it does give this, and the carefully defined acceptable variants in limited circumstances, in explicit detail of course). Some of the elements that may not be obvious to people new to this field include, for example, strict rules that the questions have to be asked exactly as written. Interviewers are forbidden from inferring either sex or gender from appearance, names, etc. Or that the preferred word to use for statistics relating to the whole population is "Persons", not "Total" or "People", when presenting total population counts for sex.

As per the Standard, the key variable in the "Cisgender and Trans and Gender Diverse Classification" has to be derived by comparing the answer to "What was this person's sex recorded at birth" and "What is this person's gender". 

There's a decision table in the Standard on how to do this; it gets complicated when one or both of the questions had "Another Term" selected for sex at birth, or "Prefer not to answer" to gender now. In either of these cases the correct value for Cisgender and Trans and Gender Divsere variable becomes "Inadequately described". 

If sex at birth differs from gender now, or gender now is "non-binary" or "different term", the person is classified as "trans". If sex at birth matches gender now, the person is classified as "cis". All this of course looks straightforward and simple when set out clearly. It makes sense, and flows naturally from the realisation that cis/trans status is quite different from gender now . 

It is both offensive and leads to poor quality data for example to give people a choice of male, female, trans-male, trans-female in response to a question on gender. This is precisely because the whole point of "transitioning" for many people is that now you are just whatever the final gender is, not an amalgam of previous and current genders - "trans-women *are women*", and so on. So the correct statistical procedure is to ask separate questions on sex at birth and gender now, and combine the two to determine cis / trans status.

## What's the information for, and what we know

The outcry from the LGBTIQ+ community in response to some of these questions being reported as scrapped from the next Census had it seemed to me two substantive points:

1. More detailed information on these issues is needed for government policy and program decision-making, particularly in health and social policy.
2. There is a "right to be counted" and recognised for who people are, and the Census is an important part of the state validating this aspect of people's existence.

> "Without comprehensive and inclusive Australian census data, the full diversity of LGBTQ+ communities remain invisible and marginalised. This unnecessary lack of data hinders proper coordination of public health measures and social policy. We need political leadership."

*[Lukas Parker on Mastodon](https://aus.social/@Lukas/113035241786325182)*

I think the second of these two arguments - about the rights and symbolism - is actually the stronger. 

Important background on why this is such a sore point is that in the 2021 Census, there was a question on sex that was designed before the 2021 Standard came fully into force (Census questionnaires take years to develop). The very small proportion of people who picked "non-binary" in response to the question on sex was [unhelpfully picked up by a politician as evidence that there were hardly any transgender people in Australia](https://www.abc.net.au/news/2022-10-07/latham-one-nation-transgender-abs-census-population-problematic/101507074). This is clearly an invalid interpretation of a question about sex (not about gender, and certainly not about cis/trans gender status), but one that is unfortunately likely to be repeated in the lack of clearly more appropriate estimates. 

Note that, as described above, the Standard now would require "Another term (please specify)" as the valid third choice for a question on sex at birth. But this still gets one nowhere in terms of counting the number of trans people; a second question on "gender now" is required.

For health or social policy planning, any question in the Census needs to be justified in terms of the extra benefits gained from making *everyone* in the country (not just a survey sample) fill it in. This might be justified, for example, because very fine level of spatial detail is needed that could never be provided by surveys; or a definitive population total is needed for survey design purposes; or we need to know this information on all individuals for later analysis with integrated data. 

I have in fact seen the argument that these population totals are needed for survey calibration and design made in this recent debate but unfortunately have lost the link. But I don't think it's particularly compelling, and have yet to see a good technical argument to the contrary. I believe that survey data could give us good enough estimates of the totals for decision-making, down to number of persons per state, and understanding of characteristics, and creating purposive samples for any given relationship to population proportions. 

This is certainly the case for sexual orientation, where we can go (for example) to the latest (2020) [General Social Survey Table 5](https://www.abs.gov.au/statistics/people/people-and-communities/general-social-survey-summary-results-australia/2020#data-downloads) and see that 773,000 (our of 20.3m) persons aged 15 or over are estimated to be Gay, Lesbian, Bisexual or 'Other'. 

The relative margin of error for that estimate is 9.0%, so perhaps a confidence interval would be around 700 and 850 thousand persons - between 3.5% and 4.2% of the total persons aged 15 and over. 9% isn't great, but I'm confident the measurement error is a bigger risk than sampling error here. That is, people who either don't understand, don't know, or don't want to tell an interviewer (or a government form online) their sexual orientation. A census would remove the sampling error but doesn't help measurement error.

Relating to the number of trans persons in Australia, an estimate could be derived from the [2022 National Health Survey](https://www.abs.gov.au/methodologies/national-health-survey-methodology/2022#overview), which included all four necessary questions in the sex, gender, etc Standard mentioned above. Surprisingly given the public interest in the issue, I haven't been able to find such an estimate published anywhere. From eyeballing and back-of-enveloping the size of confidence intervals in one of the reports based on it, the [National Study of Mental Health and Wellbeing](https://www.abs.gov.au/statistics/health/mental-health/national-study-mental-health-and-wellbeing/2020-2022), I infer that trans people were between 1% and 4% of the *sample* of the National Health Survey. However, as young people were deliberately oversampled in the survey and we definitely think age is correlated to trans status, this is surely an overestimate of the population compared to what you could do with the original microdata. 

Someone should do this! Or if it exists, let me know in the comments. One hour's work (for someone with access to the microdata), compared to the very large cost of including a question in the census. Alternatively, if the data isn't good enough for estimating the number of trans persons in Australia, we should have a discussion of why and what to do about it. Many of the candidate reasons why - to do with measurement or validity error of some kind - would also mean that using the Census for this purpose wouldn't help.

Anyway, my blog is about playing around with data and charts and not politics, so let's go to a visualisation of some of the published results from the 2020 General Social Survey. This survey included the question on sexual orientation and a good range of results are reported from it. After a lot of iteration I came up with this chart:

<object type="image/svg+xml" data='/img/0278-lollipops.svg' width='100%'><img src='/img/0278-lollipops.png' width='100%'></object>

You may need to zoom in on this, but it should be readable on most screens. In this chart I have selected the survey questions that had the largest estimated ratio of difference between heterosexuals and gay, lesbian or bisexual choosing that answer, having first filtered out any questions with less than 5 percentage points of absolute difference. There's a lot packed in but there's some interesting things here:

* Why are more young people reporting as gay, lesbian or bisexual? Obviously there are several candidate explanations: people become straighter as they get older (unlikely); more people are gay, lesbian or bisexual than before; or more people are *admitting to themselves and interviewers* that they are gay, lesbian or bisexual than before. Most likely the answer is some version of this latter point. Cultural norms have changed astonishingly fast, in historical terms, in this space so it's not surprising that there is a generational difference in reported prevalence.
* Gay, lesbian and bisexual people apparently less likely to be unemployed and more likely to be in the highest income quintile
* Gay, lesbian and bisexual people much more likely to be not married or in a de facto marriage than heterosexual people.
* Sadly but unsurprisingly, gay, lesbian and bisexual people much more likely to have experienced discrimination or disagree with the statement that the police and justice system can be trusted

Note that I am very deliberately using the expression "LGB+" here in these charts, not the familiar "LGBTIQ+" expression. LGBTIQ of course does not aspire to be a statistical classification and is more like a social or political coalition; it combines sexual orientation (LGB), sexual characteristics (the "I" for Intersex ) and trans/cis gender status (the "T" for Trans and possibly the "Q" for Queer, although that could really refer to some combination of the three concepts).

Here's the code that downloads the data, tidies it up and presents that chart:

{% highlight R lineanchors %}
library(tidyverse)
library(readxl)
library(glue)


download.file("https://www.abs.gov.au/statistics/people/people-and-communities/general-social-survey-summary-results-australia/2020/GSS_Table5.xlsx",
              destfile = "gss_table5.xlsx", mode = "wb")
demog_cats <- c(
  "Sex", "Whether currently smokes", "Age group", "Employed",
  "Level of highest non-school qualification",
  "Engagement in employment or study",
  "Family composition of household",
  "Marital status",
  "Main Source of Household Income",
  "Current weekly household equivalised gross income quintiles"
)

d <- read_excel("gss_table5.xlsx", skip = 6, sheet = "Table 5.1_Estimate") |>
  rename(variable = ...1) |>
  filter(!is.na(variable)) |>
  mutate(category = ifelse(is.na(Heterosexual), variable, NA)) |>
  tidyr::fill(category, .direction = "down") |>
  filter(!is.na(Heterosexual)) |>
  mutate(sequence = 1:n()) |>
  gather(sexuality, value, -variable, -category, -sequence) |>
  mutate(value = as.numeric(value)) |>
  group_by(sexuality, category) |>
  mutate(prop = value / sum(value)) |>
  mutate(variable = fct_reorder(variable, sequence),
         var_wrap = fct_reorder(str_wrap(variable, 20), sequence)) |>
  ungroup() |>
  mutate(cat_type = ifelse(category %in% demog_cats, 
                           "Characteristics", "Experiences and attitudes")) |>
  mutate(sexuality = ifelse(sexuality == "Gay, Lesbian or Bisexual", "Gay, Lesbian, Bisexual or Other", sexuality))

# check no typos in the demography categories
stopifnot(all(demog_cats %in% d$category))

# some categories have long answers and are difficult to present on a chart
difficult_cats <- c("Community involvement", "Cultural tolerance and discrimination",
                    "Family and community support", "Crime and safety", "Stressors")

# draw basic faceted barchart
d |>
  filter(!category %in% difficult_cats) |>
  filter(sexuality != "Total persons") |>
  filter(variable != "Persons aged 15 years and over") |>
  ggplot(aes(x = var_wrap, y = prop, fill = sexuality)) +
  geom_col(position = "dodge") +
  facet_wrap(~str_wrap(category, 35), scales = "free") +
  labs(x = "", fill = "",
       title = "Comparison of LGB+ and heterosexual attitudes",
       subtitle = "Selected questions from Australia's General Social Survey, 2020") +
  scale_y_continuous(label = percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.8, 0.1))

sc <- 1.1

# better visualisation that focuses on results.
# number of questions to pick to highlight:
k <- 22
d2 <- d |>
  select(category, variable, sexuality, prop, cat_type) |>
  spread(sexuality, prop) |>
  mutate(ratio = `Gay, Lesbian, Bisexual or Other` / Heterosexual,
         rabs = pmax(ratio, 1 / ratio),
         diff = `Gay, Lesbian, Bisexual or Other` - Heterosexual) |>
  mutate(label = case_when(
    category %in% c("Age group", "Marital status", "Employed", "Engagement in employment or study") ~ 
      as.character(variable),
    TRUE ~ glue("{category}:\n'{variable}'"))) |>
  mutate(lab_seq = case_when(
    grepl("employ", label, ignore.case = TRUE) ~ -150,
    grepl("income", label, ignore.case = TRUE) ~ -200,
    cat_type == "Characteristics" ~ -as.numeric(as.factor(label)),
    TRUE ~ diff)) |>
  mutate(label2 = fct_reorder(label, lab_seq))  |>
  arrange(desc(rabs)) |>
  # don't show any where the absolute difference is less than 5 percentage points:
  filter(abs(diff) > 0.05) |>
  slice(1:k)
  
d3 <- d2 |>
  select(label2, glb = `Gay, Lesbian, Bisexual or Other`, 
             hs = Heterosexual, ratio, cat_type) |>
  mutate(glb2 = ifelse(glb > hs, glb -0.026, glb + 0.026)) |>
  # variable for using to draw colour of segments and arrows:
  mutate(variable = ifelse(ratio > 1, "Gay, Lesbian, Bisexual or Other",
                           "Heterosexual"))

# draw lollipop / arrow plot
d2 |>
  select(label2, `Gay, Lesbian, Bisexual or Other`, Heterosexual, cat_type) |>
  gather(variable, value, -label2, -cat_type) |>
  ggplot(aes(x = value, y = label2, colour = variable)) +
  facet_wrap(~cat_type, scales = "free_y") +
  geom_segment(data = d3, linewidth = 1.1,
               aes(yend = label2, xend =glb2, x = hs, 
                   colour = stage(start = variable, 
                                  after_scale = prismatic::clr_lighten(colour, space = "combined"))),
               arrow = arrow(angle = 15, length = unit(0.15, "inches"))) +
  geom_point(size = 4) +
  scale_x_continuous(label = percent) +
  labs(x = "Prevalence of response",
       y = "",
       colour = "",
       title = "Key differences between LGB+ and heterosexual Australians",
       subtitle = "Responses from the General Social Survey 2020")
{% endhighlight %}

In case anyone's interested, I did over 100 iterations of this chart, looking for something that really presents some analytical findings in an interpretable and aesthetically pleasing way. It's much harder to do presentation charts than the straightforward ones that just show all the numbers at once. One interesting point is that the great majority of the work is in reshaping and tidying data - in this case, different (but obviously derivative) data frames for drawing the points and for drawing the arrow segments. A nice example of how data management and plot polishing become an iterative process. You'd never publish the data in the format for a plot like this, it needs to be up to the end analyst to identify this as the plot they want, and reshape the data accordingly.

There was also a lot of effort going into decisions like "Do I need the 'category' (e.g. Age group) for each label on the charts, or are some self-explanatory, and how do I code this in without retyping all the data?".

That code also produced a more straightforward chart which I show here for comparison. It's the same info (although question categories with long names are excluded for readability), but puts a lot more work on the reader to pick out the key messages. The final plot I used tries to help the reader with this by doing a bunch of analysis ourselves first and presenting in a way that highlights the most important aspects of it.

<object type="image/svg+xml" data='/img/0278-many-facets.svg' width='100%'><img src='/img/0278-many-facets.png' width='100%'></object>

That's all for today. Do read (and follow, in your next survey questionnaire with questions on sex or gender) that standard on sex, gender, etc. - it's good stuff.

### Late edits / update

- thanks to @baptnz@social.nz who pointed out on Mastodon a better method of making the arrows appear light rather than the transparency hack I originally used. His method, using the 'prismatic' package, I have edited my code above and is now in use.
- immediately after I wrote all the above I discovered that while I was publishing the original post, the Government has now announced that the questions on sexual orientation, sex at birth, and gender now will all be included in the Census; but not the question on sexual characteristics. So this is a good outcome for those wanting to estimate gender issues and trans/cis status, although those with interest in sexual characteristics are still disappointed.