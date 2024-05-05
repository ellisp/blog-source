---
layout: post
title: Survey books, courses and tools
date: 2018-05-05
tag: 
   - Surveys
   - StatsEducation
   - Books
   - Tools
description: Books, online courses and tools on surveys I've recently visited and liked.  
image: /img/total-survey-error.png
socialimage: https:/freerangestats.info/img/total-survey-error.png
category: R
---

## Surveys everywhere!

Surveys are everywhere, and thinking about them in different ways has dominated my work so far this year.  What's right and wrong with them, how to improve them, do we need them, can we replace this one or that one with administrative data, why do the media (and nearly everyone else) so misunderstand and misinterpret them... those sorts of questions.

Well, they work (mostly), and they're not going anywhere fast.   To take just the most obvious example, have the (unfair) complaints about the pollsters letting down the pundits in the UK and USA in 2016 led to a collapse in the polling industry?  No.  A quick glance at the [Five Thirty Eight data download page](https://data.fivethirtyeight.com/) shows that there have been more than 1,500 polls with a combined sample size of 2.3 *million* Americans in the past 18 months just to help people judge "Are Democrats or Republicans Winning the Race for Congress?"  

In the world of official statistics they're still indispensable in most - perhaps all - countries, despite the rise and promise of administrative data.  It would be difficult to measure under-employment rates, firm-level innovation rates, or smoking prevalence from administrative data, to pick three examples at random.

<img src='/img/total-survey-error.png' align='right'  width='250px'>

I love the thought and rigour that has gone into designing and analysing surveys, by academics, national statistical offices and (to a certain extent) market researchers.  The unifying concept is [total survey error](https://academic.oup.com/poq/article/74/5/849/1817502)
 as seen in this diagram.  This gives us categories like measurement error, coverage error and adjustment error without which it's difficult even to think about whether our estimation process is working and how to improve it.  The concepts evolved through the second half of the twentieth century and matured in the 1980s and 1990s; I don't think any single author can (or does) claim ownership of the whole idea.  

It's a simple but brilliant organising framework for design, analysis and just talking about surveys, and it's one that should be paid more attention by those working with data outside the survey field too.  Even with survey professionals there's far too little attention to sources of error other than the easily-quantified sampling error (inevitably the only acknowledgement of uncertainty quoted in reporting of opinion polls, for example); but in much analysis and discussion in the context of today's data revolution one sees not even that.  I commend the diagram on the right and its many variants to any serious analyst's attention.

So back to this blog... I thought I'd note down good books on this I've read recently and some other stuff.

## Books

My university training emphasised the analysis and sample design aspects of surveys and didn't cover in any detail issues such as questionnaire development and testing, interviewer effects, mode effects and the like.  Can't do everything.  I suspect these are bits of craft more often taught on the job, although there's no reason for this.  I might be wrong about it - certainly there's good university research going on in these areas.  

In my reading this year I emphasised books about what feel to me to be the messier aspects of survey quality and measurement and non-response error.  These include subtle changes of wording and how mode effects are changing as people change the way they relate to strangers asking them questions (whether face to face, on the phone, or online). 

### Paul Biemer and others (edited), [Total Survey Error in Practice](https://www.goodreads.com/book/show/35836150-total-survey-error-in-practice), 2017

I gave this five stars.  It's a great (and very in-depth - not for the faint-hearted) collection of applications and theorising on survey quality.  Covers all aspects - design, details of question wording, data collection in various modes (old and new), evaluation and analysis.  Very much rooted in the real world of today, and today's data problems including new data sources beyond the classic survey.

### Don Dillman and others, [Internet, Phone, Mail, and Mixed-Mode Surveys: The Tailored Design Method](https://www.goodreads.com/book/show/24376057-internet-phone-mail-and-mixed-mode-surveys), 2014.

Another five star entry for me.  This is the latest edition of the bible for putting these survey things together (with an emphasis on questionnaires and their delivery rather than sample design) and it's a very thorough, well evidenced setting out of what works, what doesn't, and how we know.

### Floyd Fowler, [Survey Research Methods](https://www.goodreads.com/book/show/19835325-survey-research-methods), 2013

Four stars.  Nice overview, much more introductory than the other two.  Would suit a busy researcher who needs good understanding of the range of the issues rather than a survey specialist.

## Massive Open Online Course
 
I had the pleasure this year of checking out the Coursera [Survey Data Collection and Analytics specialization](https://www.coursera.org/specializations/data-collection) (a collection of seven courses) with an eye to being able to recommend a general introduction for analysts to the design, management, delivery and analysis of surveys.  The courses are provided via the Coursera platform, by Maryland and Michigan universities.  I rate the overall specialization as four or five stars, and have rated the individual courses as three, four or five stars on Coursera.  In my search for affordable online courses in this area, this was the only one that covered the material in both the range and the depth that I was looking for.  Other courses available are either too superficial, or focus on just one aspect of surveys, or more commonly both.
 
The content is spot on, but the delivery can be heavy going - it's more like a traditional university course than a lot of online courses.  That's a strength in part as well as an issue of course.

The courses cover all aspects of survey management, from a framework in which to plan research, through questionnaire design, the technical aspects of data collection via various modes, processing (weighting, imputation, etc), and analysis.  The courses are rigorous and backed up with the latest research into survey effectiveness, including aspects such as the importance of interviewer effects in inflating the variance of estimates; the relative effectiveness of different modes of delivery in different circumstances; and modern software for analysis (Stata and R examples are provided in the later courses, but this is not a course in Stata or R).
 
There are no pre-requisites for the course but it would be helpful to have some background in university level statistics; otherwise some of the material in courses 4, 5 and 6 on sample design, imputation and weighting, and analysis would be a difficult introduction to those topics (but not impossible).  Overall the courses are rated "intermediate" and there is a fair amount of both material and rigour (in-lecture quizzes, assessed quizzes, and assignments).  A student new to the topics might take five or six months to complete the whole specialisation, with 2 – 6 hours of effort per week.
 
I strongly recommend the first six courses (and optionally the seventh “capstone” project) for anyone working hands-on on an important survey, particularly one that needs to meet standards of official statistics or of rigorous research.  I would recommend the first course ("Framework for Data Collection and Analysis") for managers of people working on surveys, and for people who need to use surveys in their work and need an introduction to concepts such as “Total Survey Error” and a quality framework for surveys.    Other individual courses may be of interest to people with specific needs (eg questionnaire design, sampling issues).
 
For someone aiming to be a hands-on analyst of surveys in the depth covered in these course, I would recommend in parallel building general skills in R.  The best way to do this will depend on learning style but I recommend either the "data analyst with R" stream on [Data Camp](https://www.datacamp.com) or working through the free book [R for Data Science](http://r4ds.had.co.nz/) .  These do not cover survey-specific issues but would build the background skills needed to make the most of the material in courses 5 and 6 of the survey specialization.  Data Camp in particular provides an excellent platform for step by step learning and practice of technical skills; these would need to be placed in context by other training or support.


## Tools

### Survey data collection platforms

The market of suppliers and solutions for professional questionnaire development and delivery is [surprisingly crowded](https://www.capterra.com/survey-software/).  There are in fact many good quality outfits that provide good tools for developing and testing questionnaires, delivery (whether Computer-Assisted Personal Interviewing with accompany tablet software, Computer-Assisted Telephone Interviewing, or web-based) and managing invitations and the like.  Those that I've looked at all looked to be good quality, and mostly offered to look after data management in the cloud during collection too.

The surprise for me in this space was the World Bank's very high quality offering - [Survey Solutions](http://support.mysurvey.solutions/).  It's very good, very powerful (uses C# for more complex things but very simple to get the knack of), great control over survey logic, validation and so-on.  It's definitely up there with or better than the commercial competition, and free for developing in.  Growing fast, with a rapid enhancement development cycle, satisfactory video-based training aimed at the different users (interviewers, designers, etc), and a vibrant user community.  For some customers (eg member governments) the Bank might host data management during collection too.  I definitely recommend looking at Survey Solutions if you're doing a serious survey, particularly CAPI in difficult conditions.

### Processing, analysis and sample design

For analysis of complex surveys, Thomas Lumley's brilliantly effective and comprehensive R `survey` package is the day to day workhorse which meets virtually all my survey-specific needs.  That is, on top of the usual R environment and packages I use for data management and graphics of course.  His book on complex surveys is my handbook for the analytical stage and I hope there will be a new edition soon.

There are other excellent relevant R packages too; see the [CRAN official statistics and survey methodology task view](https://cran.r-project.org/web/views/OfficialStatistics.html).

Dealing with surveys is also a strength of Stata.  

The philosophy of complex survey analysis in Stata is very similar to `survey` in R.  One specifies the data, design (primary sample units, strata, and so on), then uses specialist analysis functions that do things like means, totals, proportions, models in the appropriate way given that design.   This abstracts nearly all the complexity away from the analyst and leads to rapid development and readable code.  

[Q](https://www.qresearchsoftware.com/q-and-displayr) is a nice product for analysis of surveys aimed at people who aren't specialist statisticians.  It's got much more grunt (better graphics and statistics) than the other cross-tabs tools I've seen aimed at market researchers, but is still fairly simple to use.  It handles complexities like multiple response questions ('pick as many as apply...') elegantly and can do statistical inference that appropriately takes the weights into account (not other aspects of sample design though, I think).  For data transform, reshape or graphics that get too much for the point-and-click, the user can write R (nice) or JavaScript (yuck - this isn't what JavaScript was designed for...).  The R connection isn't brilliant - the data and analysis are sent to a distant server and analysed there before coming back to the main Q client, so if you were going to do much of that you'd be better off just using R.  Q is pricey per user compared to R or Stata.

If used out of the box, SPSS will interpret weights as frequencies which leads to wildly inappropriate statistical inference.  It's common in market research to adjust for this by scaling weights to add to the sample size rather than the population but this is very crude, only very partially fixes the problem, and makes it much harder to produce population estimates.  There's a "complex survey" module available that adds in the necessary functionality but it costs extra on top of base SPSS.  I've never seen it in use.  

I don't recommend SPSS for this or any purpose.  There's a nice open source competitor in this space [jamovi](https://www.jamovi.org/), built on R but adding SPSS-like GUI control; but at the moment it can't handle complex surveys at all.  This may change, because anything available in R is in principle simple to add to jamovi.

SAS can handle complex surveys appropriately of course.  It comes with a price tag, and generally less functionality than available for much less cost in R and Stata.

Microsoft Power BI not for serious statistical inference, but it is an effective exploratory and a great dashboarding tool.  In my most recent post a month ago I showed how to [work-around the way it doesn't deal naturally with weighted data](/blog/2018/04/11/weighted-survey-data-with-power-bi).

None of the above is comprehensive, just some of the survey-related things I've been thinking about recently.

That's all for today.  If I were assigning exercises it would be - go have another look at the total survey error diagram, and think about how it applies to other data analysis!
