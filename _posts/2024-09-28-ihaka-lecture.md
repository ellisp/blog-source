---
layout: post
title: Git, peer review, tests and toil
date: 2024-09-28
description: I was honoured to give the third and final Ihaka Lecture in the 2024 series. My talk had the theme "Making R work in government".
image: /img/git-project-diagram-spc-themed.png
socialimage: https:/freerangestats.info/img/git-project-diagram-spc-themed.png
category: R
---

This week I was in Auckland New Zealand to deliver the third and final of the 2024 series of [the Ihaka Lectures](https://www.auckland.ac.nz/en/science/about-the-faculty/department-of-statistics/ihaka-lecture-series.html), named after legendary denizen of University of Auckland's statistics department Ross Ihaka, one of the two co-founders of the statistical computing language R.

I have added links to the video of the talk (it was live-streamed), my slides, and the 'storyline' summary I used to help me structure the talk to my mostly-neglected [presentations page on this blog](/presentations/index.html).

Here is perhaps the key image from the talk, a slide showing an all-purpose workflow for an analytical project, drawing on a large and persistent data warehouse, plus project specific data, and having a deliberate processing stage to combine the two into an analysis-ready "project-specific database". I've been using variants of this diagram for more than 10 years now, and it will be familiar to anyone from my days with New Zealand's Ministry of Business, Innovation and Employment, or international management consultancy Nous Group.

<img src='/img/git-project-diagram-spc-themed.png' title='Diagram of workflows showing an enterprise data warehouse, project specific data, R combining the two into a project-specific database, R used for analysis, and output in Word, PowerPoint, Quarto, Shiny, Tableau or Power BI' width = '95%'>

Overall, I emphasised the importance of R being part of a broader toolkit and a broader transformation - with Git and SQL the two non-negotiable must-have partners to successfully make R work in government.

I also talked a bit about how errors in analysis are universal, invisible, and catastrophic. If that doesn't motivate people to start doing some decent quality control, I don't know what will! 

The 'storyline' is a great technique I was trained on in a course on writing for the New Zealand public sector. I always find it helps to structure reports and presentations if I take the time to plan them first. In case people are interested in making their own summaries of this sort, you could use the [RMarkdown source code of that storyline](https://github.com/ellisp/ihaka-lecture-2024/blob/main/ellis-ihaka-storyline.Rmd), which of course is available in GitHub (or I'd be a bit of a hypocrite wouldn't I). It uses the flexdashboard template. Of course a storyline doesn't need to be written in RMarkdown, but I find it a simple and disciplined way to write them without having to worry about formatting.