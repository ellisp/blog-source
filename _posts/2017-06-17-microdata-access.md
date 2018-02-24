---
layout: post
title: Stats NZ encouraging active sharing for microdata access projects
date: 2017-06-17
tag: 
   - CodingStyle
   - Reproducibility
   - WorkRelated
description: Excited that New Zealand's Government Statistician is promoting reproducibility and open access to code, tabular outputs and research products from research with confidentialised microdata.
image: /img/data-in-idi-oct-16.png
socialimage: http://ellisp.github.io/img/data-in-idi-oct-16.png
category: R
---

### The Integrated Data Infrastructure (IDI) is an amazing research tool

One of New Zealand's most important data assets is the [integrated data infrastructure](http://www.stats.govt.nz/browse_for_stats/snapshots-of-nz/integrated-data-infrastructure.aspx):

> "The Integrated Data Infrastructure (IDI) is a large research database containing microdata about people and households. Data is from a range of government agencies, Statistics NZ surveys including the 2013 Census, and non-government organisations. The IDI holds over 166 billion facts, taking up 1.22 terabytes of space – and is continually growing. Researchers use the IDI to answer complex questions to improve outcomes for New Zealanders."

*Quote from [Stats NZ](http://www.stats.govt.nz/browse_for_stats/snapshots-of-nz/integrated-data-infrastructure.aspx)*

The data already linked to the IDI "spine" is impressive:

[<img src='/img/data-in-idi-oct-16.png' width='100%'>](http://www.stats.govt.nz/browse_for_stats/snapshots-of-nz/integrated-data-infrastructure/idi-data.aspx)

### Security and access is closely controlled

The IDI can be accessed only from a small number of very secure datalabs with frosted windows and heavy duty secure doors.  We have one of these facilities at my workplace, the Social Investment Unit (to become the Social Investment *Agency* from 1 July 2017). *Note/reminder: as always, I am writing this blog in my own capacity, not representing the views of my employer*.  

User credentials and permissions are closely guarded by Stats NZ and need to be linked to clearly defined public-good (ie non-commercial) [research projects](http://www.stats.govt.nz/browse_for_stats/snapshots-of-nz/integrated-data-infrastructure/researchers-using-idi.aspx).  All output is checked by Stats NZ against [strict rules for confidentialisation](http://www.stats.govt.nz/tools_and_services/microdata-access/data-lab/microdata-output-guide.aspx) (eg random rounding; cell suppression in specified situations; no individual values even in scatter charts; etc) before it can be taken out of the datalab and shown to anyone.  All this is quite right - even though the IDI does not contain names and addresses for individuals (they are stripped out after matching, before analysts get to use the data) and there are strict rules against trying to reverse the confidentialisation, it does contain very sensitive data and must be closely protected to ensure continued social permission for this amazing research tool.

### More can be done to build capability, including by sharing code

However, while the security restrictions are necessary and acceptable, other limitations on its use can be overcome - and Stats NZ and others are working to do so.  The biggest such limitation is *capability* - skills and knowledge to use the data well. 

The IDI is a largish and complex SQL Server datamart, with more than 40 origin systems from many different agencies (mostly but not all of them government) outside of Stats NZ's direct quality control.  Using the data requires not just a good research question and methodology, but good coding skills in SQL plus at least one of R, SAS or Stata.  Familiarity with the many data dictionaries and metadata of varied quality is also crucial, and in fact is probably the main bottleneck in expanding IDI usage.  

A typical workflow includes a data grooming stage of hundreds of lines of SQL code joining many database tables together in meticulously controlled ways before analysis even starts.  That's why one of the objectives at my work has been to create assets like the "Social Investment Analytical Layer", a repository of code that creates analysis-ready tables and views with a standardised structure (each row a combination of an individual and an "event" like attending school, paying tax, or receiving a benefit) to help shorten the barrier to new analysts. 

We [publish that and other code with a GPL-3 open source license on GitHub](https://github.com/nz-social-investment-unit/social_investment_analytical_layer) and have plans for more.  Although the target audience of analysts with access to the IDI is small, documenting and tidying code to publication standard and sharing it is important to help that group of people grow in size.  Code sharing until recently has been ad hoc and very much depending on "who you know", although there is a Wiki in the IDI environment where key snippets of data grooming code is shared.  We see the act of publishing on GitHub as taking this to the next step - amongst other things it really forces us to document and debug our code very thoroughly!

### Stats NZ is showing leadership on open data / open science

Given the IDI is an expensive taxpayer-created asset, my personal view is that all code and output should be public.  So I was very excited a few weeks back when Liz MacPherson, the Government Statistician (a statutory position responsible for New Zealand's official statistics system, and also the Chief Executive of Stats NZ) sent out an email to researchers "encouraging active sharing for microdata access projects" including the IDI.

I obtained Liz's permission to reproduce the full text of her email of 29 May 2017:

<div style="font-family:Times New Roman;">
<hr>
<p><b>Subject:</b> Stats NZ: Encouraging active sharing for microdata access projects</p>

<p>Stats NZ is all about unleashing the power of data to change lives. New Zealand has set a clear direction to increase the availability of data to improve decision making, service design and data driven innovation.
To support this direction, Stats NZ is moving to an open data / open science environment, where research findings, code and tables are shared by default.</p>

<p>We are taking steps to encourage active sharing for microdata access projects. All projects that are approved for access to microdata will be asked to share their findings, code and tables (once confidentiality checked).</p>

<p>The benefits of sharing code and project findings to facilitate reproducible research have been identified internationally, particularly among research communities looking to grow access to shared tools and resources. To assist you with making code open there is guidance available on how to license and use repositories like GitHub in the New Zealand Government Open Access and Licensing (NZGOAL) Framework – Software Extension.</p>

<p>Stats NZ is proud to now be leading the Open Data programme of work, and as such wants to ensure that all tables in published research reports are made available as separate open data alongside the reports for others to work with and build on.
There are many benefits to sharing: helping to build communities of interest where researchers can work collaboratively, avoid duplication, and build capability. Sharing also provides transparency for the use that is made of data collected through surveys and administrative sources, and accountability for tax-payer dollars that are spent on building datasets and databases like the IDI.</p>

<p>One of the key things Stats NZ checks for when assessing microdata access applications is that there is an intention to make the research findings available publicly. This is why the application form asks about intended project outputs and plans for dissemination. </p>

<p>While I have been really pleased to see the use that is made of the IDI wiki and Meetadata, the sharing of findings, code and tables is still disproportionately small in relation to the number of projects each year. This is why I am taking steps to encourage sharing. Over time, this will become an expectation.</p>

<p>More information will be communicated by the Integrated Data team shortly by email, at the IDI Forums and on Meetadata. </p>

<p>In the meantime, if you have any suggestions about how to make this process work for all, or if you have any questions, please contact ....., Senior Manager, Integrated Data ......</p>

<p>Warm regards</p>

<p>Liz</p>


<p>Liz MacPherson</p>

<p>Government Statistician/Chief Executive</p>

<p><a href='http://stats.govt.nz'>Stats NZ Tatauranga Aotearoa</a> </p>

<hr>
</div>

This is great news, and a big step in the right direction.  It really is impressive for the Government Statistician to be promoting reproducibility and open access to code, outputs and research products.  I look forward to seeing more microdata analytical code sharing, and continued mutual support for raising standards and sharing experience.

Now to get version control software into the datalab environment please! (and yes, there is a Stats NZ project in flight to fix this critical gap too).  My views on the critical importance of version control are set out in [this earlier post](/blog/2016/09/16/version-control)...
