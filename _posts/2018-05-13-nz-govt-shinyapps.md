---
layout: narrowwithcommentsandnextprev
title: Fifteen New Zealand government Shiny web apps
date: 2018-05-13
tag: 
   - WorkRelated
   - Shiny
   - Tools
description: I had a brief look around New Zealand government agency websites and found 15 high quality web apps written in the Shiny platform.
image: /img/0122-treasury-insights-small.gif
socialimage: https:/freerangestats.info/img/0122-treasury-insights-small.gif
category: R
---
<style>
table, td, th {
    border: 3px solid white;
	border-collapse:separate;
    border-spacing:0 5px;
}

img {
	width: 100%;
}
</style>


<h2>At a glance:</h2>
   <p>I had a brief look around New Zealand government agency websites and found 15 high quality web apps written in the Shiny platform.</p>
   <p class="meta">13 May 2018</p>
<hr>

Here's fifteen nice web applications built with RStudio's Shiny framework.  All of these are owned and maintained by New Zealand government departments and have a main purpose as making public data more available and accessible for non-specialist users.  I think it's fair to say New Zealand has been a leader in using Shiny for this.  

As a commissioner of several of those below and author of one, I can say the appeal of Shiny for this sort of public dissemination of data is the combination of *cheap*, *quick* and *pretty good*.  It's rare to have so little trade-off between those three items of the project management dilemma.  Usually it's "cheap, quick, good - pick any two".  

The modest trade-off in my view is in some small aspects of the eventual quality and performance.  The server-client transfers can seem clunky (certainly compared to something written in pure JavaScript), particularly when you're at the bottom of the world using a server in the USA.  They have an annoying habit of freezing and going grey on your users if something goes wrong with the connection.  And as they get more complicated, you start using more JavaScript, HTML and CSS anyway.  But the results are 80% or 90% good enough for many use cases, and development and deployment costs are materially less than other options.  I find Shiny more flexible, powerful and controllable (eg fonts, polish, etc which can all be done with CSS) than Tableau or Power BI, and cheaper and quicker than writing your own app from the ground up.

So, go Shiny!	
	
### Ministry of Business, Innovation and Employment

<table>

<tr><td>I'm pretty sure the New Zealand Tourism Forecasts was the first use of Shiny by a government agency in New Zealand.  My old team produced this tool in 2015.  It's nice and simple, not particularly ambitious, but it does the job of letting the user play around with the forecasts much more effectively and better presented than previous dissemination methods (ie Excel pivot tables, if you were lucky).</td><td width="350px"><img src='/img/0122-tourism-forecasts-small.gif'></td></tr>

<tr><td>The <a href='http://tourismdashboard.mbie.govt.nz/'>New Zealand Tourism Dashboard</a> was our first ambitious big Shiny project.  A brilliant job by Jimmy Oh, first of his sequence of high quality boundary-pushing apps for MBIE.  It combines data from MBIE itself, Stats NZ, and direct from the web.  The <a href='https://github.com/nz-mbie/tourism-dashboard-public'>source code is on GitHub</a>.</td><td width="350px"><img src='/img/0122-tourism-dashboard-small.gif'></td></tr>

<tr><td>Building on the style of the tourism dashboard came the <a href='http://sectorsdashboard.mbie.govt.nz/'>New Zealand Sectors Dashboard</a>.  It aims to be a one-stop shop for all information about New Zealand's economy by sectors. It brings together a range of economic datasets produced by MBIE and Statistics New Zealand into one easy-to-use tool. </td><td width="250px"><img src='/img/0122-sectors-dashboard-small.gif'></td></tr>

<tr><td>And the <a href='https://mbienz.shinyapps.io/labour-market-dashboard_prod/'>New Zealand Labour Market Dashboard</a>.  It displays labour market information from many different sources in one place.</td><td width="250px"><img src='/img/0122-labour-dashboard-small.gif'></td></tr>

<tr><td>The <a href='https://mbienz.shinyapps.io/urban-development-capacity/'>Urban Development Capacity Dashboard</a>, jointly branded by MBIE and the Ministry for the Environment, provides charts, maps, tables and underlying data on local markets for housing and business space.</td><td width="250px"><img src='/img/0122-urban-dashboard-small.gif'></td></tr>

<tr><td>The <a href='https://mbienz.shinyapps.io/mtagdp/'>Modelled Territorial Authority GDP</a> is the only Shiny app on this page I can claim to have authored personally.  It was the tail end (dissemination) of a big project producing new granular estimates of value add by industry, district and city.  There is a paper and presentation on this on the <a href='/presentations/index.html'>presentations part of my website</a> and <a href='https://github.com/nz-mbie/MTAGDP'>source code for the app, and the much bigger job of creating the data, is on GitHub</a>.</td><td width="250px"><img src='/img/0122-mtagdp-small.gif'></td></tr>

 </table>

### Stats NZ

<table>

<tr><td>The <a href='https://statisticsnz.shinyapps.io/livingcostsexplorer/'>Living Cost Explorer</a> presents data from Household Living-costs Price Indexes.  It shows how price changes vary depending upon the average basket of goods of different types of people, such as beneficiaries, Māori and superannuitants.</td><td width="350px"><img src='/img/0122-livingcost-small.gif'></td></tr>

<tr><td><a href='https://statisticsnz.shinyapps.io/irrigated_land/'>Irrigated land in New Zealand</a> uses maps and graphs to present spatial information on irrigation of New Zealand land.</td><td width="350px"><img src='/img/0122-irrigation-small.gif'></td></tr>

<tr><td>This <a href='https://statisticsnz.shinyapps.io/landcover/'>Landcover</a> tool shows composition and changes in land cover.</td><td width="250px"><img src='/img/0122-landcover-small.gif'></td></tr>

<tr><td>The third of these spatial / environmentally themed tools, <a href='https://statisticsnz.shinyapps.io/livestock_update/'>Livestock numbers</a> has graphs and maps showing the distribution of cattle (different types thereof), sheep and deer.</td><td width="250px"><img src='/img/0122-livestock-small.gif'></td></tr>

<tr><td>The <a href='https://shinyapps.stats.govt.nz/tkpie/'>Iwi cultural well-being from Te Kupenga 2013</a> app may be the first Shiny app with an option to swap the user interface into Te reo Māori.</td><td width="250px"><img src='/img/0122-tkpie-small.gif'></td></tr>

<tr><td>This series of <a href='http://innovation.stats.govt.nz/initiatives/experimental-estimates-of-income-from-linked-administrative-data-methods-and-results/'>Experimental estimates of income</a> has been derived from the tax data available in the Integrated Data Infrastructure as part of ongoing work at Stats NZ to increase the use of administrative data in the production of statistics.</td><td width="250px"><img src='/img/0122-income-small.gif'></td></tr>

 </table>

### Ministry of Health

<table>

<tr><td> An interactive tool for exploring <a href='https://minhealthnz.shinyapps.io/nz-health-survey-2016-17-annual-data-explorer/'>New Zealand Health Survey data</a>.It presents the latest results by sex, age, ethnic group and neighbourhood deprivation, as well as changes over time.</td><td width="350px"><img src='/img/0122-nzhs-small.gif'></td></tr>

<tr><td>A tool to allow <a href='https://minhealthnz.shinyapps.io/datapharm-beta/'> summary data about prescriptions and dispensings</a> funded by the New Zealand Government.</td><td width="350px"><img src='/img/0122-datapharm-small.gif'></td></tr>

 </table>

### Treasury

<table>

<tr><td>Treasury's <a href='https://insights.apps.treasury.govt.nz/'>Insights</a> tool provides information drawn from a range of public sector agencies including extensive use of the Integrated Data Infrastructure.</td><td width="350px"><img src='/img/0122-treasury-insights-small.gif'></td></tr>

 </table>
  
 Nice collection.  Anyone know of any others?	
 
 <b>Disclaimer:</b> I was part of the commissioning team for several of the MBIE Shiny apps, and (as noted above) the author of one.  I haven't been involved in development of any of the others listed above.
 
 
### Making animated GIFs of websites

To make the animated GIFs used in this website and keep them to a reasonable size (under 2MB each), here's what I did.

* I did the original screen captures using the open source [CamStudio](http://camstudio.org/) application and saved them as .avi files.   Even though only a part of my screen was captured, with the original screen resolution and about 30 - 45 seconds of content, these files were large; typically 700MB or larger.
* I used a [Python program](https://gist.github.com/michaelosthege/cd3e0c3c556b70a79deba6855deb2cc8) that GitHub user michaelosthege had published as a Gist to convert from .avi to .gif format.  These were about a quarter or fifth the size, but still too large (150MB - 200MB) to use on the web
* I found another [Python program](https://gist.github.com/PaulineLc/46bb8ddec8a1c3279c24482ae48a1e06) by PaulineLc on another Gist that shrank and sped up animated GIFs.

Is it ironic that a blog post celebrating R Shiny used Python for playing around with the animated images?  I don't think so at all; it's just a matter of using the convenient and easy tool for the job at hand.  Python is awesome with everything to do with images; There are ways in R to do this too (or they could certainly be developed) but it was easier to find out how to do it in Python.  I am not as good with Python as with R, but I know how to copy and paste a program someone else has written when it does exactly what I need!

