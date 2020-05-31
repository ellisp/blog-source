---
layout: post
title: A health data firm making extraordinary claims about its data
date: 2020-05-30
tag: 
   - Health
   - Tools
description: Surgisphere, a tiny startup that claims to be providing large real world data for scientific health studies, is probably fabricating data at scale.
image: /img/0185-surgisphere-linkedin.png
socialimage: http://freerangestats.info/img/0185-surgisphere-linkedin.png
category: 
---

## A peer-reviewed study that probably used fabricated data

If you're following at all the search for COVID-19 treatments, and possibly even if not, you will have seen the flurry of media coverage for [the observational study in The Lancet 'Hydroxychloroquine or chloroquine with or without a macrolide for treatment of COVID-19: a multinational registry analysis](https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)31180-6/fulltext). It made the news not least because hydroxychloroquine is the drug President Trump says he is taking in the belief that it will reduce his chance of catching COVID-19. This view is not backed up evidence until some randomised trials come in. Getting in before the trials, the Lancet study used propensity score matching to try to control for the non-random treatment. It found that taking hydroxychloroquine and chloroquine were associated with an increased risk of heart problems.

I am highly skeptical of the powers of hydroxychloroquine with relation to COVID-19 ('skeptical' in the sense that I have suspended judgement for now - there simply isn't evidence either way). But I want the test of its properties to be done properly, with random controlled trials. And if we are to use observational studies (which I do not object to, they just aren't as useful as an experiment where you can manipulate the treatment), they have to use real data.

The data in that study, and in at least [one preprint on a second treatment](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3580524), were provided by an Illinois firm called Surgisphere. Allegedly the data represents the treatment and health outcomes of 96,032 patients from 671 hospitals in six continents. However, there is simply no plausible way I can think of that the data are real. 

I'll say that again - *I believe with very high probability the data behind that high profile, high consequence Lancet study are completely fabricated*.

If Surgisphere can name the 671 participating hospitals or otherwise prove that the data is real I will retract that statement, delete this post or write whatever humbling apology they desire. But I think there's nearly zero chance of that happening. 

## Could Surgisphere really have patient data from 671 hospitals?

I'm far from the first to ask for more information on this amazing new database no-one had heard of, and they've had a week to explain. This what they came up with:

> The Surgisphere registry is an aggregation of the deidentified electronic health records of customers of QuartzClinical, Surgisphere’s machine learning program and data analytics platform. Surgisphere directly integrates with the EHRs of our hospital customers to provide them actionable data insights to improve efficiency and effectiveness. As part of these QuartzClinical customer agreements, Surgisphere, as a global healthcare data collaborative, has permission to include these hospitals’ EHR data in its queryable registry/database of real-world, real-time patient encounters.

...

> While our data use agreements with these institutions prevents us from sharing patient level data or customer names, we are able to complete appropriate analyses and share aggregate findings to the wider scientific community. 

("EHR" is Electronic Health Record ie patients' personal data). Frankly, this doesn't pass the laugh test. 

I can imagine why any hospital customers would not want to be named, because if it came out that they are allowing their data to go to Illinois to be analysed at will - the Lancet article says it was "deemed" that ethical approval was not needed - there would surely be an outcry. This would be a much bigger scandal than Facebook giving data to Cambridge Analytica. After all, what we post on Facebook was seen by many people as quasi-public. *Imagine having your electronic health records - patient demographics, medical history, medications, allergies, lab results, radiology results - given to Cambridge Analytica*.

In Australia, we recently had a major public controversy about sharing health records between health providers. I can't *imagine* the reaction if it was found they were being shared with overseas researchers without permission or knowledge. And the fact that hospitals aren't named by Surgisphere means that no patient of any hospital in the world knows whether or not their data is used in this study.

But hang on, you might say, this data (which remember, I think doesn't exist but let's pretend it does for the sake of argument) isn't going to a shady outfit like Cambridge Analytica, it's going to the "global healthcare data collaborative" Surgisphere.

Right, let's look at Surgisphere. Surgisphere has [five employees](https://www.linkedin.com/company/surgisphere/) with LinkedIn accounts. Other than the CE and co-author of the Lancet paper, these are a VP of Business Development and Strategy, a Vice President of Sales and Marketing, and two Science Editors (actually, one Science Editor and one Scoence Editor, which does not inspire confidence in their attention to detail while editing). LinkedIn also records [one employee of QuartzClinical](https://www.linkedin.com/in/ariane-anderson-60189331/) - a director of sales marketing.

<img src="/img/0185-surgisphere-linkedin.png" width = '100%'>

Here are some of the people you might expect to work for a genuine global health care data collaborative, that had sold software to 671 hospitals and integrated with their electronic health record (EHR) systems, and that coordinates an ongoing international health research collaboration:

- global network manager and coordinators
- hospital/customer liaison team
- support staff / help desk
- trainers, and developers of training material
- researchers
- legal team to deal with privacy and contract issues in dealing with 670+ hospitals. Issues coming from the EU's GDPR alone would keep a substantial legal team busy I'm sure.
- software or database developers. Like, maybe a humble extract-transform-load developer or two to get those billions of rows of transactions data into a database.
- database administrators and data engineers
- EHR integration solutions specialists
- data governance lead
- if any of the above are outsourced, a procurement team to handle all the sub-contracting

Surgisphere does not have any of these people, except for Sapan Desai who doubles up as chief executive and medical researcher (a good indication of the size of the firm - most CEs are not also active publishing researchers). Judging from its LinkedIn profile, his team is three sales executives and two science editors. 

Nor does Surgisphere or any of its staff have a presence on GitHub. Nor an explanation anywhere of the impressive data engineering that would be required to wrangle all that data. Nor journal articles, conference papers or even blog posts describing its network, the APIs that connect it, how proud they are of their Hadoop cluster on AWS, which database platform they use, etc etc etc - all the things that real firms that have made impressive innovations (like the first ever world-wide database of individual level hospital data would be, if it were real).

Yet Surgisphere claims to have sold software to 671 hospitals. What would it cost to deploy machine learning data analytics software to a hospital and integrate it with the EHR? This isn't some light and easy integration like installing a stats package on a PC and giving it an ODBC connection to a database. The integration to the EHR systems and the way we know they use the data means, at a minimum, sending all the data to the cloud. That means you need to deal with network and security architects, have extremely robust testing, bullet-proof security (remember, some of the closest guarded sensitive data in the world), go through who knows what red tape at each hospital in terms of convincing their data governance people of what you are doing.

I don't know, but $1 million each deployment can't be far off the mark. Certainly not less than $300k a pop. So Surgisphere should be a billion dollar company if it's done this 670 times, but it clearly is not. In fact, [Dun and Bradstreet](https://www.dnb.com/business-directory/company-profiles.surgisphere_corporation.4f5a118de8afca42c865aa2e3e64fd91.html) estimate its revenue at $45,245. You couldn't even do the discovery stage of an EHR integration project at a single hospital for that, never mind deploy anything. 

Of course, EHR integration is a real thing, and it's done usually to move patient information securely around. For example, a quick google found [this useful presentation](https://www.gl-hc.org/wp-content/uploads/2015/10/Klausing-Porter-EMRIntegration-SummerSummit.pdf) about EMR integration (EMR and EHR are basically interchangeable terms) in the Great Lakes region. I notice Surgisphere is conspicuously absent from the list of presenters on slide 10. This makes it kind of surprising (but not really) that they claim in the Lancet article to have data on most COVID-19 hospital cases in North America diagnosed before 14 April 2020 - 63,315 such cases in the study according to [Table S1](https://www.thelancet.com/cms/10.1016/S0140-6736(20)31180-6/attachment/b67ebe63-0307-4167-82b1-404d6ad85da9/mmc1.pdf.), which would have been a clear majority of all hospital cases.

## What about QuartzClinical software?

What about this QuartzClinical software that is claimed to have been sold to 671 hospitals and is sending the data back to Chicago? It has its [own website](https://www.quartzclinical.com/). It claims to use "machine learning and advanced statistical analysis" to help decision making. Remarkably, it "successfully integrates your electronic health record, financial system, supply chain, and quality programs into one platform". Let's revise my $1 million estimate to $10 million over three years, minimum, if that means you're replacing those things with one platform. But probably it just means a data warehouse that pulls from your various data sources, and has an analytical layer and recommendation engine on top. Straightforward business intelligence stuff, but still a big project for a hospital.

I can't say more than that because the QuartzClinical site is very light on details. It doesn't have any customer testimonials. It doesn't talk about what's under the hood. It doesn't have any information on versions or history or the forward roadmap. It *does* claim to have won some awards though. Let's see:

- "Grand Prize in Quality, International Hospital Federation 39th World Hospital Congress 2015". **Nope**, that [actually went](https://worldhospitalcongress.org/past-winners/) to Texas Children's Hospital for "Advanced Population Health - the critical role of care delivery systems".
- "Second place Dr Kwang Tae Kim Grand Award, International Hospital Federation 41st World Hospital Congress, 2017". **Nope**, [the two honorable mentions](https://worldhospitalcongress.org/past-winners/) were "Achieving high reliability through care coordination for patients who require emergency surgery" by Northwest Community Hospital, USA and "the application of improving clinical alert system to reduce the unexpected cardiac arrest event in Taiwan (Yuan’s General Hospital, Taiwan). Neither of these sound like something QuartzClinical would have been part of.
- "Institute for Healthcare Improvement - Four of the Best from the IHI Scientific Symposium (2017)." I couldn't find this 'award' so it's **possible** they really did get listed in some such "four of the best" list. The only mention of Quartz Clinical on the ihi.org website is as [an exhibitor at the 2018 symposium](http://www.ihi.org/education/Conferences/National-Forum/Pages/List-of-Supporters.aspx). It's possible they also exhibited a year earlier and got some kind of recognition.
- American Hospital Association McKesson Quest for Quality Prize for 2017. This went to the [Memorial Medical Center of Springfield Illinois](https://www.aha.org/award/2017-12-11-quest-quality-prize-honorees). From their [description of how they won](https://www.memorialmedical.com/quality/quest-for-quality) I don't see anything that seems linked to software like QuartzClinical. Instead, they did things like changing the process for dealing with hip fractures, and placed handrails in hospital rooms. However, according to his LinkedIn profile, Surgisphere CEO Sapan Desai worked for the Memorial Medical Center from mid 2014 to mid 2016 as Director of "Quality Alliance and Predictive Analysis", so its **plausible** he had some role in the program that led them winning the award, even if QuartzClinical was not involved.
- Frost and Sullivan Healthcare Innovation Technology Award 2019. **Yes**, this one Surgisphere do seem to [have genuinely won](https://ww2.frost.com/news/press-releases/frost-sullivan-recognizes-industry-leaders-excellence-best-practices-awards-ceremony/). However, the [Wikipedia page for Frost and Sullivan](https://en.wikipedia.org/wiki/Frost_%26_Sullivan) says that these awards are "based on research using a proprietary methodology, which is sometimes based on a single article produced by the receiver of the award", describing them as a vanity award that the recipient pays a fee to communicate. I can't judge that. 

In addition to these five claimed awards, there is [this media release](https://www.prnewswire.com/news-releases/ihfs-global-healthcare-quality-award-recognizes-surgisphere-executive-sapan-desai-md-300637851.html) saying that Sapan Desai "received an honorable mention for his outstanding achievements in quality and patient safety, corporate social responsibility, innovations in service delivery at affordable cost, healthcare leadership, and management practices" at the IHF's Dr. Kwang Tae Kim Grand Award ceremony in Taipei, Taiwan in 2018. This appears false. The Dr Kwang Tae Kim awards are for [hospitals and health care organisations](https://worldhospitalcongress.org/awards/), not individuals. All five [mentions of Sapan Desai on the IHF website](https://www.google.com/search?rlz=1C1CHBF_enNZ696NZ696&sxsrf=ALeKk037Xrp4vLj2fZoBN4nle-GHEpMVTQ%3A1590824125335&ei=vQzSXojNE-rhz7sPptScqAY&q=sapan+desai++site%3Ahttps%3A%2F%2Fwww.ihf-fih.org%2F&oq=sapan+desai++site%3Ahttps%3A%2F%2Fwww.ihf-fih.org%2F&gs_lcp=CgZwc3ktYWIQA1DJPljJPmCCQWgAcAB4AIABoQGIAbUCkgEDMC4ymAEAoAEBqgEHZ3dzLXdpeg&sclient=psy-ab&ved=0ahUKEwjIn4_XidvpAhXq8HMBHSYqB2UQ4dUDCAw&uact=5) relate to him giving conference talks, there is no mention of him getting an award. The fact that his own press release announcing his 'honorable mention' does not link to any authoritative source for that is suspicious in itself.

So one correct claim (Frost and Sullivan), one exaggerated (the Memorial Medical Center award, which was not for QuartzClinical but at least was an award, with a plausible connection to Desai), three apparently false (relating to the International Hospital Federation) and one uncertain (the Institute for Healthcare Improvement).

I was particularly puzzled by the 2015 IHF Grand Prize in Quality. It seems such a specific and easily disprovable claim, and as well as being on the QuartzClinical site it is made repeated by Sapan Desai as individual, for example in [his bio for this event in 2018](https://event.icebergevents.com.au/whc2018/speakers/dr-sapan-desai-united-states-of-america) - "He is the recipient of the international grand prize in healthcare quality by the International Hospital Federation in 2015."  Was he perhaps working at the Texas Children's Hospital? (no, he wasn't).

Then I came across [this piece](https://www.memorialmedical.com/about/news/post/siumemorial-earn-top-quality-award-from-world-hospital-congress) claiming a "top quality award" at that 2015 IHF 39th congress. Despite the headline, the text actually reports Desai was given "first prize for the best presentation", for his "Improving the Success of Strategic Management Using Big Data". There's no record of this award on the IHF site, although he definitely did give that presentation. It is plausible he got an award for best presentation. I now think that at some point in subsequent CV-garnishing, this evolved into the claimed "Grand Prize in Healthcare Quality". 

My best guess is that the other apparently false claims of awards, if they have any basis, are exaggerations of conference awards or honorable mentions for talks that have been exaggerated into significant awards for software.
 
How else might we know those awards before 2019 weren't for QuartzClinical? Well it was only launched in January 2019 as seen by this ['review blog'](http://themarthareview.com/quartzclinical-launches-cloud-based-healthcare-data-analytics-platform/) which transparently just repeats media releases verbatim.
 
What we're left with, with QuartzClinical is a description of software that seems to combine data warehousing from multiple sources with an analytical layer that then provides decision-supporting algorithms. The analysis is apparently done off-premise of the customer (because we know Surgisphere claim they retain all the data for future use). The data sources include both the finance and electronic health records and at a minimum would need some moderately complex data engineering and pipelines for deployment. The firm that owns it has no capability for ICT project management, software development, deployment or support. There is very little references to this software on the web other than its own promotional material. It has an [entry in venddy.com](https://venddy.com/vendorprofile/1532728950465x586640762398019500), a site that allows vendors and purchasers of health systems to review each other, but zero user reviews. The promotional material appears on the web from early 2019 onwards so we know it is around a year old. The owner has a record of exaggerating his CV well beyond the point of being misleading (eg an honorable mention for giving a paper evolves over time to the Grand Prize in Quality).

What is the probability that a new cloud-based data analytics tool, which integrates with the most sensitive data systems hospitals have (finance and electronic health records) and transfers that data across international boundaries, goes from zero to deployed in 671 hospitals on six continents in 12 months, yet has no user reviews and no discussion on the web from excited IT managers involved in its deployment? Zero, that is the probability; or as close to zero as counts. 

## 'Surgical Outcomes'

Next, a few words about Surgical Outcomes, the international collaborative network of QuartzClinical customers (hospitals and health care centres) that are so trustingly giving their data to Surgisphere. Here is the [Surgical Outcomes website](https://surgisphere.com/). It is an odd combination of hype about machine learning, and six-sigma process improvement. You can join the Collaborative for $295 per year and access online education resources for continuing medical education / maintenance of certification. Or pay $2,495 to access other services such as participation in the "research collaborative".

There are many screenshots of a business intelligence tool, presumably QuartzClinical (which is promoted heavily) allowing the user to drill down (for example) into surgical procedures and understand cost drivers, accompanied with goofy videos on the power of data and importance of performance metrics.

There is a [frankly weird blog](https://surgicaloutcomes.com/blog/) with about 100 posts, starting in September 2019. These combine basic statistical instruction on topics such as propensity score matching with quality control and project management advice.  Some of the statistics is simply wrong; one example chosen at random being this screenshot which incorrectly names the limits of a confidence interval "parameters".

<img src="/img/0185-eg-stats-mistake.png" width = '100%'>

The oldest blog post on the Surgical Outcomes site is from September 2019 and titled "[How do I sign up](https://surgicaloutcomes.com/how-do-i-sign-up/)". I think we can safely say this is the beginning of the Surgical Outcomes "international collaborative network".  Here is a screenshot from that blog:

<img src="/img/0185-signup.png" width = '100%'>

You and I know, dear reader, that this is not how hospitals agree to share patients' personal data. In particular, it is not how hospitals in other countries decide to share their data with a firm in the US. We also know that "a quick technology assessment" is not what is needed before deploying an analytical platform. Not one that draws data from the hospital's finance and EHR systems, stores it in the cloud, conducts machine learning on it and returns decision recommendations integrated with the hospital's own processes.

## The article itself

I haven't even mentioned the data issues we can glean directly from a reading of the article, other than in passing about the surprising high proportion of North American hospital cases that were in-sample. Several of the more obvious errors relate to Australia and have been reported on in the media. For example, many more cases in Australia than existed at the time of the study, as [reported in The Guardian](https://www.theguardian.com/science/2020/may/28/questions-raised-over-hydroxychloroquine-study-which-caused-who-to-halt-trials-for-covid-19). Surgisphere responded that a recently joined hospital (could there be any other kind!) "self-designated as belonging to the Australasia continental designation ...  This hospital should have more appropriately been assigned to the Asian continental designation.” Hmm, so the secret database has dreadful data quality but sure, mistakes happen. 

But as [Thomas Lumley points out](https://www.statschat.org.nz/2020/05/30/austral-asia/), the missclassified hospital had to have 546 hospitalised COVID-19 cases by 14 April and self-describe itself as being in Australasia. Indonesia had enough hospital cases by then but it seems unlikely they was a concentration of this size in one hospital. And would an Indonesian hospital self-describe as Australasian (no, it would not). And could this data be shared legally with a firm that doesn't even know which country's laws it needs to abide by? (no it could not). 

Then there's the smoking rate being three times in North America what it is in South; the small range in average BMI; the implausible detailed data for Africa; the ethnicity data that is illegal to collect in some countries; and on and on.

I don't want to write any more, it makes me upset and angry just thinking about this. It's all said better in the links at the bottom of the page anyway.

Previously, I had more or less gone along but thought there was exaggeration when people said "peer review is broken" but now I really believe it. In the future my motto is really going to be "publish the data and code or it didn't happen" - not just as "this is good practice" but as in "if you don't, I need to think you might be making this up". With sensitive data we'll need to find ways to provide synthesised or other disclosure-controlled versions.

Here's a good quote from Andrew Gelman (link included later)

> The good news about this episode is that it’s kinda shut up those people who were criticizing that Stanford antibody study because it was an un-peer-reviewed preprint. The problem with the Stanford antibody study is not that it was an un-peer-reviewed preprint; it’s that it had bad statistical analyses and the authors supplied no data or code. 

I hope I'm wrong about this whole thing. Maybe Desai's ETL developers, support staff, and EHR integration specialists just aren't on LinkedIn while his sales people are. Maybe hospitals really are knowingly and happily sharing our data with an American firm, and the data is stored in European servers to comply with the GDPR and there's even some patient permission given somewhere that hasn't been mentioned. Perhaps QuartzClinical is wrapped in some other firm's software so it has been deployed to 671 hospitals without any reviews or discussion because its branding is hidden.

I would feel bad about writing such a long aggressive post as this in that case. But it seems very unlikely. It is dreadful to think that the most likely explanation of what we're seeing is simply that the data are fabricated, in what is possibly a criminal conspiracy, and the science publication process is so broken that it gets through. It just seems to me very likely that this explanation is the correct one.

## Some other criticisms

- An excellent [Open letter](https://zenodo.org/record/3862789#.XtJg3jozaUn) with multiple signatures by various researchers led by James Watson (not the DNA guy). Very measured and asks excellent questions.
- James Todaro's [critique of the article](https://www.medicineuncensored.com/a-study-out-of-thin-air), much more of a focus on Surgisphere's credibility (like my post above) than is in the above letter. 
- [Guardian Australia coverage of some aspects of the controversy](https://www.theguardian.com/world/2020/may/29/covid-19-surgisphere-hydroxychloroquine-study-lancet-coronavirus-who-questioned-by-researchers-medical-professionals)
- The [latest](https://statmodeling.stat.columbia.edu/2020/05/30/the-good-news-about-this-episode-is-that-its-kinda-shut-up-those-people-who-were-criticizing-that-stanford-antibody-study-because-it-was-an-un-peer-reviewed-preprint/) of quite a number of posts on Andrew Gelman's Statistical Modelling blog
- [#LancetGate on Twitter](https://twitter.com/search?q=%23LancetGate&src=typeahead_click) (mostly in French)
