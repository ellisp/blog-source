---
layout: post
title: How to recruit data analysts for the public sector
date: 2018-01-23
tag: 
   - WorkRelated
   - NewZealand
   - R
description: Reflections on recruiting data scientists for the public sector, which could maybe be used as practical guidance for someone.
image: /img/kitchener.jpg
socialimage: http://ellisp.github.io/img/kitchener.jpg
category: R
---

## A management challenge

<img src = '/img/kitchener.jpg' align='right'  width='180px' />

Between 2011 and 2017 I selected somewhere between 20 and 30 staff and contractors, for New Zealand public sector roles with titles like Analyst, Senior Analyst and Principal Analyst.  Alternative names for these roles could have been (and sometimes were) "researcher", "data analyst", "statistician", "R developer", "data scientist" and in one case "Shiny developer" - with or without "Senior" or "Principal" in front of the name.  I must have been part of over 50 job interviews for such roles, and read some hundreds of technical exercises and perhaps more than 1,000 job applications.  Perhaps it was only 500 applicants, counting people who applied more than once; but it was certainly lots.  This was when my own positions had titles such as Manager Tourism Research and Evaluation, Manager Sector Trends, and General Manager - Evidence and Insights.  This blog post is aimed at people with similar job titles doing the recruiting, but might also be of interest for people applying for data science roles.

There's quite a literature out there on the web about [recruiting data scientists](https://www.google.co.nz/search?q=how+to+recruit+data+scientist&rls=com.microsoft:en-US:{referrer:source?}&ie=UTF-8&oe=UTF-8&sourceid=ie7&rlz=1I7MXGB_enNZ512&gfe_rd=cr&dcr=0&ei=eZIsWru-A-Tc8wfi75H4CA), but most of it seems to be based in quite a different world from mine.  I don't have the time or the inclination to do a comprehensive study of all the advice, but it seems to fit into several broad categories:

1.  Alleged *technical Data Scientist interview questions* such as "explain the use of combinatorics in data science", "what is an eigenvalue and eigenvector", "how do you choose the k in k-means cluster analysis" and "write a function that takes in two sorted lists and outputs a sorted list that is their union" (these are all suggested questions from a real site I won't link to).  I mean, WTF?  Would anyone really want to choose your team based on ability to answer that sort of question in an interview setting?  
2. A reaction to the above, pages that emphasise *non-technical qualities* like enthusiasm for learning, ability to communicate, team orientation, client orientation etc.  These are generally much more sensible, but not particularly specific to data science and hence very incomplete.  Maybe advice like this is useful for a data scientist thrust from their technical corner into a management role they're not ready for, but much more likely if you've reached a management level in the public sector, you would have been thinking about these things for your own and others' teams since you were an egg.  More importantly, your public sector organisation will have all sorts of processes designed (successfully or not) to recruit people with these qualities, so it *should* be the least of your problems when designing the recruitment.
3. Pages for employers which are more about *how to attract data scientists*, with ideas ranging from "higher salaries" and "give them time and opportunities to invent cool stuff" (both useful) through to "put your lead candidate up in the coolest expensive hotel they've ever been, with gorgeous models walking through the lobby" (yuck for all sorts of reasons).  Ideas like the first two might be useful for strategising, but such resources are basically silent on the tactical specifics of picking the wheat from the chaff.
4. A surprising number of pages that have *basically no content* other than the vacuous ("recruiters, work closely with hiring managers to build out accurate job descriptions") and seem to exist as click-bait to sell advertising.

Most of the advice out there does not seem based in the world that I lived in as a recruiting manager within a public sector organisation.  My main challenge was typically to get an effective assessment of technical skills - that is, the ability to *apply technical skills to our sort of problems* in a way that will work in our particular organisation - in the recruitment process.  I suspect this is also the challenge for other managers; even more so those that have less personal hands-on experience with the latest tools.

The important checking up on "team orientation" and "ability to concentrate" are usually hard-coded into the recruitment process and a core part of the standard interview or tests provided by my organisation.  These are not something I had to particularly think of differently for a data scientist than when selecting (for example) policy advisors or business analysts.  On the other hand, a question about explaining eigenvalues would be far too technical for an interview, and also has the material disadvantage of being the wrong "technique" for any analytical job I've recruited for (because the real technique is not knowing how to define an eigenvalue but *applying* a related analytical tool to data).

There are two big challenges in any recruitment problem, of which the "recruiting a data scientist" is only a special case:

- It's *really hard* to judge, on the basis of [anything really](https://www.wired.com/2015/04/hire-like-google/), who is going to be effective in a role.  Work sample tests, structured interviews, and tests of general cognitive ability are the best methods, but they are all fraught with challenges.  Rubrics and standardisation are really, *really* important - not just out of fairness to the candidates, but to give you, the recruiting manager, a fighting chance of selecting one of the better candidates.  Methods that feel intuitively right to many people - including to managers who haven't been properly knocked into shape by the HR department - such as informal interviews and unstructured reference checks are pretty much useless in predicting performance, at least unless they are a small part of a much broader systematic strategy.
- In recruiting for a technical role, most managers (and even more so, recruitment teams advising them) aren't going to have the current, hands-on skills to judge their team's work.  And even if a manager does have those skills on the day they became a manager, the skills will quickly degrade both through lack of practice and through the state-of-the-art moving on (particularly in something as fast-changing as data science).  This is a reality, and not just in data science but in any technical field, from the military to medicine.  It causes problems for writing role descriptions, recruitment, and day to day program and workflow management.  Managing this is the day to day challenge for a manager; don't despair, there are methods for doing it and they emphatically do *not* require being as technically skilled or knowledgeable as all your staff.  The [Johns Hopkins Executive Data Science Specialization](https://www.coursera.org/specializations/executive-data-science) on Coursera is one resource that takes this seriously in the data science context (I haven't reviewed it in detail, so this is only a tentative endorsement, but the syllabus looks right).

> "The colonel doesn't need to be a crack shot with a rifle, but they need to know what a rifle is and take seriously the issue of when it is used and for what."

*I think this quote is from me, but let me know if I've nicked it from someone else.*

## A complication - recruitment under constraints

In a larger organisation with a more formal approach to human resources - and in any public sector organisation, where aspects of recruitment may be governed by specific law eg requiring decisions to be based on merit rather than politics - there are additional complications.  Individual recruiting managers have only limited control over key elements of recruiting staff such as: 

- The *position description* often needs to be in a standard template and include standard organisation-wide language that consumes much space and attention.  It will also generally have a standard organisation-wide approach to spelling out "competencies", "behaviours" and (if it's any good) a small (<7) number of selection criteria which may or may not be amenable to selecting technical specialists.  The recruiting manager will have control over only a tiny part of the overall document; possibly over none of it at all if it is negotiated text representing a number of roles across the organisation.  Position descriptions may deliberately generic to allow movement within an organisation, and hence may refer to generic "technical skills" rather than specific fields of statistical and data knowledge, computer languages,  etc.  Position descriptions will also often have a heavy emphasis on people and organisational management, communication and influencing skills (which are well understood by senior managers and HR professionals) with relatively little space for spelling out technical skills (which aren't).
- The *advert* is a public statement from the agency and needs to be consistent with its overall communications strategy.  For example, an organisation that prides itself on its gravitas and speaking with dignity and respect in public policy debates has to be cautious in using humour in its job adverts.
- *Selection criteria and rating scales* may be required by policy to be defined and weighted in advance.  This is a good thing that leads to better recruitment decisions, but surprisingly doesn't always seem to come naturally.
- The *interview process* is likely to be highly structured and standardised around behavioural questions such as "This is a question about X.  Tell us about a time you had to deal with problem Y by showing you had quality X.".  This is actually a good thing - such standardised questions based in genuine experience will get you a much better comparison of candidates than the questions untrained recruiters intuitively asked - but it does require careful preparation from both the panel *and* the applicants to work well.  Of course, it will make clear how ridiculous some of recommended "technical" questions are when you try to put them into this format - 

> "Tell us about a time you solved a difficult technical problem and delivered improved outcomes, by defining on a whiteboard an eigenvalue and an eigenvector."

*[Don't use this interview question for real (unless you cut out everything after "outcomes")]*

## So, some thoughts...

By 2017 I wasn't doing this recruitment the same way I was in 2011.  In 2011, people told me "you can't make public servants code" (turned out to be wrong) and "you can't require public servants to know R" (also wrong, although I never made R a requirement for a role, so long as people demonstrated they could learn it fast and had equivalent skills in another language like Stata, SAS or Python to prove it).  So, here's where I ended up in my recruitment processes.

### Job description 

* Keep job descriptions simple and more or less future-proof - they're a bother to change.  Don't refer to really specific tasks or responsibilities (like "manage processing, analysis and dissemination the International Visitor Survey") if it can be avoided.  If examples are necessary to make the job seem real, use language like "such as", in case that responsibility moves to someone else.  For example: "manage the processing, analysis and dissemination of complex data products such as official statistics drawing on population surveys".  Then if the particular survey you thought they would manage gets moved to someone else in the team (or another team elsewhere in the organisation), your new recruit is still working to the job description so long as they are working with *some* complex data product or other.  Basically, keep you business plan and program management separate from your job descriptions.  Of course, this is team management 101, not really specific to recruiting data scientists.
* Use organisation-generic language as much as possible on non-specialist skills like leadership, communication and organisational influencing.  This saves reinventing the wheel, but more importantly, it's good to be able to say that data scientists are measured against the same competencies and behaviours as others in the organisation (in my case, this usually meant using the language from policy advisors' job descriptions).
* Include the strongest language you can on technical requirements for the duties, with liberal use of language such as "solve business problems via advanced statistical or machine learning methods using R" or "curate data into analysis-ready state using a combination of database technologies and analytical languages".  Other people involved in job descriptions - senior managers and HR - won't prompt you to do this, it's up to you.  Talk to people currently doing the job (in your or other organisations) if necessary to get the wording right.  
* Don't be afraid to push it beyond what people currently do.  If people currently in role need more skills to deliver the results, work on this with them.  This could mean one or more of an exciting professional development for them, re-sizing the job, or even the need to restructure the team - in any case it needs to be addressed, not brushed under the carpet.  On the other hand, don't fall into organisational change without thinking it through, when you just wanted to fill a vacancy.  Caution.
* Don't make anything an essential *selection* requirement unless it really is.  The *job* might require fitting models with R, but does the candidate need to already be an R expert when they come in or just have demonstrated the ability and inclination to learn the right computer language when required?  Similarly, it might be nice if they were an expert in subject matter X, but can they develop that expertise in the job or must they have it to start with?  Generally, I favour framing selection criteria so people can demonstrate a high level of skills in something relevant (but not exactly the same as what you need) in combination with ability *and willingness* to learn fast.  After all, everything in data science seems to change every couple of years at the moment anyway.  An R expert in a time capsule from 2011 would have as much or more catching up to do for 2017 R as someone migrating from Python.  And Julia, H2O, Stan and TensorFlow only came into public *existence* in 2009, 2011, 2012 and 2015 respectively.

### Advert

- Watch the terminology used by the sort of people you want to recruit (follow them on Twitter) and try to use that language so they recognise themselves.
- Use edgy and exciting language to make clear this really is an unusually interesting, demanding, important and exciting job with amazing learning and other challenges.
- However, avoid language that is demeaning, exclusionary via connotation of any particular culture (sex, age, ethnicity, music preference, or whatever) or that overly emphasise individuals over the team.  Words to avoid include master, ninja, rockstar and nerd.  **Never, ever, use the phrase "Chuck Norris" in an advert for a data scientist.**
- Advertise well beyond the usual public sector locations, in technically specific locations such as [statisticians' email lists](http://www.maths.uq.edu.au/research/research_centres/anzstat/) and [technology-specific job boards](https://www.r-users.com/).

In 2015 thanks to a supportive HR team I had a very successful recruitment round that used the theme ["Data Ninjas wanted - no seriously, we need 3 of them"](https://www.linkedin.com/pulse/data-ninjas-wanted-seriously-we-need-3-them-van-echten-triplow-/).  We led with :


- **get paid to play with data**
- **cutting edge open source tools**
- **continuous improvement and learning atmosphere**

The "ninja" language really cut through but I wouldn't use it again; since then I've seen in other contexts enough complaints about that word to convince me it is associated with masculine culture (and others that it is a cliche).  Seek alternatives.  You don't want to be [this guy](https://medium.com/@sodevious/tech-recruiter-posts-sexist-ad-for-ruby-dev-uses-excuse-its-because-im-not-a-programmer-d829e4a08a93), although he did lots more wrong than just describe the developer required as a "super ninja".

I *would* use the other language in that advert again though.  For example, the recruitment specialist said to me "what might be different about the sort of people you want for this job", which is where the "get paid to play with data" idea came from.  It's something I've heard many people say, and frankly I think it myself; I think it really contributed to the great response I got for that advert.


### Practical exercise

This is the important bit.  Interviews alone just don't cut it for these sorts of jobs.  I used a written exercise, as similar as possible to a realistic work task, as my main short-listing tool.  Medium-listing is done from the CV and covering letter (where ideally you have asked them to address specific selection criteria).  Then from this medium list - maybe 10 people per vacancy - you can really identify the people with the right skills by giving them a task as similar as possible to what they would need to do in the job.  These people are all given a few days to complete a written exercise, in their own time and with their own tools.  The exercise is used as the short-listing instrument, and is also referred to extensively in the interview and final decision between those on the short list.

Obviously, exactly what sort of technical exercise will depend on the particular role you are filling, but I can't emphasise its importance enough.  It's *much* more important than the interview in terms of picking the best person for the job.

It's important that the technical exercise:

- be as close as possible to a plausible real life example
- showcase *communication* as well as *analytical* skills - typically by requiring one product that communicates results to a non-specialist audience, and one that communicates analysis to a specialist peer-reviewer
- be doable in a relatively finite space of time (1-4 hours) with tools and data that applicants can access without accessing unusually expensive equipment of services (I think, for these roles, it's fair to assume viable candidates have access to a modest computer, open source software or their preferred commercial equivalent, and Internet connection, but not much more)
- *not* involve unrealistic time or memorisation pressures (no-one really programs without accessing Stack Overflow or uses an unfamiliar statistical technique without Wikipedia, so don't pretend they have to)
- *not* use fancy esoteric skills or specialist technologies that actually won't be essential in the job (eg if they don't need to be able to create a Hadoop cluster in the cloud, don't make this an implicit essential criterion for selection)
- not involve data that any of the candidates have a "head start" on (often, I waited to see who the candidates were before I chose a dataset)
- use public data and in particular not involve sensitive data or organisational/political context
- can be answered to a degree by any viable candidate, and in excellent fashion by an excellent candidate
- demonstrate at least to a small degree all of: 
    - data manipulation needed (eg joins and reshaping)
	- statistical analysis needed (eg survey and time series analysis)
	- reproducible workflow and working in teams
	- communication to non specialists

Taking all that into account, the typical selection exercise I use for a data analyst role will be something like this:

> An influential (but hypothetical, for this process) industry stakeholder has come to the Minister for Tourism arguing that visitors to New Zealand who go to Queenstown in New Zealand's South Island do more activities overall, spend more money per day, and stay longer in New Zealand than visitors who do not visit Queenstown.  Further, they argue that while visitors in general are more likely to visit Queenstown now than 10 years ago, this isn't the case for visitors from Europe; and that something should be done about it.  The Minister has asked the tourism policy team to advise if all this is true and *they* have asked *you* for analytical assistance.  

> The data in this [downloadable zip file](http://www.mbie.govt.nz/info-services/sectors-industries/tourism/tourism-research-data/ivs/data-download) is a copy of genuine survey data from a Ministry relational database.  It comes from the International Visitor Survey, which has sampled tourists departing from New Zealand continuously since 1997 and includes questions on spend, activities (eg cycling, ski-ing, visiting museums) and locations visited.  It is weighted each quarter year to the total population of departing tourists aged 15 or more.  Your task is to provide a 2-3 page document for the policy team explaining whether the factual claims made to the Minister are correct, what caveats should be held around the conclusions, and anything else they need to know, in language accessible to people with a good understanding of tourism but no specialist skills or interest in data and statistics.  You should provide two documents: a written document (PDF or Word, include charts and tables if helpful) for the policy team to read and consider before they respond to the Minister; and a working document or documents (eg R code, knitr/rmarkdown report, Excel workbook, SAS project) and accompanying documentation aimed at one of your fellow analysts so they can easily understand, reproduce and peer review your work.  

> In your analysis, you will need to combine several tables of data eg to answer the questions on activities as there is one table with a row per visitor, and another table with a row per visitor-activity combination.  You are free to use any analytical tools you have access to, but note that in the actual work situation this task would involve a combination of SQL and R.  To keep the exercise within reasonable limits, do not address the "should something be done about it" part of the original query, and do not use any data sources other than that linked to above.  Note that this exercise is testing your writing and communication skills as well as your analytical skills; so please carefully consider the two different audiences for the two documents you need to produce.

Additional guidance would be given on the survey design, weights, and some of the columns in various tables that are particularly relevant (for example, the `vw_IVSSurveyMainHeader.csv` file is the table with one row per respondent; there are many different ways of defining "spend" with this data, but the `WeightedSpend` column is the correct one to use for this exercise; "Weighted" in `WeightedSpend` refers to outlier treatment, not to survey weighting; the `PopulationWeight` column is the survey weight).

### Interview

Job interviews are expensive in terms of time and mental effort (intellectual and emotional) for both candidates and panels; and they are also not as effective in predicting performance as one would hope.  So I like the interview to be the final stage, and of as small a number of candidates as possible.  As much weight as possible should be given to the experience and demonstrated skills of the candidates in doing tasks that resemble the job, which (in the case of data scientists) rarely resembles a job interview.  

Just like the technical exercise, interview questions should be ones that any candidate can answer but excellent candidates can answer excellently.  In organisations I've worked for since 2000 there have been policies to use structured [behavioural questions](http://nz.hudson.com/job-seekers/career-advice/behavioural-interview-questions) in interview, and typically HR will provide samples for the manager.  It's important to choose and tweak these carefully to make sure you have a clear relationship between interview questions and selection criteria.  My data scientist or statistician interviews all follow the same basic structure.  After the introductions and explaining how the interview will work, I start with two non-behavioural questions:

- *Ice breaker* "Tell us why you've applied for this job and how it fits in with your overall career plans"
- *Technical* "Thanks for completing the technical exercise.  Could you tell us *how* you went about this - what do you do first in a situation like this, how does your thinking evolve?" *and any specific follow-ups needed based on what they'd written*.

Then I have about four to six behavioural questions, all of which follow the template "This is a question about X.  Tell us about a time you had to ...., and hence showed your ability to do X.".  Typically, one of these questions will be technical and along the lines of 

- "Tell us about a difficult analytical problem you had to solve with data; what the problem was, how you went about it, what methods you used, etc."  

The other questions will be more generic skills asking for examples of behaviours such as problem solving, working in teams, persisting through difficulties, managing time and resources.  Nearly always, one of my questions is a variant on 

- "Tell us about a time you had to develop new skills or knowledge really quickly".  

The pitch of the questions will depend on the seniority of the role of course (eg entry-level roles seeking examples that could be met by university study, volunteer work or holiday jobs if there is limited work experience to draw on).  I expect additional technical details to emerge as collateral in the answers to these non-technical questions, but if necessary toward the end of the interview might ask additional questions like 

- "Working with databases hasn't come up in your answers so far but is an important part of the job.  What experience do you have with relational databases?  If you haven't got that experience, how would you go about learning to use these tools?"

These interviews always end with two things:

* "is there anything you want to add, that hasn't come up?"
* "is there anything you want to ask us?"

It's important to note that these are two separate opportunities!  Sometimes people focus just on the "any questions for us" and miss the part about "anything you want to add".  When asked "anything you want to add", even if all their skills and behaviours have been well showcased through the interview the candidate should still make a final pitch drawing it all together - basically, this is their chance to ask for the job.  The "anything you want to ask us" is much less important.  Candidates should keep questions short and limited to things they really do need to know; the panel are busy, and probably apart from the recruiting manager they will not be particularly interested in the conversation at this point.

Interview panels should include some kind of customer representative, at least one person with sufficient technical skills to judge the applicants, and the recruiting manager. They should have two or three people on them and a mix of genders and (if possible) ethnicities.

I can safely "give away the questions" for interviews I'm involved with.  The aim of the interview is not to surprise people with questions and test whether they come up with the "right" answer, but to understand their skills and experience.  The more they know in advance what will be asked and hence how to prepare for it, the better chance the panel has of making a good decision.

### Psychometric tests

I don't do these for recruitment unless they are organisationally required.  In fact I have some fairly strong views about them; probably something for a blog post of its own.

## Summary

So there we have it.  My key points for recruiting data scientists for the public sector (which may well apply more broadly):

* Focus on demonstrated ability to do things that resemble the actual job, with a particular emphasis on aptitude and willingness to quickly *learn* and *apply* relevant new skills rather than an exact technical match.
* After medium-listing, put a lot of effort into a technical exercise, give candidates a few days to do it, and use it for short-listing.
* Use the "two products" model for the technical exercise - one product (text and charts) for a non-technical customer, and one (code etc) for a peer reviewing team member.  This lets the exercise serve a dual purpose of testing both analysis and communication.
* After short-listing, make the final decision based on structured interviews with behavioural questions that focus on demonstrated skills and behaviours.

This worked well for me, and I would argue mine were some of the strongest analytical teams anywhere in New Zealand, not just the public sector.  I hope this might be helpful for others recruiting analysts too.