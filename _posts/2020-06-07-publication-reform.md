---
layout: post
title: Forecasts for the 2020 New Zealand elections using R and Stan
date: 2020-06-07
tag: 
   - Metascience
description: My forecasts for the 2020 New Zealand general election are out, and predict a comfortable win for Jacinda Ardern's Labour Party either alone or in coalition.
image: /img/0185-loupy-ellis-tweets.png
socialimage: http://freerangestats.info/img/0185-loupy-ellis-tweets.png
category: Other
---

<blockquote class="twitter-tweet"><p lang="en" dir="ltr">I can understand this position that editor filtering and peer review are hard but it begs the real question of what value are journals adding? Why not publish just on the pre-print servers and move all review to PubPeer or similar. Skip Elsevier et al completely.</p>&mdash; Peter Ellis (@ellis2013nz) <a href="https://twitter.com/ellis2013nz/status/1269400072187338752?ref_src=twsrc%5Etfw">June 6, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet"><p lang="en" dir="ltr"><a href="https://twitter.com/hashtag/reviewer2?src=hash&amp;ref_src=twsrc%5Etfw">#reviewer2</a> : &quot;I don&#39;t have to read very much of this manuscript to realize there is nothing meaningful that can be learned from it&quot; ffs if you&#39;re not going to read it, don&#39;t review it. 😠</p>&mdash; Ben Bond-Lamberty (@BenBondLamberty) <a href="https://twitter.com/BenBondLamberty/status/1268641821493620736?ref_src=twsrc%5Etfw">June 4, 2020</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

The scientific publishing process has a range of well known problems, the symptoms of which include:

1. Access to published science is restricted - journal subscriptions are expensive items even for university libraries, and basically not available to others. See for example the critique from [The Cost of Knowledge](http://thecostofknowledge.com/)
2. Researchers [resent petty gatekeeping by anonymous reviewers](https://twitter.com/search?q=%23reviewer2&src=typed_query), which is sometimes in an offensive spirit and sometimes requires changes the authors are convinced subtract value (for example, insisting on an outmoded statistical technique because it is familiar to the reviewer; or insisting on referencing the reviewer's own favourite publications). 
3. Academics find peer review difficult and unrewarding work. Some of them resent giving free labour to profit-seeking firms (see point 1). Editors complain that it can be hard to get enough of the right reviewers.
4. The gatekeeping isn't effective in assuring readers of research quality. If something as transparently bad as the Surgisphere fraud could pass the editorial filter and peer review processes of the Lancet and the New England Journal of Medicine and it is expected that post-publication peer review is required to pick up the flaws, then it is genuinely hard to understand what the imprimatur of "published in a peer review journal" adds. In fact, the authority granted by passing this (apparently not to high) bar makes post-publication peer review harder
5. On the other hand, readers from outside of the scientific establishment are now exposed more than previously to "pre-print" science. Many people fear that preprints are given too much exposure, given they have not been subjected to peer review. The media and Twitter cycles and the nature of public debate are powerful forces for making this worse, for example by creating amplifiers for misinterpretations or overinterpretations based on single studies.
6. At the same as all this, we have the [replication crisis](https://en.wikipedia.org/wiki/Replication_crisis), the growing realisation that much published science is based on shoddy statistical methods. While great methodological advances have been made in combating this (growing awareness, open code and data, improved statistics, improved meta-studies), problems remain with incentives and failures from all the methods above.  

Ionnadis as one of the leaders in exposing the problems of #6, yet an example of problems with 5. Surgisphere case illustrates the problems with 3 and 4.

All this may seem bad, but the joint solution to all these problems is evolving fast. Much of it in fact is already in place. Here is what I see as the elements of the solution, in rough order of how it will emerge:

1. Make the preprint servers the default for publication
2. Strengthen existing post-publication peer review as executed on PubPeer - the question-based, problem-seeking type
3. Complement this with a new positive form of post-publication peer review - badges or certificates indicating a stamp of approval from an appropriately certified reviewer, covering off just one dimension of quality. Articles that accrue gold-standard badges on all dimensions can be treated as gold-standard research, with much greater confidence than we can now something
4. Gradually abandon "pre-publication" peer review. There really is no such thing as pre-publication any more, with researchers announcing findings by press release, blogs and preprints.
5. Let the journals wither away, or re-invent themselves as open-access journals that are essentially collections of articles in the manner of "Peeriodicals" promoted by PubPeer 

As mentioned, this solution is already evolving spontaneously, with the basics of items 1 and 2 in place and growing in strength. The key features have been 

- the rapid rise of the pre-print servers, 
- improved post-publication peer review through sites like PubPeer
- growing expectations of open data and (at least) open code, with acting on those expectations made easy by sites like GitHub

Peer-reviewed Open Access journals are also an important part of the movement towards a solution. However, I am arguing that the whole concept of a journal and pre-publication peer review are out-moded.  I think these can be seen as very much an interim step rather than likely to be part of an enduring solution (of course, existing open access journals, and maybe even closed journals, are likely to evolve or re-invent themselves into something useful along the lines of part #5 of my solution)

The only new part of my proposed solution is item #3, badges or certificates. I believe these are a necessary complement to the sort of post-publication peer review that evolves spontaneously and is likely to be of the critical, problem-finding, quesiton-asking type. That sort of review is essential but not enough for a reader to know whether a given article has passed some benchmark. The dimensions would need some joint development by the scientific community but they might be something like

- data provenance is authoritative and audited
- results can be reproduced with the original data
- any software developed for the research works as described
- statistical methods are sound
- study makes a new contribution to the field, either with new findings or replicating or failing to replicate previous findings
- publication duly describes and takes into account previous literature in the field 
- publication duly describes and takes into account relative literature in other fields
- writing is clear
- data visualisations are high quality
- conclusions follow from the analysis
- results have been replicated with new data

And so on. As soon we set out a list like this it is obvious that different experts are needed for most of these dimensions, and how impossible it is for existing pre-publication review to check off all of these in a timely fashion with only two reviewers. In fact, it seems that existing pre-publication peer review is limited to only a few of these dimensions in any one instance, with the issues covered depending to a significant degree on the luck of the draw of the expertise and interest of the particular reviewer.

The Imperial College microsimulation in C as an example of a massive effort just to check "any software developed for hte research works as described".

need to allocate resources somehow to the peer review, particularly the positive badging. People will spontaneously do the problem-seeking type.


