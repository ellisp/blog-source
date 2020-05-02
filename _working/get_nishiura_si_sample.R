# returns sample from posterior estimates from Nishiura et al
# SI data

get_nishiura_si_sample <- function() {

    Nishiura_supp_3Fe <- tribble(~EL, ~ER, ~SL, ~SR,	~source,
    85,	86,	86,	87,	"https://www.cdc.gov.tw/Bulletin/Detail/1T3s2mxcCjqlE0MczXRD8w?typeid=9",
    83,	84,	84,	85,	"https://m.tagesspiegel.de/salmen-ingo/11371436.html",
    83,	84,	84,	85,	"https://www.dw.com/en/germany-confirms-three-further-cases-of-coronavirus/a-52181064",
    84,	85,	86,	87,	"https://www.dw.com/en/germany-confirms-three-further-cases-of-coronavirus/a-52181064",
    84,	85,	86,	87,	"https://www.dw.com/en/germany-confirms-three-further-cases-of-coronavirus/a-52181064",
    84,	87,	80,	90,	"https://www.stmgp.bayern.de/presse/aktuelle-informationen-zur-coronavirus-lage-in-bayern-bayerisches-gesundheitsministerium-5/",
    83,	84,	80,	91,	"https://www.stmgp.bayern.de/presse/aktuelle-informationen-zur-coronavirus-lage-in-bayern-bayerisches-gesundheitsministerium-5/",
    83,	84,	80,	93,	"https://www.stmgp.bayern.de/presse/aktuelle-informationen-zur-coronavirus-lage-in-bayern-bayerisches-gesundheitsministerium-5/",
    77,	78,	80,	81,	"https://www.nejm.org/doi/full/10.1056/NEJMc2001272",
    76,	77,	74,	86,	"http://news.china.com.cn/txt/2020-01/26/content_75649963.htm",
    76,	77,	74,	86,	"http://news.china.com.cn/txt/2020-01/26/content_75649963.htm",
    80,	81,	85,	86,	"https://www.info.gov.hk/gia/general/202001/27/P2020012700049.htm",
    75,	76,	71,	78,	"http://wsjkj.zhuhai.gov.cn/zwgk/tzgg/content/post_2461447.html",
    30,	66,	75,	76,	"https://www.health.nsw.gov.au/news/Pages/20200125_03.aspx",
    64,	67,	68,	69,	"https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30154-9/fulltext",
    80,	81,	84,	85,	"http://news.china.com.cn/2020-01/30/content_75657770.htm",
    80,	81,	84,	85,	"http://news.china.com.cn/2020-01/30/content_75657770.htm",
    80,	81,	82,	83,	"http://news.china.com.cn/2020-01/30/content_75657770.htm",
    68,	69,	83,	84,	"https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(20)30154-9/",
    49,	50,	54,	55,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    49,	50,	58,	59,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    56,	57,	63,	64,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    41,	42,	48,	49,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    50,	51,	53,	54,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    64,	65,	71,	72,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    89,	90,	92,	93,	"https://www.info.gov.hk/gia/general/202002/02/P2020020200627.htm?fontSize=1")

    Nishiura_supp_12F <- tribble(~ER, ~EL, ~SL, ~SR,	~source,
    48,	47,	50,	51,	"https://www.nejm.org/doi/full/10.1056/NEJMc2001272",
    48,	47,	49,	50,	"https://vietnaminsider.vn/vietnam-reports-a-new-case-of-coronavirus-infection-in-nha-trang/",
    53,	52,	56,	57,	"https://www.e-epih.org/journal/view.php?number=1074",
    55,	54,	56,	57,	"https://www.nejm.org/doi/full/10.1056/NEJMc2001468",
    55,	54,	56,	57,	"https://www.nejm.org/doi/full/10.1056/NEJMc2001468",
    56,	55,	56,	57,	"https://www.cdc.gov.tw/Bulletin/Detail/1T3s2mxcCjqlE0MczXRD8w?typeid=9",
    51,	50,	54,	55,	"http://news.china.com.cn/2020-01/30/content_75657770.htm",
    51,	50,	52,	53,	"http://news.china.com.cn/2020-01/30/content_75657770.htm",
    55,	54,	64,	65,	"https://www.straitstimes.com/singapore/health/novel-coronavirus-cases-in-singapore",
    57,	56,	60,	61,	"https://www.e-epih.org/journal/view.php?number=1074",
    57,	56,	59,	60,	"https://www.e-epih.org/journal/view.php?number=1074",
    57,	56,	60,	61,	"https://www.e-epih.org/journal/view.php?number=1074",
    57,	56,	60,	61,	"https://www.e-epih.org/journal/view.php?number=1074",
    52,	51,	54,	55,	"https://www.nejm.org/doi/full/10.1056/NEJMc2001468",
    52,	51,	54,	55,	"https://www.nejm.org/doi/full/10.1056/NEJMc2001468",
    56,	55,	64,	65,	"https://en.vietnamplus.vn/vietnam-reports-two-more-coronavirus-infection-cases/168264.vnp",
    56,	55,	64,	65,	"https://en.vietnamplus.vn/vietnam-reports-two-more-coronavirus-infection-cases/168264.vnp",
    60,	59,	62,	63,	"https://www.info.gov.hk/gia/general/202002/02/P2020020200627.htm",
    60,	59,	63,	64,	"https://www.straitstimes.com/singapore/health/novel-coronavirus-cases-in-singapore",
    60,	59,	62,	63,	"https://www.straitstimes.com/singapore/health/novel-coronavirus-cases-in-singapore",
    51,	50,	59,	60,	"https://www.e-epih.org/journal/view.php?number=1074",
    63,	62,	66,	67,	"https://www.e-epih.org/journal/view.php?number=1074",
    20,	19,	24,	25,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    20,	19,	28,	29,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    27,	26,	33,	34,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    12,	11,	18,	19,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    21,	20,	23,	24,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316",
    35,	34,	41,	42,	"https://www.nejm.org/doi/full/10.1056/NEJMoa2001316")

    Nishiura_supp_3Fe <- Nishiura_supp_3Fe %>%
      mutate(EL=as.integer(EL),
             ER=as.integer(ER),
             SL=as.integer(SL),
             SR=as.integer(SR)) %>%
      arrange(EL, ER, SL, SR,	source)

    Nishiura_supp_12F <- Nishiura_supp_12F %>%
      mutate(EL=as.integer(EL),
             ER=as.integer(ER),
             SL=as.integer(SL),
             SR=as.integer(SR)) %>%
      arrange(EL, ER, SL, SR,	source)

    nish_si <- Nishiura_supp_12F %>%
                mutate(type=0L) %>%
                select(EL, ER, SL, SR, type) %>%
                as.data.frame()

    nish_si2 <- Nishiura_supp_3Fe %>%
                mutate(type=0L) %>%
                select(EL, ER, SL, SR, type) %>%
                as.data.frame()

    nish_si_both <- nish_si %>%
                bind_rows(Nishiura_supp_3Fe %>%
                            mutate(type=0L) %>%
                            select(EL, ER, SL, SR, type)) %>%
                as.data.frame()

    ## estimate the serial interval from data
    suppressMessages({
      nish_si_fit <- coarseDataTools::dic.fit.mcmc(dat = nish_si,
                           dist = "L",
                           init.pars = init_mcmc_params(nish_si, "L"),
                           burnin = 1000,
                           n.samples = 5000)
    })

    ## use coarse2estim to turn this in the right format for estimate_R
    nish_si_sample <- coarse2estim(nish_si_fit, thin = 100)$si_sample

    return(nish_si_sample)
}
