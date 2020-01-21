library(rio)
library(survey)

load("../data/NZES2017Release14-07-19.rdata")

nzes2017 <- rio::import("../data/NZES2017Release14-07-19.dta")


names(nzes2017)

nzes2017$demo_not_best <- with(nzes2017, as.numeric(rdemobest %in% 4:5))


nzes <- svydesign(~1, data = nzes2017, weights = ~rwt)


svytable(~rdemocracy, design = nzes)
table(nzes2017$rdemocracy)

svytable(~rpartyvote + demo_not_best, design = nzes)

svychisq(~rpartyvote + demo_not_best, design = nzes)

svyby(~demo_not_best, by = ~rpartyvote, design = nzes, FUN = svymean)

attributes(nzes2017)
View(nzes2017)
str(nzes2017)
attr(nzes2017$rpartyvote, "labels")
attr(nzes2017$rdemobest, "labels")
