######################################################################
# Biostatistiques et analyse des donnÃ©es de santÃ© avec R
# chapter_3.tex
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Christophe Lalanne, Mounir Mesbah
######################################################################


######################################################################
## Décrire une variable numérique selon les modalités d'une varia...
######################################################################
> with(birthwt, mean(age[bwt < 2500]))
[1] 22.30508

> with(birthwt, tapply(age, low, mean))
      No      Yes 
23.66154 22.30508 

> f <- function(x) c(m=mean(x), s=sd(x))
> with(birthwt, tapply(age, low, f))
$No
        m         s 
23.661538  5.584522 

$Yes
        m         s 
22.305085  4.511496 

> histogram(~ age | low, data=birthwt, type="count")

> densityplot(~ age | low, data=birthwt, bw=2.5)

> densityplot(~ age, data=birthwt, groups=low, bw=2.5, 
              auto.key=TRUE)


######################################################################
## Décrire deux variables qualitatives
######################################################################
> with(birthwt, table(low, smoke))
     smoke
low   No Yes
  No  86  44
  Yes 29  30

> margin.table(xtabs(~ low + smoke, data=birthwt), margin=1)
low
 No Yes 
130  59 

> prop.table(xtabs(~ low + smoke, data=birthwt), margin=1)
     smoke
low          No       Yes
  No  0.6615385 0.3384615
  Yes 0.4915254 0.5084746

> barchart(xtabs(~ low + smoke, data=birthwt), stack=FALSE, 
           auto.key=TRUE, ylab="low")


######################################################################
## Echantillons indépendants
######################################################################
> t.test(lwt ~ ui, data=birthwt, var.equal=TRUE)

	Two Sample t-test

data:  lwt by ui
t = 2.1138, df = 187, p-value = 0.03586
alt hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.8753389 25.3544748
sample estimates:
 mean in group No mean in group Yes 
         131.7578          118.6429 

> with(birthwt, tapply(lwt, ui, var))
      No      Yes 
940.8472 783.7196 

> bwplot(lwt ~ ui, data = birthwt, pch = "|")

> t.test(lwt ~ ui, data=birthwt)

	Welch Two Sample t-test

data:  lwt by ui
t = 2.2547, df = 39.163, p-value = 0.02982
alt hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  1.351128 24.878685
sample estimates:
 mean in group No mean in group Yes 
         131.7578          118.6429 


######################################################################
## x1
######################################################################
> x1 <- c(1.83,0.50,1.62,2.48,1.68,1.88,1.55,3.06,1.30)
> x2 <- c(0.878,0.647,0.598,2.05,1.06,1.29,1.06,3.14,1.29)

> d <- data.frame(x1, x2)
> dim(d)
[1] 9 2
> summary(d)
       x1              x2       
 Min.   :0.500   Min.   :0.598  
 1st Qu.:1.550   1st Qu.:0.878  
 Median :1.680   Median :1.060  
 Mean   :1.767   Mean   :1.335  
 3rd Qu.:1.880   3rd Qu.:1.290  
 Max.   :3.060   Max.   :3.140  

> with(d, t.test(x1, x2, paired=TRUE))

	Paired t-test

data:  x1 and x2
t = 3.0354, df = 8, p-value = 0.01618
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.1037787 0.7599991
sample estimates:
mean of the differences 
              0.4318889 

> library(reshape2)
> dm <- melt(d)
No id variables; using all as measure variables
> dim(dm)
[1] 18  2
> summary(dm)
 variable     value      
 x1:9     Min.   :0.500  
 x2:9     1st Qu.:1.060  
          Median :1.425  
          Mean   :1.551  
          3rd Qu.:1.867  
          Max.   :3.140  

> with(dm, tapply(value, variable, mean))
      x1       x2 
1.766667 1.334778 

> dm$id <- factor(rep(1:9, 2))
> head(dm, n=3)
  variable value id
1       x1  1.83  1
2       x1  0.50  2
3       x1  1.62  3

> library(gridExtra)
> p1 <- stripplot(value ~ variable, data=dm, groups=id, type="l", 
                 col="grey50", lwd=1.2)
> p2 <- xyplot(x2 ~ x1, data=d, type=c("p", "g"), 
              abline=list(a=0, b=1))
> grid.arrange(p1, p2, nrow=1)


######################################################################
## Cas de deux proportions
######################################################################
> tab <- with(birthwt, table(smoke, low))
> tab
     low
smoke No Yes
  No  86  29
  Yes 44  30
> prop.table(tab, margin=1)
     low
smoke        No       Yes
  No  0.7478261 0.2521739
  Yes 0.5945946 0.4054054
> prop.test(tab[,c(2,1)], correct=FALSE)

	2-sample test for equality of proportions without continuity
	correction

data:  tab[, c(2, 1)]
X-squared = 4.9237, df = 1, p-value = 0.02649
alternative hypothesis: two.sided
95 percent confidence interval:
 -0.29039121 -0.01607177
sample estimates:
   prop 1    prop 2 
0.2521739 0.4054054 

> prop.test(c(29,30), c(86+29, 44+30), correct=FALSE)

	2-sample test for equality of proportions without continuity
	correction

data:  c(29, 30) out of c(86 + 29, 44 + 30)
X-squared = 4.9237, df = 1, p-value = 0.02649
alternative hypothesis: two.sided
95 percent confidence interval:
 -0.29039121 -0.01607177
sample estimates:
   prop 1    prop 2 
0.2521739 0.4054054 


######################################################################
## Test du chi-deux
######################################################################
> chisq.test(xtabs(~ low + smoke, data=birthwt))

	Pearson's Chi-squared test with Yates' continuity correction

data:  xtabs(~low + smoke, data = birthwt)
X-squared = 4.2359, df = 1, p-value = 0.03958

> with(birthwt, chisq.test(table(low, smoke)))
> summary(xtabs(~ low + smoke, data=birthwt)

> res <- chisq.test(xtabs(~ low + smoke, data=birthwt))
> res$expected
     smoke
low         No      Yes
  No  79.10053 50.89947
  Yes 35.89947 23.10053


######################################################################
## Mesures de risque et odds-ratio
######################################################################
> tab <- xtabs(~ smoke + low, data=birthwt)
> tab <- tab[c(2,1),c(2,1)]
> tab
     low
smoke Yes No
  Yes  30 44
  No   29 86

> library(vcd)
> oddsratio(tab, log=FALSE)
[1] 2.021944

> library(epiR)
> epi.2by2(tab)
            Outcome +   Outcome -   Total   Inc risk *     Odds
Exposed +          30          44      74         40.5    0.682
Exposed -          29          86     115         25.2    0.337
Total              59         130     189         31.2    0.454

Point estimates and 95 % CIs:
---------------------------------------------------------------
Inc risk ratio                             1.61 (1.06, 2.44)
Odds ratio                                 2.01 (1.03, 3.96)
Attrib risk *                              15.32 (1.61, 29.04)
Attrib risk in population *                6.00 (-4.33, 16.33)
Attrib fraction in exposed (%)             37.80 (5.47, 59.07)
Attrib fraction in population (%)          19.22 (-0.21, 34.88)
---------------------------------------------------------------
 * Cases per 100 population units 


######################################################################
## Approches non paramétriques et tests exacts
######################################################################
> wilcox.test(lwt ~ ui, data=birthwt)

	Wilcoxon rank sum test with continuity correction

data:  lwt by ui
W = 2896, p-value = 0.01626
alternative hypothesis: true location shift is not equal to 0

> with(d, wilcox.test(x1, x2, paired=TRUE))

	Wilcoxon signed rank test

data:  x1 and x2
V = 40, p-value = 0.03906
alternative hypothesis: true location shift is not equal to 0

> wilcox.test(value ~ variable, data=dm, paired=TRUE)

> fisher.test(xtabs(~ low + smoke, data=birthwt))

	Fisher's Exact Test for Count Data

data:  xtabs(~low + smoke, data = birthwt)
p-value = 0.03618
alternative hypothesis: true odds ratio is not equal to 1
95 percent confidence interval:
 1.028780 3.964904
sample estimates:
odds ratio 
  2.014137 


######################################################################
## Applications
######################################################################
D. hyoscyamine hydrobromide :
0.7 -1.6 -0.2 -1.2 -0.1  3.4  3.7  0.8  0.0  2.0
L. hyoscyamine hydrobromide :
1.9  0.8  1.1  0.1 -0.1  4.4  5.5  1.6  4.6  3.4

> data(sleep)
> mean(sleep$extra[sleep$group == 1]) # D. hyoscyamine hydro.
> mean(sleep$extra[sleep$group == 2]) # L. hyoscyamine hydro.
> m <- with(sleep, tapply(extra, group, mean))
> m[2] - m[1]

> sdif <- sleep$extra[sleep$group == 2] - 
          sleep$extra[sleep$group == 1]
> c(mean=mean(sdif), sd=sd(sdif))

> histogram(~ sdif, breaks=seq(0, 5, by=0.5), xlab="LHH - DHH")

> t.test(extra ~ group, data=sleep, paired=TRUE)

> barchart(with(sleep, tapply(extra, group, mean)))

> regime <- matrix(c(26,38,21,44), nrow=2)
> dimnames(regime) <- list(c("amélioration","pas d'amélioration"), 
                          c("régime","pas de régime"))

chisq.test(regime)

> chisq.test(regime)$expected

> fisher.test(regime)

> prop.test(c(26,21), c(64,65))

> library(foreign)
> hc <- read.spss("health-camp.sav", to.data.frame=TRUE)

> head(hc)

> str(hc)

> table(hc[,c("BEFORE","AFTER")])
> round(prop.table(table(hc[,c("BEFORE","AFTER")])), 2)

> margin.table(table(hc[,c("BEFORE","AFTER")]), 1)
> margin.table(table(hc[,c("BEFORE","AFTER")]), 2)

> hc.tab <- table(hc[,c("BEFORE","AFTER")])
> mcnemar.test(hc.tab)

> binom.test(6, 6+29)
> mcnemar.test(hc.tab, correct=FALSE)

