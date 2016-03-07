######################################################################
# Biostatistiques et analyse des donnÃ©es de santÃ© avec R
# chapter_7.tex
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Christophe Lalanne, Mounir Mesbah
######################################################################

> library(survival)
> data(lung)


######################################################################
## Format de représentation des données
######################################################################
> st <- with(lung, Surv(time=time, event=status))
> head(st)
[1]  306   455  1010+  210   883  1022+

> head(subset(lung, select=c(time, status)))
  time status
1  306      2
2  455      2
3 1010      1
4  210      2
5  883      2
6 1022      1


######################################################################
## Table de mortalité/survie
######################################################################
> s <- survfit(st ~ 1, data=lung)
> s
Call: survfit(formula = st ~ 1, data = lung)

records   n.max n.start  events  median 0.95LCL 0.95UCL 
    228     228     228     165     310     285     363 

> summary(s)

> summary(s, times=seq(1, 200, by=20))
Call: survfit(formula = st ~ 1, data = lung)

 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    1    228       0    1.000  0.0000        1.000        1.000
   21    220       8    0.965  0.0122        0.941        0.989
   41    217       3    0.952  0.0142        0.924        0.980
   61    211       7    0.921  0.0179        0.887        0.957
   81    205       7    0.890  0.0207        0.851        0.932
  101    196       6    0.864  0.0227        0.821        0.910
  121    189       6    0.837  0.0245        0.791        0.887
  141    184       5    0.815  0.0257        0.766        0.867
  161    176       8    0.780  0.0275        0.728        0.836
  181    159      15    0.713  0.0301        0.656        0.774

> summary(s, times=300)$surv
[1] 0.5306081

> lung$sex <- factor(lung$sex, labels=c("Male", "Female"))
> s2 <- survfit(st ~ sex, data=lung)
> s2
Call: survfit(formula = st ~ sex, data = lung)

           records n.max n.start events median 0.95LCL 0.95UCL
sex=Male       138   138     138    112    270     212     310
sex=Female      90    90      90     53    426     348     550

> summary(s2, times=seq(5,20,by=5))
Call: survfit(formula = st ~ sex, data = lung)

                sex=Male 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5    138       0    1.000  0.0000        1.000        1.000
   10    138       0    1.000  0.0000        1.000        1.000
   15    132       7    0.949  0.0187        0.913        0.987
   20    131       0    0.949  0.0187        0.913        0.987

                sex=Female 
 time n.risk n.event survival std.err lower 95% CI upper 95% CI
    5     90       1    0.989   0.011        0.967            1
   10     89       0    0.989   0.011        0.967            1
   15     89       0    0.989   0.011        0.967            1
   20     89       0    0.989   0.011        0.967            1


######################################################################
## Courbe de Kaplan-Meier
######################################################################
> plot(s, xlab="Temps", ylab="Probabilité de survie")

> plot(s2, conf.int="none", col=c("black", "gray"), xlab="Temps", 
       ylab="Prob. survie")
> legend("bottomleft", legend=levels(lung$sex), lty=1, 
         col=c("black", "gray"), bty="n")


######################################################################
## Fonction de risque cumulé
######################################################################
> plot(s, fun="cumhaz")


######################################################################
## Test d'égalité des fonctions de survie
######################################################################
> survdiff(st ~ sex, data=lung)
Call:
survdiff(formula = st ~ sex, data = lung)

             N Observed Expected (O-E)^2/E (O-E)^2/V
sex=Male   138      112     91.6      4.55      10.3
sex=Female  90       53     73.4      5.68      10.3

 Chisq= 10.3  on 1 degrees of freedom, p= 0.00131 

> survdiff(st ~ sex, data=lung, rho=1)
Call:
survdiff(formula = st ~ sex, data = lung, rho = 1)

             N Observed Expected (O-E)^2/E (O-E)^2/V
sex=Male   138     70.4     55.6      3.95      12.7
sex=Female  90     28.7     43.5      5.04      12.7

 Chisq= 12.7  on 1 degrees of freedom, p= 0.000363 


######################################################################
## Régression de Cox
######################################################################
> m <- coxph(st ~ sex, data=lung)
> m
Call:
coxph(formula = st ~ sex, data = lung)


            coef exp(coef) se(coef)     z      p
sexFemale -0.531     0.588    0.167 -3.18 0.0015

Likelihood ratio test=10.6  on 1 df, p=0.00111  n= 228

> coxph(Surv(time, status) ~ age + strata(sex), data=lung)
Call:
coxph(formula=Surv(time, status) ~ age + strata(sex), data=lung)


      coef exp(coef) se(coef)    z     p
age 0.0162      1.02  0.00919 1.76 0.078

Likelihood ratio test=3.18  on 1 df, p=0.0744  n= 228

> summary(m)
Call:
coxph(formula = st ~ sex, data = lung)

  n= 228, number of events= 165 

             coef exp(coef) se(coef)      z Pr(>|z|)   
sexFemale -0.5310    0.5880   0.1672 -3.176  0.00149 **
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

          exp(coef) exp(-coef) lower .95 upper .95
sexFemale     0.588      1.701    0.4237     0.816

Concordance= 0.579  (se = 0.022 )
Rsquare= 0.046   (max possible= 0.999 )
Likelihood ratio test= 10.63  on 1 df,   p=0.001111
Wald test            = 10.09  on 1 df,   p=0.001491
Score (logrank) test = 10.33  on 1 df,   p=0.001312

> sna <- survfit(coxph(st ~ 1), type="aalen")
> summary(sna)

h <- -log(sna$surv)
hest <- data.frame(time=sna$time, d=sna$n.event, n=sna$n.risk, 
                  aalen=h, s=sna$surv)
kmest <- data.frame(time=s$time, s=s$surv)
psurv <- merge(hest, kmest, by="time")

> xyplot(s.x + s.y ~ time, data=psurv, type=c("l", "g"), 
         ylab="Prob. survival",
         auto.key=list(corner=c(0,0), text=c("Nelson-Aalen", 
                                             "Kaplan-Meier"),
                       lines=TRUE, points=FALSE))


######################################################################
## Applications
######################################################################
5 105 111 120 125 158 183 241 246 247 254 263 264 265 274 288 291
295 297 345 361 362 375 380 383

> pb <- read.table("data/pbc.txt", header=TRUE)
> names(pb)[1:20]

> pb$rx <- factor(pb$rx, labels=c("Placebo", "DPCA"))
> table(pb$rx)
> pb$sex <- factor(pb$sex, labels=c("M","F"))

> prop.table(table(pb$status))

> prop.table(with(pb, table(status, rx)), 1)

> xyplot(number ~ years, data=pb, pch=pb$status, cex=.8)

> with(pb, tapply(years, rx, median))

> with(pb, table(status[years > 10.5]))
> with(pb, table(sex[years > 10.5 & status == 1]))

> subset(pb, years > 10.5 & status == 1, sex)

> idx <- c(5,105,111,120,125,158,183,241,246,247,254,263,264,265,
           274,288,291,295,297,345,361,362,375,380,383)
> table(pb$status[pb$number %in% idx])

> pb.transp <- subset(pb, number %in% idx, c(age, sex, years))

> mean(pb.transp$age)
> table(pb.transp$sex)
> median(pb.transp$years * 365)

> library(survival)
> head(with(pb, Surv(time=years, event=status)))

> s <- survfit(Surv(years, status) ~ 1, data=pb)
> summary(s)

> plot(s)

> s <- survfit(Surv(years, status) ~ rx, data=pb)

> survdiff(Surv(years, status) ~ rx, data=pb)

> survdiff(Surv(years, status) ~ rx, data=pb, rho=1)

> agec <- cut(pb$age, c(26, 40, 55, 79), include.lowest=TRUE)

> survdiff(Surv(years, status) ~ rx + strata(agec), data=pb)

> summary(coxph(Surv(years, status) ~ rx + strata(agec), data=pb))

> placebo.time <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,
                    22,23)
> placebo.status <- rep(1, length(placebo.time))
> mp.time <- c(6,6,6,6,7,9,10,10,11,13,16,17,19,20,22,23,25,32,
               32,34,35)
> mp.status <- c(1,1,1,0,1,0,1,0,0,1,1,0,0,0,1,1,0,0,0,0,0)
> mp <- data.frame(tx=rep(c("Placebo","6-MP"), c(21,21)),
                   time=c(placebo.time, mp.time), 
                   status=c(placebo.status, mp.status))
> summary(mp)

> with(mp, Surv(time, status))

> s <- survfit(Surv(time, status) ~ tx, data=mp)
> summary(s)

> plot(survfit(Surv(time, status) ~ tx, data=mp, 
               subset=tx=="6-MP"))

> plot(s, fun="cumhaz")

> prostate <- read.table("prostate.dat", header=TRUE)
> str(prostate)
> head(prostate)
> prostate$Treatment <- factor(prostate$Treatment)
> table(prostate$Status)

> library(survival)
> with(prostate, Surv(time=Time, event=Status))

> survfit(Surv(Time, Status) ~ 1, data=prostate)

> survfit(Surv(Time, Status) ~ Treatment, data=prostate)

> plot(survfit(Surv(Time, Status) ~ Treatment, data=prostate))

> survdiff(Surv(time=Time, event=Status) ~ Treatment, 
           data=prostate)

> summary(coxph(Surv(time=Time, event=Status) ~ Treatment, 
          data=prostate))

