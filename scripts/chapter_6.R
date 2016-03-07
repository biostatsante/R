######################################################################
# Biostatistiques et analyse des donnÃ©es de santÃ© avec R
# chapter_6.tex
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Christophe Lalanne, Mounir Mesbah
######################################################################


######################################################################
## Tableaux de contingence
######################################################################
> tab <- matrix(c(73, 23, 15, 11, 42, 25), nrow=2)
> rownames(tab) <- c(">= 2.5 kg", "< 2.5 kg")
> colnames(tab) <- c("White", "Black", "Other")
> tab
          White Black Other
>= 2.5 kg    73    15    42
< 2.5 kg     23    11    25

> with(birthwt, table(low, race))
     race
low   White Black Other
  No     73    15    42
  Yes    23    11    25


######################################################################
## Odds-ratio et stratification
######################################################################
> t1 <- with(subset(birthwt, race == "White"), table(ui, low))
> t2 <- with(subset(birthwt, race == "Black"), table(ui, low))
> t3 <- with(subset(birthwt, race == "Other"), table(ui, low))
> tab <- array(c(t1, t2, t3), dim=c(2,2,3), 
+              dimnames=list(c("ui=0","ui=1"), c(">= 2.5 kg", 
                                                 "< 2.5 kg"),
+                            levels(birthwt$race)))
> tab
, , White

     >= 2.5 kg < 2.5 kg
ui=0        65       18
ui=1         8        5

, , Black

     >= 2.5 kg < 2.5 kg
ui=0        14        9
ui=1         1        2

, , Other

     >= 2.5 kg < 2.5 kg
ui=0        37       18
ui=1         5        7

> dimnames(tab) <- list(c("ui=0","ui=1"), 
                        c(">= 2.5 kg", "< 2.5 kg"), 
                        levels(birthwt$race))

> xtabs(~ ui + low + race, data=birthwt)
, , race = White

     low
ui    No Yes
  No  65  18
  Yes  8   5

, , race = Black

     low
ui    No Yes
  No  14   9
  Yes  1   2

, , race = Other

     low
ui    No Yes
  No  37  18
  Yes  5   7

> mantelhaen.test(tab)

	Mantel-Haenszel chi-squared test with continuity correction

data:  tab
Mantel-Haenszel X-squared = 4.2339, df = 1, p-value = 0.03962
alternative hypothesis: true common odds ratio is not equal to 1
95 percent confidence interval:
 1.133437 6.015297
sample estimates:
common odds ratio 
         2.611122 

> library(vcd)
> woolf_test(tab)

	Woolf-test on Homogeneity of Odds Ratios (no 3-Way
	assoc.)

data:  tab
X-squared = 0.093904, df = 2, p-value = 0.9541


######################################################################
## Sensibilité et spécificité d'un test diagnostique
######################################################################
> dat <- as.table(matrix(c(670,202,74,640), nrow=2, byrow=TRUE))
> colnames(dat) <- c("Dis+","Dis-")
> rownames(dat) <- c("Test+","Test-")
> dat
      Dis+ Dis-
Test+  670  202
Test-   74  640

> sens <- dat["Test+","Dis+"]/sum(dat[,"Dis+"])
> spec <- dat["Test-","Dis-"]/sum(dat[,"Dis-"])
> round(c(sens=sens, spec=spec), 3)
 sens  spec 
0.901 0.760 

> sens + c(-1, 1) * 1.96 * sqrt(sens * (1-sens)/sum(dat))
[1] 0.8858082 0.9152670


######################################################################
## Valeurs prédictives positive et négative
######################################################################
> vpp <- dat["Test+","Dis+"]/sum(dat["Test+",])
> vpn <- dat["Test-","Dis-"]/sum(dat["Test-",])
> round(c(VPP=vpp, VPN=vpn), 3)
  VPP   VPN 
0.768 0.896 

> binom.test(dat["Test+","Dis+"], sum(dat["Test+",]))

	Exact binomial test

data:  dat["Test+", "Dis+"] and sum(dat["Test+", ])
number of successes=670, number of trials=872, p-value<2.2e-16
alt hypothesis: true probability of success is not equal to 0.5
95 percent confidence interval:
 0.7388926 0.7959784
sample estimates:
probability of success 
             0.7683486 


######################################################################
## Tableau de synthèse des propriétés diagnostiques d'un test
######################################################################
> epi.tests(dat, conf.level = 0.95)
          Disease +    Disease -      Total
Test +          670          202        872
Test -           74          640        714
Total           744          842       1586

Point estimates and 95 % CIs:
---------------------------------------------------------
Apparent prevalence                    0.55 (0.52, 0.57)
True prevalence                        0.47 (0.44, 0.49)
Sensitivity                            0.90 (0.88, 0.92)
Specificity                            0.76 (0.73, 0.79)
Positive predictive value              0.77 (0.74, 0.80)
Negative predictive value              0.90 (0.87, 0.92)
Positive likelihood ratio              3.75 (3.32, 4.24)
Negative likelihood ratio              0.13 (0.11, 0.16)
---------------------------------------------------------

> diagstats <- epi.tests(dat, conf.level = 0.95)

> diagstats$rval$diag.or
       est    lower    upper
1 28.68611 21.51819 38.24174
> diagstats$rval$nnd
       est  lower    upper
1 1.513701 1.4091 1.648743


######################################################################
## Estimation des paramètres du modèle
######################################################################
> glm(low ~ lwt, data=birthwt, family=binomial("logit"))

Coefficients:
(Intercept)          lwt  
    0.99831     -0.03093  

Degrees of Freedom: 188 Total (i.e. Null);  187 Residual
Null Deviance:	    234.7 
Residual Deviance: 228.7 	AIC: 232.7

> m <- glm(low ~ lwt, data=birthwt, family=binomial("logit"))
> summary(m)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.0951  -0.9022  -0.8018   1.3609   1.9821  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)  
(Intercept)  0.99831    0.78529   1.271   0.2036  
lwt         -0.03093    0.01357  -2.279   0.0227 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 234.67  on 188  degrees of freedom
Residual deviance: 228.69  on 187  degrees of freedom
AIC: 232.69

Number of Fisher Scoring iterations: 4

> table(birthwt$ui)

 No Yes 
161  28 
> levels(birthwt$ui)
[1] "No"  "Yes"

> birthwt$ui <- relevel(birthwt$ui, ref="No")

> m2 <- glm(low ~ ui, data=birthwt, family=binomial)
> summary(m2)

Call:
glm(formula = low ~ ui, family = binomial, data = birthwt)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.1774  -0.8097  -0.8097   1.1774   1.5967  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -0.9469     0.1756  -5.392 6.97e-08 ***
uiYes         0.9469     0.4168   2.272   0.0231 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 234.67  on 188  degrees of freedom
Residual deviance: 229.60  on 187  degrees of freedom
AIC: 233.6

Number of Fisher Scoring iterations: 4

> exp(coef(m2))
(Intercept)       uiYes 
   0.387931    2.577778 

> oddsratio(xtabs(~ ui + low, data=birthwt), log=FALSE)
[1] 2.577778

> confint(m2)
Waiting for profiling to be done...
                 2.5 %     97.5 %
(Intercept) -1.3007911 -0.6105528
uiYes        0.1250472  1.7717715

> anova(m2, test="Chisq")
Analysis of Deviance Table

Model: binomial, link: logit

Response: low

Terms added sequentially (first to last)


     Df Deviance Resid. Df Resid. Dev Pr(>Chi)  
NULL                   188     234.67           
ui    1   5.0761       187     229.60  0.02426 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


######################################################################
## Prédiction par intervalles
######################################################################
> head(predict(m2, type="link"), n=4)
           85            86            87            88 
 2.220446e-16 -9.469277e-01 -9.469277e-01  2.220446e-16 
> head(predict(m2, type="response"), n=4)
       85        86        87        88 
0.5000000 0.2795031 0.2795031 0.5000000 

> xyplot(fitted(m) ~ lwt, data=birthwt)

> pp <- predict(m2, type="link", se.fit=TRUE)
> lwr <- pp$fit - qnorm(0.975) * pp$se.fit
> upr <- pp$fit + qnorm(0.975) * pp$se.fit
> ppd <- data.frame(y=pp$fit, lwr=lwr, upr=upr)
> head(ppd)
               y        lwr        upr
85  2.220446e-16 -0.7407968  0.7407968
86 -9.469277e-01 -1.2911393 -0.6027161
87 -9.469277e-01 -1.2911393 -0.6027161
88  2.220446e-16 -0.7407968  0.7407968
89  2.220446e-16 -0.7407968  0.7407968
91 -9.469277e-01 -1.2911393 -0.6027161

> head(sapply(ppd, plogis))
             y       lwr       upr
[1,] 0.5000000 0.3228299 0.6771701
[2,] 0.2795031 0.2156600 0.3537225
[3,] 0.2795031 0.2156600 0.3537225
[4,] 0.5000000 0.3228299 0.6771701
[5,] 0.5000000 0.3228299 0.6771701
[6,] 0.2795031 0.2156600 0.3537225

> head(exp(pp$fit+1.96*pp$se.fit)/(1+exp(pp$fit+1.96*pp$se.fit)))
      85       86       87       88       89       91 
0.677173 0.353724 0.353724 0.677173 0.677173 0.353724 


######################################################################
## Cas des données groupées
######################################################################
> tab <- xtabs(~ ui + low, data=birthwt)
> tab
     low
ui     No Yes
  No  116  45
  Yes  14  14

> tab2 <- data.frame(ui=c("No","Yes"), n=c(45,14), 
                     tot=c(45+116, 14+14))
> tab2
   ui  n tot
1  No 45 161
2 Yes 14  28

> glm(n/tot ~ ui, data=tab2, family=binomial, weights=tot)
> glm(cbind(n, tot-n) ~ ui, data=tab2, family=binomial)


######################################################################
## Courbe ROC
######################################################################
> m3 <- glm(low ~ lwt + race, data=birthwt, family=binomial)
> pred <- prediction(predict(m3, type="response"), birthwt$low)
> perf <- performance(pred, "tpr","fpr")
> plot(perf, ylab="Sensibilité", xlab="1-Spécificité")


######################################################################
## Applications
######################################################################
> tab <- as.table(matrix(c(815,115,208,327), nrow=2, byrow=TRUE, 
                         dimnames=list(EST=c("+","-"), 
                                       CAD=c("+","-"))))
> tab

> library(epiR)
> epi.tests(tab)

> library(epicalc)
> roc.from.table(tab, graph=FALSE)

> bp <- read.table("hdis.dat", header=TRUE)
> str(bp)

> blab <- c("<117","117-126","127-136","137-146",
            "147-156","157-166","167-186",">186")
> clab <- c("<200","200-209","210-219","220-244",
            "245-259","260-284",">284")
> bp$bpress <- factor(bp$bpress, labels=blab)
> bp$chol <- factor(bp$chol, labels=clab)

> str(bp)
> summary(bp)

> round(xtabs(hdis/total ~ bpress + chol, data=bp), 2)

> blab2 <- c(111.5,121.5,131.5,141.5,151.5,161.5,176.5,191.5)
> bp$bpress <- rep(blab2, each=7)
> dfrm <- aggregate(bp[,c("hdis","total")], 
                    list(bpress=bp[,"bpress"]), sum)

> head(dfrm, 5)

> logit <- function(x) log(x/(1-x))
> dfrm$prop <- dfrm$hdis/dfrm$total
> dfrm$logit <- logit(dfrm$hdis/dfrm$total)

> log((dfrm$hdis/dfrm$total)/(1-dfrm$hdis/dfrm$total))

> glm(cbind(hdis,total-hdis) ~ bpress,data=dfrm,family=binomial)

> summary(glm(cbind(hdis, total-hdis) ~ bpress, data=dfrm, 
              family=binomial))

> glm.res <- glm(cbind(hdis, total-hdis) ~ bpress, data=dfrm, 
                 family=binomial)
> predict(glm.res)

> cbind(dfrm, logit.predit=predict(glm.res))

> dfrm$prop.predit <- predict(glm.res, type="response") 
> f <- function(x) 
    1/(1+exp(-(coef(glm.res)[1]+coef(glm.res)[2]*x)))
> xyplot(hdis/total ~ bpress, data=dfrm, aspect=1.2, cex=.8,
         xlab="Pression artérielle",ylab="Probabilité infarctus",
         panel=function(x, y, ...) {
           panel.xyplot(x, y, col="gray30", pch=19, ...)
           panel.curve(f, lty=3, col="gray70")
           panel.points(x, dfrm$prop.predit, col="gray70", ...)
         })

> alcool <- matrix(c(666,104,109,96), nr=2, 
                   dimnames=list(c("Témoin","Cas"), 
                                 c("<80",">=80")))
> alcool

> library(vcd)
> oddsratio(alcool, log=FALSE)

> confint(oddsratio(alcool, log=FALSE))

> prop.test(c(96,109), c(200,775), correct=FALSE)

> library(reshape2)
> alcool.df <- melt(alcool)
> names(alcool.df) <- c("maladie", "exposition", "n")
> levels(alcool.df$maladie) <- c(1,0)
> alcool.df$maladie <- relevel(alcool.df$maladie, "0")

> glm.res <- glm(maladie ~ exposition, data=alcool.df, 
                 family=binomial, weights=n)
> summary(glm.res)

> exp(coef(glm.res)[2])
> exp(confint(glm.res)[2,])

> alcool2 <- data.frame(expos=c("<80",">=80"), cas=c(104,96), 
                        total=c(104+666, 96+109))
> summary(glm(cbind(cas, total-cas) ~ expos, data=alcool2, 
              family=binomial))

> sck <- read.table("sck.dat", header=FALSE)
> names(sck) <- c("ck", "pres", "abs")
> summary(sck)

> sum(sck[,c("pres","abs")])

> ni <- apply(sck[,c("pres","abs")], 2, sum)

> sck$pres.prop <- sck$pres/ni[1]
> sck$abs.prop <- sck$abs/ni[2]

> apply(sck[,c("pres.prop","abs.prop")], 2, sum)

> xyplot(pres.prop + abs.prop ~ ck, data=sck, type=c("b", "g"),
         auto.key=TRUE, ylab="Fréquence")

> glm.res <- glm(cbind(pres,abs) ~ ck, data=sck, family=binomial)
> summary(glm.res)

> glm.pred <- predict(glm.res, type="response")
> names(glm.pred) <- sck$ck

> glm.pred[glm.pred >= 0.5]

> sck$malade <- sck$pres/(sck$pres+sck$abs)
> xyplot(glm.pred ~ sck$ck, type="l", 
         ylab="Probabilité", xlab="ck", 
         panel=function(...) {
           panel.xyplot(...)
           panel.xyplot(sck$ck, sck$malade, pch=19, col="grey")
         })

> sck.expand <- data.frame(ck=c(rep(sck$ck, sck$pres), 
                                rep(sck$ck, sck$abs)), 
                           malade=c(rep(1,ni[1]), rep(0,ni[2])))
> table(sck.expand$malade)
> with(sck.expand, tapply(malade, ck, sum))

> glm.res2 <- glm(malade ~ ck, data=sck.expand, family=binomial)
> sck.expand$prediction <- ifelse(predict(glm.res2, 
                                          type="response")>=0.5, 
                                          1, 0)
> with(sck.expand, table(malade, prediction))

> classif.tab <- with(sck.expand, table(malade, prediction))
> sum(diag(classif.tab))/sum(classif.tab)

> library(ROCR)
> pred <- prediction(predict(glm.res2, type="response"), 
                    sck.expand$malade)
> perf <- performance(pred, "tpr","fpr")
> plot(perf, ylab="Sensibilité", xlab="1-Spécificité")
> grid()
> abline(0, 1, lty=2)

> performance(pred, "auc")@"y.values"

