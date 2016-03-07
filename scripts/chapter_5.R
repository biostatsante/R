######################################################################
# Biostatistiques et analyse des donnÃ©es de santÃ© avec R
# chapter_5.tex
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Christophe Lalanne, Mounir Mesbah
######################################################################


######################################################################
## Statistiques descriptives
######################################################################
> summary(birthwt[,c("bwt","lwt")])
> summary(birthwt[,c(10,3)])
> summary(subset(birthwt, select=c(bwt, lwt)))
> sapply(birthwt[,c("bwt","lwt")], summary)
> apply(birthwt[,c(10,3)], 2, summary)
         bwt   lwt
Min.     709  80.0
1st Qu. 2414 110.0
Median  2977 121.0
Mean    2945 129.8
3rd Qu. 3487 140.0
Max.    4990 250.0

> histogram(~ bwt + lwt, data=birthwt, breaks=NULL, xlab="",
            scales=list(x=list(relation="free")))


######################################################################
## Diagramme de dispersion et courbe loess
######################################################################
> birthwt$lwt <- birthwt$lwt/2.2
> xyplot(bwt ~ lwt, data=birthwt, type=c("p","g","smooth"))


######################################################################
## Mesures d'association paramétrique et non paramétrique
######################################################################
> with(birthwt, cor(bwt, lwt))
[1] 0.1857333

> with(birthwt, cor(bwt, lwt, method="spearman"))
[1] 0.2488882

> cor(birthwt[,c("age","lwt","bwt")])
           age       lwt        bwt
age 1.00000000 0.1800732 0.09031781
lwt 0.18007315 1.0000000 0.18573328
bwt 0.09031781 0.1857333 1.00000000


######################################################################
## Estimation par intervalles et test d'inférence
######################################################################
> with(birthwt, cor.test(bwt, lwt))

	Pearson's product-moment correlation

data:  bwt and lwt
t = 2.5848, df = 187, p-value = 0.0105
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.04417405 0.31998094
sample estimates:
      cor 
0.1857333 

> cor.test(~ bwt + lwt, data=birthwt, subset=bwt > 2500)

	Pearson's product-moment correlation

data:  bwt and lwt
t = 1.6616, df = 128, p-value = 0.09905
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 -0.02757158  0.30974090
sample estimates:
      cor 
0.1453043 


######################################################################
## Droite de régression
######################################################################
> lm(bwt ~ lwt, data=birthwt)

Call:
lm(formula = bwt ~ lwt, data = birthwt)

Coefficients:
(Intercept)          lwt  
   2369.624        9.744 

> r <- lm(bwt ~ lwt, data=birthwt)
> summary(r)

Call:
lm(formula = bwt ~ lwt, data = birthwt)

Residuals:
     Min       1Q   Median       3Q      Max 
-2192.12  -497.97    -3.84   508.32  2075.60 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 2369.624    228.493  10.371   <2e-16 ***
lwt            9.744      3.770   2.585   0.0105 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 718.4 on 187 degrees of freedom
Multiple R-squared:  0.0345,	Adjusted R-squared:  0.02933 
F-statistic: 6.681 on 1 and 187 DF,  p-value: 0.0105

> xyplot(bwt ~ lwt, data=birthwt, type=c("p","g","r"))


######################################################################
## Estimation par intervalles et tableau d'analyse de variance
######################################################################
> confint(r)
                  2.5 %     97.5 %
(Intercept) 1918.867879 2820.37916
lwt            2.307459   17.18061

> res <- cbind(coef(r), confint(r))
> colnames(res)[1] <- "Coef"
> round(res, 3)
                Coef    2.5 %   97.5 %
(Intercept) 2369.624 1918.868 2820.379
lwt            9.744    2.307   17.181

> anova(r)
Analysis of Variance Table

Response: bwt
           Df   Sum Sq Mean Sq F value Pr(>F)  
lwt         1  3448639 3448639  6.6814 0.0105 *
Residuals 187 96521017  516155                 
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


######################################################################
## Prédictions à partir du modèle de régression
######################################################################
> head(fitted(r))
      85       86       87       88       89       91 
3175.721 3056.135 2834.680 2847.967 2843.538 2918.833 

> head(cbind(birthwt$bwt, fitted(r)))
   [,1]     [,2]
85 2523 3175.721
86 2551 3056.135
87 2557 2834.680
88 2594 2847.967
89 2600 2843.538
91 2622 2918.833

> xyplot(bwt + fitted(r) ~ lwt, data=birthwt)

> dp <- data.frame(lwt=seq(35, 120, by=5))
> bwtpred <- predict(r, newdata=dp)
> dp$bwt <- bwtpred
> head(dp)
  lwt      bwt
1  35 2710.665
2  40 2759.385
3  45 2808.105
4  50 2856.825
5  55 2905.546
6  60 2954.266


######################################################################
## Diagnostic du modèle et analyse des résidus
######################################################################
> head(resid(r))
       85        86        87        88        89        91 
-652.7211 -505.1352 -277.6798 -253.9671 -243.5380 -296.8329 
> head(birthwt$bwt - fitted(r))
       85        86        87        88        89        91 
-652.7211 -505.1352 -277.6798 -253.9671 -243.5380 -296.8329 

> xyplot(resid(r) ~ fitted(r), type=c("p","g"))


######################################################################
## Lien avec l'ANOVA
######################################################################
> lm(bwt ~ race, data=birthwt)

Call:
lm(formula = bwt ~ race, data = birthwt)

Coefficients:
(Intercept)    raceBlack    raceOther  
     3102.7       -383.0       -297.4  

> bwtmeans <- with(birthwt, tapply(bwt, race, mean))
> bwtmeans
   White    Black    Other 
3102.719 2719.692 2805.284 
> bwtmeans[2:3] - bwtmeans[1]
    Black     Other 
-383.0264 -297.4352 

> lm(bwt ~ race - 1, data=birthwt)

Call:
lm(formula = bwt ~ race - 1, data = birthwt)

Coefficients:
raceWhite  raceBlack  raceOther  
     3103       2720       2805  

> anova(lm(bwt ~ race, data=birthwt))
Analysis of Variance Table

Response: bwt
           Df   Sum Sq Mean Sq F value   Pr(>F)   
race        2  5015725 2507863  4.9125 0.008336 **
Residuals 186 94953931  510505                    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


######################################################################
## Régression linéaire multiple
######################################################################
> lm(bwt ~ scale(lwt, scale=FALSE) + ftv, data=birthwt) 


######################################################################
## Applications
######################################################################
> cystic <- read.table("cystic.dat", header=TRUE)
> str(cystic)
> summary(cystic)

> cystic$Sex <- factor(cystic$Sex, labels=c("M","F"))
> table(cystic$Sex)

> with(cystic, cor(PEmax, Weight))

> with(cystic, cor.test(PEmax, Weight))

> splom(cystic[,-c(1,3)], varname.cex=0.7, cex=.8)

> round(cor(cystic[,-3]), 3)

> round(cor(cystic[,-3], method="spearman"), 3)

> library(ppcor)
> with(cystic, pcor.test(PEmax, Weight, Age))

> cystic$Age.ter <- cut(cystic$Age, 
                        breaks=quantile(cystic$Age, 
                                        c(0,0.33,0.66,1)), 
                        include.lowest=TRUE)
> cystic2 <- subset(cystic, as.numeric(Age.ter) %in% c(1,3))
> cystic2$Age.ter <- factor(cystic2$Age.ter)
> xyplot(PEmax ~ Weight, data=cystic2, groups=Age.ter, 
         auto.key=list(corner=c(0,1)))

> fram <- read.csv("data/Framingham.csv")
> head(fram)
> str(fram)

> table(fram$sex)
> fram$sex <- factor(fram$sex, labels=c("M","F"))

> apply(fram, 2, function(x) sum(is.na(x)))

> with(fram, table(sex[!is.na(bmi)]))

> xyplot(sbp ~ bmi | sex, data=fram, type=c("p","g"), alpha=0.5, 
         cex=0.7, pch=19)

> with(subset(fram, sex=="M"), cor(sbp, bmi, use="pair"))
> with(subset(fram, sex=="F"), cor(sbp, bmi, use="pair"))

> library(psych)
> r.test(n=2047, r12=0.23644, n2=2643, r34=0.37362)

> library(gridExtra)
> p1 <- histogram(~ bmi, data=fram)
> p2 <- histogram(~ log(bmi), data=fram)
> p3 <- histogram(~ sbp, data=fram)
> p4 <- histogram(~ log(sbp), data=fram)
> grid.arrange(p1, p2, p3, p4)

> reg.resM <- lm(log(sbp) ~ log(bmi), data=fram, subset=sex=="M")
> reg.resF <- lm(log(sbp) ~ log(bmi), data=fram, subset=sex=="F")
> summary(reg.resM)   # Hommes
> confint(reg.resM)
> summary(reg.resF)   # Femmes
> confint(reg.resF)

> res <- data.frame(pente=c(coef(reg.resM)[2], 
                            coef(reg.resF)[2]), 
                    rbind(confint(reg.resM)[2,], 
                          confint(reg.resF)[2,]))
> rownames(res) <- c("M","F")
> colnames(res)[2:3] <- c("2.5 %", "97.5 %")
> round(res, 3)

> options(contrasts=c("contr.sum", "contr.poly"))

> xyplot(bwt ~ lwt | race, data=birthwt, layout=c(3,1), 
         type=c("p","g"), aspect=0.8)

> xyplot(bwt ~ lwt, data=birthwt, groups=race)

> reg.res <- lm(bwt ~ scale(lwt, scale=FALSE), data=birthwt)
> summary(reg.res)

> reg.res2 <- lm(bwt ~ race, data=birthwt)
> summary(reg.res2)

> aov.res <- aov(bwt ~ race, data=birthwt)
> summary(aov.res)

> summary.lm(aov.res)

> anova(reg.res2)

> grp.means <- with(birthwt, tapply(bwt, race, mean))
> grp.means[2:3] - grp.means[1] # modèle de régression reg.res2

> m <- lm(bwt ~ lwt, data=birthwt)
> d <- data.frame(lwt=60)
> predict(m, newdata=d, interval="confidence") 

