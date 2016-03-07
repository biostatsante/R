######################################################################
# Biostatistiques et analyse des donnÃ©es de santÃ© avec R
# chapter_4.tex
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Christophe Lalanne, Mounir Mesbah
######################################################################


######################################################################
## Statistiques descriptives et structuration des données
######################################################################
> d <- data.frame(replicate(4, rnorm(5, mean=12, sd=1.2)))
> d
        X1        X2       X3       X4
1 12.67078  9.729374 12.47485 12.38534
2 12.52286 11.428080 10.16006 11.86140
3 12.77444 11.866122 12.42084 13.01769
4 10.20481 10.663182 10.90229 10.45179
5 13.55448 11.733637 10.49526 10.76963
> summary(d)
       X1              X2               X3              X4       
 Min.   :10.20   Min.   : 9.729   Min.   :10.16   Min.   :10.45  
 1st Qu.:12.52   1st Qu.:10.663   1st Qu.:10.50   1st Qu.:10.77  
 Median :12.67   Median :11.428   Median :10.90   Median :11.86  
 Mean   :12.35   Mean   :11.084   Mean   :11.29   Mean   :11.70  
 3rd Qu.:12.77   3rd Qu.:11.734   3rd Qu.:12.42   3rd Qu.:12.39  
 Max.   :13.55   Max.   :11.866   Max.   :12.47   Max.   :13.02  

> sapply(d, mean)
      X1       X2       X3       X4 
12.34548 11.08408 11.29066 11.69717 

> dm <- melt(d, value.name="y", variable.name="trt")
No id variables; using all as measure variables
> head(dm)
  trt         y
1  X1 12.670780
2  X1 12.522859
3  X1 12.774442
4  X1 10.204814
5  X1 13.554482
6  X2  9.729374
> summary(dm)
 trt          y         
 X1:5   Min.   : 9.729  
 X2:5   1st Qu.:10.621  
 X3:5   Median :11.798  
 X4:5   Mean   :11.604  
        3rd Qu.:12.487  
        Max.   :13.554  
> with(dm, tapply(y, trt, mean))
      X1       X2       X3       X4 
12.34548 11.08408 11.29066 11.69717 

> aggregate(y ~ trt, data=dm, mean)
  trt        y
1  X1 12.34548
2  X2 11.08408
3  X3 11.29066
4  X4 11.69717


######################################################################
## Le modèle d'ANOVA à un facteur
######################################################################
> aov(bwt ~ race, data=birthwt)
Call:
   aov(formula = bwt ~ race, data = birthwt)

Terms:
                    race Residuals
Sum of Squares   5015725  94953931
Deg. of Freedom        2       186

Residual standard error: 714.4963
Estimated effects may be unbalanced

> summary(aov(bwt ~ race, data=birthwt))
             Df   Sum Sq Mean Sq F value  Pr(>F)   
race          2  5015725 2507863   4.913 0.00834 **
Residuals   186 94953931  510505                   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> res <- aov(bwt ~ race, data=birthwt)
> summary(res)

> bwplot(bwt ~ race, data=birthwt)

> dotplot(race ~ bwt, data=birthwt, type="a")


######################################################################
## aggregate()
######################################################################
> bwtmeans <- aggregate(bwt ~ race, data=birthwt, mean)
> bwtmeans
   race      bwt
1 White 3102.719
2 Black 2719.692
3 Other 2805.284
> barchart(bwt ~ race, data=bwtmeans, horizontal=FALSE, 
           ylim=c(2350, 3250))

> bartlett.test(bwt ~ race, data=birthwt)

	Bartlett test of homogeneity of variances

data:  bwt by race
Bartlett's K-squared = 0.65952, df = 2, p-value = 0.7191

> unique(predict(res))
[1] 2719.692 2805.284 3102.719


######################################################################
## Comparaisons par paires de traitement
######################################################################
> with(birthwt, pairwise.t.test(bwt, race, 
                                p.adjust.method="bonferroni"))

	Pairwise comparisons using t tests with pooled SD 

data:  bwt and race 

      White Black
Black 0.049 -    
Other 0.029 1.000

P value adjustment method: bonferroni 

> t1 <- t.test(bwt ~ race, data=birthwt, subset=race != "Other")
> t2 <- t.test(bwt ~ race, data=birthwt, subset=race != "Black")
> c(t1$p.value, t2$p.value) * 3
[1] 0.03509656 0.03277981
> with(birthwt, pairwise.t.test(bwt, race, p.adj="bonf", 
                                pool.sd=FALSE))

	Pairwise comparisons using t tests with non-pooled SD 

data:  bwt and race 

      White Black
Black 0.035 -    
Other 0.033 1.000

P value adjustment method: bonferroni 


######################################################################
## Test de tendance linéaire
######################################################################
> table(birthwt$ftv)

  0   1   2   3   4   6 
100  47  30   7   4   1 

> birthwt$ftv2 <- birthwt$ftv
> birthwt$ftv2[birthwt$ftv2 > 2] <- 2
> birthwt$ftv2 <- factor(birthwt$ftv2, labels=c("0","1","2+"))
> table(birthwt$ftv2)

  0   1  2+ 
100  47  42 

> xyplot(bwt ~ ftv2, data=birthwt, type=c("p","a"),jitter.x=TRUE)

> summary(aov(bwt ~ ftv2, data=birthwt))
             Df   Sum Sq Mean Sq F value Pr(>F)
ftv2          2  1887925  943963    1.79   0.17
Residuals   186 98081730  527321   

> anova(lm(bwt ~ as.numeric(ftv2), birthwt))
Analysis of Variance Table

Response: bwt
                  Df   Sum Sq Mean Sq F value Pr(>F)
as.numeric(ftv2)   1   542578  542578  1.0205 0.3137
Residuals        187 99427078  531696  

> levels(birthwt$ftv2)
[1] "0"  "1"  "2+"
> sort(unique(as.numeric(birthwt$ftv2)))
[1] 1 2 3

> birthwt$ftv3 <- ordered(birthwt$ftv2)
> res <- aov(bwt ~ ftv3, data=birthwt)
> summary(res, split=list(ftv3=c(Linear=1, Quadratic=2)))
                   Df   Sum Sq Mean Sq F value Pr(>F)
ftv3                2  1887925  943963   1.790  0.170
  ftv3: Linear      1   542578  542578   1.029  0.312
  ftv3: Quadratic   1  1345348 1345348   2.551  0.112
Residuals         186 98081730  527321               

> summary.lm(res)
v
Call:
aov(formula = bwt ~ ftv3, data = birthwt)

Residuals:
     Min       1Q   Median       3Q      Max 
-2156.14  -484.88    26.12   578.86  1882.00 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2974.67      56.81  52.360   <2e-16 ***
ftv3.L         60.63      94.42   0.642    0.522    
ftv3.Q       -163.29     102.23  -1.597    0.112    
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 726.2 on 186 degrees of freedom
Multiple R-squared:  0.01888,	Adjusted R-squared:  0.008335 
F-statistic:  1.79 on 2 and 186 DF,  p-value: 0.1698


######################################################################
## Approche non paramétrique de l'ANOVA à un facteur
######################################################################
> kruskal.test(bwt ~ race, data=birthwt)

	Kruskal-Wallis rank sum test

data:  bwt by race
Kruskal-Wallis chi-squared = 8.5199, df = 2, p-value = 0.01412


######################################################################
## Construction du tableau d'ANOVA
######################################################################
> m <- aov(bwt ~ race + ht + race:ht, data=birthwt)

> summary(m)
             Df   Sum Sq Mean Sq F value  Pr(>F)   
race          2  5015725 2507863   4.973 0.00789 **
ht            1  1776713 1776713   3.523 0.06211 . 
race:ht       2   889649  444825   0.882 0.41568   
Residuals   183 92287568  504304                   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> summary(aov(bwt ~ race + ht, data=birthwt))
             Df   Sum Sq Mean Sq F value  Pr(>F)   
race          2  5015725 2507863   4.979 0.00783 **
ht            1  1776713 1776713   3.528 0.06193 . 
Residuals   185 93177217  503661                   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

> xyplot(bwt ~ race, data=birthwt, groups=ht, type=c("a","g"), 
         auto.key=list(corner=c(0,1), title="ht", 
                       cex.title=1, points=FALSE, lines=TRUE))

> aggregate(bwt ~ race + ht, data=birthwt, mean)
   race  ht      bwt
1 White  No 3110.890
2 Black  No 2751.826
3 Other  No 2852.413
4 White Yes 2954.000
5 Black Yes 2473.333
6 Other Yes 2063.000

> model.tables(m)
Tables of effects

 race 
    White  Black  Other
    158.1 -224.9 -139.3
rep  96.0   26.0   67.0

 ht 
        No  Yes
     25.15 -371
rep 177.00   12

 race:ht 
       ht
race    No     Yes   
  White  -12.6  229.6
  rep     91.0    5.0
  Black  -13.9  106.7
  rep     23.0    3.0
  Other   23.3 -367.0
  rep     63.0    4.0


######################################################################
## Diagnostic du modèle
######################################################################
> htrace <- with(birthwt, interaction(race, ht))
> bartlett.test(bwt ~ htrace, data=birthwt)

	Bartlett test of homogeneity of variances

data:  bwt by htrace
Bartlett's K-squared = 5.1225, df = 5, p-value = 0.4011


######################################################################
## Applications
######################################################################
> library(foreign)
> polymsm <- read.dta("polymorphism.dta")
> head(polymsm)

> with(polymsm, tapply(age, genotype, mean))
> with(polymsm, tapply(age, genotype, sd))

> bwplot(age ~ genotype, data=polymsm)

> histogram(~ age | genotype, data=polymsm)

> aov.res <- aov(age ~ genotype, data=polymsm)
> summary(aov.res)

> mse <- unlist(summary(aov.res))["Mean Sq2"]
> se <- sqrt(mse)

> ni <- table(polymsm$genotype)
> n <- sum(ni)
> m <- with(polymsm, tapply(age, genotype, mean))
> lci <- m - qt(0.975, n-3) * se / sqrt(ni)
> uci <- m + qt(0.975, n-3) * se / sqrt(ni)
> rbind(lci, uci) 

> library(epiR)
> epi.conf(polymsm$age[polymsm$genotype=="1.6/1.6"], 
           ctype = "mean.single")

> mm <- as.data.frame(cbind(m, lci, uci))
> mm$g <- levels(polymsm$genotype)
> rownames(mm) <- NULL
> dotplot(m ~ g, data=mm, ylim=c(40,75),
          panel=function(x, y, ...) {
            panel.dotplot(x, y, ...)
            panel.segments(x, mm$lci, x, mm$uci)
          })

> xYplot(Cbind(m,lci,uci) ~ 1:3, data=mm, 
         scales=list(x=list(at=1:3, labels=mm$g)), 
         xlab="", ylim=c(40,75))

> library(foreign)
> weights <- read.spss("weights.sav", to.data.frame=TRUE)
> str(weights)

> table(weights$PARITY)
> round(prop.table(table(weights$PARITY))*100, 1)

> round(with(weights, tapply(WEIGHT, PARITY, mean)), 2)
> round(with(weights, tapply(WEIGHT, PARITY, sd)), 2)

> aov.res <- aov(WEIGHT ~ PARITY, data=weights)
> summary(aov.res)

> stripplot(WEIGHT ~ PARITY, data=weights, ylab="Poids (kg)", 
            jitter.data=TRUE)

> histogram(~ WEIGHT | PARITY, data=weights)

> library(car)
> leveneTest(WEIGHT ~ PARITY, data=weights)

> PARITY2 <- weights$PARITY
> levels(PARITY2)[3:4] <- "2 siblings or more"

> aov.res2 <- aov(WEIGHT ~ PARITY2, data=weights)
> summary(aov.res2)

> lm.res <- lm(WEIGHT ~ as.numeric(PARITY2), data=weights)
> summary(lm.res)

> summary(lm(WEIGHT ~ as.ordered(PARITY2), data=weights))

> data(ToothGrowth)
> str(ToothGrowth)

> ToothGrowth$dose <- factor(ToothGrowth$dose)

> xtabs(~ dose + supp, data=ToothGrowth)

> fm <- len ~ supp * dose
> replications(fm, data=ToothGrowth)

> f <- function(x) c(mean = mean(x), sd = sd(x))
> aggregate(fm, ToothGrowth, f)

> aov.fit <- aov(fm, data = ToothGrowth)
> summary(aov.fit)

> model.tables(aov.fit, type = "means", se = TRUE, 
               cterms = "supp:dose")

> xyplot(len ~ dose, data = ToothGrowth, groups = supp, 
         type = c("a", "g"), xlab = "Dose (mg)", 
         ylab = "Tooth length", lwd = 2, 
         auto.key = list(space = "top", columns = 2, 
                         points = FALSE, lines = TRUE))

> bwplot(len ~ interaction(supp, dose), data = ToothGrowth, 
         do.out = FALSE, pch = "|")

> bartlett.test(len ~ interaction(supp, dose), data=ToothGrowth)

