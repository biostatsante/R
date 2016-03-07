######################################################################
# Biostatistiques et analyse des donnÃ©es de santÃ© avec R
# chapter_2.tex
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Christophe Lalanne, Mounir Mesbah
######################################################################


######################################################################
## Tendance centrale et forme de la distribution
######################################################################
> sum(is.na(birthwt$bwt))
[1] 0

> mean(birthwt$bwt)
[1] 2944.587
> median(birthwt$bwt)
[1] 2977

> summary(birthwt$bwt)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    709    2414    2977    2945    3487    4990 

> quantile(birthwt$bwt)
  0%  25%  50%  75% 100% 
 709 2414 2977 3487 4990 

> quantile(birthwt$bwt, probs=seq(0.1, 1, by=0.1))


######################################################################
## Indicateurs de dispersion
######################################################################
> quantile(birthwt$bwt, probs=c(0.75)) 
  - quantile(birthwt$bwt, probs=c(0.25))
 75% 
1073

> diff(quantile(birthwt$bwt, probs=c(0.25, 0.75)))
 75% 
1073 

> var(birthwt$bwt)
[1] 531753.5
> sd(birthwt$bwt)
[1] 729.2143
> sd(birthwt$bwt)^2
[1] 531753.5


######################################################################
## Résumer une variable catégorielle
######################################################################
> table(birthwt$race)

White Black Other 
   96    26    67

> tab <- table(birthwt$race)
> tab

White Black Other 
   96    26    67 
> prop.table(tab)

    White     Black     Other 
0.5079365 0.1375661 0.3544974 

> which.max(tab)
White 
    1 

> xtabs(~ birthwt$race)
birthwt$race
White Black Other 
   96    26    67 

> table(cut(birthwt$age, breaks=c(15,19,24,29,45), 
            include.lowest=TRUE))

[15,19] (19,24] (24,29] (29,45] 
     48      69      42      27 


######################################################################
## Représenter graphiquement la distribution d'une variable
######################################################################
> library(lattice)


######################################################################
## Cas des variables numériques
######################################################################
> stripplot(~ poids)

> stripplot(~ bwt, data=birthwt, jitter.data=TRUE, amount=.05, 
            xlab="Poids (g)")

> histogram(~ bwt, data=birthwt, type="count", xlab="Poids (g)", 
            ylab="Effectifs")

> histogram(~ bwt, data=birthwt, subset=smoke == "Yes", 
            type="count")

> densityplot(~ bwt, data=birthwt, xlab="Poids (g)", 
              ylab="Densité")

help(panel.densityplot)

> bwplot(~ bwt, data=birthwt, xlab="Poids (g)")


######################################################################
## Cas des variables catégorielles
######################################################################
> barchart(xtabs(~ race, data=birthwt), xlab="Ethncité", 
           ylab="Effectifs", horizontal=FALSE)

> dotplot(xtabs(~ race, data=birthwt), ylab="Ethnicité", 
          xlab="Effectifs")


######################################################################
## Intervalles de confiance pour une moyenne
######################################################################
> qnorm(0.975)
[1] 1.959964

> sd(birthwt$bwt) / sqrt(length(birthwt$bwt))
[1] 53.04254

> with(birthwt, sd(bwt) / sqrt(length(bwt)))

> mean(birthwt$bwt) - qnorm(0.975) * sd(birthwt$bwt) / 
  sqrt(length(birthwt$bwt))
[1] 2840.626
> mean(birthwt$bwt) + qnorm(0.975) * sd(birthwt$bwt) / 
  sqrt(length(birthwt$bwt))
[1] 3048.549

> bwt.lci <- mean(birthwt$bwt) - qnorm(0.975) * sd(birthwt$bwt) / 
             sqrt(length(birthwt$bwt))
> bwt.uci <- mean(birthwt$bwt) + qnorm(0.975) * sd(birthwt$bwt) / 
             sqrt(length(birthwt$bwt))
> c(bwt.lci, bwt.uci)
[1] 2840.626 3048.549

> mean(birthwt$bwt) + c(-1,1) * qnorm(0.975) * sd(birthwt$bwt) / 
  sqrt(length(birthwt$bwt))

> qt(0.975, df=18)
[1] 2.100922


######################################################################
## Intervalles de confiance pour une proportion
######################################################################
> tab <- prop.table(table(birthwt$smoke))
> p <- tab[2]
> n <- nrow(birthwt)
> p + c(-1,1) * qnorm(0.975) * sqrt((p*(1-p))/n)
[1] 0.3219487 0.4611201

> tab

       No       Yes 
0.6084656 0.3915344


######################################################################
## Applications
######################################################################
24.9,25.0,25.0,25.1,25.2,25.2,25.3,25.3,25.3,25.4,25.4,25.4,25.4,
25.5,25.5,25.5,25.5,25.6,25.6,25.6,25.7,25.7,25.8,25.8,25.9,26.0

> x <- c(24.9,25.0,25.0,25.1,25.2,25.2,25.3,25.3,25.3,25.4,25.4,
         25.4,25.4,25.5,25.5,25.5,25.5,25.6,25.6,25.6,25.7,25.7,
         25.8,25.8,25.9,26.0)

> median(x)
> mean(x)

> quantile(x)

> table(x)
> names(table(x)[table(x)==max(table(x))])

> var(x)

> xc <- cut(x, breaks=c(24.9,25.2,25.5,25.8,26.0), 
           include.lowest=TRUE, right=FALSE)
> table(xc)

> histogram(~ x,type="count",breaks=c(24.9,25.2,25.5,25.8,26.0))

7,47,58,74,177,232,273,285,317,429,440,445,455,468,495,497,532,
571,579,581,650,702,715,779,881,900,930,968,1077,1109,1314,1334,
1367,1534,1712,1784,1877,1886,2045,2056,2260,2429,2509

> s <- c(7,47,58,74,177,232,273,285,317,429,440,445,455,468,495,
         497,532,571,579,581,650,702,715,779,881,900,930,968,
         1077,1109,1314,1334,1367,1534,1712,1784,1877,1886,2045,
         2056,2260,2429,2509)

> median(s)

> table(s < 900)

> quantile(s, 0.9)

> table(s <= quantile(s, 0.9))

> tailles <- scan("elderly.dat", na.strings=".")

> sum(is.na(tailles))
> table(is.na(tailles))

> m <- mean(tailles, na.rm=TRUE)    # moyenne
> s <- sd(tailles, na.rm=TRUE)      # écart-type
> n <- sum(!is.na(tailles))         # nombre d'observations
> m - qnorm(0.975) * s/sqrt(n)      # borne inf. IC 95 %
> m + qnorm(0.975) * s/sqrt(n)      # borne sup. IC 95 %

> densityplot(~ tailles)

> densityplot(~ tailles, bw=4)

