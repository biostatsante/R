######################################################################
# Biostatistiques et analyse des donnÃ©es de santÃ© avec R
# chapter_1.tex
# R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
# Christophe Lalanne, Mounir Mesbah
######################################################################


######################################################################
## Gestion de variables numériques
######################################################################
> x <- c(2523,2551,2557,2594,2600,2622,2637,2637,2663,2665)  

> x
[1] 2523 2551 2557 2594 2600 2622 2637 2637 2663 2665

> y <- c(82.7,70.5,47.7,49.1,48.6,56.4,53.6,46.8,55.9,51.4)  

> ls()
[1] "x" "y"  

> length(x)
[1] 10  

> x[5]
[1] 2600


######################################################################
## Opérations sur une variable numérique
######################################################################
> x[5] / 1000
[1] 2.6

> x / 1000
[1] 2.523 2.551 2.557 2.594 2.600 2.622 2.637 2.637 2.663 2.665

> x2 <- x / 1000
> x
[1] 2523 2551 2557 2594 2600 2622 2637 2637 2663 2665
> x2
[1] 2.523 2.551 2.557 2.594 2.600 2.622 2.637 2.637 2.663 2.665

> mean(x)
[1] 2604.9

> range(x)
[1] 2523 2665
> c(min(x), max(x))
[1] 2523 2665
> var(x)
[1] 2376.767

> sum(x)
[1] 26049
> sum(x^2)
[1] 67876431
> sum(x^2 - mean(x))
[1] 67850382


######################################################################
## Gestion de variables catégorielles
######################################################################
> z <- c(1, 1, 2, 2, 2, 1, 1, 1, 2, 2)
> z
[1] 1 1 2 2 2 1 1 1 2 2

> factor(z, levels = c(1,2), labels=c("NF","F"))
 [1] NF NF F  F  F  NF NF NF F  F 
Levels: NF F

> z <- factor(z, labels=c("NF","F"))
> levels(z)
[1] "NF" "F" 
> nlevels(z)
[1] 2


######################################################################
## Manipulation de variables catégorielles
######################################################################
> vems <- sample(1:3, 10, replace=TRUE)
> vems
 [1] 3 2 1 3 3 3 2 2 1 2

> vems <- factor(vems, levels=c(1,2,3), 
                 labels=c("critique","bas","normal"))
> vems
 [1] normal   bas      critique normal   normal   normal   bas      
 [8] bas critique bas     
Levels: critique bas normal

> table(vems)
vems
critique      bas   normal 
       2        4        4 

> levels(vems)
[1] "critique" "bas"      "normal"  
> levels(vems)[1:2] <- "critique ou bas"
> levels(vems)
[1] "critique ou bas" "normal"         
> table(vems)
vems
critique ou bas          normal 
              6               4 


######################################################################
## Sélection indexée d'observations
######################################################################
> x[c(3,5)]
[1] 2557 2600

> x[3:5]
[1] 2557 2594 2600

> vems[2]
[1] critique ou bas
Levels: critique ou bas normal


######################################################################
## Sélection critériée d'observations
######################################################################
> x[y < 50]
[1] 2557 2594 2600 2637

> y < 50
 [1] FALSE FALSE  TRUE  TRUE  TRUE FALSE FALSE  TRUE FALSE FALSE

> x[z == "NF"]
[1] 2523 2551 2622 2637 2637

> x[z == "NF" & y <= 55]
[1] 2637 2637


######################################################################
## Représentation et traitement des valeurs manquantes
######################################################################
> c(2523, 2551, NA, 2594, 2600, 2622, 2637, 2637, 2663, 2665)

> x[3] <- NA
> x
 [1] 2523 2551   NA 2594 2600 2622 2637 2637 2663 2665

> is.na(x)
 [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
> which(is.na(x))
[1] 3

> complete.cases(x)
 [1]  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE

> y[complete.cases(x)]
[1] 82.7 70.5 49.1 48.6 56.4 53.6 46.8 55.9 51.4


######################################################################
## Données univariées
######################################################################
2523 2551 2557 2594 2600 2622 2637 2637 2663 2665

> x <- scan("poids.dat")
Read 10 items
> head(x, n=3)
[1] 2523 2551 2557


######################################################################
## Données multivariées
######################################################################
0 19 182 2 0 0 0 1 0 2523
0 33 155 3 0 0 0 0 3 2551
0 20 105 1 1 0 0 0 1 2557

> bt <- read.table("birthwt.dat", header=FALSE)
> varnames <- c("low","age","lwt","race","smoke","ptl","ht", 
                "ui","ftv","bwt")
> names(bt) <- varnames
> head(bt)
  low age lwt race smoke ptl ht ui ftv  bwt
1   0  19 182    2     0   0  0  1   0 2523
2   0  33 155    3     0   0  0  0   3 2551
3   0  20 105    1     1   0  0  0   1 2557
4   0  21 108    1     1   0  0  1   2 2594
5   0  18 107    1     1   0  0  1   0 2600
6   0  21 124    3     0   0  0  0   0 2622

0,19,182,2,0,0,0,1,0,2523
0,33,155,3,0,0,0,0,3,2551
0,20,105,1,1,0,0,0,1,2557


######################################################################
## Sauvegarde de données dans un fichier externe
######################################################################
> write.csv(bt, file="bt.csv")


######################################################################
## Construction d'un tableau structuré de données
######################################################################
> d <- data.frame(x, y, z)
> names(d) <- c("poids.bébé", "poids.mère", "cig.mère")
> head(d, n=3)
  poids.bébé poids.mère cig.mère
1       2523       82.7       NF
2       2551       70.5       NF
3       2557       47.7        F

> d[1,]
  poids.bébé poids.mère cig.mère
1       2523       82.7       NF

> d[c(1,2), 2]
[1] 82.7 70.5


######################################################################
## Les données birthwt
######################################################################
> data(birthwt, package="MASS")
> c(nrow(birthwt), ncol(birthwt))
[1] 189  10
> names(birthwt)
 [1] "low"   "age"   "lwt"   "race"  "smoke" "ptl"   "ht"   
 [8] "ui"    "ftv"   "bwt"  
> head(birthwt, n=2)
   low age lwt race smoke ptl ht ui ftv  bwt
85   0  19 182    2     0   0  0  1   0 2523
86   0  33 155    3     0   0  0  0   3 2551

> birthwt$bwt[1:5]
[1] 2523 2551 2557 2594 2600

> summary(birthwt$race)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  1.000   1.000   1.000   1.847   3.000   3.000 

> ouinon <- c("No", "Yes")
> birthwt$smoke <- factor(birthwt$smoke, labels=ouinon)
> birthwt$race <- factor(birthwt$race, levels=c(1, 2, 3), 
                         labels=c("White", "Black", "Other"))

> birthwt <- within(birthwt, {
    low <- factor(low, labels=ouinon)
    ht <- factor(ht, labels=ouinon)
    ui <- factor(ui, labels=ouinon)
  })

> summary(birthwt[,1:5])
  low           age             lwt           race    smoke    
 No :130   Min.   :14.00   Min.   : 80.0   White:96   No :115  
 Yes: 59   1st Qu.:19.00   1st Qu.:110.0   Black:26   Yes: 74  
           Median :23.00   Median :121.0   Other:67            
           Mean   :23.24   Mean   :129.8                       
           3rd Qu.:26.00   3rd Qu.:140.0                       
           Max.   :45.00   Max.   :250.0                       

> mean(birthwt$lwt[birthwt$smoke == "Yes"]) ## poids en livres
[1] 128.1351
> table(birthwt$ht[birthwt$lwt/2.2 > 60])   ## poids en kg

 No Yes 
 55   7 
> min(birthwt$bwt[birthwt$ui == "No"])
[1] 1135

> with(birthwt, lwt[1:5])
[1] 182 155 105 108 107
> with(birthwt, mean(lwt[smoke == "Yes"]))
[1] 128.1351


######################################################################
## Applications
######################################################################
3.64 2.27 1.43 1.77 4.62 3.04 1.01 2.14 3.02 5.62 5.51 5.51 1.01 
1.05 4.19 2.63 4.34 4.85 4.02 5.92

> log10(50)

> X <- c(3.64,2.27,1.43,1.77,4.62,3.04,1.01,2.14,3.02,5.62,5.51,
         5.51,1.01,1.05,4.19, 2.63,4.34,4.85,4.02,5.92)
> length(X[X <= log10(50)])

> X[X == 3.04] <- 3.64

> X[7] <- NA

> Xc <- X[X > log10(50)]
> round(median(10^Xc), 0)

6.379 6.683 5.120 ...

> x <- scan("dosage.txt")

Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, : 
  scan() attendait 'a real' et a reçu '2,914'

> x <- scan("dosage.txt", what="character")
> str(x)
> head(x)

> x[x == "2,914"] <- "2.914"
> x <- as.numeric(x)
> head(x)
> round(mean(x), 3)

> write.table(x, file="data.txt", row.names=FALSE)

"x"
6.379
6.683
5.12
6.707
6.149
5.06

Group Before After
g1 80.5  82.2
g1 84.9  85.6
g1 81.5  81.4
g1 82.6  81.9

> anorex <- read.table("anorexia.dat", header=TRUE)
> names(anorex)
> head(anorex)

> nrow(anorex)

> table(anorex$Group)

> anorex$Before <- anorex$Before/2.2
> anorex$After <- anorex$After/2.2

> anorex$Before.kg <- anorex$Before/2.2
> anorex$After.kg <- anorex$After/2.2

> anorex$poids.diff <- anorex$After - anorex$Before
> head(anorex)

> mean(anorex$poids.diff[anorex$Group == "g1"])
> range(anorex$poids.diff[anorex$Group == "g1"])

