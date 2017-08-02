# ASRI restoration
Cam Webb and Andrew MacDonald  
31/7/2017  




```r
## loading libraries
library(RMySQL)
```

```
## Loading required package: DBI
```

```r
library(knitr)
```


```r
# This Rmarkdown document caches the output download of this code chunk, so it
# should not need to be run every time the document is compiled.

# On andrew's computer, sensitive info is stored in environment variables.
user_name <- Sys.getenv("asriusr")
password <- Sys.getenv("asripwd")
con <- dbConnect(MySQL(), user=user_name, password=password,
                 dbname="restor", host="mysql.phylodiversity.net")
# dbListTables(con)
# dbListFields(con, "event")

# get meas data
meas <- dbGetQuery(con, "SELECT plot.plotCode, sdl.tag, sdl.id AS sdlID,
                         sdl.plantYear,
                         meas.height, meas.width, meas.dead, event.months
                         FROM  `meas` , plot, sdl, event
                         WHERE meas.sdlID = sdl.id
                         AND sdl.plotID = plot.id
                         AND sdl.plantYear < 2011
                         AND meas.eventID = event.id")
```

```
## Warning in .local(conn, statement, ...): Decimal MySQL column 4 imported as
## numeric
```

```
## Warning in .local(conn, statement, ...): Decimal MySQL column 5 imported as
## numeric
```

```r
# get plot and sdl data
plot <- dbGetQuery(con, "SELECT * FROM plot WHERE id < 423")
sdltax <- dbGetQuery(con, "SELECT sdl.id, taxon.code as species, taxon.local
                             FROM sdl, taxon WHERE sdl.taxonID = taxon.id")
tax <- dbGetQuery(con, "SELECT code, local as localname, gen, sp from taxon
                          ORDER BY code")

dbDisconnect(con)
```

```
## [1] TRUE
```


```r
kable(head(meas))
```



plotCode    tag   sdlID   plantYear   height   width  dead    months
---------  ----  ------  ----------  -------  ------  -----  -------
A10           1       1        2009     10.0      NA  no          18
A10           1       1        2009     36.0    28.5  no           8
A10           1       1        2009     38.0    29.0  no           5
A10           1       1        2009     34.5    23.0  no           3
A10           1       1        2009     28.0    32.0  no           0
A10           1       1        2009       NA      NA  NA          NA

```r
kable(head(plot))
```



 id  plotCode    pressing   roundup   weeding   cardboard  fertilizer   description    control   edge   setupYear  pcFast    pcFastTx   plusPoles   plusPolesTx   polesOnly   polesOnlyTx  notes    earthAddition   nsdl   underAcacia   diptMyco
---  ---------  ---------  --------  --------  ----------  -----------  ------------  --------  -----  ----------  -------  ---------  ----------  ------------  ----------  ------------  ------  --------------  -----  ------------  ---------
  1  A11                0         1         0           0  NPK          NA                   0      1        2009  mixed           NA           0            NA           0            NA  B                    0    177             0         NA
  2  A12                0         1         0           0  NPK          NA                   0      1        2009  mixed           NA           0            NA           0            NA  B                    0    177             0         NA
  3  A13                0         1         0           0  NPK          NA                   0      1        2009  mixed           NA           0            NA           0            NA  B                    0    177             0         NA
  4  A8                 0         1         0           0  NPK          NA                   0      1        2009  mixed           NA           0            NA           0            NA  B                    0    177             0         NA
  5  A9                 0         1         1           1  NPK          NA                   0      1        2009  5                9           0            NA           0            NA  C                    0    177             0         NA
  6  B10                1         0         1           0  NPK          NA                   0     NA        2009  mixed           NA           0            NA           0            NA  NA                   0    177             0         NA

```r
kable(head(sdltax))
```



 id  species   local    
---  --------  ---------
 48  RAM       rambutan 
 56  RAM       rambutan 
 58  RAM       rambutan 
 62  RAM       rambutan 
 63  RAM       rambutan 
 67  RAM       rambutan 

```r
kable(head(tax))
```



code   localname        gen             sp          
-----  ---------------  --------------  ------------
BBU    belabuk          NA              NA          
BEL    belian           Eusideroxylon   zwageri     
BIN    bintangor laut   Calophyllum     inophyllum  
BLK    belabak          Shorea          atrinervosa 
BON    Bongang          Nesia           NA          
BUN    bunut            Calophyllum     NA          


## data manipulation


```r
# modify treatment codes for plotting -- and modelling? 

# change name of the "plot" object, so that changes can be compared
plot_original <- plot
# Create one factor for roundup and cardboard combination. represent treatment
# with a capital letter, and control with lowercase. use R for roundup and C for
# control
plot$treatPre[plot$roundup == 0 & plot$cardboard == 0] <- "rc"
plot$treatPre[plot$roundup == 1 & plot$cardboard == 0] <- "Rc"
plot$treatPre[plot$roundup == 1 & plot$cardboard == 1] <- "RC"
plot$treatPre[plot$roundup == 0 & plot$cardboard == 1] <- "rC"
plot$treatPre <- as.factor(plot$treatPre)

# convert 1 and 0 into "yes" and "no" for four different categorical variables
for (i in c("pressing", "roundup", "weeding", "cardboard")) {
  plot[plot[,i] == 1,i] <- "yes"
  plot[plot[,i] == 0,i] <- "no"
}

unique(plot_original$fertilizer)
```

```
## [1] "NPK"   "KON"   "ORG"   "NONE"  "MULCH" "SEKAM"
```

```r
# There are five levels to the `fertilizer` factor: "NPK", "KON", "ORG", "NONE",
# "MULCH", "SEKAM". Two of these levels are synonymous (i think?) "NONE" and
# "KON" (perhaps for Kontrol).
plot$any.fertilizer[plot$fertilizer =="ORG" | plot$fertilizer =="NPK"] <- "yes"
plot$any.fertilizer[plot$fertilizer == "KON" | plot$fertilizer == "NONE"] <- "no"
plot$any.fertilizer <- as.factor(plot$any.fertilizer)
plot[plot[,"fertilizer"] == "KON","fertilizer"] <- "None"
plot[plot[,"fertilizer"] == "ORG","fertilizer"] <- "Compost"
```


```r
summary(plot)
```

```
##        id          plotCode           pressing           roundup         
##  Min.   :  1.0   Length:422         Length:422         Length:422        
##  1st Qu.:106.2   Class :character   Class :character   Class :character  
##  Median :211.5   Mode  :character   Mode  :character   Mode  :character  
##  Mean   :211.5                                                           
##  3rd Qu.:316.8                                                           
##  Max.   :422.0                                                           
##                                                                          
##    weeding           cardboard          fertilizer       
##  Length:422         Length:422         Length:422        
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##  description           control             edge          setupYear   
##  Length:422         Min.   :0.00000   Min.   :0.0000   Min.   :2009  
##  Class :character   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:2010  
##  Mode  :character   Median :0.00000   Median :0.0000   Median :2010  
##                     Mean   :0.02143   Mean   :0.2258   Mean   :2010  
##                     3rd Qu.:0.00000   3rd Qu.:0.0000   3rd Qu.:2011  
##                     Max.   :1.00000   Max.   :1.0000   Max.   :2011  
##                     NA's   :2         NA's   :329                    
##     pcFast             pcFastTx      plusPoles        plusPolesTx 
##  Length:422         Min.   : 9.0   Min.   :0.00000   Min.   :13   
##  Class :character   1st Qu.:13.0   1st Qu.:0.00000   1st Qu.:13   
##  Mode  :character   Median :13.0   Median :0.00000   Median :25   
##                     Mean   :12.2   Mean   :0.01422   Mean   :25   
##                     3rd Qu.:13.0   3rd Qu.:0.00000   3rd Qu.:37   
##                     Max.   :13.0   Max.   :1.00000   Max.   :37   
##                     NA's   :412                      NA's   :416  
##    polesOnly        polesOnlyTx      notes           earthAddition     
##  Min.   :0.00000   Min.   :13.0   Length:422         Min.   :0.000000  
##  1st Qu.:0.00000   1st Qu.:13.0   Class :character   1st Qu.:0.000000  
##  Median :0.00000   Median :13.0   Mode  :character   Median :0.000000  
##  Mean   :0.07109   Mean   :15.4                      Mean   :0.009479  
##  3rd Qu.:0.00000   3rd Qu.:13.0                      3rd Qu.:0.000000  
##  Max.   :1.00000   Max.   :37.0                      Max.   :1.000000  
##                    NA's   :392                                         
##       nsdl        underAcacia         diptMyco      treatPre
##  Min.   :  0.0   Min.   :0.00000   Min.   :0.0000   rc: 32  
##  1st Qu.:100.0   1st Qu.:0.00000   1st Qu.:1.0000   rC: 40  
##  Median :160.0   Median :0.00000   Median :1.0000   Rc:164  
##  Mean   :138.5   Mean   :0.01185   Mean   :0.8699   RC:186  
##  3rd Qu.:177.0   3rd Qu.:0.00000   3rd Qu.:1.0000           
##  Max.   :177.0   Max.   :1.00000   Max.   :1.0000           
##                                    NA's   :276              
##  any.fertilizer
##  no  :317      
##  yes : 95      
##  NA's: 10      
##                
##                
##                
## 
```


```r
# ------------ function, ordered box plot
ordBP <- function(df, myfactor, title) {
  bp <- boxplot(df$htgr ~ df[,myfactor], plot=F)
  bpord <- order(bp$stats[3,])
  bp$stats <- bp$stats[,bpord]
  bp$names <- bp$name[bpord]
  bp$conf <- bp$conf[,bpord]
  bp$n <- bp$n[bpord]
  bxp(bp, outline=F, varwidth=T, notch=T, main=title,
      ylab="Relative growth rate", las=2)
}
#------------------------------------------

# -------------- function, ordered spine plot
ordSP <- function(df, myfactor, title) {
  z <- table(as.factor(df[,myfactor]), as.factor(df$died))
  z <- z[order(z[,2]/z[,1]),]
  z <- z[,c(2,1)]
  par(las=2)
  spineplot(z, main=title, ylab="Survival")
}
```

## Analysis of seedling survival and growth in ASRI restoration site


```r
for (year in 2009:2010) {
#year <- 2009

  if (year == 2009) { mon <- c(0,18) }
  if (year == 2010) { mon <- c(1,14) }
  
  cat("## Seedlings planted in", year, ", from month", mon[1],
      "to month", mon[2], "\n\n")
  
  ### ** beware! This is a multi GB memory hog!  The `drop=T` is not
  ###   working now and so there are NAs in the sdlID code which cause
  ###   the merge to match millions of NA to millions of NAs. Now using
  ###   subset
  
  ###   this will only match sdls with records in both years (all=F)
  
  #    a <- merge(all.x = T,
  #           meas[ meas$plantYear == year & meas$months == mon[1],  , drop=T],
  #           meas[ meas$plantYear == year & meas$months == mon[2],  , drop=T],
  #           by = "sdlID")[ , c("sdlID", "plotCode.x",
  #                              "height.x", "width.x", "dead.x",
  #                              "height.y", "width.y", "dead.y")]     
  
  a <- merge(all.x = T,
             subset(meas, plantYear == year & months == mon[1]),
             subset(meas, plantYear == year & months == mon[2]),
             by = "sdlID")[ , c("sdlID", "plotCode.x",
                                "height.x", "width.x", "dead.x",
                                "height.y", "width.y", "dead.y")]  
  
  
  # prepare
  a$died[a$dead.x == "yes"] <- "died"
  a$died[a$dead.x == "no" & a$dead.y == "yes"] <- "died"
  a$died[a$dead.x == "no" & is.na(a$dead.y)] <- "died" # not found
  a$died[a$dead.x == "no" & a$dead.y == "no"] <- "lived"
  
  
  a$died <- as.factor(a$died)
  a$htgr <- (a$height.y - a$height.x) / a$height.x
  a$htgr[is.nan(a$htgr)] <- NA
  a$htgr[is.infinite(a$htgr)] <- NA
  a <- merge(a, plot, by.x = "plotCode.x", by.y = "plotCode")
  a <- merge(a, sdltax, by.x = "sdlID", by.y = "id", all.x=T)
  
  # Lived and died:
  cat("### Overall seedling survival\n\n")
  #print(table(a$died))
  cat("Survival: **", (length(a$died[a$died=="lived"]) / length(a$died)) * 100,
      "%** out of **", length(a$died) , "** seedlings\n\n", sep="")
  
  # BY FACTOR
  
  for (myfactor in  c("species", "pressing", "roundup", "weeding",
                      "cardboard", "fertilizer", "any.fertilizer","nsdl")) {
    
    cat("\n\n### Survival and growth vs", myfactor, "\n\n")
    
    # can't use levels, as these seem to be inheritted through the merge
    if (length(table(a[, myfactor])[table(a[, myfactor])!=0]) == 1) {
      cat("**Only one level of ", myfactor, "**\n\n", sep="")
      next
    }
    
    cat("\n#### Survival\n\n")
    
    surv_chi <- chisq.test(as.factor(a$died), as.factor(a[,myfactor]))
    
    print(kable(broom::tidy(surv_chi)))
    
    
    cat("\n#### Growth\n\n")
    
    grow_aov <- anova(lm(a$htgr ~ as.factor(a[,myfactor])))
    
    print(kable(broom::tidy(grow_aov)))
    
    cat("\n\n")
    
    # png(paste(year,"_surv_", myfactor, ".png", sep=""),
    #     height = 400, width = (400 + length(levels(as.factor(a[,myfactor])))*20))
    ordSP(a, myfactor, paste("Effect of", myfactor, "on seedling survival,", year,
                             "plantings"))
    
    # png(paste(year,"_RGR_", myfactor, ".png", sep=""),
    #     height = 400, width = (400 + length(levels(as.factor(a[,myfactor])))*20))
    ordBP(a, myfactor, paste("Effect of", myfactor, "on seedling growth,", year,
                             "plantings"))
  }
  
  
}
```

## Seedlings planted in 2009 , from month 0 to month 18 

### Overall seedling survival

Survival: **79.45734%** out of **14005** seedlings



### Survival and growth vs species 


#### Survival

```
## Warning in chisq.test(as.factor(a$died), as.factor(a[, myfactor])): Chi-
## squared approximation may be incorrect
```

```
## Warning: Installed Rcpp (0.12.11.3) different from Rcpp used to build dplyr (0.12.11).
## Please reinstall dplyr to avoid random crashes or undefined behavior.
```



 statistic   p.value   parameter  method                     
----------  --------  ----------  ---------------------------
  1156.963         0          31  Pearson's Chi-squared test 

#### Growth



term                          df      sumsq       meansq   statistic   p.value
-------------------------  -----  ---------  -----------  ----------  --------
as.factor(a[, myfactor])      31   19561.04   631.001272    170.3113         0
Residuals                   8562   31722.10     3.704988          NA        NA


![](analysis_output_files/figure-html/responses-1.png)<!-- -->

```
## Warning in bxp(bp, outline = F, varwidth = T, notch = T, main = title, ylab
## = "Relative growth rate", : some notches went outside hinges ('box'): maybe
## set notch=FALSE
```

![](analysis_output_files/figure-html/responses-2.png)<!-- -->

### Survival and growth vs pressing 


#### Survival



 statistic    p.value   parameter  method                                                       
----------  ---------  ----------  -------------------------------------------------------------
 0.2732739   0.601144           1  Pearson's Chi-squared test with Yates' continuity correction 

#### Growth



term                           df        sumsq       meansq   statistic   p.value
-------------------------  ------  -----------  -----------  ----------  --------
as.factor(a[, myfactor])        1     547.6771   547.677123    95.77967         0
Residuals                   11194   64008.3384     5.718094          NA        NA


![](analysis_output_files/figure-html/responses-3.png)<!-- -->![](analysis_output_files/figure-html/responses-4.png)<!-- -->

### Survival and growth vs roundup 


#### Survival



 statistic   p.value   parameter  method                                                       
----------  --------  ----------  -------------------------------------------------------------
  59.56125         0           1  Pearson's Chi-squared test with Yates' continuity correction 

#### Growth



term                           df       sumsq        meansq   statistic   p.value
-------------------------  ------  ----------  ------------  ----------  --------
as.factor(a[, myfactor])        1    1163.881   1163.881267    205.5221         0
Residuals                   11194   63392.134      5.663046          NA        NA


![](analysis_output_files/figure-html/responses-5.png)<!-- -->![](analysis_output_files/figure-html/responses-6.png)<!-- -->

### Survival and growth vs weeding 


#### Survival



 statistic     p.value   parameter  method                                                       
----------  ----------  ----------  -------------------------------------------------------------
   8.45415   0.0036421           1  Pearson's Chi-squared test with Yates' continuity correction 

#### Growth



term                           df        sumsq       meansq   statistic   p.value
-------------------------  ------  -----------  -----------  ----------  --------
as.factor(a[, myfactor])        1     240.8808   240.880842    41.92513         0
Residuals                   11194   64315.1347     5.745501          NA        NA


![](analysis_output_files/figure-html/responses-7.png)<!-- -->![](analysis_output_files/figure-html/responses-8.png)<!-- -->

### Survival and growth vs cardboard 


#### Survival



 statistic   p.value   parameter  method                                                       
----------  --------  ----------  -------------------------------------------------------------
  27.27784     2e-07           1  Pearson's Chi-squared test with Yates' continuity correction 

#### Growth



term                           df          sumsq      meansq   statistic     p.value
-------------------------  ------  -------------  ----------  ----------  ----------
as.factor(a[, myfactor])        1   7.415061e-01   0.7415061   0.1285785   0.7199173
Residuals                   11194   6.455527e+04   5.7669532          NA          NA


![](analysis_output_files/figure-html/responses-9.png)<!-- -->![](analysis_output_files/figure-html/responses-10.png)<!-- -->

### Survival and growth vs fertilizer 


#### Survival



 statistic     p.value   parameter  method                     
----------  ----------  ----------  ---------------------------
  10.39568   0.0055285           2  Pearson's Chi-squared test 

#### Growth



term                           df         sumsq      meansq   statistic     p.value
-------------------------  ------  ------------  ----------  ----------  ----------
as.factor(a[, myfactor])        2      27.23826   13.619128     2.36234   0.0942465
Residuals                   11193   64528.77730    5.765101          NA          NA


![](analysis_output_files/figure-html/responses-11.png)<!-- -->![](analysis_output_files/figure-html/responses-12.png)<!-- -->

### Survival and growth vs any.fertilizer 


#### Survival



 statistic     p.value   parameter  method                                                       
----------  ----------  ----------  -------------------------------------------------------------
  9.369857   0.0022058           1  Pearson's Chi-squared test with Yates' continuity correction 

#### Growth



term                           df         sumsq      meansq   statistic     p.value
-------------------------  ------  ------------  ----------  ----------  ----------
as.factor(a[, myfactor])        1      14.24837   14.248370     2.47121   0.1159769
Residuals                   11194   64541.76719    5.765747          NA          NA


![](analysis_output_files/figure-html/responses-13.png)<!-- -->

### Survival and growth vs nsdl 

**Only one level of nsdl**

## Seedlings planted in 2010 , from month 1 to month 14 

### Overall seedling survival

Survival: **79.45595%** out of **7389** seedlings



### Survival and growth vs species 


#### Survival

```
## Warning in chisq.test(as.factor(a$died), as.factor(a[, myfactor])): Chi-
## squared approximation may be incorrect
```

![](analysis_output_files/figure-html/responses-14.png)<!-- -->

 statistic   p.value   parameter  method                     
----------  --------  ----------  ---------------------------
  491.7287         0          22  Pearson's Chi-squared test 

#### Growth



term                          df      sumsq       meansq   statistic   p.value
-------------------------  -----  ---------  -----------  ----------  --------
as.factor(a[, myfactor])      22    6979.74   317.260888    126.5424         0
Residuals                   6117   15336.25     2.507151          NA        NA


![](analysis_output_files/figure-html/responses-15.png)<!-- -->![](analysis_output_files/figure-html/responses-16.png)<!-- -->

### Survival and growth vs pressing 

**Only one level of pressing**



### Survival and growth vs roundup 

**Only one level of roundup**



### Survival and growth vs weeding 


#### Survival



 statistic     p.value   parameter  method                                                       
----------  ----------  ----------  -------------------------------------------------------------
 0.0118199   0.9134249           1  Pearson's Chi-squared test with Yates' continuity correction 

#### Growth



term                          df          sumsq      meansq   statistic     p.value
-------------------------  -----  -------------  ----------  ----------  ----------
as.factor(a[, myfactor])       1   1.398491e-01   0.1398491   0.0414983   0.8385849
Residuals                   7101   2.393031e+04   3.3699918          NA          NA


![](analysis_output_files/figure-html/responses-17.png)<!-- -->![](analysis_output_files/figure-html/responses-18.png)<!-- -->

### Survival and growth vs cardboard 

**Only one level of cardboard**



### Survival and growth vs fertilizer 

**Only one level of fertilizer**



### Survival and growth vs any.fertilizer 

**Only one level of any.fertilizer**



### Survival and growth vs nsdl 


#### Survival



 statistic   p.value   parameter  method                     
----------  --------  ----------  ---------------------------
  23.63675   7.4e-06           2  Pearson's Chi-squared test 

#### Growth



term                          df        sumsq      meansq   statistic     p.value
-------------------------  -----  -----------  ----------  ----------  ----------
as.factor(a[, myfactor])       2      44.3923   22.196150    6.597684   0.0013719
Residuals                   7100   23886.0596    3.364234          NA          NA


![](analysis_output_files/figure-html/responses-19.png)<!-- -->![](analysis_output_files/figure-html/responses-20.png)<!-- -->

## Species list


```r
kable(tax)
```



code   localname           gen              sp             
-----  ------------------  ---------------  ---------------
BBU    belabuk             NA               NA             
BEL    belian              Eusideroxylon    zwageri        
BIN    bintangor laut      Calophyllum      inophyllum     
BLK    belabak             Shorea           atrinervosa    
BON    Bongang             Nesia            NA             
BUN    bunut               Calophyllum      NA             
CEM    cempedak            Artocarpus       integer        
CIN    kayu cina           Podocarpus       neriifolius    
DUK    duku                Lansium          domesticum     
DUR    durian              Durio            zibethinus     
GAH    gaharu              Aquilaria        malaccensis    
GLI    NA                  Gliricidia       sepium         
HOP    resak 1             Hopea            NA             
JBJ    jambu biji          NA               NA             
JBL    jambu bol           Syzygium         NA             
JEN    jengkol             Archeodendron    jiringa        
JER    jeruk               Citrus           NA             
JUN    Jungkang            Palaquium        NA             
KAY    kayu batu           NA               NA             
KEC    kecupu              Dipterocarpus    tempehes       
KEM    kemayau             Dacryodes        NA             
KIJ    pau kijang          Irvingia         malayana       
KLJ    kelanjau            Pentaspedon      motleyi        
KLP    kelampai            Pimelodendron    NA             
KOP    kopi                NA               NA             
KTP    ketapang            Terminalia       cattapa        
LAB    leban               Vitex            pinnata        
MAC    mahang 1            Macaranga        NA             
MAJ    majau               Shorea           pauciflora     
MAN    mangga              Mangifera        indica         
MEB    merbau              Intsia           palembanica    
MED    medang 1            NA               NA             
MER    meranti merah 2     Shorea           NA             
MET    meranti merah 1     Shorea           parvistipulata 
MEU    Meranti umang       NA               NA             
MEX    'meranti extreme'   Shorea           leprasula      
NAN    nangka              Artocarpus       heterophyllus  
NYA    nyatoh 1            NA               NA             
PAK    Paket               NA               NA             
PEK    pekawai burung      Palaquium        NA             
PET    petai               Parkia           speciosa       
PST    NA                  Parastemon       urophyllus     
PUL    pulai               Alstonia         NA             
PUT    putat               Planchonia       NA             
RAM    rambutan            Nephelium        lappaceum      
RMB    rambai              Baccaurea        motleyana      
RNG    rengas              NA               NA             
SEM    semangkok           Scaphium         NA             
SEN    sengon              Paraserianthes   falcataria     
SIR    sirsak              NA               NA             
SND    senderiung          Trema            NA             
SUK    sukun               Artocarpus       incisus        
SUN    sungkai             Peronema         canescens      
TEN    tengkawang          Shorea           NA             
UBA    ubah 1              Syzygium         NA             
VER    NA                  Vernonia         NA             

