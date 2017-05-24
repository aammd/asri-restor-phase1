
source("secrets.R")

if (1) {
  # connect to DB
  library(RMySQL)
  con <- dbConnect(MySQL(), user="cwebb", password=password,
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

  # head(meas)

  # get plot and sdl data
  plot <- dbGetQuery(con, "SELECT * FROM plot WHERE id < 423")
  sdltax <- dbGetQuery(con, "SELECT sdl.id, taxon.code as species, taxon.local
                             FROM sdl, taxon WHERE sdl.taxonID = taxon.id")
  tax <- dbGetQuery(con, "SELECT code, local as localname, gen, sp from taxon
                          ORDER BY code")
  
  dbDisconnect(con)
  
# Add treatment codes, e.g...:
plot$treatPre[plot$roundup == 0 & plot$cardboard == 0] <- "rc"
plot$treatPre[plot$roundup == 1 & plot$cardboard == 0] <- "Rc"
plot$treatPre[plot$roundup == 1 & plot$cardboard == 1] <- "RC"
plot$treatPre[plot$roundup == 0 & plot$cardboard == 1] <- "rC"
plot$treatPre <- as.factor(plot$treatPre)

for (i in c("pressing", "roundup", "weeding", "cardboard")) {
  plot[plot[,i] == 1,i] <- "yes"
  plot[plot[,i] == 0,i] <- "no"
}

plot$any.fertilizer[plot$fertilizer =="ORG" | plot$fertilizer =="NPK"] <- "yes"
plot$any.fertilizer[plot$fertilizer == "KON" | plot$fertilizer == "NONE"] <- "no"
plot$any.fertilizer <- as.factor(plot$any.fertilizer)
plot[plot[,"fertilizer"] == "KON","fertilizer"] <- "None"
plot[plot[,"fertilizer"] == "ORG","fertilizer"] <- "Compost"

}



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

# Begin output (note - must have echo=F if called from source) 
sink(file="anal.md", append=F)
cat("% ASRI restoration\n")
### cat("css: anal.css\n\n")
cat("# Analysis of seedling survival and growth in ASRI restoration site\n\n")
cat("----\n\n")
sink()

# for each year:

for (year in 2009:2010) {
#year <- 2009

if (year == 2009) { mon <- c(0,18) }
if (year == 2010) { mon <- c(1,14) }

sink(file="anal.md", append=T)
cat("## Seedlings planted in", year, ", from month", mon[1],
    "to month", mon[2], "\n\n")
sink()

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
sink(file="anal.md", append=T)
cat("### Overall seedling survival\n\n")
#print(table(a$died))
cat("Survival: **", (length(a$died[a$died=="lived"]) / length(a$died)) * 100,
    "%** out of **", length(a$died) , "** seedlings\n\n", sep="")
sink()


# BY FACTOR

for (myfactor in
     c("species", "pressing", "roundup", "weeding", "cardboard", "fertilizer", "any.fertilizer","nsdl")) {
  sink(file="anal.md", append=T)
  cat("### Survival and growth vs.", myfactor, "\n\n")
  # can't use levels, as these seem to be inheritted through the merge
  if (length(table(a[, myfactor])[table(a[, myfactor])!=0]) == 1) {
    cat("**Only one level of ", myfactor, "**\n\n", sep="")
    sink()
    next
  }

  cat("#### Survival\n\n")
  cat("![](", paste(year, "_surv_", myfactor, ".png", sep=""), ")\n\n", sep="")
  cat("<pre>\n")
  print(chisq.test(as.factor(a$died), as.factor(a[,myfactor])))
  cat("</pre>\n\n")
  cat("#### Growth\n\n")
  cat("![](", paste(year, "_RGR_", myfactor, ".png", sep=""), ")\n\n", sep="")
  cat("<pre>\n");
  print(anova(lm(a$htgr ~ as.factor(a[,myfactor]))))
  cat("</pre>\n\n")
  sink()

  png(paste(year,"_surv_", myfactor, ".png", sep=""),
      height = 400, width = (400 + length(levels(as.factor(a[,myfactor])))*20))
  ordSP(a, myfactor, paste("Effect of", myfactor, "on seedling survival,", year,
                           "plantings"))
  dev.off()
      
  png(paste(year,"_RGR_", myfactor, ".png", sep=""),
      height = 400, width = (400 + length(levels(as.factor(a[,myfactor])))*20))
  ordBP(a, myfactor, paste("Effect of", myfactor, "on seedling growth,", year,
                           "plantings"))
  dev.off()
}

sink(file="anal.md", append=T)
cat("----\n\n")
sink()

}

sink(file="anal.md", append=T)
cat("## Species list\n\n<pre>\n")
print(tax)
cat("</pre>\n\n")
sink()

# because multimarkdown has a bug when there are lots of tags:
system("gawk '{if ($0 ~ /<\\/pre>/) v = 0; else if(v) print \"      \" $0; else if ($0 ~ /<pre>/) v=1; else print $0}' anal.md > anal.tmp; mv -f anal.tmp anal.md")


# stop()

# ################### NOTES

# just the common ones:
# common <- data.frame(table(a$code))[order(data.frame(table(a$code))[,2],
#          decreasing =T),]
# common <- as.vector(common[1:10,1])
# b <- a
# b <- b[!is.na(match(b$code, common)), ,drop=T]
# png(paste(year, "_RGR_spp_common.png", sep=""), height=500, width=700)
# ordBP(b, paste("Relative Growth Rate of seedlings planted in", year, ", between months", mon[1], "and",mon[2]))
# dev.off()

  # subsetting:
  # remember drop=T, e.g.
  # a <- meas[ meas$plantYear == 2010, , drop=T]
  # check for factor removal: levels(a$plotCode)
