## Bat Standings Points Gained (spg) Calculation (Std. 5x5 Roto scoring and 
##  ESPN roster settings)

##Define Model Inputs!!!!!

#Projection Model to use
batspg <- duffyproj

##Replacement level (replvl) for each pos (if 10 1B in league then 11th is replace)
replvlC <- 11
replvl1B <- 28
replvl2B <- 16
replvl3B <- 19
replvlSS <- 16
replvlOF <- 66
#Number of teams in fantasy league
nteam <-10

# Historical spg vs stat slopes from 
#  http://razzball.com/stats-needed-to-win-your-fantasy-baseball-league/
HRspgslope <- 10.4
Rspgslope <- 24.6
RBIspgslope <- 24.6
SBspgslope <- 9.4
AVGspgslope <- 0.0024


# Projected Average stats for projected drafted hitters (DH)

AVGavgDH <- 0.271
ABavgDH <- 527.2


#  Total number of players drafted, and per team
nhittot <- (replvlC-1)+(replvl1B-1)+(replvl2B-1)+(replvl3B-1)+(replvlSS-1)+(replvlOF-1)
nhit <- nhittot/nteam


# Stats needed to calculate rate stat spg , 
# stats of average team (Tm) with n-1 players (n1)
ABavgTm <- nhit * ABavgDH
HavgTm <- ABavgTm * AVGavgDH
HavgPl <- HavgTm / nhit
ABn1Tm <- (nhit-1) * ABavgDH
Hn1Tm <- (nhit-1) * HavgPl

# Counting stat spg
batspg$HRspg <- batspg$HR / HRspgslope
batspg$Rspg <- batspg$R / Rspgslope
batspg$RBIspg <- batspg$RBI / RBIspgslope
batspg$SBspg <- batspg$SB / SBspgslope

# Rate stat (AVG) spg
batspg$AVGspg <- ((batspg$H+Hn1Tm)/(batspg$AB+ABn1Tm)-AVGavgDH)/AVGspgslope

# Total spg
batspg$TOTspg <- with(batspg, HRspg +Rspg+RBIspg+SBspg+AVGspg)

#Define RepPos 
batspg$RepPos <- batspg$Pos

# Standardizing pos to replacement positions (RepPos) defined above
batspg$RepPos <- gsub("LF", "OF", batspg$RepPos)
batspg$RepPos <- gsub("CF", "OF", batspg$RepPos)
batspg$RepPos <- gsub("RF", "OF", batspg$RepPos)
batspg$RepPos <- gsub("DH", "1B", batspg$RepPos)


# decreasing order before splitting by replacement position
batspg <- batspg[ order(batspg$TOTspg, decreasing=T),]

require(stringr)
# Players split by position to define replacement level
spgC <- subset(batspg, str_detect(batspg$Pos, "F")==FALSE)
spgC <- subset(spgC, str_detect(spgC$Pos, "C"))
spgC <- spgC[order(spgC$TOTspg, decreasing=TRUE), c("nameFirst","nameLast","Pos",
                                                    "HRspg","Rspg","RBIspg","SBspg",
                                                    "AVGspg","TOTspg")]

spg1B <-subset(batspg, str_detect(batspg$RepPos, "1B"))[ ,c("nameFirst","nameLast","Pos",
                                                            "HRspg","Rspg","RBIspg","SBspg",
                                                            "AVGspg","TOTspg")]
spg2B <-subset(batspg, str_detect(batspg$RepPos, "2B"))[ ,c("nameFirst","nameLast","Pos",
                                                            "HRspg","Rspg","RBIspg","SBspg",
                                                            "AVGspg","TOTspg")]
spg3B <-subset(batspg, str_detect(batspg$RepPos, "3B"))[ ,c("nameFirst","nameLast","Pos",
                                                            "HRspg","Rspg","RBIspg","SBspg",
                                                            "AVGspg","TOTspg")]
spgSS <-subset(batspg, str_detect(batspg$RepPos, "SS"))[ ,c("nameFirst","nameLast","Pos",
                                                            "HRspg","Rspg","RBIspg","SBspg",
                                                            "AVGspg","TOTspg")]
spgOF <-subset(batspg, str_detect(batspg$RepPos, "F"))[ ,c("nameFirst","nameLast","Pos",
                                                           "HRspg","Rspg","RBIspg","SBspg",
                                                           "AVGspg","TOTspg")]


# Defining replacement level for each stat category
# as the average of the replvl-2 to the replvl+2 players category spg
repCspg <- c( mean(spgC$HRspg[(replvlC-2):(replvlC+2)]),
              mean(spgC$Rspg[(replvlC-2):(replvlC+2)]),
              mean(spgC$RBIspg[(replvlC-2):(replvlC+2)]),
              mean(spgC$SBspg[(replvlC-2):(replvlC+2)]),
              mean(spgC$AVGspg[(replvlC-2):(replvlC+2)]),
              mean(spgC$TOTspg[(replvlC-2):(replvlC+2)]))
rep1Bspg <- c( mean(spg1B$HRspg[(replvl1B-2):(replvl1B+2)]),
               mean(spg1B$Rspg[(replvl1B-2):(replvl1B+2)]),
               mean(spg1B$RBIspg[(replvl1B-2):(replvl1B+2)]),
               mean(spg1B$SBspg[(replvl1B-2):(replvl1B+2)]),
               mean(spg1B$AVGspg[(replvl1B-2):(replvl1B+2)]),
               mean(spg1B$TOTspg[(replvl1B-2):(replvl1B+2)]))
rep2Bspg <- c( mean(spg2B$HRspg[(replvl2B-2):(replvl2B+2)]),
               mean(spg2B$Rspg[(replvl2B-2):(replvl2B+2)]),
               mean(spg2B$RBIspg[(replvl2B-2):(replvl2B+2)]),
               mean(spg2B$SBspg[(replvl2B-2):(replvl2B+2)]),
               mean(spg2B$AVGspg[(replvl2B-2):(replvl2B+2)]),
               mean(spg2B$TOTspg[(replvl2B-2):(replvl2B+2)]))
rep3Bspg <- c( mean(spg3B$HRspg[(replvl3B-2):(replvl3B+2)]),
               mean(spg3B$Rspg[(replvl3B-2):(replvl3B+2)]),
               mean(spg3B$RBIspg[(replvl3B-2):(replvl3B+2)]),
               mean(spg3B$SBspg[(replvl3B-2):(replvl3B+2)]),
               mean(spg3B$AVGspg[(replvl3B-2):(replvl3B+2)]),
               mean(spg3B$TOTspg[(replvl3B-2):(replvl3B+2)]))
repSSspg <- c( mean(spgSS$HRspg[(replvlSS-2):(replvlSS+2)]),
               mean(spgSS$Rspg[(replvlSS-2):(replvlSS+2)]),
               mean(spgSS$RBIspg[(replvlSS-2):(replvlSS+2)]),
               mean(spgSS$SBspg[(replvlSS-2):(replvlSS+2)]),
               mean(spgSS$AVGspg[(replvlSS-2):(replvlSS+2)]),
               mean(spgSS$TOTspg[(replvlSS-2):(replvlSS+2)]))
repOFspg <- c( mean(spgOF$HRspg[(replvlOF-2):(replvlOF+2)]),
               mean(spgOF$Rspg[(replvlOF-2):(replvlOF+2)]),
               mean(spgOF$RBIspg[(replvlOF-2):(replvlOF+2)]),
               mean(spgOF$SBspg[(replvlOF-2):(replvlOF+2)]),
               mean(spgOF$AVGspg[(replvlOF-2):(replvlOF+2)]),
               mean(spgOF$TOTspg[(replvlOF-2):(replvlOF+2)]))

# Making data frame of rep lvl spg 
replvlspg <- rbind(t(as.data.frame(repCspg)), t(as.data.frame(rep1Bspg)), 
                   t(as.data.frame(rep2Bspg)), t(as.data.frame(rep3Bspg)), 
                   t(as.data.frame(repSSspg)), t(as.data.frame(repOFspg)))
posdf <- as.data.frame(c("C","1B","2B","3B","SS","OF"))
replvlspg <- cbind(posdf, replvlspg)
names(replvlspg) <- c("RepPos","RepHRspg","RepRspg","RepRBIspg","RepSBspg","RepAVGspg","RepTOTspg")

# Defining rep lvl for players with NA positions
repNAspg <- c("NA", mean(replvlspg$RepHRspg),mean(replvlspg$RepRspg),
              mean(replvlspg$RepRBIspg),mean(replvlspg$RepSBspg),
              mean(replvlspg$RepAVGspg),mean(replvlspg$RepTOTspg))
repNAspgdf <-as.data.frame(t(as.data.frame(repNAspg)))
names(repNAspgdf) <- c("RepPos","RepHRspg","RepRspg","RepRBIspg","RepSBspg","RepAVGspg","RepTOTspg")
replvlspg <- rbind(replvlspg, repNAspgdf)

# Replvl category spg to numeric
replvlspg$RepHRspg <- as.numeric(replvlspg$RepHRspg)
replvlspg$RepRspg <- as.numeric(replvlspg$RepRspg)
replvlspg$RepRBIspg <- as.numeric(replvlspg$RepRBIspg)
replvlspg$RepSBspg <- as.numeric(replvlspg$RepSBspg)
replvlspg$RepAVGspg <- as.numeric(replvlspg$RepAVGspg)

# To recognize NA as a string
for(i in 1:nrow(batspg)){
  if(is.na(batspg$RepPos[i])==T) batspg$RepPos[i] <- "NA"
}
replvlspg$RepPos <- as.character(replvlspg$RepPos)

# Adding on rep lvl columns
batspg <- merge(batspg, replvlspg, by="RepPos", all.x=TRUE)

# Category spg above replacement (AR)
batspg$HRspgAR <- batspg$HRspg - batspg$RepHRspg
batspg$RspgAR <- batspg$Rspg - batspg$RepRspg
batspg$RBIspgAR <- batspg$RBIspg - batspg$RepRBIspg
batspg$SBspgAR <- batspg$SBspg - batspg$RepSBspg
batspg$AVGspgAR <- batspg$AVGspg - batspg$RepAVGspg

# Total spg above replacement
batspg$TOTspgAR <- batspg$HRspgAR + batspg$RspgAR+ batspg$RBIspgAR+batspg$SBspgAR+batspg$AVGspgAR

# Order in decreasing
batspg <- batspg[ order(batspg$TOTspgAR, decreasing=TRUE),]

# Filter to important info
batspg <- batspg[, c("nameLast","nameFirst","teamID","Pos","TOTspgAR")]

