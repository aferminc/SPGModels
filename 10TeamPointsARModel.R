## Bat Fantasy Points above replacement Calculation (Std. Points league scoring and 
##  ESPN roster settings)

##Define Model Inputs!!!!!

#Projection Model to use
batFPts <- duffyproj

##Replacement level (replvl) for each pos (if 10 1B in league then 11th is replace)
replvlC <- 11
replvl1B <- 28
replvl2B <- 16
replvl3B <- 19
replvlSS <- 16
replvlOF <- 66
#Number of teams in fantasy league
nteam <-10

# Points settings for each stat
TBpoints <- 1
Rpoints <- 1
SBpoints <- 1
BBpoints <-1
RBIpoints <- 1
Kpoints <- -1

#  Total number of players drafted, and per team
nhittot <- (replvlC-1)+(replvl1B-1)+(replvl2B-1)+(replvl3B-1)+(replvlSS-1)+(replvlOF-1)
nhit <- nhittot/nteam


# Calculating Fantasy points (FPts)
batFPts$TBFPts <- batFPts$TB * TBpoints
batFPts$RFPts <- batFPts$R * Rpoints
batFPts$SBFPts <- batFPts$SB * SBpoints
batFPts$BBFPts <- batFPts$BB * BBpoints
batFPts$RBIFPts <- batFPts$RBI * RBIpoints
batFPts$KFPts <- batFPts$K * Kpoints

# Total FPts
batFPts$TOTFPts <- with(batFPts, TBFPts+RFPts+SBFPts+BBFPts+RBIFPts+KFPts)

#Define RepPos 
batFPts$RepPos <- batFPts$Pos

# Standardizing pos to replacement positions (RepPos) defined above
batFPts$RepPos <- gsub("LF", "OF", batFPts$RepPos)
batFPts$RepPos <- gsub("CF", "OF", batFPts$RepPos)
batFPts$RepPos <- gsub("RF", "OF", batFPts$RepPos)
batFPts$RepPos <- gsub("DH", "1B", batFPts$RepPos)


# decreasing order before splitting by replacement position
batFPts <- batFPts[ order(batFPts$TOTFPts, decreasing=T),]

require(stringr)
# Players split by position to define replacement level
FPtsC <- subset(batFPts, str_detect(batFPts$Pos, "F")==FALSE)
FPtsC <- subset(FPtsC, str_detect(FPtsC$Pos, "C"))
FPtsC <- FPtsC[order(FPtsC$TOTFPts, decreasing=TRUE), c("nameFirst","nameLast","Pos",
                                                    "TBFPts","RFPts","RBIFPts","SBFPts",
                                                    "BBFPts","KFPts","TOTFPts")]

FPts1B <-subset(batFPts, str_detect(batFPts$RepPos, "1B"))[ ,c("nameFirst","nameLast","Pos",
                                                            "TBFPts","RFPts","RBIFPts","SBFPts",
                                                            "BBFPts","KFPts","TOTFPts")]
FPts2B <-subset(batFPts, str_detect(batFPts$RepPos, "2B"))[ ,c("nameFirst","nameLast","Pos",
                                                            "TBFPts","RFPts","RBIFPts","SBFPts",
                                                            "BBFPts","KFPts","TOTFPts")]
FPts3B <-subset(batFPts, str_detect(batFPts$RepPos, "3B"))[ ,c("nameFirst","nameLast","Pos",
                                                            "TBFPts","RFPts","RBIFPts","SBFPts",
                                                            "BBFPts","KFPts","TOTFPts")]
FPtsSS <-subset(batFPts, str_detect(batFPts$RepPos, "SS"))[ ,c("nameFirst","nameLast","Pos",
                                                            "TBFPts","RFPts","RBIFPts","SBFPts",
                                                            "BBFPts","KFPts","TOTFPts")]
FPtsOF <-subset(batFPts, str_detect(batFPts$RepPos, "F"))[ ,c("nameFirst","nameLast","Pos",
                                                           "TBFPts","RFPts","RBIFPts","SBFPts",
                                                           "BBFPts","KFPts","TOTFPts")]


# Defining replacement level for each stat category
# as the average of the replvl-2 to the replvl+2 players category FPts
repCFPts <- c( mean(FPtsC$TBFPts[(replvlC-2):(replvlC+2)]),
              mean(FPtsC$RFPts[(replvlC-2):(replvlC+2)]),
              mean(FPtsC$SBFPts[(replvlC-2):(replvlC+2)]),
              mean(FPtsC$BBFPts[(replvlC-2):(replvlC+2)]),
              mean(FPtsC$RBIFPts[(replvlC-2):(replvlC+2)]),
              mean(FPtsC$KFPts[(replvlC-2):(replvlC+2)]),
              mean(FPtsC$TOTFPts[(replvlC-2):(replvlC+2)]))
rep1BFPts <- c( mean(FPts1B$TBFPts[(replvl1B-2):(replvl1B+2)]),
               mean(FPts1B$RFPts[(replvl1B-2):(replvl1B+2)]),
               mean(FPts1B$SBFPts[(replvl1B-2):(replvl1B+2)]),
               mean(FPts1B$BBFPts[(replvl1B-2):(replvl1B+2)]),
               mean(FPts1B$RBIFPts[(replvl1B-2):(replvl1B+2)]),
               mean(FPts1B$KFPts[(replvl1B-2):(replvl1B+2)]),
               mean(FPts1B$TOTFPts[(replvl1B-2):(replvl1B+2)]))
rep2BFPts <- c( mean(FPts2B$TBFPts[(replvl2B-2):(replvl2B+2)]),
                mean(FPts2B$RFPts[(replvl2B-2):(replvl2B+2)]),
                mean(FPts2B$SBFPts[(replvl2B-2):(replvl2B+2)]),
                mean(FPts2B$BBFPts[(replvl2B-2):(replvl2B+2)]),
                mean(FPts2B$RBIFPts[(replvl2B-2):(replvl2B+2)]),
                mean(FPts2B$KFPts[(replvl2B-2):(replvl2B+2)]),
                mean(FPts2B$TOTFPts[(replvl2B-2):(replvl2B+2)]))
rep3BFPts <- c( mean(FPts3B$TBFPts[(replvl3B-2):(replvl3B+2)]),
                mean(FPts3B$RFPts[(replvl3B-2):(replvl3B+2)]),
                mean(FPts3B$SBFPts[(replvl3B-2):(replvl3B+2)]),
                mean(FPts3B$BBFPts[(replvl3B-2):(replvl3B+2)]),
                mean(FPts3B$RBIFPts[(replvl3B-2):(replvl3B+2)]),
                mean(FPts3B$KFPts[(replvl3B-2):(replvl3B+2)]),
                mean(FPts3B$TOTFPts[(replvl3B-2):(replvl3B+2)]))
repSSFPts <- c( mean(FPtsSS$TBFPts[(replvlSS-2):(replvlSS+2)]),
                mean(FPtsSS$RFPts[(replvlSS-2):(replvlSS+2)]),
                mean(FPtsSS$SBFPts[(replvlSS-2):(replvlSS+2)]),
                mean(FPtsSS$BBFPts[(replvlSS-2):(replvlSS+2)]),
                mean(FPtsSS$RBIFPts[(replvlSS-2):(replvlSS+2)]),
                mean(FPtsSS$KFPts[(replvlSS-2):(replvlSS+2)]),
                mean(FPtsSS$TOTFPts[(replvlSS-2):(replvlSS+2)]))
repOFFPts <- c( mean(FPtsOF$TBFPts[(replvlOF-2):(replvlOF+2)]),
                mean(FPtsOF$RFPts[(replvlOF-2):(replvlOF+2)]),
                mean(FPtsOF$SBFPts[(replvlOF-2):(replvlOF+2)]),
                mean(FPtsOF$BBFPts[(replvlOF-2):(replvlOF+2)]),
                mean(FPtsOF$RBIFPts[(replvlOF-2):(replvlOF+2)]),
                mean(FPtsOF$KFPts[(replvlOF-2):(replvlOF+2)]),
                mean(FPtsOF$TOTFPts[(replvlOF-2):(replvlOF+2)]))

# Making data frame of rep lvl FPts 
replvlFPts <- rbind(t(as.data.frame(repCFPts)), t(as.data.frame(rep1BFPts)), 
                   t(as.data.frame(rep2BFPts)), t(as.data.frame(rep3BFPts)), 
                   t(as.data.frame(repSSFPts)), t(as.data.frame(repOFFPts)))
posdf <- as.data.frame(c("C","1B","2B","3B","SS","OF"))
replvlFPts <- cbind(posdf, replvlFPts)
names(replvlFPts) <- c("RepPos","RepTBFPts","RepRFPts","RepSBFPts","RepBBFPts",
                       "RepRBIFPts","RepKFPts", "RepTOTFPts")

# Defining rep lvl for players with NA positions
repNAFPts <- c("NA", mean(replvlFPts$RepTBFPts),mean(replvlFPts$RepRFPts),
              mean(replvlFPts$RepSBFPts),mean(replvlFPts$RepBBFPts),
              mean(replvlFPts$RepRBIFPts),mean(replvlFPts$RepKFPts), mean(replvlFPts$RepTOTFPts))
repNAFPtsdf <-as.data.frame(t(as.data.frame(repNAFPts)))
names(repNAFPtsdf) <- c("RepPos","RepTBFPts","RepRFPts","RepSBFPts","RepBBFPts",
                       "RepRBIFPts","RepKFPts","RepTOTFPts")
replvlFPts <- rbind(replvlFPts, repNAFPtsdf)

# Replvl category FPts to numeric
replvlFPts$RepTBFPts <- as.numeric(replvlFPts$RepTBFPts)
replvlFPts$RepRFPts <- as.numeric(replvlFPts$RepRFPts)
replvlFPts$RepRBIFPts <- as.numeric(replvlFPts$RepRBIFPts)
replvlFPts$RepSBFPts <- as.numeric(replvlFPts$RepSBFPts)
replvlFPts$RepBBFPts <- as.numeric(replvlFPts$RepBBFPts)
replvlFPts$RepKFPts <- as.numeric(replvlFPts$RepKFPts)

# To recognize NA as a string
for(i in 1:nrow(batFPts)){
  if(is.na(batFPts$RepPos[i])==T) batFPts$RepPos[i] <- "NA"
}
replvlFPts$RepPos <- as.character(replvlFPts$RepPos)

# Adding on rep lvl columns
batFPts <- merge(batFPts, replvlFPts, by="RepPos", all.x=TRUE)

# Category FPts above replacement (AR)
batFPts$TBFPtsAR <- batFPts$TBFPts - batFPts$RepTBFPts
batFPts$RFPtsAR <- batFPts$RFPts - batFPts$RepRFPts
batFPts$RBIFPtsAR <- batFPts$RBIFPts - batFPts$RepRBIFPts
batFPts$SBFPtsAR <- batFPts$SBFPts - batFPts$RepSBFPts
batFPts$BBFPtsAR <- batFPts$BBFPts - batFPts$RepBBFPts
batFPts$KFPtsAR <- batFPts$KFPts - batFPts$RepKFPts

# Total FPts above replacement
batFPts$TOTFPtsAR <- batFPts$TBFPtsAR + batFPts$RFPtsAR+ batFPts$RBIFPtsAR+
  batFPts$SBFPtsAR+batFPts$BBFPtsAR+batFPts$KFPtsAR

# Order in decreasing
batFPts <- batFPts[ order(batFPts$TOTFPtsAR, decreasing=TRUE),]

# Filter to important info
batFPts <- batFPts[, c("nameLast","nameFirst","teamID","Pos","TOTFPtsAR", "TOTFPts")]

