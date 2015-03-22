## Pitch Fantasy Points Above Replacement 
# (Std points  scoring and 10 team ESPN settings)

# Defining Model Inputs

# Pitch projection model
pitchFPts <- duffypproj

# Don't need pos info for this calculation...but here anyway
espnpos <- subset(pprojdb, Model=="ESPN")[, c("nameFirst","nameLast",
                                              "Model","Pos")]
# Replacement level 
replvlSP <- 71
replvlRP <- 31

# Number of teams in fantasy league
nteam <- 10

# Points setting for each stat
IPpoints <- 3
Wpoints <- 5
SVpoints <- 5
Hpoints <- -1
ERpoints <- -2
Lpoints <- -5
Kpoints <- 1
BBpoints <- -1

# Number of pitchers drafted and per team
npitchtot <- (replvlSP-1)+(replvlRP-1)
npitch <- npitchtot/ nteam

# Counting stat FPts
pitchFPts$IPFPts <- pitchFPts$IP * IPpoints
pitchFPts$WFPts <- pitchFPts$W * Wpoints
pitchFPts$SVFPts <- pitchFPts$SV * SVpoints
pitchFPts$HFPts <- pitchFPts$H * Hpoints
pitchFPts$ERFPts <- pitchFPts$ER * ERpoints
pitchFPts$LFPts <- pitchFPts$L * Lpoints
pitchFPts$KFPts <- pitchFPts$K * Kpoints
pitchFPts$BBFPts <- pitchFPts$BB * BBpoints

# Total FPts
pitchFPts$TOTFPts <- with(pitchFPts, IPFPts+WFPts+SVFPts+HFPts+ERFPts+LFPts+KFPts+BBFPts)

# Adding position info
pitchFPts <- merge(  pitchFPts,espnpos[,c("nameFirst","nameLast","Pos")],
                    by=c("nameLast","nameFirst"), all=TRUE)

# Splitting position for pitchers with mutliple positions
pitchFPts$Posl <- strsplit(as.character(pitchFPts$Pos.y), ",")

# Replacement position
for(i in 1:nrow(pitchFPts)){
  pitchFPts$RepPos[i] <- pitchFPts$Posl[[i]][1]
  
}
# Decreasing order
pitchFPts <- pitchFPts[ order(pitchFPts$TOTFPts, decreasing=T), ]


require(stringr)
#Splitting by SP vs RP
FPtsSP <-subset(pitchFPts, str_detect(pitchFPts$RepPos, "SP"))[,c("nameFirst","nameLast","Pos.y",
                                                               "IPFPts","WFPts","SVFPts",
                                                               "HFPts","ERFPts","LFPts",
                                                               "KFPts","BBFPts")]
FPtsRP <-subset(pitchFPts, str_detect(pitchFPts$RepPos, "RP"))[,c("nameFirst","nameLast","Pos.y",
                                                                  "IPFPts","WFPts","SVFPts",
                                                                  "HFPts","ERFPts","LFPts",
                                                                  "KFPts","BBFPts")]
# Defining rep lvl FPts for each stat category
repSPFPts <- c( mean(FPtsSP$IPFPts[(replvlSP-2):(replvlSP+2)]),
               mean(FPtsSP$WFPts[(replvlSP-2):(replvlSP+2)]),
               mean(FPtsSP$SVFPts[(replvlSP-2):(replvlSP+2)]),
               mean(FPtsSP$HFPts[(replvlSP-2):(replvlSP+2)]),
               mean(FPtsSP$ERFPts[(replvlSP-2):(replvlSP+2)]),
               mean(FPtsSP$LFPts[(replvlSP-2):(replvlSP+2)]),
               mean(FPtsSP$KFPts[(replvlSP-2):(replvlSP+2)]),
               mean(FPtsSP$BBFPts[(replvlSP-2):(replvlSP+2)]))
repRPFPts <- c( mean(FPtsRP$IPFPts[(replvlRP-2):(replvlRP+2)]),
                mean(FPtsRP$WFPts[(replvlRP-2):(replvlRP+2)]),
                mean(FPtsRP$SVFPts[(replvlRP-2):(replvlRP+2)]),
                mean(FPtsRP$HFPts[(replvlRP-2):(replvlRP+2)]),
                mean(FPtsRP$ERFPts[(replvlRP-2):(replvlRP+2)]),
                mean(FPtsRP$LFPts[(replvlRP-2):(replvlRP+2)]),
                mean(FPtsRP$KFPts[(replvlRP-2):(replvlRP+2)]),
                mean(FPtsRP$BBFPts[(replvlRP-2):(replvlRP+2)]))

# Compiliing rep lvl FPts to data frame
reppitchlvlFPts <- rbind( t(as.data.frame(repSPFPts)), t(as.data.frame(repRPFPts)))
pposdf <- as.data.frame(c("SP","RP"))
reppitchlvlFPts <- cbind(pposdf, reppitchlvlFPts)
names(reppitchlvlFPts) <- c("RepPos","RepIPFPts","RepWFPts","RepSVFPts",
                            "RepHFPts","RepERFPts","RepLFPts",
                            "RepKFPts","RepBBFPts")

# Defining rep lvl FPts for players with NA pos
reppitchNAFPts <- c("NA", mean(reppitchlvlFPts$RepIPFPts),mean(reppitchlvlFPts$RepWFPts),
                   mean(reppitchlvlFPts$RepSVFPts),
                   mean(reppitchlvlFPts$RepHFPts),mean(reppitchlvlFPts$RepERFPts),mean(reppitchlvlFPts$RepLFPts),
                   mean(reppitchlvlFPts$RepKFPts),mean(reppitchlvlFPts$RepBBFPts))
reppitchNAFPtsdf <-as.data.frame(t(as.data.frame(reppitchNAFPts)))
names(reppitchNAFPtsdf) <- c("RepPos","RepIPFPts","RepWFPts","RepSVFPts",
                             "RepHFPts","RepERFPts","RepLFPts",
                             "RepKFPts","RepBBFPts")
reppitchlvlFPts <- rbind(reppitchlvlFPts, reppitchNAFPtsdf)

# As numeric.
reppitchlvlFPts$RepIPFPts <- as.numeric(reppitchlvlFPts$RepIPFPts)
reppitchlvlFPts$RepWFPts <- as.numeric(reppitchlvlFPts$RepWFPts)
reppitchlvlFPts$RepSVFPts <- as.numeric(reppitchlvlFPts$RepSVFPts)
reppitchlvlFPts$RepHFPts <- as.numeric(reppitchlvlFPts$RepHFPts)
reppitchlvlFPts$RepERFPts <- as.numeric(reppitchlvlFPts$RepERFPts)
reppitchlvlFPts$RepLFPts <- as.numeric(reppitchlvlFPts$RepLFPts)
reppitchlvlFPts$RepKFPts <- as.numeric(reppitchlvlFPts$RepKFPts)
reppitchlvlFPts$RepBBFPts <- as.numeric(reppitchlvlFPts$RepBBFPts)

for(i in 1:nrow(pitchFPts)){
  if(is.na(pitchFPts$RepPos[i])==T) pitchFPts$RepPos[i] <- "NA"
}

# Adding rep lvl FPts
reppitchlvlFPts$RepPos <- as.character(reppitchlvlFPts$RepPos)
pitchFPts <- merge(pitchFPts, reppitchlvlFPts, by="RepPos", all.x=TRUE)

# FPts for each category above replacement (AR)
pitchFPts$IPFPtsAR <- with(pitchFPts, IPFPts-RepIPFPts)
pitchFPts$WFPtsAR <- with(pitchFPts, WFPts-RepWFPts)
pitchFPts$SVFPtsAR <- with(pitchFPts, SVFPts-RepSVFPts)
pitchFPts$HFPtsAR <- with(pitchFPts, HFPts-RepHFPts)
pitchFPts$ERFPtsAR <- with(pitchFPts, ERFPts-RepERFPts)
pitchFPts$LFPtsAR <- with(pitchFPts, LFPts-RepLFPts)
pitchFPts$KFPtsAR <- with(pitchFPts, KFPts-RepKFPts)
pitchFPts$BBFPtsAR <- with(pitchFPts, BBFPts-RepBBFPts)

# Total FPts above replacement
pitchFPts$TOTFPtsAR <- with(pitchFPts, IPFPtsAR+WFPtsAR+SVFPtsAR+HFPtsAR+ERFPtsAR+LFPtsAR+KFPtsAR+BBFPtsAR)

# Decreasing order
pitchFPts <- pitchFPts[order(pitchFPts$TOTFPtsAR, decreasing=TRUE), ]

# Filter to important info
pitchFPts <- pitchFPts[ , c("nameLast","nameFirst", "teamID","Pos.y","TOTFPtsAR", "TOTFPts")]
names(pitchFPts) <- c("nameLast","nameFirst","teamID","Pos","TOTFPtsAR", "TOTFPts")

