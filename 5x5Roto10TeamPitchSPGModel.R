## Pitch Standings Points Gained Calculation 
# (Std 5x5  scoring and 10 team ESPN settings)

# Defining Model Inputs

# Pitch projection model
pitchspg <- duffypproj

# Don't need pos info for this calculation...but here anyway
espnpos <- subset(pprojdb, Model=="ESPN")[, c("nameFirst","nameLast",
                                              "Model","Pos")]
# Replacement level 
replvlSP <- 71
replvlRP <- 31

# Number of teams in fantasy league
nteam <- 10

# Historical spg vs stat slopes from 
#  http://razzball.com/stats-needed-to-win-your-fantasy-baseball-league/
Wspgslope <- 3.03
SVspgslope <- 9.95
Kspgslope <- 39.3
ERAspgslope <- -0.076
WHIPspgslope <- -0.015

# Projected Average stats for projected drafted pitchers (DP)
IPavgDP <- 159.8
ERAavgDP <- 3.351
WHIPavgDP <- 1.183

# Number of pitchers drafted and per team
npitchtot <- (replvlSP-1)+(replvlRP-1)
npitch <- npitchtot/ nteam

# Stats needed to calculate rate stat spg , 
# stats of average team (Tm) with n-1 players (n1)
IPavgTm <- npitch * IPavgDP
ERavgTm <- IPavgTm * ERAavgDP / 9
ERavgPl <- ERavgTm / npitch
IPn1Tm <- (npitch-1)*IPavgDP
ERn1Tm <- (npitch-1) * ERavgPl
WHavgTm <- IPavgTm * WHIPavgDP
WHavgPl <- WHavgTm / npitch
WHn1Tm <- (npitch-1) * WHavgPl

# Counting stat spg
pitchspg$Wspg <- pitchspg$W / Wspgslope
pitchspg$SVspg <- pitchspg$SV / SVspgslope
pitchspg$Kspg <- pitchspg$K / Kspgslope


# Rate stat spg
pitchspg$ERAspg <- (((pitchspg$ER + ERn1Tm)/(pitchspg$IP + IPn1Tm))-(ERAavgDP/9))/ERAspgslope
pitchspg$WHIPspg <- (((pitchspg$H+ pitchspg$BB + WHn1Tm)/(pitchspg$IP + IPn1Tm))-
                       WHIPavgDP)/WHIPspgslope
# Total spg
pitchspg$TOTspg <- with(pitchspg, Wspg +SVspg+Kspg+ERAspg+WHIPspg)

# Adding position info
pitchspg <- merge(  pitchspg,espnpos[,c("nameFirst","nameLast","Pos")],
                    by=c("nameLast","nameFirst"), all=TRUE)

# Splitting position for pitchers with mutliple positions
pitchspg$Posl <- strsplit(as.character(pitchspg$Pos.y), ",")

# Replacement position
for(i in 1:nrow(pitchspg)){
  pitchspg$RepPos[i] <- pitchspg$Posl[[i]][1]
  
}
# Decreasing order
pitchspg <- pitchspg[ order(pitchspg$TOTspg, decreasing=T), ]


require(stringr)
# Split by position
spgSP <-subset(pitchspg, str_detect(pitchspg$RepPos, "SP"))[,c("nameFirst","nameLast","Pos.y",
                                                               "Wspg","SVspg","Kspg",
                                                               "ERAspg","WHIPspg","TOTspg")]
spgRP <-subset(pitchspg, str_detect(pitchspg$RepPos, "RP"))[,c("nameFirst","nameLast","Pos.y",
                                                               "Wspg","SVspg","Kspg",
                                                               "ERAspg","WHIPspg","TOTspg")]

# Defining rep lvl spg for each stat category
repSPspg <- c( mean(spgSP$Wspg[(replvlSP-2):(replvlSP+2)]),
               mean(spgSP$SVspg[(replvlSP-2):(replvlSP+2)]),
               mean(spgSP$Kspg[(replvlSP-2):(replvlSP+2)]),
               mean(spgSP$ERAspg[(replvlSP-2):(replvlSP+2)]),
               mean(spgSP$WHIPspg[(replvlSP-2):(replvlSP+2)]))
repRPspg <- c( mean(spgRP$Wspg[(replvlRP-2):(replvlRP+2)]),
               mean(spgRP$SVspg[(replvlRP-2):(replvlRP+2)]),
               mean(spgRP$Kspg[(replvlRP-2):(replvlRP+2)]),
               mean(spgRP$ERAspg[(replvlRP-2):(replvlRP+2)]),
               mean(spgRP$WHIPspg[(replvlRP-2):(replvlRP+2)]))

# Compiliing rep lvl spg to data frame
reppitchlvlspg <- rbind( t(as.data.frame(repSPspg)),t(as.data.frame(repRPspg)))
pposdf <- as.data.frame(c("SP","RP"))
reppitchlvlspg <- cbind(pposdf, reppitchlvlspg)
names(reppitchlvlspg) <- c("RepPos","RepWspg","RepSVspg","RepKspg","RepERAspg","RepWHIPspg")

# Defining rep lvl spg for players with NA pos
reppitchNAspg <- c("NA", mean(reppitchlvlspg$RepWspg),mean(reppitchlvlspg$RepSVspg),
                   mean(reppitchlvlspg$RepKspg),
                   mean(reppitchlvlspg$RepERAspg),mean(reppitchlvlspg$RepWHIPspg))
reppitchNAspgdf <-as.data.frame(t(as.data.frame(reppitchNAspg)))
names(reppitchNAspgdf) <- c("RepPos","RepWspg","RepSVspg","RepKspg","RepERAspg","RepWHIPspg")
reppitchlvlspg <- rbind(reppitchlvlspg, reppitchNAspgdf)

# As numeric.
reppitchlvlspg$RepWspg <- as.numeric(reppitchlvlspg$RepWspg)
reppitchlvlspg$RepSVspg <- as.numeric(reppitchlvlspg$RepSVspg)
reppitchlvlspg$RepKspg <- as.numeric(reppitchlvlspg$RepKspg)
reppitchlvlspg$RepERAspg <- as.numeric(reppitchlvlspg$RepERAspg)
reppitchlvlspg$RepWHIPspg <- as.numeric(reppitchlvlspg$RepWHIPspg)

for(i in 1:nrow(pitchspg)){
  if(is.na(pitchspg$RepPos[i])==T) pitchspg$RepPos[i] <- "NA"
}

# Adding rep lvl spg
reppitchlvlspg$RepPos <- as.character(reppitchlvlspg$RepPos)
pitchspg <- merge(pitchspg, reppitchlvlspg, by="RepPos", all.x=TRUE)

#Spg for each category above replacement (AR)
pitchspg$WspgAR <- with(pitchspg, Wspg-RepWspg)
pitchspg$SVspgAR <- with(pitchspg, SVspg-RepSVspg)
pitchspg$KspgAR <- with(pitchspg, Kspg-RepKspg)
pitchspg$ERAspgAR <- with(pitchspg, ERAspg-RepERAspg)
pitchspg$WHIPspgAR <- with(pitchspg, WHIPspg-RepWHIPspg)
# Total spg above replacement
pitchspg$TOTspgAR <- with(pitchspg, WspgAR+SVspgAR+KspgAR+ERAspgAR+WHIPspgAR)

# Decreasing order
pitchspg <- pitchspg[order(pitchspg$TOTspgAR, decreasing=TRUE), ]

# Filter to important info
pitchspg <- pitchspg[ , c("nameLast","nameFirst", "teamID","Pos.y","TOTspgAR")]
names(pitchspg) <- c("nameLast","nameFirst","teamID","Pos","TOTspgAR")

