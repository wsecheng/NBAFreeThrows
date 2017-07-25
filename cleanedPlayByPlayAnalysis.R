library(plyr) 
library(tidyverse)
library(stringr)
library(scales)
library(gridExtra)
library(grid)
library(reshape2)

options(tibble.width = NULL)

plays <- read_csv('test.csv', col_names = c('Team', 'Id', 'HomeOrAway', 'Quarter', 'Time', 'Action', 'Score'), col_types = cols(Time = col_character()))

freeThrows <- plays %>% separate(Score, into = c('AwayScore', 'HomeScore')) %>% separate(Time, into = c("Minutes","Seconds")) %>% 
                        mutate(Minutes = as.numeric(Minutes), Seconds = as.numeric(Seconds), HomeScore = as.numeric(HomeScore), AwayScore = as.numeric(AwayScore)) %>% 
                        mutate(TimeElapsed = 12 -(Minutes + Seconds/60)) %>% filter(grepl('free throw', plays$Action))

misses <- str_locate(freeThrows$Action, 'misses')[,1]
makes <- str_locate(freeThrows$Action, 'makes')[,1]
misses[is.na(misses)] = makes[is.na(misses)]

index = misses - 2

freeThrows <- freeThrows %>% mutate(Player = substr(Action,1, index), Outcome = substr(Action, index+2, 100)) %>% separate(Outcome, into = c("MakeMiss", "Details"), extra = 'merge')

freeThrows$Details <- sub('free throw', '', freeThrows$Details) %>% trimws()
whichFreeThrow <- str_locate(freeThrows$Details, '[0-9]')[,1]
freeThrows <- freeThrows %>% mutate(FreeThrowOrder = substr(Details,whichFreeThrow, 100)) %>% separate(FreeThrowOrder, into = c("Number", "OutOf"), sep = " of ")
freeThrows$Details[freeThrows$Details != 'technical'] <- "Regular"
freeThrows$Details[freeThrows$Details == 'technical'] <- "Technical"

freeThrows <- freeThrows %>% select(-Action) %>% mutate(HomeOrAway = recode(freeThrows$HomeOrAway, 'vs' = "H", '@' = "A")) %>% filter(HomeOrAway == "H")

#freeThrows <- separate(freeThrows, Numbers, c("Number", "OutOf"), sep= 'of') %>%
 # mutate(HomeOrAway = recode(freeThrows$HomeOrAway, 'vs' = "H", '@' = "A"), Type = recode(Type, 'free throw' = 'Regular', 'technical free throw' = "Technical"), 
  #       Number = as.numeric(Number), OutOf = as.numeric(OutOf))

freeThrows$Previous <- NA
notFirstFreeThrowOrTech <- which(freeThrows$Number %in% c(2,3))
freeThrows$Previous[notFirstFreeThrowOrTech] <- freeThrows$MakeMiss[notFirstFreeThrowOrTech-1]
freeThrows$Previous[is.na(freeThrows$Previous)] <- ''

write.csv(freeThrows, 'freeThrows.csv')
############################## Does making/missing first free throw affect player making the second?  ############################## 

### Group freeThrows tibbles to get counts for each make/miss combination ###
aggregated <- freeThrows %>% group_by(Player, MakeMiss, Previous) %>% summarise(total.count = n()) %>% ungroup(aggregated) %>% complete(Player, MakeMiss, Previous)
aggregated$Previous[which(aggregated$Previous == '')] <- NA

### Pivot grouped tibble ###
secondFTonly <- aggregated %>% unite(New, MakeMiss, Previous, sep = "_") %>% spread(key = New, value = total.count) %>% separate(Player, into=c("FirstName", "LastName"), sep=" ")
secondFTonly <- secondFTonly[complete.cases(secondFTonly),] %>% mutate(MakeAfterMake = makes_makes/(makes_makes+makes_misses), 
                                                                      MakeAfterMiss = misses_makes/(misses_misses+misses_makes), 
                                                                      Makes = (makes_makes + makes_NA + makes_misses),
                                                                      Misses = (misses_misses + misses_NA + misses_makes),
                                                                      Total = (makes_makes + makes_misses + misses_misses + misses_makes + makes_NA + misses_NA),
                                                                      Percentage = Makes/Total,
                                                                      MissMotivation = MakeAfterMiss-MakeAfterMake)


### Top 25 Tibbles for ggploting ###
secondFTonly25Total <- top_n(secondFTonly, 25, Total)
secondFTonly25Percentage <- top_n(secondFTonly, 25, Percentage)
secondFTonly25PercentageLow <- top_n(secondFTonly, -25, Percentage)

g <- ggplot(data=secondFTonly, aes(MakeAfterMake, MakeAfterMiss)) + 
              geom_point(aes(MakeAfterMake, MakeAfterMiss, colour = MissMotivation, size=Total) , alpha = 0.2) + 
              geom_abline(slope=1, linetype = "dotted") + 
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))

g1 <- g + geom_point(data = secondFTonly25Total, aes(colour = MissMotivation, size=Total))  + 
  geom_text(data= secondFTonly25Total, aes(label=LastName), vjust = 2) + 
  scale_colour_gradient(low = 'slategray1', high = 'slategray')

g2 <- g + geom_point(data = secondFTonly25Percentage, aes(colour = MissMotivation, size=Total))  + 
  geom_text(data= secondFTonly25Percentage, aes(label=LastName), vjust = 2) + 
  scale_colour_gradient(low = 'darkseagreen1', high = 'darkseagreen4')

g3 <- g + geom_point(data = secondFTonly25PercentageLow, aes(colour = MissMotivation, size=Total))  + 
  geom_text(data= secondFTonly25PercentageLow, aes(label=LastName), vjust = 2) + 
  scale_colour_gradient(low = 'mistyrose1', high = 'mistyrose4')

multiplot(g1,g2,g3, cols =2)


multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}



############################## Do players make more free throws in the clutch? ############################## 
freeThrows2 <- filter(freeThrows, HomeOrAway == 'H') %>% mutate(QuarterNumber = as.double(str_extract(Quarter, "\\d")),
                                                                MinutesIntoGame = as.integer(floor(TimeElapsed + 12*(QuarterNumber)-1)),
                                                                NthThirdMinute = TimeElapsed %/% 3 + 1,
                                                                PointDifferential = abs(HomeScore-AwayScore))

## Get Reference Tables to filter certain players ##
byPlayer <- freeThrows2 %>%  group_by(Player, MinutesIntoGame, MakeMiss) %>% summarise(total.count = n()) %>% ungroup(byPlayer) %>% complete(Player, MinutesIntoGame, MakeMiss) %>% spread(key = MakeMiss, value = total.count)
reference <- byPlayer %>% group_by(Player) %>% summarise(SeasonMakes = sum(makes, na.rm=TRUE), SeasonMisses = sum(misses, na.rm=TRUE)) %>% mutate(SeasonTotal = SeasonMakes + SeasonMisses, Percentage = SeasonMakes/SeasonTotal)

## Get top 5 and bottom 5 players (min 125 FT's made) ##
top5Players <- top_n(filter(reference, SeasonMakes >= 125), 5, Percentage)$Player
bottom5Players <- top_n(filter(reference, SeasonMakes >= 125), -5, Percentage)$Player

allPlayers <- freeThrows2 %>% group_by(QuarterNumber) %>% summarise(Makes = sum(MakeMiss == "makes"), Misses = sum(MakeMiss == "misses"))
allPlayers <- mutate(allPlayers, Percentage = Makes/(Makes+Misses)) 

top5 <- filter(freeThrows2, Player %in% top5Players) %>% group_by(Player, QuarterNumber) %>% summarise(Makes = sum(MakeMiss == "makes"), Misses = sum(MakeMiss == "misses"))
top5 <- mutate(top5, Percentage = Makes/(Makes+Misses)) %>% ungroup(top5) %>%
        #add dummy rows for legend workaround
        add_row(Player = " ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) %>% 
        add_row(Player = "  ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) %>% 
        add_row(Player = "   ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) %>% 
        add_row(Player = "    ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) %>% 
        add_row(Player = "     ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) %>% 
        add_row(Player = "      ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) %>%
        add_row(Player = "       ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) %>%
        add_row(Player = "        ", QuarterNumber = 1, Makes = 0, Misses = 0, Percentage = 0 ) 

bottom5 <- filter(freeThrows2, Player %in% bottom5Players) %>% group_by(Player, QuarterNumber) %>% summarise(Makes = sum(MakeMiss == "makes"), Misses = sum(MakeMiss == "misses"))
bottom5 <- mutate(bottom5, Percentage = Makes/(Makes+Misses)) 

all <- freeThrows2 %>% group_by(QuarterNumber) %>% summarise(Makes = sum(MakeMiss == "makes"), Misses = sum(MakeMiss == "misses"))
all <- mutate(all, Percentage = Makes/(Makes+Misses)) 

legendvalues <- c("Paul George" = "blue4", "Stephen Curry" = "dodgerblue2", "Kyrie Irving" = "cyan2", "Isaiah Thomas" = "paleturquoise2", "Danilo Gallinari" = "lightcyan1",
                  " " = "white", "  " = "white", "   " = "white", "    " = "white", "     " = "white", "      " = "white", "       " = "white", "        " = "white",
                  "Mason Plumlee" = "mistyrose1", "Dwight Howard" = "lightpink1", "Bismack Biyombo" = "indianred1", "DeAndre Jordan" = "red1", "Andre Drummond" = "red4")
names <- c("Paul George", "Stephen Curry", "Kyrie Irving", "Isaiah Thomas", "Danilo Gallinari",
           " ", "  ", "   ", "    ", "     ", "      ", "       ", "        ",
           "Mason Plumlee","Dwight Howard", "Bismack Biyombo", "DeAndre Jordan", "Andre Drummond")


ggByQuarter <- ggplot(data=top5, aes(QuarterNumber, Percentage)) +
  geom_line(aes(colour = Player), size=.75) + geom_line(data = bottom5, aes(QuarterNumber, Percentage, colour = Player), size=.75) +
  geom_line(data = all, aes(QuarterNumber, Percentage), size = .75, linetype = 2) + geom_text(data = all[all$QuarterNumber == 4,], aes(label = 'NBA Average'), hjust = 1, vjust = -1) +
  ggtitle("Top/Bottom 5 Free Throw Percentages By Quarter (min. 125 makes)\n") + labs(x = '\nQuarter', y = 'FT Percentage\n\n') +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1), breaks=seq(0,1,.05)) + 
  scale_color_manual(values=legendvalues, breaks = names) +  
  theme(panel.grid.major = element_line(color = "grey90"), plot.title=element_text(size = 14, face = "bold.italic", hjust = 0.5), axis.title=element_text(size = 12, face = 'italic'), 
        axis.line = element_line(colour = "black"), legend.text=element_text(size = 11), legend.justification = "top") + 
  theme(legend.title=element_blank(), legend.key = element_blank(),  panel.background = element_blank())

ggByQuarter



clutch <- filter(freeThrows2, MinutesIntoGame > 43, PointDifferential <= 6) %>% group_by(Player) %>% summarise(ClutchMakes = sum(MakeMiss == "makes"), ClutchMisses = sum(MakeMiss == "misses")) %>%
          mutate(ClutchTotal = ClutchMakes + ClutchMisses, PercentageClutch = ClutchMakes/ClutchTotal) %>% 
          inner_join(reference, by = "Player") %>% mutate(PercentageDifference = PercentageClutch - Percentage) %>% 
          filter(ClutchTotal >= 50) %>% 
          mutate(rankClutch = dense_rank(desc(PercentageClutch)), rankSeason = dense_rank(desc(Percentage)), rankDifference = rankClutch-rankSeason) %>% 
          arrange(desc(PercentageClutch)) 

clutchRanking <- clutch %>%
                 select(Player, rankSeason, rankClutch) %>% melt() %>% inner_join(clutch, by = "Player")

p <- ggplot(clutchRanking, aes(variable, value, group = Player, colour = rankDifference, label = Player))
p1 <- p + geom_line(size = .8) + 
      geom_text(data = subset(clutchRanking, variable == "rankClutch"), aes(x = variable, size = 1, hjust = 0)) +
      geom_text(data = subset(clutchRanking, variable == "rankSeason"), aes(x = variable, size = 1, hjust = 1)) +
      scale_colour_gradient(low="firebrick1", high="forestgreen") +
      theme(legend.position = "none",  panel.background = element_blank()) + scale_y_continuous(trans = "reverse", breaks = seq(1,17,1))

clutchRanking2 <- clutch %>% melt()
clutchRanking2 <- ddply(clutchRanking2, .(variable), transform, rescale = rescale(value))
p <- ggplot(clutchRanking2, aes(variable, Player)) + geom_tile(aes(fill = rescale), colour = "white") + scale_fill_gradient(low = "white", high = "steelblue") +
     scale_x_discrete(position = "top") + theme(legend.position = "none", panel.background = element_blank(), 
                                                axis.title = element_blank(), axis.ticks = element_blank(), axis.text.x = element_text(angle = 40, hjust = 0))
p


###################### K - Mean's Clustering ###############################
set.seed(20)
Cluster <- kmeans(select(secondFTonly, Percentage, MakeAfterMiss), 3, nstart=20)

testing <- secondFTonly
testing$Cluster <- as.factor(Cluster$cluster)

ggplot(data=testing, aes(Percentage, MakeAfterMiss)) + 
  geom_point(aes(colour = Cluster, size=Total)) + 
  geom_abline(slope=1, linetype = "dotted") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))








