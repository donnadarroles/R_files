#Who dataset
#country with the lowest literacy
WHO[1:10,c("Country","LiteracyRate")]
lowlit <- min(WHO$LiteracyRate, na.rm = TRUE)
countrylowlit <- subset(WHO, LiteracyRate == lowlit)
countrylowlit$Country

#Richest country in Europe based on GNI
europe <- subset(WHO, Region == 'Europe')
maxgni <- max(europe$GNI, na.rm = T)
richineuro <- subset(europe, GNI == maxgni)
richineuro$Country

#mean life expectancy
africa <- subset(WHO, Region == 'Africa')
mean(africa$LifeExpectancy)

#number of countries with a population over 10M
pop10m <- subset(WHO, Population > 10000)
NROW(pop10m)
#(top 5) country in the Americas with the highest child mortality rate
top5 <- subset(WHO, Region == "Americas")
top5america <- order(top5$ChildMortality,decreasing=T)
top5country <- top5[top5america,1]
head(top5country,5)
#NBA Dataset
#year Bulls has the  highest winning percentage
bulls <- subset(Historical_NBA_Performance, Team == 'Bulls')
a2 <- max(bulls$'Winning Percentage')
bullhighest <- subset(bulls, `Winning Percentage` == a2)
bullhighest$Year
#Teams with an even win-loss record
winlossteam <- subset(Historical_NBA_Performance, `Winning Percentage` == 0.5, select = c(2))
unique(winlossteam)
#Season Stats Dataset
#What year/season does Lebron James scored the highest?
lebron <- subset(Seasons_Stats, Player == 'LeBron James')
highpts <- max(lebron$PTS, na.rm = T)
lebronhighpts <- subset(lebron, PTS == highpts)
lebronhighpts$Year
#What year/season does Michael Jordan scored the highest?
jordan <- subset(Seasons_Stats, Player == 'Michael Jordan*')
highpts <- max(jordan$PTS, na.rm = T)
jordanhighpts <- subset(jordan, PTS == highpts)
jordanhighpts$Year
#Player efficiency rating of Kobe Bryant in the year where his MP is lowest?
kobe <- subset(Seasons_Stats, Player == 'Kobe Bryant')
lowmp <- min(kobe$MP, na.rm = T)
kobeper <- subset(kobe, MP == lowmp)
kobeper$PER
#National Universities Dataset
#University with the most number of undergrads
National_Universities_Rankings$`undergrad` = as.numeric(gsub(pattern = "\\,|\\,",replacement = "",National_Universities_Rankings$`Undergrad Enrollment`))
undergraduniv <- max(National_Universities_Rankings$undergrad)
univname <- subset(National_Universities_Rankings, undergrad == undergraduniv)
univname$Name
#Average Tuition in the Top 10 University
National_Universities_Rankings[1:10,c('Name',"Tuition and fees")]
National_Universities_Rankings$tuitionfees = as.numeric(gsub(pattern = "\\$|\\,",replacement = "", National_Universities_Rankings$`Tuition and fees`))
mean(National_Universities_Rankings$tuitionfees)