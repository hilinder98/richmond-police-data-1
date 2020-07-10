##test
##
##
library(readr)
Police_Department_Incident_Reports_2018_to_Present <- read_csv("~/Desktop/richmond police data/police data 2/richmond-police-data-1/Police_Department_Incident_Reports__2018_to_Present.csv")
View(Police_Department_Incident_Reports_2018_to_Present)

data18 <- Police_Department_Incident_Reports_2018_to_Present
View(data18)
table(data18['Incident Category'])

richmond18 <- data18[which(data18['Police District'] == "Richmond"),] ## filters for only incidents responded to by the richmond police

## finding the absolute number of incidents for each category

incidenttable <- table(richmond18['Incident Category'])

rankincidenttable <- sort(incidenttable, decreasing = TRUE)


## finding the percentage of incidents by category

prop.table(rankincidenttable) 

round(prop.table(rankincidenttable), 4)

## finding the abosulte number of incidents by sub category of Larcency Theft

larceny18 <- richmond18[which(richmond18['Incident Category'] == "Larceny Theft"),]

larceny18table <- table(larceny18['Incident Subcategory'])

ranklarceny18table <- sort(larceny18table, decreasing = TRUE)

## finding the proportion of larceny incidents by subcategory

prop.table(ranklarsceny18table) 

round(prop.table(ranklarceny18table), 4)

## finding the absolute number of incidents by case status in the larceny category

status18table <- table(larceny18['Resolution'])

## finding the proportion of larceny cases that have been closed


prop.table(status18table)

## bar plots for the incident types, larceny subcategories and the status of larceny cases 

barplot(rankincidenttable, las = 2, cex.names = .5, ylim =c(0, 10000), main = "Breakdown of police incidents in the Richmond by incident type")

ggplot()

barplot(ranklarceny18table, las = 2, cex.names = 1, ylim =c(0, 10000), main = "Breakdown of larceny incidents by subcategory")

barplot(status18table, las = 2, cex.names = 1,ylim =c(0, 10000), main = "Breakdown of Larceny case statuses")

## changes over time

changesincidenttype <- table(richmond18$`Incident Category`,richmond18$`Incident Year`)

rankchangesincidenttype <- changesincidenttype[order(-rowSums(changesincidenttype)),]

changeslarcerny18table <-  table(larceny18$`Incident Subcategory`,larceny18$`Incident Year`)

rankchangeslarcerny18table <- changeslarcerny18table[order(-rowSums(changeslarcerny18table)),]

changesstatus18table <- table(larceny18$`Resolution`,larceny18$`Incident Year`)

rankchangesstatus18table <-  changesstatus18table[order(-rowSums(changesstatus18table)),]

## finding the proportion of cases by incident category that took place in the richmond

## lets find the proportion of petty larceny incidents in the richmond as a proportion of the total incidents in the city

nrow(data18[which(data18$`Police District`=="Richmond" & 
                      data18$`Incident Category`== "Larceny Theft" 
                  & data18$`Incident Year` == '2018'),])/
nrow(data18[which( data18$`Incident Category`== "Larceny Theft" &
                       data18$`Incident Year` == '2018'),])

## creating a function that creates a list of

propcategorydistrictyear <- function(district = "Richmond", year = 2018, category = "Larceny Theft"){
outcome <-    nrow(data18[which(data18$`Police District`== district & 
                                data18$`Incident Category`== category & 
                                data18$`Incident Year` == year),])/
              nrow(data18[which(data18$`Incident Category`== category &
                                data18$`Incident Year` == year),]) 
whatiwant <- c("in", year, category,"in the",district, "made up", outcome*100, "percent of san francisco's total")
return (whatiwant)
}

richmond <- vector("list", 0)
for(i in 1:3){
    richmondlarceny[[i]] <- propcategorydistrictyear(year = i+2017)   
    richmondlarceny
}

taravallarceny <- vector("list", 0)
for(i in 1:3){
    taravallarceny[[i]] <- propcategorydistrictyear(district = "Taraval", year = i+2017)   
    taravallarceny
}


