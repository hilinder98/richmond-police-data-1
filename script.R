##test
##
##
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

prop.table(ranklarceny18table) 

round(prop.table(ranklarceny18table), 4)

## finding the absolute number of incidents by case status in the larceny category

status18table <- table(larceny18['Resolution'])

## finding the proportion of larceny cases that have been closed

prop.table(status18table)
