data03$Date <- strptime(data03$Date, "%m/%e/%Y") ## turns the date column from a character vector to a date vector


propcatdistrictyear <- function(district = "RICHMOND", year = 2003, category = "LARCENY/THEFT"){
    outcome <-    nrow(data03[which(data03$`PdDistrict`== district & 
                                        data03$`Category`== category & 
                                        data03$`Year` == year),])/
        nrow(data03[which(data03$`Category`== category &
                              data03$`Year` == year),]) 
    whatiwant <- c("in", year, category,"in the",district, "made up", outcome*100, "percent of san francisco's total")
    return (whatiwant)
}

richmondlarc <- vector("list", 0)
for(i in 1:15){
    richmondlarc[[i]] <- propcatdistrictyear(year = i+2002)   
}
richmondlarc

richmondburg <- vector("list", 0)
for(i in 1:15){
    richmondburg[[i]] <- propcatdistrictyear(category = "BURGLARY", year = i+2002)   
}
richmondburg


catdistrictyear <- function(district = "RICHMOND", year = 2003, category = "LARCENY/THEFT"){
    outcome <-    nrow(data03[which(data03$`PdDistrict`== district & 
                                    data03$`Category`== category & 
                                    data03$`Year` == year),])
    whatiwant <- c("in", year,"in the",district, "there were", outcome,"cases of", category)
    return (whatiwant)
}

totrichlarc <-  vector("list", 0)
for(i in 1:15){
    totrichlarc[[i]] <- catdistrictyear(year = i+2002)   
}
totrichlarc
