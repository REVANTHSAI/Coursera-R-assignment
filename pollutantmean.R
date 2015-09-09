 
 pollutantmean <- function(directory, pollutant, id = 1:332) {
   
   
   data <- NA
   
   
   for(i in id) {
     csv <-   read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=''))

     data <- rbind(data, csv)
   }
   
   mean(data[[pollutant]], na.rm = T)
 }
