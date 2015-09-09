complete <- function(directory, id = 1:332) {
       nobs = numeric()
    for (i in id) {

     newRead = read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=''))
        nobs = c(nobs, sum(complete.cases(newRead)))
    }
    return(data.frame(id, nobs))
}
