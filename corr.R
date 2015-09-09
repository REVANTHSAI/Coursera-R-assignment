corr <- function(directory, threshold = 0) {
   
    df = complete(directory)
    ids = df[df["nobs"] > threshold, ]$id
    corrr = numeric()
    for (i in ids) {

        newRead = read.csv(paste(directory, "/", formatC(i, width = 3, flag = "0"), 
            ".csv", sep = ""))
        dff = newRead[complete.cases(newRead), ]
        corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
    }
    return(corrr)
}
cr <- corr("specdata", 150)
head(cr)