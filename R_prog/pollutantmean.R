pollutantmean<-function(directory, pollutant, id=1:332){
  ##'directory' is a character vector of length 1 indicating the location of the CSV file
  ##'pollutant' is a character vector of length 1 indicating the name of the pollutant
  ##'id' is an integer vector indicating the monitor ID
 
    list_files <- list.files(directory, full.names=TRUE)   #creates list of filenames
    dataf <- data.frame() ##empty data frame
    
    for(i in id) {
            dataf <-rbind(dataf,read.csv(list_files[i]))
    }
    mean(dataf[,pollutant],na.rm=TRUE)
}