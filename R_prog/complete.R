complete<-function(directory, id=1:332){
  ##'directory' is a character vector of length 1 indicating the location of the CSV file
  ##'id' is an integer vector indicating the monitor ID
  
  list_files<-list.files(directory, full.names=TRUE) ##creates list of filenames
  dataf<-data.frame() ##empty dataframe
  
    for(i in id){
     file_name<-read.csv(list_files[i])
     nobs<-sum(complete.cases(file_name))
     name_nobs<-data.frame(i,nobs)
     dataf<-rbind(dataf,name_nobs)
    }
    colnames(dataf)<-c("id","nobs")
    dataf
}