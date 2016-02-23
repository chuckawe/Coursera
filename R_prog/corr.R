corr<-function(directory, threshold=0){
  ##'directory' is a character vector of length 1 indicating the location of CSV files
  
  ##'threshold' is a numeric vector of length 1 indicating the number of 
  ##completly obeserved observation
 
  files_list<-list.files(directory,full.names=TRUE) ## creates list of filenames
  datav<-vector(mode="numeric",length=0) ## No threshold monitor vector
  
    for(i in 1:length(files_list)){
        vectfile<-read.csv(files_list[i])
        completes<-sum((!is.na(vectfile$sulfate))&(!is.na(vectfile$nitrate)))
        
        if (completes > threshold){
                sulfates_temp<- vectfile[which(!is.na(vectfile$sulfate)),]
                totals_temp<-sulfates_temp[which(!is.na(sulfates_temp$nitrate)),]
                datav<-c(datav,cor(totals_temp$sulfate,totals_temp$nitrate))
        }
    }
    datav
  
}