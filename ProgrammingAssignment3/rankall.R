## Ranks Hospitals in all States

rankall<-function (outcome, num="best"){
        hospdata<-read.csv("outcome-of-care-measures.csv")
        
##Check whether Outcome is valid entry
##Will stop or continue function based on return
        
        if(!((outcome=="heart attack")|(outcome=="heart failure")|(outcome=="pneumonia"))){
          stop("Invalid Outcome")
        }
        ocol<-if (outcome=="heart attack"){
          11
        } else if (outcome=="heart failure"){
          17
        } else {
          23
        }
        hospdata[,ocol]<-as.numeric(levels(hospdata[,ocol])[hospdata[,ocol]])
        hospdata[,2]<-as.character(hospdata[,2])
        ranking<-vector()
        states<-levels(hospdata[,7])

        for(i in 1:length(states)) {
                statdata<-hospdata[grep(states[i],hospdata$State),]
                order<-statdata[order(statdata[,ocol],statdata[,2],na.last=NA),]
                hospital<-if(num=="best") {
                  order[1,2]
                } else if(num=="worst") {
                  order[nrow(order),2]
                } else{
                  order[num,2]
                }
                ranking<-append(ranking,c(hospital, states[i]))
        }
        ranking<-as.data.frame(matrix(ranking, length(states),2,byrow=TRUE))
        colnames(ranking)<-c('Hospital','State')
        rownames(ranking)<-states

        ranking
}