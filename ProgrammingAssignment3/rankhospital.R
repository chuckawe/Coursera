##Ranks Hospitals in each State by Outcome

rankhospital<-function(state,outcome,num){
        hospdata<-read.csv("outcome-of-care-measures.csv")

##Check whether State is valid entry, then if Outcome is valid entry
##Will stop or continue function based on return
        
        states<-levels(hospdata[,7])
        test<-FALSE
        for(i in 1:length(states)){
          if(state==states[i]){
            test<-TRUE
          }
        }
        if (!test){
          stop("Invalid State")
        }
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
        statdata<-hospdata[grep(state,hospdata$State),]
        order<-statdata[order(statdata[,ocol],statdata[,2],na.last=NA),]
        if(num=="best"){
          order[1,2]
        } else if(num=="worst"){
          order[nrow(order),2]
        } else{
          order[num,2]
        }
}