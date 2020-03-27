
#LOAD DEPENDENCIES

rm(list=ls())

library(httr)
library(dplyr)

base.url <- "https://api.groupme.com/v3/groups"



#STEP 1: INSERT API KEY

         #GET API KEY BY LOGGING INTO dev.groupme.com
         #INSERT YOUR API KEY AFTER "api.key <-" AS TEXT STRING
         #REMOVE DEFAULT BELOW UNLESS YOU CHOOSE TO SAVE YOUR KEY AT THAT LOCATION


api.key <- names(read.delim("~/groupme-api/private/api-key.txt")) #INSERT API KEY HERE AND REMOVE names(read.delim("~/groupme-api/private/api-key.txt"))


#STEP 2: CREATE FUNCTION FOR MAKING CHAT KEY.

         #THE KEY IDENTIFIES EACH UNIQUE GROUPME CHAT AND WILL BE LISTED IN THE "id" COLUMN OF THE RESULTING chat.key DATAFRAME 
         #THIS KEY WILL BE USED TO IDENTIFY WHICH KEY TO INSERT INTO index.groupme() FUNCTION


make.chat.key <- function(){group.request <- httr::GET(base.url,
                                            query = (list(token = api.key,
                                                          per_page = 100)
                                                     )
                                            )

    group.response <- content(group.request)[["response"]]
    
    chat.key <-  data.frame()
    
    for (h in seq_along(group.response)){
      
        chat.key <- bind_rows(chat.key, as.data.frame(t(group.response[[h]])))
        
        }
    
    return(chat.key)
}

#STEP 3: CREATE FUNCTION FOR INDEXING GROUPME

         #THIS FUNCTION INDEXES THE GROUPME
         #IT TAKES ONE PARAMETER - chat.key - THAT CAN BE IDENTIFIED BY RUNNING THE make.chat.key() FUNCTION
         #IT DEFAULTS TO chat.id=chat.key["id"][[1,1]]) WHICH WILL EVALUATE TO THE FIRST GROUPME ID LISTED IN THE chat.key DATAFRAME

index.groupme <- function(chat.id=chat.key["id"][[1,1]]){

    max.id <- 0

    i <<- 1

    out.df <<- data.frame()

    length.messages <- list()
  
    continue <<- 1
    
    while (continue==1){
      
        request.messages <<- httr::GET(paste0(base.url,"/",chat.id,"/messages"),
                                      query = (list(token = api.key,
                                                    limit = 100,
                                                    after_id = max.id)
                                               )
                                      )
        
        if (request.messages["status_code"] !=200) {
          
            message(paste0("Completed with request status: ", request.messages["status_code"]," in the final GET request."))
          
            break
        }
        
        messages <- content(request.messages)[["response"]][["messages"]]
        
        if (length(messages)<1){
          
          message(paste0("Completed with request status: ", request.messages["status_code"], " and message count of ", length(messages)," in the final request"))
          
          break
          
          }
        
        
        n.messages <<- content(request.messages)[["response"]][["count"]][1]
    
        length.messages <- length(messages)
    
        df.row <- list()
    
        for (k in (seq_along(messages))) {
      
            df.row[[k]] <- messages[k][[1]]
      
            out.df <<- bind_rows(out.df,as.data.frame((t(df.row[[k]]))))
      
            max.id <- messages[length.messages][[1]]$id
            
            message(paste0("i: ", i, " k: ", k, " length: ", nrow(out.df)))
            
            }
    
        i <<- i+1
        
        if (nrow(out.df)>=n.messages){
           
           message("Index Complete")
          
          continue <<- 0
         
         }
    }
    
    return(out.df)

}


#STEP 4: RUN FUNCTIONS

  #4.A: RUN MAKE CHAT KEY.  
        #USED FOR LOOKING UP chat.id PARAMETER FOR index.groupme() FUNCTION
        #SAVES RESULTS IN DATAFRAME NAMED chat.key

chat.key <- make.chat.key()


  #4.B: RUN MAKE INDEX DATAFRAME BY ENTERING chat.id FROM chat.key() FUNCTION
        #WILL DEFAULT TO chat.key$id[1] WHICH EVALUATES TO FIRST CHAT IN CHAT KEY
        #SAVES RESULTS IN DATAFRAME NAMED df

df <- index.groupme( #INSERT CHAT ID HERE AS chat.id = #XXX
                       )

#save(df,file=paste0("~/groupme-api/private/",chat.id,"/message-data/.Rdata"))