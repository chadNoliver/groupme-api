rm(list=ls())

library(httr)
library(dplyr)

base.url <- "https://api.groupme.com/v3/groups"

api.key <- #insert your key here as a text string
           #get key by logging into group me at dev.groupme.com
           names(read.delim("~/groupme-api/private/api-key.txt"))





#create function for 
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


chat.key <- make.chat.key()


df <- index.groupme()

#Lookup chat id created from make.chat.key() and saved in chat.key data.frame and insert it here. As chat.id=XXXX
#Note that this will default to chat.key$id[1] which evaluates to your first chat

#save(df,file="~/groupme-api/private/message-data.Rdata")