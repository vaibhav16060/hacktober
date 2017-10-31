library(bnlearn)
library(xlsx)

clean_data <- function(df){

  means <- 1:11
  sds <- 1:11
  low <- 1:11
  high <- 1:11
  for(i in 1:ncol(df)){
    means[i] <- mean(df[, i])
    sds[i] <- sd(df[, i])
    low[i] <- means[i] - sds[i] * 3 #greater than 3 steps
    high[i] <- means[i] + sds[i] * 3
  }
  ret_df <- data.frame(praf=numeric(0), pmek=numeric(0), plcg=numeric(0), PIP2=numeric(0), PIP3=numeric(0), p44.42=numeric(0), pakts473=numeric(0), PKA=numeric(0), PKC=numeric(0), P38=numeric(0), pjnk=numeric(0))

  for(i in 1:nrow(df)){
    flag = 0
    for(j in 1:ncol(df)){
      if(low[j] > df[i, j]){
        flag = 1
        break
      }
      #this entry need to be removed
    }
    for(j in 1:ncol(df)){
      if(high[j] < df[i, j]){
        flag = 1
        break
      }
      #this entry need to be removed
    }
    if(flag == 0){
      #add entry to new df
      ret_df <- rbind(ret_df, df[i, ])
    }
  }
  return(ret_df)
}

cluster_formation <- function(ret_df){

  clus_df <- data.frame(praf=rep(0, nrow(ret_df)), pmek=rep(0, nrow(ret_df)),
                        plcg=rep(0, nrow(ret_df)), PIP2=rep(0, nrow(ret_df)), 
                        PIP3=rep(0, nrow(ret_df)), p44.42=rep(0, nrow(ret_df)), 
                        pakts473=rep(0, nrow(ret_df)), PKA=rep(0, nrow(ret_df)), 
                        PKC=rep(0, nrow(ret_df)), P38=rep(0, nrow(ret_df)), pjnk=rep(0, nrow(ret_df)))
  for(i in 1:ncol(ret_df)){
    clus_df[, i] <- kmeans(ret_df[, i], 3)$cluster
  }
  return(clus_df)
}

#function to remove the cycle from the graph
remove_cycle <- function(adj_list){

  color <- rep(0, nrow(adj_list))
  queue <- rep(0, nrow(adj_list))
  queue_f <- 1
  queue_r <- 1
  queue[1] <- 1
  color[1] <- 1
  while(TRUE){
    i <- queue[queue_f]
    queue_f <- queue_f + 1
    for(j in 1:ncol(adj_list)){
      if(adj_list[i, j] == 1){ #there is an edge
        if(color[j] == 0){
          #enqueue the data
          queue[queue_r] <- j
          queue_r <- queue_r + 1
          color[j] <- 1
        }
        else{
          #this was already visited
          adj_list[i, j] <- 0
        }
      }
    }
    if(queue_f == queue_r){
      break
    }
  }
  return(adj_list)
}

change_graph_randomly <- function(options_to_evaluate, graph, clus_df){ 
  add_copy <- options_to_evaluate
  add_order <- sample(1:110, 110, replace=FALSE) 
  add_counter <- 1
  delete_copy <- options_to_evaluate
  delete_order <- sample(1:110, 110, replace=FALSE)
  delete_counter <- 1
  limit <- nrow(add_copy) * 2
  for(i in 1:limit){
    #randomly select an operation add edge, delete edge or reverse edge
    temp_graph <- empty.graph(cols)
    choice <- floor(runif(1, min=1, max=3))
    if(choice == 1){
      #Add an edge
      if(add_counter > nrow(options_to_evaluate)){
        limit <- limit + 1 #this is a wrong choice by random
      }
      else{
        temp <- amat(graph)
        temp[add_copy$X[add_counter], add_copy$Y[add_counter]] <- 1
        temp <- remove_cycle(temp)
        amat(temp_graph) <- temp
        add_counter <- add_counter + 1
      }
    }
    else(choice == 2){
      #delete an edge
      if(delete_counter > nrow(options_to_evaluate)){
        limit <- limit + 1 #this is a wrong choice by random
      }
      else{
        temp <- amat(graph)
        temp[add_copy$X[delete_counter], add_copy$Y[delete_counter]] <- 0
        amat(temp_graph) <- temp
        delete_counter <- delete_counter + 1
      }
    }
    if(score(temp_graph, clus_df, type="bic") > score(graph, clus_df, type="bic")){
      graph <- temp_graph
    }
  }
  return(graph)
}

df <- read.xlsx("1. cd3cd28.xls", sheetName = "Sheet1")
ret_df <- clean_data(df)
clus_df <- cluster_formation(ret_df)
cols <- c("praf", "pmek", "plcg", "PIP2", "PIP3", "p44.42", "pakts473", "PKA", "PKC", "P38", "pjnk")
#this is to be used for randomly adding and removing the edges
options_to_evaluate <- expand.grid(cols,cols) 
colnames(options_to_evaluate) <- c("X", "Y")
options_to_evaluate <- options_to_evaluate[-as.numeric(rownames(options_to_evaluate[options_to_evaluate$X == options_to_evaluate$Y, ])),]
#options_to_evaluate["Done"] <- rep(0, nrow(options_to_evaluate))
#array has been formed
for(i in 1:500){
  graph <- change_graph_randomly(options_to_evaluate, graph, clus_df)
}
