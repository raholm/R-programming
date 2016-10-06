## ------------------------------------------------------------------------
library(Lab6)
head(Lab6::knapsack_objects)

## ---- fig.show='hold'----------------------------------------------------
my_df <- Lab6::knapsack_objects
start.time<-Sys.time()
Lab6::knapsack_brute_force(x=my_df[1:16,], W=3500)
end.time<-Sys.time()
time.taken <- end.time - start.time
time.taken

## ------------------------------------------------------------------------
start.time<-Sys.time()
Lab6::knapsack_dynamic(x=my_df[1:500, ], W=3500)
end.time<-Sys.time()
time.taken <- end.time - start.time
time.taken

## ------------------------------------------------------------------------
start.time<-Sys.time()
invisible.output <- Lab6::knapsack_greedy(x=my_df[1:1000000,], W=3500)
end.time<-Sys.time()
time.taken <- end.time - start.time
time.taken

