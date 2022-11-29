library(tidyverse)
library(igraph) 
library(stringr)
library(imputeTS, include.only = 'na.replace') 
# Global variables
periods <- readRDS(
  "C:\\Users\\TomBo\\Documents\\2023\\2023 Math Project III\\output\\periods.Rds")
n <- 35 # number of industries
inputs <- periods[[1]] |> filter(Industry!="Industry")|>
    select(Industry) |> 
    unique() |>  
    na.omit()
inputs <- inputs[(1:n),] |> as.matrix() |> as.vector()
countries <- periods[[1]] |> filter(Country!="Country")|>
    select(Country) |> 
    unique() |> 
    filter(Country !="TOT") |> 
    na.omit()    
tables_innov <- list()
tables_pred_innov <- list()

# Functions
ttoutput <- 
  function(periods, C, j){
  "Compute the total output table for a country in 1995"
  tt_output1 <- periods[[j]] |>
    filter(Country == C) |>
    select("Industry", "Total Output") |> 
    mutate(across(!Industry, as.numeric))
}
niot <- 
  function(periods, C, j){
  "Compute the input/output matrix in 1995"
  X1 <- periods[[j]] |> 
    filter(Country == C) |> 
    select(contains(C)) |>
    select(ends_with("1"):ends_with("35")) |> 
    mutate_all(as.numeric)
  ?select
  return(X1)
}
adjmat <- 
  function(X1){
  "Compute the adjacency matrix in 1995"
  A <- X1
  for (i in (1:ncol(A))){
    if(sum(X1[,i]) != 0){
      A[,i] <- X1[,i]/sum(X1[,i])
    }
    else{
      A[,i] <- c(rep(0,nrow(A)))
    }
  }
  A <- as.matrix(A)
}
prodimprov <- 
  function(X1, X2, A, tt_output1, tt_output2){
  "Compute productivity improvement between yyyy1 and yyyy2 in country C"
  input_diff <- (X2 - X1)
  input_diffhat <- input_diff/X1
  input_diffhat[is.na(input_diffhat)] <- 0 #fix NaN
  input_diffhat <- as.matrix(input_diffhat)
  input_changes <- diag(input_diffhat%*%t(A))
  
  output_diff <- cbind(tt_output1$Industry, 
                       select(tt_output2, !Industry) - 
                         select(tt_output1, !Industry))
  output_diffhat <- output_diff$`Total Output`/tt_output1$`Total Output`
  output_diffhat[is.na(output_diffhat)] <- 0

  prodimprov <- output_diffhat - input_changes
  return(prodimprov)
}
pred_innov <- function(ctry_innov, A, k, C, n){
  "Compute predicted innovations for a given country based on actual innovation rates"
  pred_innov <- rep(0,n)
  Abis <- A - diag(diag(A))
  tmp <- 0
  for (i in (1:n)){
    for (j in (1:n)){
      if(is.infinite(ctry_innov[j,k])!=TRUE & is.na(ctry_innov[j,k])!=TRUE){
        tmp <- tmp + as.numeric(Abis[i,j])*ctry_innov[j,k] #problem with Inf
      }
    }
    pred_innov[i] <- tmp
  }
  return(pred_innov)
}
compute_ctry_innov <- 
  function(periods, countries, niot, adjmat, ttoutput, inputs, t, span, n){
  "compute 1 yr innovation rate for period i for every industry in a given country"
  ctry_innov <- matrix(nrow=n, ncol=nrow(countries)) |> as.data.frame()
  for (j in (1:nrow(countries))){
    C = countries[j,1] |> as.character()
    tt_output1 <- ttoutput(periods, C, t)
    tt_output2 <- ttoutput(periods, C, t+span)
    X1 <- niot(periods, C, t)
    X2 <- niot(periods, C, (t+span))
    A <- adjmat(X1)
    gamma <- prodimprov(X1, X2, A, tt_output1, tt_output2)
    ctry_innov[,j] <- gamma
  }
  names(ctry_innov) <- countries[,1] |> as.matrix() |> as.vector()
  return(ctry_innov)
}
compute_ctry_pred <- 
  function(pred_innov, ctry_innov, A, n, t){ 
  ctry_pred_innov <- matrix(nrow=n, ncol=nrow(countries)) |> as.data.frame()
  for (j in (1:nrow(countries))){
    C <-  countries[j,1] |> as.character()
    X1 <- niot(periods, C, t)
    A <- adjmat(X1)
    gamma <- pred_innov(ctry_innov, A, j, C, n)
    ctry_pred_innov[,j] <- gamma
  }
  names(ctry_pred_innov) <- countries[,1] |> as.matrix() |> as.vector()
  return(ctry_pred_innov)
}

# Generate WIOD tables of 10-year productivity improvement over periods
span <- 10
for (t in (1:(length(periods)-span))){
  ctry_innov <- compute_ctry_innov(periods, countries, niot, adjmat, ttoutput, inputs, t, span = span, n)
  ctry_pred_innov <- compute_ctry_pred(pred_innov, ctry_innov, A, n, t)
  #get rid of columns with missing data
  ctry_innov <- ctry_innov[ , colSums(is.na(ctry_innov)) == 0] 
  ctry_pred_innov <- ctry_pred_innov[ , colSums(is.na(ctry_innov)) == 0]
  len <- length(tables_innov)
  tables_innov <- append(tables_innov, list(ctry_innov))
  tables_pred_innov <- append(tables_pred_innov, list(ctry_pred_innov))
}
setwd("C:\\Users\\TomBo\\Documents\\2023\\2023 Math Project III\\output")
saveRDS(tables_innov, file = "tables_innov.Rds")
saveRDS(tables_pred_innov, file = "tables_pred_innov.Rds")

# Compute correlation of predicted and observed productivity improvements
y <- 1995+10
big_innov_table <- cbind(inputs, tables_innov[[1]]) |>
  as.data.frame() |>
  pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov")
Year <- rep(y, nrow(big_innov_table))
big_innov_table <- cbind(big_innov_table, Year)
big_pred_table <- cbind(inputs, tables_pred_innov[[1]]) |>
  as.data.frame() |>
  pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov")
Year <- rep(y, nrow(big_pred_table))
big_pred_table <- cbind(big_pred_table, Year)# set up tables of all observations
for (i in (2:length(tables_innov))){
  df1 <- cbind(inputs, tables_innov[[i]]) |>
    as.data.frame() |>
    pivot_longer(
      cols = !inputs, names_to = "Country", values_to = "innov")
  Year <- rep(y+i, nrow(df1))
  df1 <- cbind(df1, Year)
  big_innov_table <- left_join(
    big_innov_table, df1, by = c("inputs", "Country", "innov", "Year"))
  
  df1 <- cbind(inputs, tables_pred_innov[[i]]) |>
    as.data.frame() |>
    pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov")
  Year <- rep(y+i, nrow(df1))
  df1 <- cbind(df1, Year)
  big_pred_table <- left_join(
    big_pred_table, df1, by = c("inputs", "Country", "innov", "Year"))
}
df <- inner_join(big_innov_table, big_pred_table,
  by = c("inputs", "Country", "Year"),
  suffix = c(".obs", ".pred")) #table of observed innov and ANNG

# Add sector of each industry
wiot <- periods[[i]]
codes <- wiot |> select(`Industry Code`, Industry)
codes <- codes[(6:40),]
sector <- c("Agriculture", rep("Industry", 16), rep("Services", 35-17))
sector <- cbind(sector, codes)
df <- left_join(df, sector, by = c("inputs"="Industry"))
saveRDS(df, file = "ANNGandObs.Rds")
