
# Set up table of average innovation rate accross periods
tmp <- names(tables_innov[[1]])
names <- data.frame("countries" = tmp)
for (t in (2:(length(tables_innov)))){
  tmp <- names(tables_innov[[t]])
  newnames <- data.frame("countries" = tmp)
  names <- inner_join(names,newnames, by = c("countries"))
} # extract names of relevant countries
avrg_innov <- matrix(0, nrow = n, ncol = nrow(names))
avrg_pred_innov <- matrix(0, nrow = n,ncol = nrow(names))
# Compute average innovation rate accross periods
for (t in (1:length(tables_innov))){
  avrg_innov <- avrg_innov + as.matrix(select(tables_innov[[t]], names[,1]))
  avrg_pred_innov <- avrg_pred_innov + as.matrix(select(tables_pred_innov[[t]], names[,1]))
}
# Format tables
avrg_innov <- cbind(inputs, avrg_innov/length(tables_innov) ) 
avrg_pred_innov <- cbind(inputs, avrg_pred_innov/length(tables_pred_innov))
avrg_innov <- avrg_innov |>
  as.data.frame() |>
  pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov") |> 
  pivot_wider(names_from = inputs, values_from = "innov")
avrg_pred_innov <- avrg_pred_innov |>
  as.data.frame() |>
  pivot_longer(cols = !inputs, names_to = "Country", values_to = "innov") |> 
  pivot_wider(names_from = inputs, values_from = "innov") 

#Compute correlation accross industries
R2 <- data.frame("R2"=rep(0,35))
coef <- data.frame("coef"=rep(0,35))
intercept <- data.frame("intercept"=rep(0,35))
tstat <- data.frame("tstat"=rep(0,35))
for (i in (1:35)){
  x <- avrg_innov[,i+1] |> sapply(as.numeric)
  y <- avrg_pred_innov[,i+1] |> sapply(as.numeric)
  M <- lm(x~y)
  R2[i,1] <- summary(M)$r.squared
  coef[i,1] <- summary(M)$coefficients[2,1]
  intercept[i,1] <- summary(M)$coefficients[1,1]
  tstat[i,1] <- summary(M)$coefficients[2,4]
}

names(summary(M))
explained_innov <- data.frame("inputs" = inputs, "R2" = R2, coef, intercept, tstat) 
explained_innov <- explained_innov |> arrange(desc(R2))
#do these fit with industries and their highest output multiplier?

par(mfrow=c(1,1))

"Compute correlation of average 1 year observed and predicted 1-year productivity improvements accross countries"
corr <- rep(0, n)
for (i in (2:n)){
  x <- avrg_innov[,i] |> sapply(as.numeric)
  y <- avrg_pred_innov[,i] |> sapply(as.numeric)
  corr[i] <- cor(x,y)
}

correlation <- data.frame("Industry" = inputs, "corr_avrg_nbr"= as.numeric(corr)) 

correlation <- correlation |> arrange(desc(corr_avrg_nbr))

"Compute correlation of average 1 year observed and predicted 1-year productivity improvements accross high income countries"
highincome <- c("AUS", "AUT", "BEL", "CAN", "ESP", "FIN", "GBR", "ITA", "JPN", "NLD", "SWE", "USA")

"Compute correlation of average 1 year observed and predicted 1-year productivity improvements accross low countries"
lowincome <- c("BGR", "BRA", "CZE", "EST", "HUN", "LTU", "LVA", "MEX", "SVN")
"Row"

#coefs are signficantly diffrent from 0
plot(avrg_innov$`Rubber and Plastics`, avrg_pred_innov$`Rubber and Plastics`)
abline(explained_innov$intercept[1], explained_innov$coef[1])
plot(avrg_innov$Construction, avrg_pred_innov$Construction)
abline(explained_innov$intercept[2], explained_innov$coef[2])
plot(avrg_innov$`Coke, Refined Petroleum and Nuclear Fuel`, avrg_pred_innov$`Coke, Refined Petroleum and Nuclear Fuel`)
abline(explained_innov$intercept[3], explained_innov$coef[3])
#too few observations