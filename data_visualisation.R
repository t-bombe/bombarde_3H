library(tidyverse)
library(glmnet)

setwd(  "C:\\Users\\TomBo\\Documents\\2023\\2023 Math Project III\\output")
df <- readRDS(
  "C:\\Users\\TomBo\\Documents\\2023\\2023 Math Project III\\output\\ANNGandObs.Rds")
tmp <- sapply(df |> select(-c(innov.obs, innov.pred)), as.factor)
df <- cbind(tmp, select(df, innov.obs), select(df, innov.pred)) |> 
  as.data.frame() |> mutate("d2" = (innov.obs - innov.pred)^2)
sum(is.na(select(df, c(innov.obs, innov.pred))))#

df <- df |> filter(Country != "BGR", Country != "RUS", Country != "TUR", is.na(sector)==FALSE)
# general
fit <- lm(innov.obs ~ innov.pred, df)
summary(fit)

# taking only positive
tmp <- df |> filter(innov.obs >= 0 & innov.pred >= 0)
fit <- lm(innov.obs ~ innov.pred, tmp)
summary(fit)

# industry
dfi <- df |> filter(sector == "Industry")
fiti <- lm(innov.obs ~ innov.pred, dfi)
summary(fiti)

# agriculture
dfa <- df |> filter(sector == "Agriculture")
fita <- lm(innov.obs ~ innov.pred, dfa)
summary(fita)

# services
dfs <- df |> filter(sector == "Services")
fits <- lm(innov.obs ~ innov.pred, dfs)
summary(fits)

# by industry
input_names <- unique(df$all_inputs)
sector <- df$sector
input_names <- input_names[(1:35)]
indusreg <- data.frame(input_names, 
                       "R2" = rep(0,35),
                       "estimate" = rep(0,35),
                       "ttest" =  rep(0,35))


for (i in (1:35)){
  tmp <- df |> filter(all_inputs == input_names[i])
  fit <- lm(tmp$innov.obs ~ tmp$innov.pred)
  results <- summary(fit)
  indusreg[i,(2:4)] <- c(results$r.squared, results$coefficients[2,1], results$coefficients[2,4])
  print(results)
}
indusreg <- left_join(indusreg, unique(select(df, all_inputs, sector)), by = c("input_names" = "all_inputs"))
                      

indusreg <- arrange(indusreg, desc(R2))
names(indusreg) <- c("Industry", "R-squared", "ANNG-Coefficient", "Pr(>|t|)", "Sector")
library(xtable)
print(xtable(indusreg, type = "latex"), file = "indusreg.tex")
tmp <- df |> filter(inputs == "Post and Telecommunications")
ggplot(tmp,aes(x=innov.pred, y=innov.obs)) + geom_point()
tmp <- df |> filter(inputs == "Private Households with Employed Persons")
ggplot(tmp,aes(x=innov.pred, y=innov.obs)) + geom_point()
tmp <- df |> filter(inputs == "Transport Equipment")
ggplot(tmp,aes(x=innov.pred, y=innov.obs)) + geom_point()
tmp <- df |> filter(inputs == "Construction")
ggplot(tmp,aes(x=innov.pred, y=innov.obs)) + geom_point()
tmp <- df |> filter(inputs == "Rubber and Plastics")
ggplot(tmp,aes(x=innov.pred, y=innov.obs)) + geom_point()

# by country
country_names <- unique(df$Country)
len <- length(country_names)
countryreg <- data.frame(country_names,
                       "R2" = rep(0,len),
                       "estimate" = rep(0,len),
                       "ttest" =  rep(0,len))
for (i in (1:length(country_names))){
  C = country_names[i]
  tmp <- df |> filter(Country == C)
  fit <- lm(tmp$innov.obs ~ tmp$innov.pred)
  results <- summary(fit)
  countryreg[i,(2:4)] <- c(results$r.squared, results$coefficients[2,1], results$coefficients[2,4])
  print(results)
  ggplot(df |> filter(Country != C),aes(x=innov.pred, y=innov.obs)) +
    geom_point() +
    labs(x = "Average nearest neighbour 10-year growth rate", y = "10-year Industry growth rate")
}
countryreg <- arrange(countryreg, desc(R2))


# Plots
ggplot(df,aes(x=innov.pred, y=innov.obs, col=sector)) +
  geom_point() +
  facet_wrap(vars(Country)) +
  labs(x = "Average nearest neighbour 10-year growth rate", y = "10-year Industry growth rate")

ggplot(df |> filter(sector == "Services"),aes(x=innov.pred, y=innov.obs, col = Country)) +
  geom_point() +
  facet_wrap(vars(Country)) +
  labs(x = "Average nearest neighbour 10-year growth rate", y = "10-year Industry growth rate")

ggplot(df |> filter(sector == "Agriculture"),aes(x=innov.pred, y=innov.obs)) +
  geom_point() + +
  facet_wrap(vars(Country)) +
  labs(x = "Average nearest neighbour 10-year growth rate", y = "10-year Industry growth rate")

ggplot(df |> filter(sector == "Industry"),aes(x=innov.pred, y=innov.obs)) +
  geom_point() + +
  facet_wrap(vars(Country)) +
  labs(x = "Average nearest neighbour 10-year growth rate", y = "10-year Industry growth rate")



ggplot(df |> filter(sector=="Industry"), aes(x=innov.pred, y=innov.obs, col=Country)) + geom_point() + labs(x = "Average nearest neighbour 10-year growth rate", y = "10-year Industry growth rate")

LTU
SWE
AUS
ESP
CYP
FIN
heavy loaded negative corr: TUR
tail(countries)


#by year
years <- unique(df$Year) |> as.vector() # careful you missed year
yearcor <- data.frame(years, rep(0,3))
for (i in (1:3)){
  tmp <- df |> filter(Year==as.numeric(years[i]))
  yearcor[i,2] <- cor(tmp$innov.pred, tmp$innov.obs)
}
tmp <- df |> filter(Year==2005)
cor(tmp$innov.obs, tmp$innov.pred)




