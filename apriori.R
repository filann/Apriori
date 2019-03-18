library(arules)
library(arulesViz)

data <- read.csv(file="C:/Users/Anna/train_1.csv", header=TRUE, sep=";")
data[["client_sex"]] <- ordered(cut(data[["client_sex"]], c(0, 1, 2)), labels = c("Male", "Female"))
data[["diagnosis"]] <- NULL
write.csv2(data[1:8], file="C:/Users/Anna/train_8.csv", sep=";")
summary(data)

tr <- as(data, "transactions")
summary(tr)

itemFrequencyPlot(tr, support = 0.1, cex.names = 0.7)

rul <- apriori (tr, parameter = list (supp = 0.25, conf = 0.8, target = 'rules'))
write(rul, file = "C:/Users/Anna/result_25_08.csv", sep = ";", col.names = NA)

time <- system.time(rul <- apriori (tr, parameter = list (supp = 0.001, conf = 0.8, target = 'rules')))

rul_lift_down <- subset(rul, lift > 1)


                                    |------------------|
                                    |    sampling      |
                                    |------------------|


supp <- 0.0005
epsilon <- 0.1
c <- 0.2
n <- -2 * log(c)/(supp * epsilon^2)
trSample <- sample(tr, n, replace = TRUE)
timeSample <- system.time(rulSample <- apriori (trSample, parameter = list (supp = supp, conf = 0.8, target = 'rules')))
match <- match(rul, rulSample, nomatch = 0)
sum(match > 0) / length(rul)

itemFrequencyPlot(trSample, population = tr, support = supp, cex.names = 0.7)



                                    |------------------|
                                    |    Vizualaz      |
                                    |------------------|

plot(rul, control = black)

