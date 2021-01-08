library(MASS)
library(ISLR)
library(lattice)
library(hexbin)
library(ggplot2)
library(car)   # vif() and qqPlot functions
library(splines)
library(corrplot)
RF <- read.csv("RF Data.csv")
RF
View(RF)
varNum <- function(x) {
  val <- 1:ncol(x)
  names(val) <- colnames(x)
  return(val)
}
varNum(RF)
RF1 <- RF[, c(4:20)]
RF1
offDiag <- function(x, y, ...) {
  panel.grid(h = -1, v = -1, ...)
  panel.hexbinplot(
    x,
    y,
    xbins = 15,
    ...,
    border = gray(.7),
    trans = function(x)
      x ^ .5
  )
  panel.loess(x , y, ..., lwd = 2, col = 'red')
}

onDiag <- function(x, ...) {
  yrng <- current.panel.limits()$ylim
  d <- density(x, na.rm = TRUE)
  d$y <- with(d, yrng[1] + 0.95 * diff(yrng) * y / max(y))
  panel.lines(d, col = rgb(.83, .66, 1), lwd = 2)
  diag.panel.splom(x, ...)
}

splom(
  RF1,
  as.matrix = TRUE,
  xlab = '',
  main = "Life expectancy : Selected Variables",
  pscale = 0,
  varname.cex = 0.8,
  axis.text.cex = 0.6,
  axis.text.col = "purple",
  axis.text.font = 2,
  axis.line.tck = .5,
  panel = offDiag,
  diag.panel = onDiag
)
varNum <- function(x) {
  num <- 1:ncol(x)
  names(num) <- names(x)
  return(num)
}
varNum(RF1)
names(RF1[17])

OptimisedRF <- RF[c(2, 4, 6, 7, 8, 10, 11, 12, 13, 16, 17)]
OptimisedRF

set.seed(137)
RF2 <- randomForest(x = OptimisedRF[, -11], y = OptimisedRF[, 11],
                    data = OptimisedRF)
RF2

set.seed(137)
RFselected <- randomForest(Alcohol ~ .,
                           data = OptimisedRF,
                           importance = TRUE,
                           proximity = TRUE)
RFselected
varImpPlot(RFselected)



# hexbin modified to 15 xbins and trans power set to 0.5.
offDiag <- function(x, y, ...) {
  panel.grid(h = -1, v = -1, ...)
  panel.hexbinplot(
    x,
    y,
    xbins = 15,
    ...,
    border = gray(.7),
    trans = function(x)
      x ^ .5
  )
  panel.loess(x , y, ..., lwd = 2, col = 'red')
}

splom(
  OptimisedRF,
  xlab = '',
  main = "Life expectancy: Selected Variables",
  pscale = 0,
  varname.cex = 0.8,
  axis.text.cex = 0.6,
  axis.text.col = "purple",
  axis.text.font = 2,
  axis.line.tck = .5,
  panel = offDiag,
  diag.panel = onDiag
)

splom(
  RF1[, c(1, 4, 8, 11, 17)],
  xlab = '',
  main = "Life Expectancy: Selected Variables",
  pscale = 0,
  varname.cex = 0.8,
  axis.text.cex = 0.6,
  axis.text.col = "purple",
  axis.text.font = 2,
  axis.line.tck = .5,
  panel = offDiag,
  diag.panel = onDiag
)

set.seed(1)
Randomforestplot = randomForest(
  Schooling ~ .,
  data = RF1,
  subset = train,
  mtry = 6,
  importance = TRUE
)
Randomforestplot

yhat.rf = predict(Randomforestplot, newdata = RF1[-train, ])
testMse <- mean((yhat.rf - RF.test) ^ 2)
testMse


den <- mean((yhat.rf - mean(RF.test)) ^ 2)


importance(Randomforestplot)

varImpPlot(Randomforestplot)

set.seed(1)
bag.RF = randomForest(
  Schooling ~ .,
  data = RF1,
  subset = train,
  mtry = 13,
  importance = TRUE
)
bag.RF
yhat.bag = predict(bag.RF, newdata = RF1[-train, ])
plot(yhat.bag, RF.test , las = 1, )
abline(0, 1)
title(main = paste("Life expectancy",
                   "Median Schooling"),
      sep = "\n")
mse <- mean((yhat.bag - RF.test) ^ 2)
mse



set.seed(1)
train = sample(1:nrow(RF1), nrow(RF1) / 2)
rfTrain <-
  randomForest(Schooling ~ .,
               data = RF1,
               subset = train,
               importance = TRUE)
yhat = predict(rfTrain, newdata = RF1[-train, ])
mse <- mean(RF1$Schooling[-train] - yhat) ^ 2
mse


RF.test = RF1[-train, "Schooling"]



den <- mean((RF.test - mean(RF.test)) ^ 2)
pVar <- 100 * (1 - mse / den)
pVar
