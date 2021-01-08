library(MASS)
library(ISLR)
library(lattice)
library(hexbin)
library(ggplot2)
library(car)   # vif() and qqPlot functions
library(splines)
library(corrplot)
library(dplyr)

LF <- read.csv("life.csv")
LF
lifedata = LF %>% mutate_all(~ ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
sum(is.na(life))
#View(LF)
varNum <- function(x) {
  val <- 1:ncol(x)
  names(val) <- colnames(x)
  return(val)
}
varNum(lifedata)
lifedata1 <- lifedata[, c(4:22)]
lifedata1
sum(is.na(lifedata1))
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
  lifedata1,
  as.matrix = TRUE,
  xlab = '',
  main = "Life expectancy : Available Variables",
  pscale = 0,
  varname.cex = 0.8,
  axis.text.cex = 0.6,
  axis.text.col = "purple",
  axis.text.font = 2,
  axis.line.tck = .5,
  panel = offDiag,
  diag.panel = onDiag
)

varNum(lifedata1)
lifedata2 <- lifedata1[,-c(3, 5, 7, 9, 11, 13, 15, 16, 17, 18)]
lifedata2

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
  lifedata2,
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
lm.fit <- lm(Life.expectancy ~ Schooling, data = LF)
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
predict(lm.fit)
residuals(lm.fit)
rstandard(lm.fit)
inf <- influence(lm.fit)
range(inf$hat)
confint(lm.fit)
confint(lm.fit, level = .99)
vals = c(3, 5, 7, 12)

predict(lm.fit, data.frame(Schooling = vals),
        interval = "confidence")

predict(lm.fit, data.frame(Schooling = vals),
        interval = "prediction")
attach(lifedata)

plot(Alcohol, Life.expectancy)

plot(Alcohol, Life.expectancy, pch = 20, col = "red")

plot(Alcohol, Life.expectancy, pch = "+", col = "blue")

ggplot(lifedata1, aes(x = Alcohol, y = Life.expectancy)) +
  geom_point(
    shape = 21,
    fill = "red",
    color = "black",
    size = 2
  ) +
  stat_smooth(method = lm,
              color = "blue",
              fill = "cyan") +
  labs(x = "Consumption of alcohol" ,
       y = "Life Expectancy",
       title = "Life Expectancy") + theme(plot.title = element_text(hjust = 0.5))



#-------------------Boxplots----------------

boxplot(life$Life.expectancy,
        main = "boxplot for Life.expectancy",
        xlab = "Life.expectancy",
        ylab = "frequency")
boxplot(life$Adult.Mortality,
        main = "boxplot for Adult.Mortality",
        xlab = "Adult.Mortality",
        ylab = "frequency")
boxplot(life$infant.deaths,
        main = "boxplot for infant.deaths ",
        xlab = "infant.deaths",
        ylab = "frequency")
boxplot(
  life$percentage.expenditure,
  main = "boxplot for percentage.expenditure",
  xlab = "percentage.expenditure",
  ylab = "frequency"
)
boxplot(life$Measles,
        main = "boxplot for Measles",
        xlab = "Measles",
        ylab = "frequency")
boxplot(
  life$under.five.deaths,
  main = "boxplot for under.five.deaths",
  xlab = "under.five.deaths",
  ylab = "frequency"
)
boxplot(life$Diphtheria,
        main = "boxplot for Diphtheria",
        xlab = "Diphtheria",
        ylab = "frequency")
boxplot(life$HIV.AIDS,
        main = "boxplot for HIV.AIDS",
        xlab = "HIV.AIDS",
        ylab = "frequency")
boxplot(life$GDP,
        main = "boxplot for GDP",
        xlab = "GDP",
        ylab = "frequency")
boxplot(life$Polio,
        main = "boxplot for Polio",
        xlab = "Polio",
        ylab = "frequency")
boxplot(life$Population,
        main = "boxplot for Population",
        xlab = "Population",
        ylab = "frequency")
boxplot(
  life$Income.composition.of.resources,
  main = "boxplot for Income.composition.of.resources",
  xlab = "Income.composition.of.resources",
  ylab = "frequency"
)
boxplot(
  life$thinness..1.19.years,
  main = "boxplot for thinness..1.19.years",
  xlab = "thinness..1.19.years",
  ylab = "frequency"
)
boxplot(
  life$thinness.5.9.years,
  main = "boxplot for thinness.5.9.years",
  xlab = "thinness.5.9.years",
  ylab = "frequency"
)
