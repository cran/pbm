## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show='hold',fig.height=3.5,fig.width=5,fig.cap = "Example of a 1:1 binding curve."----
library(pbm)
time <- seq(0, 1000)
response <- binding1to1(time, 500, 6e-7, 10000, 0.01, 0.8)
plot(time, response, type = "l")

## ---- fig.show='hold',fig.height=3.5,fig.width=5,fig.cap = "Example of a 1:1 binding curve with baseline drift."----
library(pbm)
time <- seq(0, 1000)
response <- binding1to1(time, 500, 6e-7, 10000, 0.01, 0.8, drift = 1e-04)
plot(time, response, type = "l")

## ---- fig.show='hold',fig.height=3.5,fig.width=5,fig.cap = "Example of a 2:1 binding curve."----
library(pbm)
time <- seq(0, 1000)
response <- binding2to1(time, 500, 6e-7, 10000, 0.01, 0.5, 2500, 0.001, 0.3)
plot(time, response, type = "l")

## ---- fig.show='hold',fig.height=3.5,fig.width=5,fig.cap = "Example of 2:1 heterogeneous model fit."----
library(pbm)

# Generate example binding data with noise
time <- seq(0, 1000)
response <- binding2to1(time, 500, 6e-7, 10000, 0.01, 0.5, 2500, 0.001, 0.3)
noisyresponse <- jitter(response, amount = 0.02)
data <- data.frame(time, noisyresponse)
names(data) <- c("x", "y")

# Fit a nlm to binding data
startingvalues <- list(kon1 = 70000, koff1 = 0.01, rmax1 = 0.3, kon2 = 9000, koff2 = 0.004, rmax2 = 0.3)
fit <- nls(y ~ binding2to1(x, 500, 6e-7, kon1, koff1, rmax1, kon2, koff2, rmax2),
  data = data,
  start = startingvalues)

# Plot the fitted model
plot(data$x, data$y, type = "p", pch = 4, cex = 0.5)
par(col = "red", lwd = 3)
lines(data$x, predict(fit, list(x = data$x)))

## ---- echo=FALSE, results='asis'----------------------------------------------
knitr::kable(t(coefficients(fit)))

## -----------------------------------------------------------------------------
# Choose a range of analyte concentrations and give known parameters.
conc_range <- c(6e-7, 3e-7, 1.75e-7, 8.75e-8, 2.916e-8)
kon  <- 10000
koff <- 0.01

# Calculate the time to equilibrium for each concentration.
tteq(conc_range, kon, koff)

## ---- fig.show='hold',fig.height=3,fig.width=7,fig.cap = "Selecting an appropriate association time."----
library(ggplot2)
library(gridExtra)

t <- seq(0, 300)
t0 <-  median(t)
plot1 <- ggplot()
for (conc in conc_range) {
  curve <- binding1to1(t, t0, conc, 10000, 0.01, 1)
  plot1 <- plot1 + geom_line(aes_string(x = t, y = curve))
}

t <- seq(0, 600)
t0 <-  median(t)
plot2 <- ggplot()
for (conc in conc_range) {
  curve <- binding1to1(t, t0, conc, 10000, 0.01, 1)
  plot2 <- plot2 + geom_line(aes_string(x = t, y = curve))
}

plot1 <- plot1+labs(x = "Time (s), t0 = 150")
plot1 <- plot1+labs(y = "Response")
plot2 <- plot2+labs(x = "Time (s), t0 = 300")
plot2 <- plot2+labs(y = "Response")

grid.arrange(plot1, plot2, ncol = 2)

