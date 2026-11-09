library(readxl)

data <- read_excel(file.choose())

data$ln_co2 <- log(data$co2)
data$ln_gdp <- log(data$gdp)

# Figure 1: Linear regression
plot(data$ln_gdp, data$ln_co2,
     xlab = "Log GDP per capita",
     ylab = "Log CO2 emissions per capita",
     main = "Environmental Kuznets Curve - Canada",
     pch = 19)

abline(lm(ln_co2 ~ ln_gdp, data = data),
       col = "red", lwd = 2)

# Quadratic OLS model
data$ln_gdp_sq <- data$ln_gdp^2
model <- lm(ln_co2 ~ ln_gdp + ln_gdp_sq, data = data)

summary(model)

# Figure 2: Quadratic regression
plot(data$ln_gdp, data$ln_co2,
     xlab = "Log GDP per capita",
     ylab = "Log CO2 emissions per capita",
     main = "Environmental Kuznets Curve - Canada",
     pch = 19)

ord <- order(data$ln_gdp)

lines(data$ln_gdp[ord],
      predict(model, newdata = data[ord, ]),
      col = "red", lwd = 2)