
# number of new PhDs
PhD <- c(599,685,651,630,673,728,827,912,1168,1007,1124,1214,1326,1458,1376,1321,1285,1385,1364,1516)
# corresponding year
year <- seq(from=2000,to=2019,by=1)
# shifted time
t <- year-2000
df <- data.frame(PhD,year,t)
df


# Fit the Poisson regression model with log-link
phdmodel <- glm(PhD ~ t, data = df, family = poisson(link = "log"))
print(summary(phdmodel))

# Make predictions for the years 2020-2030

newdata <- data.frame(t = 20:30)
colnames(newdata) <- c("t")
predicts <- predict(phdmodel, newdata, type = "response")

# Plot the point predictions
plot(df$t, df$PhD, xlim = c(0, 30), ylim = c(0, max(df$PhD, predicts)),
     xlab = "Time since 2000 (years)", ylab = "Number of new PhDs")
points(newdata$t, predicts, col = "red", pch = 16)






############## Create the new covariate f
df$f <- exp(df$t - 7.2) / (1 + exp(df$t - 7.2))

# Fit the Poisson regression model with log-link and covariate f
phdmodel2 <- glm(PhD ~ f, data = df, family = poisson(link = "log"))


# Make predictions for the years 2020-2030
newdata <- data.frame(f = exp((20:30) - 7.2) / (1 + exp((20:30) - 7.2)))
colnames(newdata) <- c("f")
predicts2 <- predict(phdmodel2, newdata, type = "response")

# Show point predictions for the years 2020-2030
plot(df$t, df$PhD, xlim = c(0, 30), ylim = c(0, max(df$PhD, predicts2)),
     xlab = "Time since 2000 (years)", ylab = "Number of new PhDs")
points(20:30, predicts2, col = "red", pch = 16)








# Create the new covariate f

data3 <- exp(data.frame(0:30)-7.2)/(1 + exp(data.frame(0:30)-7.2))

colnames(data3) <- c("f")
predicts3 <- predict(phdmodel2, newdata = data3, type = "link", se.fit = TRUE)
ci <- 1.96              # 95% CI

upper <- predicts3$fit + (ci * predicts3$se.fit)



lower <- predicts3$fit - (ci * predicts3$se.fit)

fit <- predicts3$fit


fit2 <- phdmodel2$family$linkinv(fit)

upper2 <- phdmodel2$family$linkinv(upper)
upper2
lower2 <- phdmodel2$family$linkinv(lower)

data3$lower<- lower2
data3$upper<- upper2

lower2

upper2







#######part d###############


# Create the new covariate f

df$f <- exp(df$t - 7.2) / (1 + exp(df$t - 7.2))

# Fit the Poisson regression models
model1 <- glm(PhD ~ f, data = df, family = poisson(link = "log"))
model2 <- glm(PhD ~ f + t, data = df, family = poisson(link = "log"))

# Compare the models using a likelihood ratio test
LRtest <- anova(model1, model2, test = "LRT")
LRtest
#The likelihood ratio test indicates that the more complex model PhD ~ f + t provides 
#a significantly better fit than the simpler model PhD ~ f, with a p-value of 0.0072. 
#Therefore, we can conclude that the variable t is a significant predictor 
#of the number of new PhDs in Norway, beyond the effect of the variable f.










