# Week 3 exercise

shiller <- readxl::read_xlsx("Shiller16.xlsx")
shiller["LD"] = log(shiller$ND)
shiller["LE"] = log(shiller$NE)
shiller["PO"] = shiller$ND/shiller$NE

plot(shiller$ND, shiller$NE)
plot(shiller$LD, shiller$LE)

# log-log form is a better way of seeing what is going on,
# it looks more linear and has greater dispersion across all values

plot(shiller$PO)
hist(shiller$PO)

# not particularly normal

summary(shiller$PO)
skimr::skim(shiller$PO)
kurt_po <- e1071::kurtosis(shiller$PO, na.rm = TRUE)
skew_po <- e1071::skewness(shiller$PO, na.rm = TRUE)

# high kurtosis for PO and right handed skew

model <- glm(LD ~ LE, data = shiller)
summary(model)

#estimated 0.876 coefficient for LE (SE 0.0109), p value 2e-16

plot(model$residuals)
kurt_res <- e1071::kurtosis(model$residuals)
skew_res <- e1071::skewness(model$residuals)
# not normal residuals, slightly skewd to the right hand side
res <- as.data.frame(model$residuals)
res["lagged"] <- c(model$residuals[2:length(model$residuals)],NA)
names(res) <- c("res","lag")
res_model <- lm(res ~ lag, data = res)
summary(res_model) # shows a significant coefficient for lag
acf(model$residuals) # shows strong acf at lag 1
# Durbin Watson statistic indicates the presence of autocorrelation

shiller["lag_LD"] <- c(shiller$LD[2:length(shiller$LD)], NA)
shiller["lag_LE"] <- c(shiller$LE[2:length(shiller$LE)], NA)
shiller["t"] <- 1:length(shiller$LE)

ts_model <- glm(LD ~ lag_LD + lag_LE + LE + t, data=shiller)
summary(ts_model)

# when accounting for lagged LE and lagged LD, LE is no longer significant, 
# there is also no significant trend. both lagged LE and lagged LD are significant

plot(ts_model$residuals) # these look better
e1071::kurtosis(ts_model$residuals) # still high kurtosis, not normal
e1071::skewness(ts_model$residuals) # skewness is much lower

hist(ts_model$residuals) # much better!

