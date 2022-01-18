le_data <- readxl::read_xlsx("./LEPCGDP17.xlsx")

##### INITIAL PLOTS 
plot(log(le_data$PCGDP), le_data$LE)
plot(le_data$PCGDP, le_data$LE)

##### MAKE LOG-LEVEL MODEL
le_data_ll <- le_data
le_data_ll$PCGDP <- log(le_data$PCGDP)
linear_model <- glm(PCGDP ~ ., data = le_data_ll[c("PCGDP","LE")])

##### SUMMARISE MODELA AND PLOT RESIDUALS
summary(linear_model)$coefficients
plot(linear_model$residuals)
