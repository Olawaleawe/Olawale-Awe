###################################################
ssh <- suppressPackageStartupMessages
ssh(library(timeSeries))
ssh(library(tseries))
ssh(library(aTSA))
ssh(library(forecast))
ssh(library(rugarch))
ssh(library(ModelMetrics))
ssh(library(keras))
##################################################
data(USDCHF)
length(USDCHF)
data <- ts(USDCHF,start=1960, frequency = 365)
plot(data)
adf.test(data)
pp.test(data )
acf(data)
pacf(data)

data_test <- data[(length(data)-99):length(data)]
data_train <- data[1:(length(data)-99-1)]
model_arima <- auto.arima(data_train)
summary(model_arima)

checkresiduals(model_arima)

arch.test(arima(data_train, order = c(0,1,2)))

model <- character()
AIC <- numeric()
for (p in 1:5){
  for(q in 1:5){
    model_g <- tseries::garch(model_arima$residuals, order = c(p,q), trace=F)
    model<-c(model,paste("mod_", p, q))
    AIC <- c(AIC, AIC(model_g))
    def <- tibble::tibble(model,AIC)
  }
}

def %>% dplyr::arrange(AIC)

model_garch <- tseries::garch(model_arima$residuals, order = c(1,1), trace=F)
## Warning in tseries::garch(model_arima$residuals, order = c(1, 1), trace = F):
## singular information
Box.test(model_garch$residuals)

acf(model_garch$residuals[-1])

Ddata_train <- diff(data_train)
# garchfit <- ugarchfit(data=Ddata_train, spec = garch1, solver = "gosolnp",trace=F)
# coef(garchfit)

#write.csv(df_eval, "df_eval.csv")

maxlen <- 7
exch_matrix<- matrix(0, nrow = length(data_train)-maxlen-1, ncol = maxlen+1) 

for(i in 1:(length(data_train)-maxlen-1)){
  exch_matrix[i,] <- data_train[i:(i+maxlen)]
}
head(exch_matrix)  
       x_train <- exch_matrix[, -ncol(exch_matrix)]
y_train <- exch_matrix[, ncol(exch_matrix)]

dim(x_train)

x_train <- array_reshape(x_train, dim = c((length(data_train)-maxlen-1), maxlen, 1))
dim(x_train)

model <- keras_model_sequential()
model %>% 
  layer_dense(input_shape = dim(x_train)[-1], units=maxlen) %>% 
  layer_simple_rnn(units=10) %>% 
  layer_dense(units = 1)
summary(model)
model %>% compile(
  loss = "mse",
  optimizer= "adam",
  metric = "mae" 
)
#history <- model %>% 
#  fit(x_train, y_train, epochs = 5, batch_size = 32, validation_split=0.1)
#save_model_hdf5(model, "rnn_model.h5")
rnn_model <- load_model_hdf5("rnn_model.h5")

maxlen <- 7
exch_matrix2<- matrix(0, nrow = length(data)-maxlen-1, ncol = maxlen+1) 

for(i in 1:(length(data)-maxlen-1)){
  exch_matrix2[i,] <- data[i:(i+maxlen)]
}

x_train2 <- exch_matrix2[, -ncol(exch_matrix2)]
y_train2 <- exch_matrix2[, ncol(exch_matrix2)]

x_train2 <- array_reshape(x_train2, dim = c((length(data)-maxlen-1), maxlen, 1))
pred <- rnn_model %>% predict(x_train2)
df_eval_rnn <- tibble::tibble(y_rnn=y_train2[(length(y_train2)-99):length(y_train2)],
                              yhat_rnn=as.vector(pred)[(length(y_train2)-99):length(y_train2)])