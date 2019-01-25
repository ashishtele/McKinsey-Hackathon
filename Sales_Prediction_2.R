
df_2 %>% 
  mstl() %>% 
  autoplot()

df_3 <- ts(train_1, frequency = 12)
df_4 <- ts(test_1)
glimpse(df)
autoplot(df_3[,c(3,7)], facet = TRUE)

fit_nat <- auto.arima(df_3[,"sales"],
                      xreg = df_3[,"is_national"])
fit_nat # -1360
residuals(fit_nat, type = "innovation") %>% 
  autoplot()
forecast(fit_nat, 
         xreg = df_4[,"is_national"],
         h = 180) %>% 
  autoplot()

xreg <- cbind(oil = df_3[,"oil_price"],
              promo = df_3[,"promotions"],
              c_promo = df_3[,"comp_prom"],
              nat = df_3[,"is_national"],
              local = df_3[,"is_local"])
fit_all <- auto.arima(df_3[,"sales"], 
                      xreg = xreg)
fit_all #-1406
  residuals(fit_all, type = "innovation") %>% 
  autoplot()

forecast(fit_all, 
         xreg = xreg,
         h = 10) %>% 
  autoplot()
checkresiduals(fit_all)



plots <- list()
for (i in 4:6) {
  fit <- auto.arima(df_3[,"sales"], xreg = fourier(df_3[,"sales"], K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(df_3[,"sales"], K=i, h=24))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("") + ylim(1.5,4.7)
}

library(TSA)
p <- periodogram(df_3[,"sales"])
data.table(period=1/p$freq, spec=p$spec)[order(-spec)][1:2]

fit0 <- auto.arima(train_1$sales)
(bestfit <- list(aicc=fit0$aicc, i=0, j=0, fit=fit0))

for(i in 1:2) {
  for (j in 1:1){
    z1 <- fourier(ts(train_1$sales, frequency=7.02), K=i)
    z2 <- fourier(ts(train_1$sales, frequency=3.49), K=j)
    fit <- auto.arima(train_1$sales, xreg=cbind(z1, z2), seasonal=F)
    if(fit$aicc < bestfit$aicc) {
      bestfit <- list(aicc=fit$aicc, i=i, j=j, fit=fit)
    }
  }
}
bestfit #-1563

fc <- forecast(bestfit$fit, 
               xreg=cbind(
                 fourier(ts(train_1$sales, frequency=7.02), K=bestfit$i, h=180),
                 fourier(ts(train_1$sales, frequency=3.49), K=bestfit$j, h=180)))
plot(fc)

fc.tbats <- forecast(tbats(train_1$sales, seasonal.periods=c(7.02, 3.49)), h=180)
plot(fc.tbats)
accuracy(fc.tbats, test_1$sales)
accuracy(fc, test_1$sales)







