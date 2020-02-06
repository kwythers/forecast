##### Dynamic harmonic regression #####
##### for longer seasonality periods 
##### seasonality is assumed to be fixed

library(tidyverse)
library(fpp2)

#####  combining Fourier terms for capturing seasonality with ARIMA errors capturing other dynamics in the data
# australian eating out expenditur
cafe04 <- window(auscafe, start=2004)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(cafe04, xreg = fourier(cafe04, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(cafe04, K=i, h=24))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("") + ylim(1.5,4.7)
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)
