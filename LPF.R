library("signal")
library("audio")

a=load.wave("Throw.wav")       
sec  = 10                              # Number of seconds to study in each split
rate = 44100                             # Number of samples per second


Sound_rate_44100 =   a[400000:(rate*sec)]

bf <- butter(3, (30/rate)) 
Sound_30_Hz_low_pass_filter_rate_44100  <- filter(bf, Sound_rate_44100[1:10000])  

t <- seq(0, 1, len = 44100) 


fs = 44100
fc = 30
y<- seq(1,10000)
y[1] = 0
#y[1] = Sound_rate_44100[1]

alpha =1/(fs+fc)   
# alpha = 1 / sqrt(1 + (fs +fc)^6)
# alpha = 1/ sqrt((1+ fs/fc) * (1+(fs/fc) +(fs/fc)^2)) 
for(n in 2:10000 ){
   y[n] =  y[n-1] + alpha* (Sound_rate_44100[n] - y[n-1])
   #y[n] =  alpha* (Sound_rate_44100[n] )
}





plot(t[1:10000], Sound_30_Hz_low_pass_filter_rate_44100[1:10000], col = "green", type = "l")
lines(t[1:10000], y[1:10000], col = "red", type = "l")

