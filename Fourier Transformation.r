# Complex Wave
xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

###
# Combination
wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)

###
# Fourier Series
wave.4 <- wave.3
wave.4[wave.3>0.5] <- 0.5
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)

###
# Repating Pattern
repeat.xs     <- seq(-2*pi,0,pi/100)
wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs,wave.3,type="l"); title("Repeating pattern")
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)

###
# Fourier Series
plot.fourier <- function(fourier.series, f.0, ts) { 
  w <- 2*pi*f.0 
  trajectory <- sapply(ts, function(t) fourier.series(t,w)) 
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
} 

# An eg
plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100)) 

###
# And the plotting of equation f(t)=0.5×sin(3wt)+0.25×sin(10wt):
acq.freq <- 100                    # data acquisition frequency (Hz); sampling rate
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time                 # fundamental frequency (Hz) 

dc.component       <- 0
component.freqs    <- c(3,10)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians)
component.strength <- c(.5,.25)    # strength of signal components 

f <- function(t,w) {  
  dc.component +  
    sum( component.strength * sin(component.freqs*w*t + component.delay))  
} 

plot.fourier(f,f.0,ts)   

###
# Phase Shifts
component.delay <- c(pi/2,0)       # delay of signal components (radians)
plot.fourier(f,f.0,ts)

###
# DC Components
# Applying a DC component of −2 to the previous ware would result in the following equation and plot:
# f(t)= −2 + 0.5×sin(3wt + π/2) + 0.25×sin(10wt)
dc.component <- -2
plot.fourier(f,f.0,ts)

###

library(stats)
fft(c(1,1,1,1)) / 4  # to normalize

fft(1:4) / 4  

###

# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}

convert.fft(fft(1:4))

# returns the x.n time series for a given time sequence (ts) and
# a vector with the amount of frequencies k in the signal (X.k)
get.trajectory <- function(X.k,ts,acq.freq) {
  
  N   <- length(ts)
  i   <- complex(real = 0, imaginary = 1)
  x.n <- rep(0,N)           # create vector to keep the trajectory
  ks  <- 0:(length(X.k)-1)
  
  for(n in 0:(N-1)) {       # compute each time point x_n based on freqs X.k
    x.n[n+1] <- sum(X.k * exp(i*2*pi*ks*n/N)) / N
  }
  
  x.n * acq.freq 
}

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  # plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
  Xk.h <- rep(0,length(Xk))
  Xk.h[i+1] <- Xk[i+1] # i-th harmonic
  harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
  points(ts, harmonic.trajectory, type="l", col=color)
}

X.k <- fft(c(4,0,0,0))                   # get amount of each frequency k: 0 Hz, 1 Hz, 2 Hz

time     <- 4                            # measuring time interval (seconds)
acq.freq <- 100                          # data acquisition frequency (Hz)
ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 

x.n <- get.trajectory(X.k,ts,acq.freq)   # create time wave

plot(ts,x.n,type="l",ylim=c(-2,4),lwd=2)
abline(v=0:time,h=-2:4,lty=3); abline(h=0)

plot.harmonic(X.k,1,ts,acq.freq,"red")
plot.harmonic(X.k,2,ts,acq.freq,"green")
plot.harmonic(X.k,3,ts,acq.freq,"blue")

# Examples
# f(t)=2+0.75×sin(3wt)+0.25×sin(7wt)+0.5×sin(10wt)

acq.freq <- 100                    # data acquisition (sample) frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
f.0 <- 1/time

dc.component <- 1
component.freqs <- c(3,7,10)        # frequency of signal components (Hz)
component.delay <- c(0,0,0)         # delay of signal components (radians)
component.strength <- c(1.5,.5,.75) # strength of signal components

f   <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

plot.fourier(f,f.0,ts=ts)

# Let’s assume that we don’t know the functional form of trajectory, 
# we only have its contents, the period and the sampling time points:

w <- 2*pi*f.0
trajectory <- sapply(ts, function(t) f(t,w))
head(trajectory,n=30)

# So, given that trajectory we can find where the frequency peaks are:
X.k <- fft(trajectory)                   # find all harmonics with fft()
plot.frequency.spectrum(X.k, xlimits=c(0,20))

# And if we only had the frequency peaks we could rebuild the signal:
x.n <- get.trajectory(X.k,ts,acq.freq) / acq.freq  # TODO: why the scaling?
plot(ts,x.n, type="l"); abline(h=0,lty=3)
points(ts,trajectory,col="red",type="l") # compare with original

# This function is just for presentation purposes:
plot.show <- function(trajectory, time=1, harmonics=-1, plot.freq=FALSE) {
  
  acq.freq <- length(trajectory)/time      # data acquisition frequency (Hz)
  ts  <- seq(0,time-1/acq.freq,1/acq.freq) # vector of sampling time-points (s) 
  
  X.k <- fft(trajectory)
  x.n <- get.trajectory(X.k,ts, acq.freq=acq.freq) / acq.freq
  
  if (plot.freq)
    plot.frequency.spectrum(X.k)
  
  max.y <- ceiling(1.5*max(Mod(x.n)))
  
  if (harmonics[1]==-1) {
    min.y <- floor(min(Mod(x.n)))-1
  } else {
    min.y <- ceiling(-1.5*max(Mod(x.n)))
  }
  
  plot(ts,x.n, type="l",ylim=c(min.y,max.y))
  abline(h=min.y:max.y,v=0:time,lty=3)
  points(ts,trajectory,pch=19,col="red")  # the data points we know
  
  if (harmonics[1]>-1) {
    for(i in 0:length(harmonics)) {
      plot.harmonic(X.k, harmonics[i], ts, acq.freq, color=i+1)
    }
  }
}

# A decreasing signal:
trajectory <- 4:1
plot.show(trajectory, time=2)

# A staircase signal:
trajectory <- c(rep(1,5),rep(2,6),rep(3,7))
plot.show(trajectory, time=2, harmonics=0:3, plot.freq=TRUE)

# A seesaw:
trajectory <- c(1:5,2:6,3:7)
plot.show(trajectory, time=1, harmonics=c(1,2))

# Assume this time-series with a strong noise component:
set.seed(101)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)
trajectory <- 3*rnorm(101) + 3*sin(3*w*ts)
plot(trajectory, type="l")

# We can check if there’s some harmonic hidden in it (there is one, 3Hz harmonics):
X.k <- fft(trajectory)
plot.frequency.spectrum(X.k,xlimits=c(0,acq.freq/2))

# And we find a peak at the 3 Hz harmonics, as expected!
# There are several R libraries (surprise!) that produce this type of frequency plots. 
# Here’s one eg (the results are not exactly the same, which might be the consequence of slightly different algorithms…):
library(GeneCycle)

f.data <- GeneCycle::periodogram(trajectory)
harmonics <- 1:(acq.freq/2)

plot(f.data$freq[harmonics]*length(trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

# Check also stats::spectrum() and TSA::periodogram().
# If there is a trend in the time series, it should be detrended.
# Eg:

trajectory1 <- trajectory + 25*ts # let's create a linear trend 
plot(trajectory1, type="l")

f.data <- GeneCycle::periodogram(trajectory1)
harmonics <- 1:20
plot(f.data$freq[harmonics]*length(trajectory1), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

# The trended time-series didn’t capture the signal.
# Let’s detrended it know, ie, find the linear trend and work with the residuals:

trend <- lm(trajectory1 ~ts)
detrended.trajectory <- trend$residuals
plot(detrended.trajectory, type="l")

f.data <- GeneCycle::periodogram(detrended.trajectory)
harmonics <- 1:20
plot(f.data$freq[harmonics]*length(detrended.trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

# Now the signal was caught!
# Also, if we are trying to identify signals of n×F Hz frequencies, 
# the time series length should be divisible by F, 
# i.e., we must do something like detrended.trajectory[-(1:(length(detrended.trajectory) %% F))].
# Let’s try with a real dataset downloaded from Quandl from retail prices of gasoline from 1995 until the present.
library(zoo) # use: index() converts Date to its index 

prices <- read.csv("retailgas.csv")       # weekly prices (1 Hz = 1 Week)
prices <- prices[order(nrow(prices):1),]  # revert data frame
plot(prices, type="l")

trend <- lm(Price ~ index(Date), data = prices)
abline(trend, col="red")

detrended.trajectory <- trend$residuals
plot(detrended.trajectory, type="l", main="detrended time series")

f.data <- GeneCycle::periodogram(detrended.trajectory)
harmonics <- 1:20 
plot(f.data$freq[harmonics]*length(detrended.trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

# And we are able to see that the stronger signals are the 1Hz, 2Hz and 3Hz which makes some sense. 
# The wages have a monthly or two-week cycle that reflects on the first harmonics 
# (1 Hz here corresponds to 1 week cycle, 2 Hz means every 2 weeks, etc.).