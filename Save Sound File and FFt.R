library("audio")
library("signal")
library(randomForest) # library for randomForest

a=load.wave("Throw.wav")          # play(a[1:100000]); plot(a[1:100000], type="l");
#a=load.wave("30hzfar.wav")
sec  = 2                               # Number of seconds to study in each split
rate = 44100                             # Number of samples per second


Sound_rate_44100 =   a[((rate*sec)+1):(rate*(sec+1))]
Sound_30_Hz_low_pass_filter_rate_44100 = Sound_rate_44100

bf <- butter(3, (30/rate))      
Sound_30_Hz_low_pass_filter_rate_44100  <- filter(bf, Sound_rate_44100)    

#write.csv(Sound_30_Hz_low_pass_filter_rate_44100[1:1000], file="24_7/Throw_LPF.csv")


##################################################
# Apply FFT                                     #
##################################################

window_size = 681
rawFeats = data.frame()
for(i in 1:(1000 - window_size + 1)) { 
  FFT_Sound_Mobile <- Mod(fft(Sound_30_Hz_low_pass_filter_rate_44100[i:(i + window_size - 1)]))
  rawFeats = rbind(rawFeats, FFT_Sound_Mobile[2:ceiling(window_size/2)])
} 

write.csv(rawFeats[,1:3], file="25_7/thrown_RawFeats_LPF_second_2.csv")

