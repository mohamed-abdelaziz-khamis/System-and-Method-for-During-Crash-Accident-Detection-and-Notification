library("audio")
library("signal")
library(randomForest) # library for randomForest

#a=load.wave("iPhone6Blink.wav")          # play(a[1:100000]); plot(a[1:100000], type="l");

#sec  = 60                                # Number of seconds to study in each split
#rate = 44100                             # Number of samples per second

#play(a[1:(rate*sec)])                    #1st  minute: Accelerating from 0 To 40KM/H
#play(a[((1*rate*sec)+1):(2*rate*sec)])   #2nd  minute: Constant speed 40KM/H
#play(a[((2*rate*sec)+1):(3*rate*sec)])   #3rd  minute: 3 Bumps
#play(a[((3*rate*sec)+1):(4*rate*sec)])   #4th  minute: Constant speed 
#play(a[((4*rate*sec)+1):(5*rate*sec)])   #5th  minute: 2 Bumps
#play(a[((5*rate*sec)+1):(6*rate*sec)])   #6th  minute: 2 Bumps
#play(a[((6*rate*sec)+1):(7*rate*sec)])   #7th  minute: 3 Small Bumps
#play(a[((7*rate*sec)+1):(8*rate*sec)])   #8th  minute: Constant speed 
#play(a[((8*rate*sec)+1):(9*rate*sec)])   #9th  minute: 1 Bump
#play(a[((9*rate*sec)+1):(10*rate*sec)])  #10th minute: 1 Brake + 1 Strong Bump + 3 Falldowns
#play(a[((10*rate*sec)+1):(11*rate*sec)]) #11th minute: Sensorlog Restarted + 4 Falldowns + Brake
#play(a[((11*rate*sec)+1):(12*rate*sec)]) #12th minute: Constant speed 
#play(a[((12*rate*sec)+1):(13*rate*sec)]) #13th minute: Constant speed 
#play(a[((13*rate*sec)+1):(14*rate*sec)]) #14th minute: Return to E-JUST
#play(a[((14*rate*sec)+1):(15*rate*sec)]) #15th minute: Decelerate till car stop and I moved out of the car
#play(a[((15*rate*sec)+1):(16*rate*sec)]) #16th minute: I am standing with the mobile outside the car
#play(a[((16*rate*sec)+1):(17*rate*sec)]) #17th minute: Three cars passed by me (Doppler effect)
#play(a[((17*rate*sec)+1):(18*rate*sec)]) #18th minute: One car passed by me (Doppler effect)
#play(a[((18*rate*sec)+1):(19*rate*sec)]) #19th minute: Car taking me again 
#play(a[((19*rate*sec)+1):(20*rate*sec)]) #20th minute: Constant speed
#play(a[((20*rate*sec)+1):(21*rate*sec)]) #21th minute: End of trip
#play(a[((21*rate*sec)+1):length(a)])     #22th minute: Stop the video outside the car


a=load.wave("Throw.wav")          # play(a[1:100000]); plot(a[1:100000], type="l");

sec  = 10                                # Number of seconds to study in each split
rate = 44100                             # Number of samples per second

#play(a[1:(rate*sec)])                    #1st  10 seconds: Mobile is thrown one time.
#plot(a[1:(rate*sec)], type="l");
#play(a[((1*rate*sec)+1):(2*rate*sec)])   #2nd  10 seconds: Mobile is thrown one time.
#plot(a[((1*rate*sec)+1):(2*rate*sec)], type="l");
#play(a[((2*rate*sec)+1):(3*rate*sec)])   #3rd  10 seconds: Mobile is thrown one time.
#plot(a[((2*rate*sec)+1):(3*rate*sec)], type="l");

bf <- butter(3, (30/rate))               # If butter(3, 0.1) this means 10 Hz low-pass filter
#t <- seq(0, 1, len = (rate*sec))        # 60 second samples: 44100 samples per second

#par(mfrow=c(4,1))


# 1st 1 second in 2nd minute: Constant speed 40KM/H
#Sin wave with rate 44100 for 10 seconds.
Sound_rate_44100 = sin(2 * pi * 30 * seq(1:441000)/44100) # a[((rate*sec)+1):(rate*(sec+1))]
play(Sound_rate_44100, rate=44100)
a=load.wave("30hzfar.wav")
play(a, rate=44100)
plot(seq(1:1000)/1000,a[1:1000])

plot(seq(1:44100)/44100, Sound_rate_44100, xlab="Time in sec", ylab="sin(2 * pi * 30 * seq(1:44100)/44100)", main = "Sound_rate_44100", type="l")
Sound_30_Hz_low_pass_filter_rate_44100  <- filter(bf, Sound_rate_44100)    

#x  <- a[1:(rate*sec)]                    # 1st  minute: Accelerating from 0 To 40KM/H
#z  <- filter(bf, x)                      # apply filter
# plot(t, x, type = "l")
# lines(t, z, col = "red")
# play(z*100, rate=rate*2)
#f = fft(z) 
# plot(f[1:5000], type="h", main = "1st  minute: Accelerating from 0 To 40KM/H")
# g=Re(fft(f, inverse = TRUE))/length(a[1:5000]) # play(g,rate=rate); plot(g, type="l");
#plot(Re(fft(f, inverse = TRUE))/length(a[1:(rate*sec)]), type="l", 
#     main = "1st  minute: Accelerating from 0 To 40KM/H", ylab = "FFT inverse")

#y <- a[((1*rate*sec)+1):(2*rate*sec)]    # 2nd  minute: Constant speed 40KM/H
#zz <- filter(bf, y)                      # apply filter
#ff = fft(zz) 
# plot(ff[1:5000], type="h", main = "2nd  minute: Constant speed 40KM/H")
#plot(Re(fft(ff, inverse = TRUE))/length(a[((1*rate*sec)+1):(2*rate*sec)]), type="l", 
#     main = "2nd  minute: Constant speed 40KM/H", ylab = "FFT inverse")

#m <- a[((9*rate*sec)+1):(10*rate*sec)]   # 10th minute: 1 Brake + 1 Strong Bump + 3 Falldowns
#zzz <- filter(bf, m)                     # apply filter
#fff = fft(zzz) 
# plot(fff[1:5000], type="h", main = "10th minute: 1 Brake + 1 Strong Bump + 3 Falldowns")
#plot(Re(fft(fff, inverse = TRUE))/length(a[((9*rate*sec)+1):(10*rate*sec)]), type="l", 
#     main = "10th minute: 1 Brake + 1 Strong Bump + 3 Falldowns", ylab = "FFT inverse")

#n <- a[((16*rate*sec)+1):(17*rate*sec)]  # 17th minute: Three cars passed by me (Doppler effect)
#zzzz <- filter(bf, n)                    # apply filter
#ffff = fft(zzzz) 
# plot(ffff[1:5000], type="h", main = "17th minute: Three cars passed (Doppler effect)")
#plot(Re(fft(ffff, inverse = TRUE))/length(a[((16*rate*sec)+1):(17*rate*sec)]), type="l", 
#     main = "17th minute: Three cars passed (Doppler effect)", ylab = "FFT inverse")

Mobile <- read.csv("Mobile_On_Passenger_Chair.csv", header = FALSE)
Engine_Top <- read.csv("Engine_Top.csv", header = FALSE)

#Mobile_Back_Driver_Seat: Start_Fly_Time_s = 0.67; End_Fly_Time_s = 0.7129;
#Mobile_Back_Passenger_Seat: Start_Fly_Time_s = 0.67; End_Fly_Time_s = 0.7177;
#Mobile_Between_Front_Seats: Start_Fly_Time_s = 0.65; End_Fly_Time_s = 0.6967;
#Mobile_On_Passenger_Chair: Start_Fly_Time_s = 0.67; End_Fly_Time_s = 0.6935;
Start_Fly_Time_s = 0.5
End_Fly_Time_s = 0.6935
Nodout_Sampling_Rate_HZ = 10000

Mobile = Mobile[(Start_Fly_Time_s*Nodout_Sampling_Rate_HZ):(End_Fly_Time_s*Nodout_Sampling_Rate_HZ), 12:14]
Engine_Top = Engine_Top[(Start_Fly_Time_s*Nodout_Sampling_Rate_HZ):(End_Fly_Time_s*Nodout_Sampling_Rate_HZ), 12:14]

Mobile_increased_rate = Mobile[1,]
Engine_Top_increased_rate = Engine_Top[1,]

for(i in 1:(nrow(Mobile)-1) )
{
  delta_X = (Mobile[(i+1),1] - Mobile[i,1])/4
  delta_Y = (Mobile[(i+1),2] - Mobile[i,2])/4
  delta_Z = (Mobile[(i+1),3] - Mobile[i,3])/4
  Mobile_increased_rate = rbind(Mobile_increased_rate,                                              
                                          c(Mobile[i,1] + 1*delta_X,
                                            Mobile[i,2] + 1*delta_Y,
                                            Mobile[i,3] + 1*delta_Z), 
                                          c(Mobile[i,1] + 2*delta_X,
                                            Mobile[i,2] + 2*delta_Y,
                                            Mobile[i,3] + 2*delta_Z),
                                          c(Mobile[i,1] + 3*delta_X,
                                            Mobile[i,2] + 3*delta_Y,
                                            Mobile[i,3] + 3*delta_Z),
                                          Mobile[i+1,])
                                               
  delta_X = (Engine_Top[(i+1),1] - Engine_Top[i,1])/4
  delta_Y = (Engine_Top[(i+1),2] - Engine_Top[i,2])/4
  delta_Z = (Engine_Top[(i+1),3] - Engine_Top[i,3])/4
  Engine_Top_increased_rate = rbind(Engine_Top_increased_rate,                                              
                                             c(Engine_Top[i,1] + 1*delta_X,
                                               Engine_Top[i,2] + 1*delta_Y,
                                               Engine_Top[i,3] + 1*delta_Z), 
                                             c(Engine_Top[i,1] + 2*delta_X,
                                               Engine_Top[i,2] + 2*delta_Y,
                                               Engine_Top[i,3] + 2*delta_Z),
                                             c(Engine_Top[i,1] + 3*delta_X,
                                               Engine_Top[i,2] + 3*delta_Y,
                                               Engine_Top[i,3] + 3*delta_Z),
                                             Engine_Top[i+1,])

}

c = 340.29 # Speed of sound at sea level m / s 
Sound_Mobile_increased_rate = data.frame()
for (t in 1:nrow(Mobile_increased_rate)){
  delta_l = sqrt ((Mobile_increased_rate[t,1]-Engine_Top_increased_rate[t,1])^2+
                  (Mobile_increased_rate[t,2]-Engine_Top_increased_rate[t,2])^2+
                  (Mobile_increased_rate[t,3]-Engine_Top_increased_rate[t,3])^2) / 1000 # Nodout coordinates are in mm
  delta_t = (1/c) * delta_l
  if(t-delta_t*44100 > 0)
  #Sound_Mobile_increased_rate = rbind(Sound_Mobile_increased_rate, (1/delta_l) * Sound_30_Hz_low_pass_filter_rate_44100[round(t-delta_t*44100)])
  Sound_Mobile_increased_rate = rbind(Sound_Mobile_increased_rate, (1/delta_l) * Sound_rate_44100[round(t-delta_t*44100)])
  else
    Sound_Mobile_increased_rate = rbind(Sound_Mobile_increased_rate, 0) 
}

plot(seq(181:nrow(Sound_Mobile_increased_rate))/40000, Sound_Mobile_increased_rate[181:nrow(Sound_Mobile_increased_rate),1],
     xlab="Time in sec", ylab="Sin with Doppler Effect and Attenuation", main = "Sound increased rate - sin wave - before and after accident", type="l")
#Peak before accident - Sin wave = 0.7170544
#Peak after accident - Sin wave = 1.578945
#((1.578945 - 0.7170544)/0.7170544) * 100 =  120.1988%

Mobile <- read.csv("2016-05-15_12-48-04.csv")
plot(seq(1:1448)/100, sqrt(Mobile$accelerometerAccelerationX^2+Mobile$accelerometerAccelerationY^2+Mobile$accelerometerAccelerationZ^2),
     xlab="Time in sec", ylab="Throw accelration (m/s^2)", main = "SQRT of ACC_X^2 + ACC_Y^2 + ACC_Z^2", 
     type="l")

#Frequency for window = 60 Hz
#Time for window = 1/60 second 
#Increased rate for window = 40000
#Window size = (1/60) * 40000 = 680
window_size = 681
rawFeats = data.frame()

##################################################
# Apply FFT                                     #
##################################################

#Mobile_Back_Driver_Seat: Start_non_Zero_index = 295;
#Mobile_Back_Passenger_Seat: Start_non_Zero_index = 299;
#Mobile_Between_Front_Seats: Start_non_Zero_index = 184;
#Mobile_On_Passenger_Chair: Start_non_Zero_index = 181;

for(i in 181:(nrow(Sound_Mobile_increased_rate) - window_size + 1)) { 
  FFT_Sound_Mobile <- Mod(fft(Sound_Mobile_increased_rate[i:(i + window_size - 1),]))
  rawFeats = rbind(rawFeats, FFT_Sound_Mobile[2:ceiling(window_size/2)])
} 

rawFeats = cbind(rawFeats, 1) #Mobile is Flying Towards the Motor:

plot((seq(1:length(rawFeats[,1]))*40000/window_size)/1000, rawFeats[,1], xlab="Sample Index * 40000 / Window size (681) in KHZ", ylab="FFT sin with Doppler Effect and Attenuation", main = "FFT sound increased rate - sin wave - before accident", type="l")
write.csv(rawFeats, file="FFT_Sound_Mobile_On_Passenger_Chair_Without_30_Hz_low_pass_filter_after_accident_sin_wave.csv")

#################################################
Sound_Mobile = a[400000:(rate*sec)]
play(Sound_Mobile) #The flying of the mobile
Sound_30_Hz_low_pass_filter_rate_44100  <- filter(bf, Sound_Mobile)


for(i in 1:(length(Sound_30_Hz_low_pass_filter_rate_44100) - window_size + 1)) { 
  FFT_Sound_Mobile <- Mod(fft(Sound_30_Hz_low_pass_filter_rate_44100[i:(i + window_size - 1)]))
  rawFeats = rbind(rawFeats, FFT_Sound_Mobile[2:ceiling(window_size/2)])
} 

rawFeats = cbind(rawFeats, 0) #Mobile is Flying Towards the Motor: But no accident

plot((seq(1:length(rawFeats[,1]))*44100/window_size)/1000, rawFeats[,1], xlab="Sample Index * 44100 / Window size (681) in KHZ", ylab="FFT sound 44100 rate - Throw - no accident", main = "FFT sound 44100 rate - Throw - no accident - with 30 HZ low pass filter", type="l")
write.csv(rawFeats, file="FFT_Sound_Mobile_Thrown_With_30_Hz_low_pass_filter.csv")

#################################################

sin_wave = sin(2 * pi * 30 * seq(1:44100)/44100)
plot(seq(1:44100)/44100, sin_wave, xlab="Time in sec", ylab="sin(2 * pi * 30 * seq(1:44100)/44100)", main = "sin(2 * pi * 30 * seq(1:44100)/44100)", type="l")
window_size = 681
rawFeats = data.frame()
for(i in 1:8820) { # (length(sin_wave) - window_size + 1)
  #8820 = 44100 / 5 = 0.2 sec
  FFT_sin_wave <- Mod(fft(sin_wave[i:(i + window_size - 1)]))
  rawFeats = rbind(rawFeats, FFT_sin_wave[2:ceiling(window_size/2)])
}
write.csv(rawFeats, file="FFT_sin_wave_window_size_681.csv")
plot(seq(1:340)/340, rawFeats[1000,], xlab="Frequency in HZ", ylab="FFT 1000th window rawFeats[1000,]", main = "FFT sin 44100 rate - sin wave", type="l")
plot(seq(1:8820)/8820, rawFeats[,100], xlab="Sample Index * 44100 / Window size (681) in HZ", ylab="FFT sin (2 * pi * 30 * seq(1:44100)/44100)", main = "FFT sound 44100 rate - sin wave", type="l")

FFT_sin_wave <- Mod(fft(sin_wave))
plot(seq(1:44100)/44100, FFT_sin_wave, xlab="Frequency in HZ", ylab="FFT sin (2 * pi * 30 * seq(1:44100)/44100)", main = "FFT sin 44100 rate - sin wave", type="l")

#Plot FFT with HZ units
plot(log(fft[1:length(fft)/2])*20, xlab="Samples", ylab="Strength", type="l", col="blue", xlim=c(0,50), main="Spectrum",
     lwd=2, axes=F)# don't plot the default axes
#build our own axes with labels
box()
axis(2)
ticks <- axTicks(1)
axis(1,at=ticks,labels=paste0(format(ticks),'Hz'))

#################################################

#FFT_Sound_Mobile_Thrown_With_30_Hz_low_pass_filter.csv
#Sound with 30_Hz_low_pass_filter/FFT_Sound_Mobile_Back_Driver_Seat.csv
#Sound with 30_Hz_low_pass_filter/FFT_Sound_Mobile_Back_Passenger_Seat.csv
#Sound with 30_Hz_low_pass_filter/FFT_Sound_Mobile_Between_Front_Seats.csv
#Sound with 30_Hz_low_pass_filter/FFT_Sound_Mobile_On_Passenger_Chair.csv

#FFT_Sound_Mobile_Thrown_Without_30_Hz_low_pass_filter.csv
#Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_Back_Driver_Seat.csv
#Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_Back_Passenger_Seat.csv
#Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_Between_Front_Seats.csv
#Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_On_Passenger_Chair.csv

rawFeats = read.csv("FFT_Sound_Mobile_Thrown_Without_30_Hz_low_pass_filter.csv")
rawFeats = rawFeats[1:20000,c(2:4,ncol(rawFeats))]
colnames(rawFeats) = c("First Harmonic", "Second Harmonic", "Third Harmonic", "Target")

rawFeats1 = read.csv("Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_Back_Driver_Seat.csv")
rawFeats1 = rawFeats1[,c(2:4,ncol(rawFeats1))]
colnames(rawFeats1) = c("First Harmonic", "Second Harmonic", "Third Harmonic", "Target")

rawFeats2 = read.csv("Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_Back_Passenger_Seat.csv")
rawFeats2 = rawFeats2[,c(2:4,ncol(rawFeats2))]
colnames(rawFeats2) = c("First Harmonic", "Second Harmonic", "Third Harmonic", "Target")

rawFeats3 = read.csv("Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_Between_Front_Seats.csv")
rawFeats3 = rawFeats3[,c(2:4,ncol(rawFeats3))]
colnames(rawFeats3) = c("First Harmonic", "Second Harmonic", "Third Harmonic", "Target")

rawFeats4 = read.csv("Sound without 30_Hz_low_pass_filter/FFT_Sound_Mobile_On_Passenger_Chair.csv")
rawFeats4 = rawFeats4[,c(2:4,ncol(rawFeats4))]
colnames(rawFeats4) = c("First Harmonic", "Second Harmonic", "Third Harmonic", "Target")

rawFeats = rbind(rawFeats, rawFeats1, rawFeats2, rawFeats3, rawFeats4)

##################################################
# READING TRAINING (TRN) AND TEST (TST) DATASETS #
##################################################

itarget     = ncol(rawFeats)                 # target index: accident label (last column)          
nfeats       = itarget - 1    	              # Without accident label (last column)

nrawdata    = nrow(rawFeats)
ntstdata    = round(nrawdata/5) 		        # number of records for testing
ntrndata 	  = nrawdata - ntstdata           # number of records for training

seed		= 1			
irawdata	= seq(1,nrawdata,1)

set.seed(seed)		
irawdata	= sample(irawdata)[1:nrawdata]		# shuffle raw records

itrain = irawdata[1:ntrndata]
trainTarget = rawFeats[itrain, itarget]
rawtrainFeats  = rawFeats[itrain, 1:nfeats]

itest		= irawdata[(ntrndata+1):nrawdata]
testTarget  = rawFeats[itest, itarget] 
rawtestFeats   = rawFeats[itest, 1:nfeats]

noAccidentCount = table(trainTarget)[1]
severeAccidentCount = table(trainTarget)[2]



##################################################
# SELECTING RF WITH BEST INTERNAL VALIDATION     #
# (RF-SCORE)    				 #
##################################################
# Tree Ensembles: Random Forest (RF)
# Reference: Big Data Machine Learning: Patterns for Predictive Analytics, By Ricky Ho, DZone Inc.
# In "bagging", we take a subset of training data (pick n random sample out of N training data, with replacement) to train up each model. 
# After multiple models are trained, we use a voting scheme to predict future data.
# Random Forest is one of the most popular bagging models; in addition to selecting n training data out of N at each decision node of the tree, 
# it randomly selects m input features from the total M input features (m ~ M^0.5). 
# Then it learns a decision tree from that. 
# Finally, each tree in the forest votes for the result.

set.seed(seed)
RF_false_rate_OOB_best = 1e10  	#dummy high value
for (RF_mtry in 2:(nfeats-1)) 
{
  RF_Score_mtry = randomForest(as.factor(trainTarget) ~ .,
                               data=data.frame(rawtrainFeats), 
                               ntree=500, mtry=RF_mtry)
  
  table <- table(RF_Score_mtry$predicted, RF_Score_mtry$y)   
  RF_false_rate_OOB = sum((table[1]/noAccidentCount), table[2:3], (table[4]/severeAccidentCount))
  
  if (RF_false_rate_OOB < RF_false_rate_OOB_best)
  { 
    RF_mbest 		= RF_mtry
    RF_false_rate_OOB_best 	= RF_false_rate_OOB
    print(paste("RF_mbest=",RF_mbest, "RF_false_rate_OOB=",round(RF_false_rate_OOB,3)))
  }
  print(paste("RF_mtry=",RF_mtry))
}

RF_Score 	= randomForest(as.factor(trainTarget) ~ ., 
                         data=data.frame(rawtrainFeats), 
                         ntree=500, mtry=RF_mbest) 

##################################################
# RF-SCORE FIT OF TRAINING DATASET			 #
##################################################

RF_trainPred 	= predict(RF_Score, newdata = data.frame(rawtrainFeats))
RF_train_accuracy_rate 		= table(RF_trainPred, trainTarget)

##################################################
# RF-SCORE PREDICTION OF TEST DATASET	       #
##################################################

RF_testPred 	= predict(RF_Score, newdata = data.frame(rawtestFeats)) 
RF_test_accuracy_rate   	= table(RF_testPred, testTarget)

##################################################
# WRITING OUT RF-SCORE PREDICTION OF TEST DATASET#
##################################################

write.csv(cbind(RF_testPred, testTarget, rawtestFeats), file="RF-Score_pred - 3 Features - Without 30 HZ Low Pass Filter.csv")                                                 
