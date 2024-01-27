library("audio")
library("signal")
library(randomForest) # library for randomForest

fileName <- c("Mobile_On_Passenger_Chair.csv",
                "Mobile_Back_Driver_Seat.csv",
                "Mobile_Back_Passenger_Seat.csv",
                "Mobile_Between_Front_Seats.csv")

Start_non_Zero_index <- c(181, 295, 299, 184)

End_Fly_Time_s <-c(0.6935, 0.7129,  0.7177, 0.6967)

#Start_Fly_Time_s<-c(0.67, 0.67, 0.67, 0.65) # Dr. Mohamed
#Start_Fly_Time_s<-c(0.612, 0.617, 0.617, 0.613)
Start_Fly_Time_s<-c(0.6, 0.6, 0.6, 0.6) # what I used to get same numbers of FFt samples
#Start_Fly_Time_s<-c(0.591, 0.594, 0.594, 0.591)

window_size = 681#1024
#Mobile_On_Passenger_Chair: Start_Fly_Time_s = 0.67;End_Fly_Time_s = 0.6935;
#Mobile_Back_Driver_Seat: Start_Fly_Time_s = 0.67;End_Fly_Time_s = 0.7129;
#Mobile_Back_Passenger_Seat: Start_Fly_Time_s = 0.67;End_Fly_Time_s = 0.7177;
#Mobile_Between_Front_Seats: Start_Fly_Time_s = 0.65;End_Fly_Time_s = 0.6967;


# 1st 1 second in 2nd minute: Constant speed 40KM/H
#Sin wave with rate 44100 for 10 seconds.

sec  = 10                                # Number of seconds to study in each split
rate = 44100   
bf <- butter(3, (30/rate))               # If butter(3, 0.1) this means 10 Hz low-pass filter
a = load.wave("Throw.wav")
Sound_Mobile = a[400000:(rate*sec)]

Sound_rate_44100 <- filter(bf, Sound_Mobile)  
#Sound_rate_44100 = Sound_Mobile
#play(Sound_rate_44100, rate=44100)

for(m in 1 : length(fileName)){
  
Engine_Top <- read.csv("Engine_Top.csv", header = FALSE)

#Mobile_On_Passenger_Chair: Start_Fly_Time_s = 0.67;End_Fly_Time_s = 0.6935;
#Mobile_Back_Driver_Seat: Start_Fly_Time_s = 0.67;End_Fly_Time_s = 0.7129;
#Mobile_Back_Passenger_Seat: Start_Fly_Time_s = 0.67;End_Fly_Time_s = 0.7177;
#Mobile_Between_Front_Seats: Start_Fly_Time_s = 0.65;End_Fly_Time_s = 0.6967;

#Start_Fly_Time_s = 0.5

Nodout_Sampling_Rate_HZ = 10000


  Mobile <- read.csv(fileName[m], header = FALSE)
  
Mobile = Mobile[(Start_Fly_Time_s[m]*Nodout_Sampling_Rate_HZ):(End_Fly_Time_s[m]*Nodout_Sampling_Rate_HZ), 12:14]
Engine_Top = Engine_Top[(Start_Fly_Time_s[m]*Nodout_Sampling_Rate_HZ):(End_Fly_Time_s[m]*Nodout_Sampling_Rate_HZ), 12:14]

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
  Sound_Mobile_increased_rate = rbind(Sound_Mobile_increased_rate, (1/delta_l) * Sound_rate_44100[round(t-delta_t*44100)])
  else
    Sound_Mobile_increased_rate = rbind(Sound_Mobile_increased_rate, 0) 
}


#Frequency for window = 60 Hz
#Time for window = 1/60 second 
#Increased rate for window = 40000
#Window size = (1/60) * 40000 = 680

rawFeats = data.frame()

##################################################
# Apply FFT                                     #
##################################################

#Mobile_On_Passenger_Chair: Start_non_Zero_index = 181;
#Mobile_Back_Driver_Seat: Start_non_Zero_index = 295;
#Mobile_Back_Passenger_Seat: Start_non_Zero_index = 299;
#Mobile_Between_Front_Seats: Start_non_Zero_index = 184;

print("start FFT")
for(i in Start_non_Zero_index[m]:(nrow(Sound_Mobile_increased_rate) - window_size + 1)) { 
  FFT_Sound_Mobile <- Mod(fft(Sound_Mobile_increased_rate[i:(i + window_size - 1),]))
  rawFeats = rbind(rawFeats, FFT_Sound_Mobile[2:ceiling(window_size/2)])
} 
print("end FFT")
rawFeats = cbind(rawFeats[1:6], 1) #Mobile is Flying Towards the Motor:

plot((seq(1:length(rawFeats[,1]))*40000/window_size)/1000, rawFeats[,1], xlab="Sample Index * 40000 / Window size (681) in KHZ", ylab="FFT sin with Doppler Effect and Attenuation", main = "FFT sound increased rate - sin wave - before accident", type="l")
print(paste("Kh_FFT_Sound_With_LPF", fileName[m], sep="_"));
write.csv(rawFeats, file=  paste("kh_FFT_681_Sound_LPF_30", fileName[m], sep="_") )
}