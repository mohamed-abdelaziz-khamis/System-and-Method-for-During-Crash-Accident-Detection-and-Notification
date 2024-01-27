rawdata = read.csv("2016-02-18_14-09-11.csv")

accelerometerAccelerationX = rawdata[,"accelerometerAccelerationX"]
accelerometerAccelerationY = rawdata[,"accelerometerAccelerationY"]
accelerometerAccelerationZ = rawdata[,"accelerometerAccelerationZ"]
deltaT = 10*(10^-3) #10 ms = 100 Hz

accelerometerVelocityX = (deltaT/2) * sum(accelerometerAccelerationX[1:2])
accelerometerVelocityY = (deltaT/2) * sum(accelerometerAccelerationY[1:2])
accelerometerVelocityZ = (deltaT/2) * sum(accelerometerAccelerationZ[1:2])
for(i in 2:(length(accelerometerAccelerationX)-1)) {
  accelerometerVelocityX[i] = (deltaT/2) * sum(accelerometerAccelerationX[1], 2*sum(accelerometerAccelerationX[2:i]), accelerometerAccelerationX[i+1])
  accelerometerVelocityY[i] = (deltaT/2) * sum(accelerometerAccelerationY[1], 2*sum(accelerometerAccelerationY[2:i]), accelerometerAccelerationY[i+1])
  accelerometerVelocityZ[i] = (deltaT/2) * sum(accelerometerAccelerationZ[1], 2*sum(accelerometerAccelerationZ[2:i]), accelerometerAccelerationZ[i+1])
}

gyroRotationX = rawdata[,"gyroRotationX"]
gyroRotationY = rawdata[,"gyroRotationY"]
gyroRotationZ = rawdata[,"gyroRotationZ"]
for(i in 1:length(gyroRotationX)) 
  if(is.na(gyroRotationX[i])){ 
    gyroRotationX[i] = gyroRotationX[i-1]
    gyroRotationY[i] = gyroRotationY[i-1]
    gyroRotationZ[i] = gyroRotationZ[i-1]
  }

processedData = cbind(accelerometerAccelerationX[-1], accelerometerAccelerationY[-1], accelerometerAccelerationZ[-1], 
                      accelerometerVelocityX, accelerometerVelocityY, accelerometerVelocityZ,
                      gyroRotationX[-1], gyroRotationY[-1], gyroRotationZ[-1])
write.csv(processedData, file="(1) 2016-02-18_14-09-11.csv")