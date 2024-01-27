# [R] FFT, frequs, magnitudes, phases
# Wolfgang Waser wolfgang.waser at rz.hu-berlin.de
# Tue Aug 30 11:35:04 CEST 2005
# Previous message: [R] your message submission
# Next message: [R] FFT, frequs, magnitudes, phases
# Messages sorted by: [ date ] [ thread ] [ subject ] [ author ]
# Hi,

# here is some info about the first part of my "homework", for those, who want 
# to break down their signal (heart beat or whatever) into a collection of pure 
# sin waves to analyse "main" frequency magnitudes and phases.

# First some very un-mathematical "applied" theory:
# 
# If you sample a waveform signal (heart beat pressure pulses, ECG, doppler flow 
# signals, etc.) with a certain data acquisition frequency, an fft of your data 
# gives you the decomposition/breakdown of the waveform signal into a series of 
# pure sin waves of different frequencies. Each sin wave in that list has:
# a) a certain "magnitude", i.e. a measure of how much that particular frequency 
# participates in the generation of your signal, and 
# b) a phase, i.e. the starting point of each sin wave.
# 
# Two characteristics of an fft have to be considered:
# 1) the highest meaningful sin wave frequency of your fft-analysis of the 
# original waveform signal is half the data acquisition frequency (actually, 
# R's fft gives you a list of frequencies up to the acquisition frequency, but 
# you can only use the first half of it, see below)
# 2) the frequency resolution of your fft-analysis depends on the sampling time. 
# The longer the sampling/analysis interval, the finer the resolution. 
# Frequency resolution is actually 1 divided by sampling time (sec).
# 
# An example:
# - some complicated waveform signal
# - 1000 Hz data acquisition frequency (going on for hours)
# - fft-analysis of data blocks of 1 sec length
# Result:
# - vector of frequencies from 1 to 500 Hz with a resolution of 1 Hz, 
# corresponding vector of magnitudes (one for each frequency) and phases 
# (dito).
# You can now e.g. pick the frequency with the highest magnitude within that 1 
# sec block and continue the fft analysis in 1 sec blocks for the complete data 
# set, analysing the time course of the "main" frequency of your waveform 
# signal.
# 
# If you need higher frequency resolution, increase the block length. Analysis 
# of a 5 sec block will give you a list of frequencies from 0.2 to 500 Hz with 
# a resolution of 0.2 Hz. However, increasing analysis-block length decreases 
# temporal resolution, i.e. "main" frequency are now calculated only every 5 
# sec and not 1 sec.
# 
# What does R's fft() deliver?
# 
# fft() is calculated with a single one-dimensional vector. Information on data 
# acquisition frequency and block length (in sec or whatever) can not be 
# included into the fft()-call.
# 
# R delivers a single one-dimensional vector of the same length as the data 
# vector containing a list of imaginary numbers.
# To extract the "magnitudes" use Mod(fft()).
# The magnitudes can also be calculated using the formula:
# magnitude = square root (real * real + imaginary * imaginary)
# real: Re(fft()), imaginary: Im(fft())
# 
# Confusingly, if you calculate fft() on a sample vector consisting of 2 pure 
# sin frequencies, you get 4 peaks, not 2.
# 
# As stated above, fft() gives only "meaningful" frequency up to half the 
# sampling frequency. R, however, gives you frequencies up to the sampling 
# frequency. The point is, that sampling a signal in discret time intervals 
# causes aliasing problems. E.g. when sampling a 50 Hz sin wave and 950 Hz sin 
# wave with 1000 Hz, the results will be identical. An fft can not distinguish 
# between the two frequencies. Therefore, the sampling frequency should always 
# be at least twice as high as the expected signal frequency.
# So for each actual frequency in the signal, fft() will give 2 peaks (one at 
# the "actual" frequency and one at sampling frequency minus "actual" 
# frequency), making the second half of the magnitude vector a mirror image of 
# the first half.
# As long as the sampling frequency was at least twice as high as the expected 
# signal frequency, all "meaningful" information is contained in the the first 
# half of the magnitude vector. A peak in the low frequency range might 
# nevertheless still be caused by a high "noise" frequency.
# 
# The vector of magnitudes extraced so far only has an index an no associated 
# frequencies.
# 
# To calculated the frequencies, simply take (or generate) the index vector (1 
# to length(magnitude vector) and divide by the length of the data block (in 
# sec).
# 
# 
# That's it for now. The second half of my "homework" will be delivered as soon 
# as I understand what to make out of the phases given by R.
# I again would expect a vector of the same length as the magnitude vector with 
# the phases (0 to 2*pi or -pi to +pi) of each frequency. However, I do not 
# know yet what R calculates.
# I would be most obliged for any comments and help.


# Wolfgang

#---------------------------------------------------------------------------
# R-script
acq.freq <- 4000       # data acquisition frequency (Hz)
sig1.freq <- 50           # frequency of 1st signal component (Hz)
sig2.freq <- 130        # frequency of 2nd signal component (Hz)
time <- 5                    # measuring time interval (s)

# vector of sampling time-points (s)
smpl.int <- (1:(time*acq.freq))/acq.freq  

# data vector containing two frequencies (2nd frequ with phase shift)
data <- sin(sig1.freq*smpl.int*2*pi)+sin(sig2.freq*smpl.int*2*pi+pi/2)

plot(data,type="l")

# calculate fft of data
test <- fft(data)

# extract magnitudes and phases
magn <- Mod(test) # sqrt(Re(test)*Re(test)+Im(test)*Im(test))
phase <- Arg(test) # atan(Im(test)/Re(test))

# select only first half of vectors
magn.1 <- magn[1:(length(magn)/2)]
#phase.1 <- Arg(test)[1:(length(test)/2)]

# plot various vectors

# plot magnitudes as analyses by R
x11()
plot(magn,type="l")

# plot first half of magnitude vector
x11()
plot(magn.1,type="l")

# generate x-axis with frequencies
x.axis <- 1:length(magn.1)/time

# plot magnitudes against frequencies
x11()
plot(x=x.axis,y=magn.1,type="l")



# Hi,
# 
# I'm in dire need of a fast fourier transformation for me
# stupid biologist, i.e. I have a heartbeat signal and
# would like to decompose it into pure sin waves, getting
# three vectors, one containing the frequencies of the sin
# waves, one the magnitudes and one the phases (that's what
# I get from my data acquisition software's FFT function).
# I'd be very much obliged, if someone could point out
# which command would do the job in R.

# fft(), but notice that it gives the complex
# transform. You need to do a little homework to get at
# the magnitude/phase values. (Basically, you just have to
# take Mod() and Arg(), but there some conventions about
# the frequencies and multipliers that one can get wrong).

# Once you've finished the "homework", others might be interested
# in your result... so it will be found in the future using 
# RSiteSearch().

# -- 
# Dr. Wolfgang Waser
# Humbolt-Universität zu Berlin
# Institute of Biology
# Department of Animal Physiology
# Philippstrasse 13, Abderhaldenhaus
# 10115 Berlin
# Germany
# Tel: +49 (0)30 2093 6173
# Fax: +49 (0)30 2093 6375
# 
# 
# Previous message: [R] your message submission
# Next message: [R] FFT, frequs, magnitudes, phases
# Messages sorted by: [ date ] [ thread ] [ subject ] [ author ]
# More information about the R-help mailing list