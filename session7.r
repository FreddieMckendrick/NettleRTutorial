###Script file for session 6###

# Chunk 1
nettle=function(a, b=10) {
  if (a>b){answer="greater"}
  if (b>a){answer = "less"}
  if (a==b){answer = "the same"}
  return(answer)}
  

# Chunk 2
  nettle=function(a, b=10) {
    if (a>b){answer="greater"}
    if (b>a){answer = "less"}
    if (a==b){answer = "the same"}
    return(answer)}
  
# Chunk 3
  y=c(rnorm(100, mean=0, sd=1), rnorm(100, mean=0.5, sd=1))
  condition=c(rep("control", times=100), rep("experimental", times=100))
  m=lm(y~condition)
  summary(m)
  
# Chunk 4
p.please=function(n, f){
  y=c(rnorm(n=n, mean = 0, sd = 1), rnorm(n=n, mean = f, sd = 1))
  condition=c(rep("control", times=n), rep("experimental", times=n))
  m=lm(y~condition)
  return(summary(m)$coefficients[2, 4])
}

# Chunk 5
hundred.p=function(n, f){
  output=NULL
  for (i in 1:100) {output[i]=p.please(n=n, f=f)}  
  return(output)
}

# Chunk 6
# First define the numbers of subjects to consider (0-500) 
subjects=seq(10, 500, by=10)
# Set up some empty vectors
power.1=NULL
power.2=NULL
power.3=NULL
# Now set a counter at zero to keep track of where we are
counter=0
# Set up a for loop to cycle through all the sample sizes to be considered
for (i in subjects){
  counter=counter+1 #Augment the counter
  # Now calculate the power at that sample size for three different values of f
  power.1[counter] = mean(hundred.p(n=i, f=0.2)<0.05)
  power.2[counter] = mean(hundred.p(n=i, f=0.4)<0.05)
  power.3[counter] = mean(hundred.p(n=i, f=0.8)<0.05)
  }

# Now we have generated the data we need, plot power ~ subjects for f=0.5
plot(power.3~subjects, 
     ylim=c(0, 1), xlab="Subjects per group", ylab="Power", type="l")
# Add lines for the other effect sizes
lines(power.2~subjects, col="green")
lines(power.1~subjects, col="red")
# Add dashed horizontal line at power of 0.8
abline(h=0.8, lty=2)
text(40, 0.9, "f = 0.8")
text(140, 0.7, "f = 0.4", col="green")
text(350, 0.6, "f = 0.2", col="red")
# End chunk 6
