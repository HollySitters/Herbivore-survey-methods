data <- read.table("Paired_Cam-FAR_d-h.txt", header=TRUE) 
attach (data)

# This method is very clunky...

library (glmmADMB)
library (dismo)

# Run the following for each model:

rout <- vector("numeric") # create the output vector
for (z in 1:10) # do the following 100 times
{
  k <- 5 
  data$k <- kfold(data, k=k)  # create the kfolds
  for (i in 1:k) # loop for the cross-validation
  {
    modeldat <- subset(data, data$k!=i) # subset out the data for building the model
    testdat<-subset(data, data$k==i) # subset out the data for testing the model
    mod1 <- glmmadmb (Cam.d ~ FAR.d + (1|swale), zeroInflation = TRUE, family = "nbinom") 
    testdat$pred <-predict(mod1, newdata=testdat) # predict onto the testdat 
    rmod<-cor.test(testdat$pred, testdat$Cam.d, method="spearman")$estimate 
    rout<-append(rout,rmod) 
  }
}
cv1 <- rout # EDIT to cv2 etc.

# Output for each model should now be saved as cv1, cv2 etc.  
# Put them all together and derive means and confidence intervals.

library(reshape2) # for melt
library(plyr) # for plyr

cv <- cbind (cv1, cv2)
cv <- data.frame(cv) # convert to a data frame
names (cv) <- c("m1", "m2")
melt <- melt (cv)
CV <- ddply(melt, .(variable), (function(x) {
  Mean = mean(x$value)
  LCL = quantile(x$value, probs=0.025)
  UCL = quantile(x$value, probs=0.975)
  data.frame (Mean=Mean, LCL=LCL, UCL=UCL)}))
write.table(CV, "grft_CV.txt", col.names=TRUE, row.names=TRUE)