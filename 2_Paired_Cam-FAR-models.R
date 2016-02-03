data <- read.table("Paired_Cam-FAR_d-h.txt", header=TRUE) 
attach (data)

library (glmmADMB)

# Run model for each species
d <- glmmadmb (Cam.d ~ FAR.d + (1|swale), zeroInflation = TRUE, family = "nbinom") 
k <- glmmadmb (Cam.k ~ FAR.k + (1|swale), zeroInflation = TRUE, family = "nbinom") 
r <- glmmadmb (Cam.r ~ FAR.r + (1|swale), zeroInflation = TRUE, family = "nbinom") 
w <- glmmadmb (Cam.w ~ FAR.w + (1|swale), zeroInflation = TRUE, family = "nbinom") 
y <- glmmadmb (Cam.y ~ FAR.y + (1|swale), zeroInflation = TRUE, family = "nbinom") 

# Save the function:
extract.FEC <- function (x, n){ # x = the glmerMod (e.g. m1), n = the model number (e.g. 1)
  library ("plyr") # for the mutate function
  Est <- coef(summary(x))[ , "Estimate"]
  SE <- coef(summary(x))[ , "Std. Error"]
  P <- coef(summary(x))[ , "Pr(>|z|)"]
  bind <- cbind (Est, SE, P)
  dbind <- data.frame(bind) # convert to a data frame
  FEC <- mutate (dbind, CI = qnorm (0.975)*SE, LCL = Est - CI, UCL = Est + CI, Model = n)
  return (FEC)}

# Run the function on each model
d_FEC <- extract.FEC (d, "deer")
k_FEC <- extract.FEC (k, "kangaroo")
r_FEC <- extract.FEC (r, "rabbit")
w_FEC <- extract.FEC (w, "wombat")
y_FEC <- extract.FEC (y, "wallaby")

# Put it all together into one file
FEC <- rbind (d_FEC, k_FEC, r_FEC, w_FEC, y_FEC) 
write.table (FEC, "paired_FEC.txt", col.names=TRUE, row.names=FALSE) 


### Run model for each species - try poisson

dp <- glmmadmb (Cam.d ~ FAR.d + (1|swale), family = "poisson") 
kp <- glmmadmb (Cam.k ~ FAR.k + (1|swale), family = "poisson") 
rp <- glmmadmb (Cam.r ~ FAR.r + (1|swale), family = "poisson") 
wp <- glmmadmb (Cam.w ~ FAR.w + (1|swale), family = "poisson") 
yp <- glmmadmb (Cam.y ~ FAR.y + (1|swale), family = "poisson") 


# Run the function on each model
dp_FEC <- extract.FEC (dp, "p.deer")
kp_FEC <- extract.FEC (kp, "p.kangaroo")
rp_FEC <- extract.FEC (rp, "p.rabbit")
wp_FEC <- extract.FEC (wp, "p.wombat")
yp_FEC <- extract.FEC (yp, "p.wallaby")

# Put it all together into one file
FECp <- rbind (dp_FEC, kp_FEC, rp_FEC, wp_FEC, yp_FEC) 
write.table (FECp, "paired_FECp.txt", col.names=TRUE, row.names=FALSE) 

### Run model for each species - try poisson zero inflated

dpz <- glmmadmb (Cam.d ~ FAR.d + (1|swale), zeroInflation = TRUE, family = "poisson") 
kpz <- glmmadmb (Cam.k ~ FAR.k + (1|swale), zeroInflation = TRUE, family = "poisson") 
rpz <- glmmadmb (Cam.r ~ FAR.r + (1|swale), zeroInflation = TRUE, family = "poisson") 
wpz <- glmmadmb (Cam.w ~ FAR.w + (1|swale), zeroInflation = TRUE, family = "poisson") 
ypz <- glmmadmb (Cam.y ~ FAR.y + (1|swale), zeroInflation = TRUE, family = "poisson") 


# Run the function on each model
dpz_FEC <- extract.FEC (dpz, "pz.deer")
kpz_FEC <- extract.FEC (kpz, "pz.kangaroo")
rpz_FEC <- extract.FEC (rpz, "pz.rabbit")
wpz_FEC <- extract.FEC (wpz, "pz.wombat")
ypz_FEC <- extract.FEC (ypz, "pz.wallaby")

# Put it all together into one file
FECpz <- rbind (dpz_FEC, kpz_FEC, rpz_FEC, wpz_FEC, ypz_FEC) 
write.table (FECpz, "paired_FECpz.txt", col.names=TRUE, row.names=FALSE) 


### Run model for each species - nbinom, not zero-inflated

dz <- glmmadmb (Cam.d ~ FAR.d + (1|swale), zeroInflation = FALSE, family = "nbinom") 
kz <- glmmadmb (Cam.k ~ FAR.k + (1|swale), zeroInflation = FALSE, family = "nbinom") 
rz <- glmmadmb (Cam.r ~ FAR.r + (1|swale), zeroInflation = FALSE, family = "nbinom") 
wz <- glmmadmb (Cam.w ~ FAR.w + (1|swale), zeroInflation = FALSE, family = "nbinom") 
yz <- glmmadmb (Cam.y ~ FAR.y + (1|swale), zeroInflation = FALSE, family = "nbinom") 


# Run the function on each model
dz_FEC <- extract.FEC (dz, "z.deer")
kz_FEC <- extract.FEC (kz, "z.kangaroo")
rz_FEC <- extract.FEC (rz, "z.rabbit")
wz_FEC <- extract.FEC (wz, "z.wombat")
yz_FEC <- extract.FEC (yz, "z.wallaby")

# Put it all together into one file
FECz <- rbind (dz_FEC, kz_FEC, rz_FEC, wz_FEC, yz_FEC) 
write.table (FECz, "paired_FECz.txt", col.names=TRUE, row.names=FALSE)
