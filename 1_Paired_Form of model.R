data <- read.table("Paired_Cam-FAR_d-h.txt", header=TRUE) 
attach (data)

library (ggplot2)
ggplot (data, aes (Cam.d)) + geom_histogram()

library (glmmADMB)

# DEER negative binomial, days
d <- glmmadmb (Cam.d ~ FAR.d + (1|swale), zeroInflation = TRUE, family = "nbinom") 
d.p <- glmmadmb (Cam.d ~ FAR.d + (1|swale), family = "poisson") 
dh <- glmmadmb (Camh.d ~ FAR.d + (1|swale), zeroInflation = TRUE, family = "nbinom") 
dh.p <- glmmadmb (Camh.d ~ FAR.d + (1|swale), family = "poisson") 
AIC (d, d.p, dh, dh.p)

# KANGAROO poisson (by 3), days
k <- glmmadmb (Cam.k ~ FAR.k + (1|swale), zeroInflation = TRUE, family = "nbinom") 
k.p <- glmmadmb (Cam.k ~ FAR.k + (1|swale), family = "poisson") 
kh <- glmmadmb (Camh.k ~ FAR.k + (1|swale), zeroInflation = TRUE, family = "nbinom") 
kh.p <- glmmadmb (Camh.k ~ FAR.k + (1|swale), family = "poisson") 
AIC (k, k.p, kh, kh.p)

# RABBIT negative binomial (by a long way), days
r <- glmmadmb (Cam.r ~ FAR.r + (1|swale), zeroInflation = TRUE, family = "nbinom") 
r.p <- glmmadmb (Cam.r ~ FAR.r + (1|swale), family = "poisson") 
rh <- glmmadmb (Camh.r ~ FAR.r + (1|swale), zeroInflation = TRUE, family = "nbinom") 
rh.p <- glmmadmb (Camh.r ~ FAR.r + (1|swale), family = "poisson") 
AIC (r, r.p, rh, rh.p)

# WOMBAT poisson (by 4), days
w <- glmmadmb (Cam.w ~ FAR.w + (1|swale), zeroInflation = TRUE, family = "nbinom") 
w.p <- glmmadmb (Cam.w ~ FAR.w + (1|swale), family = "poisson") 
wh <- glmmadmb (Camh.w ~ FAR.w + (1|swale), zeroInflation = TRUE, family = "nbinom") 
wh.p <- glmmadmb (Camh.w ~ FAR.w + (1|swale), family = "poisson") 
AIC (w, w.p, wh, wh.p)

# WALLABY negative binomial (by a long way), days
y <- glmmadmb (Cam.y ~ FAR.y + (1|swale), zeroInflation = TRUE, family = "nbinom") 
y.p <- glmmadmb (Cam.y ~ FAR.y + (1|swale), family = "poisson") 
yh <- glmmadmb (Camh.y ~ FAR.y + (1|swale), zeroInflation = TRUE, family = "nbinom") 
yh.p <- glmmadmb (Camh.y ~ FAR.y + (1|swale), family = "poisson") 
AIC (y, y.p, yh, yh.p)


# Go with negative binomila and camera frequency by day

# Check plot of rediduals and fitted values

fit.res <- function (model) {
  res <- residuals (model, type = "pearson") 
  fit <- fitted (model)
  plot (fit, res) 
}

fit.res (d)
fit.res (k)
fit.res (r)
fit.res (w)
fit.res (y)

# Check for overdispersion

od <- function (model) {
  E <- residuals (model)
  p <- length (fixef(model)) + 1
  od <- sum (E^2) / (model$n-p)
  return (od)
}

od(d)
od(k)
od(r)
od(w)
od(y)
