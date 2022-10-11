Paper=read.csv("Screening_data.csv",header=T)
f = lm(time~(A)*(B)*(C)*(D)*(E)*(F)*(G)*(H)*(I), data =Paper)
library(faraway)
halfnorm(coef(fit)[-3],nlab=5,labs=row.names(summary(fit)[[5]])[-3],ylab="Sorted Factor Effects")

f2 = lm(time~A+C+D+E+F+G+I+C:G+A:E:F, data=Paper)

library(BsMD)
DanielPlot(f)
LenthPlot(f2)

anova(f2)
summary(f2)

f3 = lm(time~A+C+D+E+F+G+I+C:G+A:E:F +block, data=Paper)
anova(f3)
summary(f3)

fs = lm(time~A+B+C+D+E+F+block, data=Paper)
anova(fs)
summary(fs)

fs2 = lm(time~A+B+C+D+E+F, data = Paper)
anova(fs2)
summary(fs2)
DanielPlot(fs3)


fs3 = lm(time~A+B+C+D+E+F + A:B+A:C+A:D+A:E+B:C+B:D+B:E+B:F+C:D+C:E+C:F+D:E+D:F+E:F, data = Paper)
anova(fs3)
summary(fs3)

fs4 = lm(time~A+B+C+D+E+F + A:B+A:C+A:D+A:E+B:C+B:D+B:E+B:F+C:D+C:E+C:F+D:E+D:F+E:F +D:G+D:I+D:H+D:G:I, data = Paper)
anova(fs4)
summary(fs4)

f4 = lm(time~A+C+D+G+I+block, data=Paper)
anova(f4)
summary(f4)

f5 = lm(time~A+C+D+G+I, data = Paper)
anova(f5)
summary(f5)

f6 = lm(time~A+C+D+block, data=Paper)
anova(f6)
summary(f6)

f7 = lm(time~A+C+D, data = Paper)
anova(f7)
summary(f7)

#Extras
#Studentised Residuals
library(MASS)
plot(studres(f4),main="Model Studentized Residuals",xlab="",ylab="Studentized Residuals",pch=16,cex=1.2,ylim=c( -2,2))  
abline(h=0,lty=2)


# check normality
qqnorm(residuals(f4),pch=16,main="Normal Plot of Residuals",xlab="NormalScores",ylab="Ordered Residuals")


#Descriptive Statistics

boxplot(Paper$time~Paper$G, ylab = "time", xlab = "Paper type", col="blue")
boxplot(Paper$time~Paper$I, ylab = "time", xlab = "# of Rotors", col="green")

#Other boxplots

par(mfrow=c(2,2))
boxplot(Paper$time~Paper$B, ylab = "time", xlab = "Rotor Width")
boxplot(Paper$time~Paper$E, ylab = "time", xlab = "Leg Width")
boxplot(Paper$time~Paper$F, ylab = "time", xlab = "Foot length")
boxplot(Paper$time~Paper$H, ylab = "time", xlab = "tape vs clip")
par(mfrow=c(1,1))


#Steepest Ascent

library(rsm)
rs_mod = rsm(time~FO(A,C,D)+TWI(A,C,D)+I(A^2 +C^2 +D^2),data = Paper)
summary(rs_mod)

rs_mod2 = update(rs_mod, .~., - I(A^2 +C^2 +D^2), data= Paper )
summary(rs_mod2)

rs_mk = rsm(time~FO(A,C,D), data = Paper)
summary(rs_mk)

rs_mk2 = rsm(time~FO(A,C,D,G,I), data = Paper)
summary(rs_mk2)


#CCD part
New_Paper=read.csv("Paper_new.csv",header=T)
rs.new = rsm(time~FO(A,C,D)+TWI(A,C,D)+I(A^2 +C^2 +D^2), data= New_Paper)
summary(rs.new)

#After Curvature
Paper.CCD = read.csv("Curvature.csv",header=T)
rs.ccd = rsm(time~SO(A,C,D), data= Paper.CCD)
summary(rs.ccd)

rs.ccd2=update(rs.ccd,.~.-TWI(A,C,D),data=Paper.CCD)
summary(rs.ccd2)


#Contour Plots
contour(rs.ccd, ~ A+C+D)
par(mfrow=c(2,2))
contour(rs.ccd, ~ A+C)
contour(rs.ccd, ~ C+D)
contour(rs.ccd, ~ A+D)
par(mfrow=c(1,1))

contour(rs.ccd2, ~ A+C+D)
par(mfrow=c(2,2))
contour(rs.ccd2, ~ A+C)
contour(rs.ccd2, ~ C+D)
contour(rs.ccd2, ~ A+D)
par(mfrow=c(1,1))

