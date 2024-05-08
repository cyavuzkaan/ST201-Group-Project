#ST201 PROJECT - [24369 - 29861 - 34971]

# Libraries: ----

library(DataExplorer)
library(ggplot2)
library(GGally)
library(car)
library(leaps)

# Data & EDA: ----

happy_data <- read.csv("C:/Users/DELL/Desktop/st201.csv")

View(happy_data)

names(happy_data)

dim(happy_data)

summary(happy_data)

str(happy_data)

x <- duplicated(happy_data$X)
sum(x==T)

table(happy_data$xx)

plot_missing(happy_data)

ggpairs(happy_data[16:17])

cor(xx, yy)

plot_correlation(happy_data)

plot_correlation(na.omit(happy_data))

pairs(happy_data[8:9])

boxplot(happy_data[2:10])

boxplot(happy_data[11:21])

plot_histogram(
  happy_data,
  title = "Histogram of Variables",
  ggtheme = theme_test(),
)


plot_qq(happy_data)


# Feature Engineering/Data Transformation: ----

col_names <- c("happy", "sclmeet", "inprdsc", "aesfdrk", "health", "rlgblg", "stfjb", "stfjbot", "brncntr", "hhmmb", "gndr", "agea", "marstgb", "domicil", "eduyrs", "hinctnta", "fairelcc", "cttresa", "dfprtal", "gptpelc")
val_remove <- list(happy = c(77, 88, 99),
                          sclmeet = c(77, 88, 99),
                          inprdsc = c(77, 88, 99),
                          aesfdrk = c(7, 8, 9),
                          health = c(7, 8, 9),
                          rlgblg = c(7, 8, 9),
                          stfjb = c(66, 77, 88, 99),
                          stfjbot = c(66, 77, 88, 99),
                          brncntr = c(7, 8, 9),
                          hhmmb = c(77, 88, 99),
                          gndr = c(9),
                          agea = c(999),
                          marstgb = c(66, 77, 88, 99),
                          domicil = c(7, 8, 9),
                          eduyrs = c(77, 88, 99),
                          hinctnta = c(77, 88, 99),
                          fairelcc = c(77, 88, 99),
                          cttresa = c(77, 88, 99),
                          dfprtal = c(77, 88, 99),
                          gptpelc = c(77, 88, 99))

for (i in col_names) {
  happy_data[[i]][happy_data[[i]] %in% val_remove[[i]]] <- NA
}


happy_data$sclmeet <- as.factor(happy_data$sclmeet)  
happy_data$gndr <- as.factor(happy_data$gndr)  
happy_data$marstgb <- as.factor(happy_data$marstgb) 
happy_data$rlgblg <- as.factor(happy_data$rlgblg)
happy_data$brncntr <- as.factor(happy_data$brncntr)
happy_data$domicil <- as.factor(happy_data$domicil)
happy_data$inprdsc <- as.factor(happy_data$inprdsc)


# Linear Regression Modelling: ----

#  1- We will fit full model:

hap_reg <- lm(happy ~ ., data = happy_data)
summary(hap_reg)


  #Best Subset Selection: ----

hap_reg_sub <- regsubsets(happy ~ ., data = happy_data)
hap_reg_sub_sum <- summary(hap_reg_sub)

print(hap_reg_sub_sum)
names(hap_reg_sub_sum)
print(hap_reg_sub_sum$bic)

which.min(hap_reg_sub_sum$bic)
plot(hap_reg_sub_sum$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(3, hap_reg_sub_sum$bic[3], col = "red", cex = 2,
       pch = 20)

plot(hap_reg_sub, scale = "bic")
#The best three-variable model includes: health, stfjb and hinctnta.

coef(hap_reg_sub, 3)

  #Backward Selection: ----
hap_reg_bwd <- regsubsets(happy ~ ., data = happy_data, method = "backward")
hap_reg_bwd_sum <- summary(hap_reg_bwd)

print(hap_reg_bwd_sum)
names(hap_reg_bwd_sum)
print(hap_reg_bwd_sum$bic)

which.min(hap_reg_bwd_sum$bic)
plot(hap_reg_bwd_sum$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(3, hap_reg_bwd_sum$bic[3], col = "red", cex = 2,
       pch = 20)

plot(hap_reg_bwd, scale = "bic")
#The best three-variable model includes: health, stfjb and hinctnta.

coef(hap_reg_bwd, 3)


  #Forward Selection: ----

hap_reg_fwd <- regsubsets(happy ~ ., data = happy_data, method = "forward")
hap_reg_fwd_sum <- summary(hap_reg_fwd)

print(hap_reg_fwd_sum)
names(hap_reg_fwd_sum)
print(hap_reg_fwd_sum$bic)

which.min(hap_reg_fwd_sum$bic)
plot(hap_reg_fwd_sum$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")
points(3, hap_reg_fwd_sum$bic[3], col = "red", cex = 2,
       pch = 20)

plot(hap_reg_fwd, scale = "bic")
#The best three-variable model includes: health, stfjb and hinctnta.

coef(hap_reg_fwd, 3)



  #Comparison of Selection Methods: ----
#The best three-variable models identified by methods are identical.
#To observe where the difference comes at:

coef(hap_reg_sub, 4)
coef(hap_reg_bwd, 4)
coef(hap_reg_fwd, 4)

#The best four-variable models identified, best subset and forward selection are equal.

coef(hap_reg_sub, 5)
coef(hap_reg_bwd, 5)
coef(hap_reg_fwd, 5)

#With the best five-variable model specifications, they all yield distinct outcomes.

# Modelling: ----

happy_data$marstgb <- as.factor(ifelse(happy_data$marstgb %in% c("2","5", "6"), NA, happy_data$marstgb))
happy_data$marstgb <- droplevels(happy_data$marstgb)
contrasts(happy_data$marstgb)

hap_sub <- lm(happy ~ health + stfjb + marstgb + hinctnta, data = happy_data)
summary(hap_sub)
#r2_sub = 0.176


happy_data$domicil <- as.factor(ifelse(happy_data$domicil %in% c("2","3","4"), NA, happy_data$domicil))
happy_data$domicil <- droplevels(happy_data$domicil)
contrasts(happy_data$domicil)

hap_bwd <- lm(happy ~ health + stfjb + hinctnta + fairelcc + domicil, data = happy_data)
summary(hap_bwd)
# r2_bwd = 0.226



#Refresh the R code here!
happy_data$marstgb <- as.factor(ifelse(happy_data$marstgb %in% c("2","3","4","5"), NA, happy_data$marstgb))
happy_data$marstgb <- droplevels(happy_data$marstgb)
contrasts(happy_data$marstgb)

hap_fwd <- lm(happy ~ health + stfjb + stfjbot + hinctnta + marstgb, data = happy_data)
summary(hap_fwd)
# r2_fwd = 0.2171


#  2- Non-linear variables: quadratic: age, eduyrs (pol of order 2)

hap_quad_reg <- lm(happy ~ health + stfjb + hinctnta + fairelcc + domicil + I(agea^2) + I(eduyrs^2), data = happy_data)
summary(hap_quad_reg)


#  3- Set the regression with dummies and interaction terms.

hap_dum_reg <- lm(happy ~ health + stfjb + hinctnta + fairelcc + domicil + stfjb:stfjbot + eduyrs:hinctnta + health:aesfdrk, data = happy_data)
sum(hap_dum_reg)

#How to find the interaction terms:
#We look at the correlation between the variables.
#sclmeet,inprdsc.
#stfjb, stfjbot.
#gndr, aesfdrk
#eduyrs, hictnta

#  4- 2&3 combined.

hap_fin_reg <- lm(happy ~ health + stfjb + hinctnta + fairelcc + domicil + I(agea^2) + I(eduyrs^2)
                  + stfjb:stfjbot + eduyrs:hinctnta + health:aesfdrk, data = happy_data)

summary(hap_fin_reg)

coef(hap_fin_reg)

confint(hap_fin_reg)

plot(xx, yy)
abline(sim_fit, lwd = 3)
abline(sim_fit, lwd = 3, col = "red")

car::crPlots(hap_fin_reg)
dev.off()


  #Our Research Questions ----

#REFRESH THE FILE HERE AGAIN!

# Politics:
pol <- lm(happy ~ fairelcc + cttresa + dfprtal + gptpelc, data = happy_data)
summary(pol)

# Mental Health:
mental <- lm(happy ~ sclmeet + inprdsc  + health + rlgblg, data = happy_data)
summary(mental)

# Household:

household <- lm(happy ~ hinctnta + marstgb + hhmmb, data = happy_data) 
summary(household)

#Career and Income:
career <- lm(happy ~ (stfjb*stfjbot) + (eduyrs*hinctnta), data = happy_data)
summary(career)

#Sense of Belonging to a Community: 

comm <- lm(happy ~ aesfdrk + domicil + hhmmb + sclmeet + inprdsc + rlgblg + brncntr, happy_data = happy_data)
summary(comm)

#Demographics:

demo <- lm(happy ~ gndr + agea + marstgb, happy_data = happy_data)
summary(demo)

# Physical Health:

physical <- lm(happy ~ gndr + health + age, data = happy_data)


# Diagnostics and Testing & Model Selection Methods ----

#After selecting the final model:

  # - Gauss-Markov Assumptions
  # - residual dist
  # - normality
vif(hap_fin_reg)  # - multicollinearity (check the correlation between the variables.) 
  # - leverage points & Outliers
  # - Low R-Squared Values: discuss the drawbacks of R-squared, it is not necessarily important for inferential aspect. 

par(mfrow = c(2,2))
plot(hap_fin_reg)



  



