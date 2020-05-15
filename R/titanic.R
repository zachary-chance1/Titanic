################################################################
#name: titanic.R
#author: zachary chance (baylor university)
#description: completes assignment 2 and performs analysis
#             on the titanic data set
#date: may 15, 2020
#################################################################

WORK.DIR = "C:/Users/Owner/Desktop/Titanic"

# set working directory
setwd(WORK.DIR)

packages <- c("MASS",
              "foreign",
              "readstata13",
              "ggplot2",
              "tidyverse",
              "glue",
              "grid",
              "gridExtra",
              "plotrix")
not_installed <- !packages %in% installed.packages()
if(any(not_installed)) install.packages(packages[not_installed])
lapply(packages, require, character.only=TRUE)

titanicdata = read.dta13("Data/titanic.dta")


#7b - dummy for women
titanicdata$genderIsFemale = 0
len = dim(titanicdata)
len = len[1]
for(i in seq(1:len)){
  if(titanicdata$sex[i] == "women"){
    titanicdata$genderIsFemale[i] = 1
  }
}


#7c - dummy for first class
titanicdata$classIsFirst = 0
for(j in seq(1:len)){
  if(titanicdata$class[j] == "1st class"){
    titanicdata$classIsFirst[j] = 1
  }
}


#7d - OLS and robust regressions - note that this requires another dummy for survival

titanicdata$survivor = 0
for(q in seq(1:len)){
  if(titanicdata$survived[q] == "yes"){
    titanicdata$survivor[q] = 1
  }
}

ols = lm(survivor ~ classIsFirst, data = titanicdata)
summary(ols)

robust = rlm(survivor ~ classIsFirst, data = titanicdata, psi = psi.hampel)
summary(robust)


#7e - the true estimator if 7d was the short regression
titanicdata$isChild = 0
for(n in seq(1:len)){
  if(titanicdata$age[n] == "child"){
    titanicdata$isChild[n] = 1
  }
}

ovbSexTerm = cov(titanicdata$genderIsFemale, titanicdata$classIsFirst) / var(titanicdata$classIsFirst)
#keep it positive because we assume women and children are more likely to survive
ovbAgeTerm = cov(titanicdata$isChild, titanicdata$classIsFirst) / var(titanicdata$classIsFirst)


olsTrueEff = ols$coefficients[2]
olsTrueEff = olsTrueEff - ovbSexTerm
olsTrueEff = olsTrueEff - ovbAgeTerm


longEqOLS = lm(survivor ~ classIsFirst + isChild + genderIsFemale, data = titanicdata)
summary(longEqOLS)

longEqRobust = rlm(survivor ~ classIsFirst + isChild + genderIsFemale, data = titanicdata, psi = psi.hampel)
summary(longEqRobust)

shortOLSCol = ols$coefficients
shortOLSCol["isChild"] = "NA"
shortOLSCol["genderIsFemale"] = "NA"
shortOLSCol["SE (Intercept)"] = sqrt(diag(vcov(ols)))[1]
shortOLSCol["SE isFirstClass"] = sqrt(diag(vcov(ols)))[2]
shortOLSCol["SE isChild"] = "NA"
shortOLSCol["SE genderIsFemale"] = "NA"
shortOLSCol["Mean Survival"] = mean(titanicdata$survivor)
shortOLSCol["Observations"] = len

shortRobustCol = robust$coefficients
shortRobustCol["isChild"] = "NA"
shortRobustCol["genderIsFemale"] = "NA"
shortRobustCol["SE (Intercept)"] = sqrt(diag(vcov(robust)))[1]
shortRobustCol["SE isFirstClass"] = sqrt(diag(vcov(robust)))[2]
shortRobustCol["SE isChild"] = "NA"
shortRobustCol["SE genderIsFemale"] = "NA"
shortRobustCol["Mean Survival"] = mean(titanicdata$survivor)
shortRobustCol["Observations"] = len

longOLSCol = longEqOLS$coefficients
longOLSCol["SE (Intercept)"] = sqrt(diag(vcov(longEqOLS)))[1]
longOLSCol["SE isFirstClass"] = sqrt(diag(vcov(longEqOLS)))[2]
longOLSCol["SE isChild"] = sqrt(diag(vcov(longEqOLS)))[3]
longOLSCol["SE genderIsFemale"] = sqrt(diag(vcov(longEqOLS)))[4]
longOLSCol["Mean Survival"] = mean(titanicdata$survivor)
longOLSCol["Observations"] = len

longRobustCol = longEqRobust$coefficients
longRobustCol["SE (Intercept)"] = sqrt(diag(vcov(longEqRobust)))[1]
longRobustCol["SE isFirstClass"] = sqrt(diag(vcov(longEqRobust)))[2]
longRobustCol["SE isChild"] = sqrt(diag(vcov(longEqRobust)))[3]
longRobustCol["SE genderIsFemale"] = sqrt(diag(vcov(longEqRobust)))[4]
longRobustCol["Mean Survival"] = mean(titanicdata$survivor)
longRobustCol["Observations"] = len

beautifulFrame = cbind(shortOLSCol,shortRobustCol,longOLSCol,longRobustCol)



beautifulFrame



#Question 8 - manual reg anatomy


longReg = lm(survivor ~ classIsFirst + isChild + genderIsFemale, data = titanicdata)
auxReg = lm(classIsFirst ~ isChild + genderIsFemale, data = titanicdata)
titanicdata$residuals = auxReg$residuals
resReg = lm(survivor ~ residuals, data = titanicdata)
summary(longReg)
summary(resReg)

longRegCol = longReg$coefficients
longRegCol["SE (Intercept)"] = sqrt(diag(vcov(longReg)))[1]
longRegCol["SE isFirstClass"] = sqrt(diag(vcov(longReg)))[2]
longRegCol["SE isChild"] = sqrt(diag(vcov(longReg)))[3]
longRegCol["SE genderIsFemale"] = sqrt(diag(vcov(longReg)))[4]

resRegCol = resReg$coefficients
resRegCol["isChild"] = "NA"
resRegCol["genderIsFemale"] = "NA"
resRegCol["SE (Intercept)"] = sqrt(diag(vcov(resReg)))[1]
resRegCol["SE isFirstClass"] = sqrt(diag(vcov(resReg)))[2]
resRegCol["SE isChild"] = "NA"
resRegCol["SE genderIsFemale"] = "NA"

resAnatTable = cbind(longRegCol,resRegCol)


#7h table - needs title, but im new to the package
grid.table(beautifulFrame)

#8 table
grid.table(resAnatTable)

