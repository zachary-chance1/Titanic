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
              "readstata13")
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





