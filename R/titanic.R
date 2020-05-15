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
titanicdata$genderIsFemale = 0

#7b - dummy for women
len = dim(titanicdata)
len = len[1]
for(i in seq(1:len)){
  if(titanicdata$sex[i] == "women"){
    titanicdata$genderIsFemale[i] = 1
  }
}
