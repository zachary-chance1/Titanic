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
