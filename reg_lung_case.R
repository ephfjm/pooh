#setwd("C:/Users/ephhyh/Desktop/data_cancer_case")


setwd("/home/ephppt/Documents/data_cancer")

population<-read.csv("singapore-residents.csv",header=TRUE)
popcount=array(0,c(3,2,5,17))
pop75=array(0,c(3,2,3))
pop80=array(0,c(3,2,3))
for(i in 1:length(population$year))
{ if ((population$year[i]>1987)&(population$year[i]<2013))
  { y=floor((population$year[i]-1988)/5)+1
    race=0
    gender=0
    agegroup=0
    if (population$level_1[i]=="Total Male Chinese") {race=1;gender=2}
    if (population$level_1[i]=="Total Female Chinese") {race=1;gender=1}
    if (population$level_1[i]=="Total Male Malays") {race=2;gender=2}
    if (population$level_1[i]=="Total Female Malays") {race=2;gender=1}
    if (population$level_1[i]=="Total Male Indians") {race=3;gender=2}
    if (population$level_1[i]=="Total Female Indians") {race=3;gender=1}
    if (population$level_2[i]==" 0 - 4 Years ") {agegroup=1}
    if (population$level_2[i]==" 5 - 9 Years ") {agegroup=2}
    if (population$level_2[i]==" 10 - 14 Years ") {agegroup=3}
    if (population$level_2[i]==" 15 - 19 Years ") {agegroup=4}
    if (population$level_2[i]==" 20 - 24 Years ") {agegroup=5}
    if (population$level_2[i]==" 25 - 29 Years ") {agegroup=6}
    if (population$level_2[i]==" 30 - 34 Years ") {agegroup=7}
    if (population$level_2[i]==" 35 - 39 Years ") {agegroup=8}
    if (population$level_2[i]==" 40 - 44 Years ") {agegroup=9}
    if (population$level_2[i]==" 45 - 49 Years ") {agegroup=10}
    if (population$level_2[i]==" 50 - 54 Years ") {agegroup=11}
    if (population$level_2[i]==" 55 - 59 Years ") {agegroup=12}
    if (population$level_2[i]==" 60 - 64 Years ") {agegroup=13}
    if (population$level_2[i]==" 65 - 69 Years ") {agegroup=14}
    if (population$level_2[i]==" 70 - 74 Years ") {agegroup=15}
    if (population$level_2[i]==" 75 - 79 Years ") {agegroup=16}
    if (population$level_2[i]==" 80 Years & Over ") {agegroup=17}
   # print(paste(population$level_1[i],population$level_2[i],race,gender,agegroup))
    if (race*agegroup*gender!=0) {popcount[race,gender,y,agegroup]=popcount[race,gender,y,agegroup]+population$value[i]}
    if ((population$year[i]<=1990)&(population$level_2[i]==" 75 Years & Over " )) {pop75[race,gender,population$year[i]-1987]=population$value[i]}
   if ((population$year[i]<=1990)&(population$level_2[i]==" 80 Years & Over " )) {pop80[race,gender,population$year[i]-1987]=population$value[i]}
}
 }
for (race in 1:3)
  for (gender in 1:2)
  {x=round(sum(pop80[race,gender,3]/pop75[race,gender,3]*pop75[race,gender,1:2]))
popcount[race,gender,1,17]=popcount[race,gender,1,17]+x
popcount[race,gender,1,16]=popcount[race,gender,1,16]+(sum(pop75[race,gender,1:2])-x)

}


lung<-read.csv("Lung_case.csv",header=TRUE)
attributes(lung)$names
Case.lung=array(0,c(4,2,5,18))
for(i in 1:length(lung$Gender))
{ print(paste(i))
  if (lung$Gender[i]=="Female") {gender=1} else {gender=2}
  if (lung$Race[i]=="Chinese") {race=1} else {
    if (lung$Race[i]=="Malay") {race=2} else {
      if (lung$Race[i]=="Indian") {race=3} else {race=4}
    }}
  age=min(18,lung$Age.min[i]/5+1)
  Case.lung[race,gender,1,age]=lung$R4[i]
  Case.lung[race,gender,2,age]=lung$R5[i]
  Case.lung[race,gender,3,age]=lung$R6[i]
  Case.lung[race,gender,4,age]=lung$R7[i]
  Case.lung[race,gender,5,age]=lung$R8[i]
}

########## Functions to rearrange the data frames before fitting the linear model ##########
## excluding the last age group (80+)
dframe_m <- function(cancer){
  ## for males ##
  sel<-cancer$Gender=='Male' & cancer$Race!='All'& cancer$Age.min<80& cancer$Age.min>29
  output = data.frame(rbind(cancer[sel,c(1:3)], #columns for gender, race, age
                            cancer[sel,c(1:3)],
                            cancer[sel,c(1:3)],
                            cancer[sel,c(1:3)],
                            cancer[sel ,c(1:3)]),
                      Case=
                        ## column for Case ##
                        c(cancer[sel,4],
                          cancer[sel,5],
                          cancer[sel,6],
                          cancer[sel,7],
                          cancer[sel,8]),
                      
                      Period =
                        ## column for period ##
                        c(rep(1990,nrow(cancer[sel,c(1:3)])),
                          rep(1995,nrow(cancer[sel,c(1:3)])),
                          rep(2000,nrow(cancer[sel,c(1:3)])),
                          rep(2005,nrow(cancer[sel,c(1:3)])),
                          rep(2010,nrow(cancer[sel,c(1:3)])))
  )
  Age=output$Age.min+2
  Age2=Age^2
  x<-7:16
  N=c(popcount[1,2,1,x],popcount[2,2,1,x],popcount[3,2,1,x],popcount[1,2,2,x],popcount[2,2,2,x],popcount[3,2,2,x],popcount[1,2,3,x],popcount[2,2,3,x],popcount[3,2,3,x],popcount[1,2,4,x],popcount[2,2,4,x],popcount[3,2,4,x],popcount[1,2,5,x],popcount[2,2,5,x],popcount[3,2,5,x])
  Case = output$Case
  #Case = replace(Case,which(Case==0),NA) # replace 0 with NA
  
  output = cbind(output,Age,Age2,N)
  return(output)
}

dframe_f <- function(cancer){
  ## for females ##
  sel<-cancer$Gender=='Female' & cancer$Race!='All'& cancer$Age.min<80& cancer$Age.min>29
  output = data.frame(rbind(cancer[sel ,c(1:3)], #columns for gender, race, age
                            cancer[sel ,c(1:3)],
                            cancer[sel ,c(1:3)],
                            cancer[sel ,c(1:3)],
                            cancer[sel ,c(1:3)]),
                      Case=
                        ## column for Case ##
                        c(cancer[sel ,4],
                          cancer[sel ,5],
                          cancer[sel ,6],
                          cancer[sel ,7],
                          cancer[sel ,8]),
                      Period =
                        ## column for period ##
                        c(rep(1990,nrow(cancer[sel ,c(1:3)])),
                          rep(1995,nrow(cancer[sel ,c(1:3)])),
                          rep(2000,nrow(cancer[sel ,c(1:3)])),
                          rep(2005,nrow(cancer[sel ,c(1:3)])),
                          rep(2010,nrow(cancer[sel ,c(1:3)])))
  )
  Age=output$Age.min+2
  Age2=Age^2
   x<-7:16
  N=c(popcount[1,2,1,x],popcount[2,2,1,x],popcount[3,2,1,x],popcount[1,2,2,x],popcount[2,2,2,x],popcount[3,2,2,x],popcount[1,2,3,x],popcount[2,2,3,x],popcount[3,2,3,x],popcount[1,2,4,x],popcount[2,2,4,x],popcount[3,2,4,x],popcount[1,2,5,x],popcount[2,2,5,x],popcount[3,2,5,x])
  Case = output$Case
  #Case = replace(Case,which(Case==0),NA) # replace 0 with NA
  output = cbind(output,Age,Age2,N)
  return(output)
}


########## Prepare data frames for model fitting ##########
lung_m = dframe_m(lung) # lung cancer males
lung_f = dframe_f(lung) # lung cancer females

################################# MODEL FOR lung CANCER (BY GENDER) ###############################
## Fit for full Poisson model and check dispersion


fit1<-glm(Case~factor(Race)*Age+factor(Race)*Age2+offset(log(N)),data=lung_m,family=poisson(link="log"))
summary(fit1)
deviance(fit1)/fit1$df.residual

### Do the fit again (stepwise selection with quasiPoisson) 
fit<-glm(Case~factor(Race)*Age+Age2,data=lung_m,offset=log(lung_m$N),family=quasipoisson(link="log"))
summary(fit)
#dump('mod',file="model_lung_M_Poisson.R")
mod=fit
dump('mod',file="model_lung_M_quasiPoisson.R")


fit1<-glm(Case~factor(Race)*Age+factor(Race)*Age2+offset(log(N)),data=lung_f,family=poisson(link="log"))
summary(fit1)
deviance(fit1)/fit1$df.residual


fit<-glm(Case~factor(Race)*Age+Age2,data=lung_f,offset=log(lung_f$N),family=quasipoisson(link="log"))
summary(fit)
mod=fit
dump('mod',file="model_lung_F_quasiPoisson.R")
