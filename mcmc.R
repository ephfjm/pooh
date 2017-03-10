#setwd('c:/users/ephfjm/documents')
source('~/pop_smoke.R')
dframe<- function(cancer){
  if (cancer$Gender[1]=='Female') {gender=1
  }else gender=2
  N=dim(cancer)[1]
  cancer<-cbind(cancer,matrix(0,N,3))
  names(cancer)[9:11]<-c('Never','Current','Ex')
  for(i in 1:N){
    if (cancer$Race[i]=='Chinese') {race=1
    } else if (cancer$Race[i]=='Malay') {race=2
    } else race=3
    if (cancer$Period[i]==1990) {period=1
    } else if (cancer$Period[i]==1995) {period=2
    } else if (cancer$Period[i]==2000) {period=3
    } else if (cancer$Period[i]==2005) {period=4
    } else period=5
    if (cancer$Age.min[i]==30) {agegroup=7
    } else if (cancer$Age.min[i]==35) {agegroup=8
    } else if (cancer$Age.min[i]==40) {agegroup=9
    } else if (cancer$Age.min[i]==45) {agegroup=10
    } else if (cancer$Age.min[i]==50) {agegroup=11
    } else if (cancer$Age.min[i]==55) {agegroup=12
    } else if (cancer$Age.min[i]==60) {agegroup=13
    } else if (cancer$Age.min[i]==65) {agegroup=14
    } else if (cancer$Age.min[i]==70) {agegroup=15
    } else agegroup=16
    for (smoke in 1:3)
    cancer[i,smoke+8]<-pop_smoke[race,gender,period,agegroup,smoke]
  }
  return(cancer)
}

lung_m_smoke<-dframe(lung_m)
lung_f_smoke<-dframe(lung_f)


library(rjags)
dataset=list(Cancer_case=lung_m_smoke$Case,N=dim(lung_m_smoke)[1],Race=lung_m_smoke$Race, Age=lung_m_smoke$Age,
             Current=lung_m_smoke$Current, Never=lung_m_smoke$Never, Ex=lung_m_smoke$Ex)
inits <- function() list(b1=0,b2=0,b3=0,a1=0,a2=0,a3=0)
lung.jags <- jags.model("jagsmodel.txt", data = dataset, inits=inits,n.chains=1)
lung.sims <- coda.samples(lung.jags, c('a1','a2','a3'),n.iter=1000,progress.bar='none',thin=1,quiet=quiet)
summary(lung.sims)
