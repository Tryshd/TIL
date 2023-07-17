## パッケージインストール
install.packages("sandwich")
library(sandwich)
install.packages("boot")
library(boot)


#### データフレーム作成
NHAI1<-c(20,83,2,9,3,8,1,4,48,38,5,11,3,8,2,4)
NHAI0<-c(1502,2136,125,365,47,139,29,48,1839,808,177,191,43,72,25,38)
Hyp1<-c(2,12,1,1,0,3,0,0,3,5,0,1,0,0,0,1)
Hyp0<-c(198,334,13,66,1,23,2,5,175,87,29,25,0,10,4,7)
Male<-append(rep(0,8),rep(1,8))
Serve<-append(rep(0,4),rep(1,4))
Serve<-append(Serve,Serve)
DM<-rep(c(0,0,1,1),4)
Old<-rep(c(0,1),8)
data<-data.frame(Male=Male,Serve=Serve,DM=DM,Old=Old,NHAI1=NHAI1,NHAI0=NHAI0,Hyp1=Hyp1,Hyp0=Hyp0)

#　重みを表す列生成
weight<-append(data[["NHAI1"]],data[["NHAI0"]])
weight<-append(weight,data[["Hyp1"]])
weight<-append(weight,data[["Hyp0"]])

# 結果変数列生成
outcome<-rep(append(rep(1,16),rep(0,16)),2)

# 曝露変数列生成 正常体温:tempreture=1,低体温:tempreture=0
temperature<-append(rep(1,32),rep(0,32))


### 解析データ
data2<-data.frame(Male=Male,Serve=Serve,DM=DM,Old=Old,temperature=temperature,outcome=outcome,weight=weight)
data2ver2 <- data2[rep(row.names(data2), data2$weight), 1:6]


############# 2項分布 エラーがでる
#' This model can be used to quantify a conditionally adjusted risk ratio with with correct standard error
#' However, error it returns an error and thus does not provide any results.
modelRR_binom <- glm(outcome~tempreture+Male+Serve+DM+Old,data=data2ver2,family = binomial("log"))


############# ポアソン分布
#' This model can be used to quantify a conditionally risk ratio using the Poisson distribuiton and log link function. 
#' However, because the Poisson distribution is used, the model provides incorrect standard error estimates.
modelRR <- glm(outcome~temperature+Male+Serve+DM+Old,data=data2ver2,family = poisson("log"))
tidy(modelRR)[2,]
#' To obtain the correct variance, we use the "sandwich" function to obtain correct sandwich (robust) standard error estimates.
sqrt(sandwich(modelRR)[2,2])


############ Marginal standardization
#' Regress the outcome against the confounders among the unexposed (model0) and then among the exposed (model1)
model0 <- glm(outcome~Male+Serve+DM+Old,data=subset(data2ver2,tempreture==0),family=binomial("logit"))
model1 <- glm(outcome~Male+Serve+DM+Old,data=subset(data2ver2,tempreture==1),family=binomial("logit"))
mu1 <- predict(model1,newdata=data2ver2,type="response")
mu0 <- predict(model0,newdata=data2ver2,type="response")

#' Marginally adjusted risk ratio
marg_stand_RR <- mean(mu1)/mean(mu0)



##ブートストラップ
#' Using the bootstrap to obtain confidence intervals for the marginally adjusted 
#' risk ratio and risk difference.
bootfunc <- function(data,index){
  boot_dat <- data[index,]
  model0 <- glm(outcome~Male+Serve+DM+Old,data=subset(boot_dat,tempreture==0),family=binomial("logit"))
  model1 <- glm(outcome~Male+Serve+DM+Old,data=subset(boot_dat,tempreture==1),family=binomial("logit"))
  mu1 <- predict(model1,newdata=boot_dat,type="response")
  mu0 <- predict(model0,newdata=boot_dat,type="response")
  
  marg_stand_RR <- mean(mu1)/mean(mu0)
  marg_stand_RD <- mean(mu1)-mean(mu0)
  res <- c(marg_stand_RD,marg_stand_RR)
  return(res)
}

set.seed(123)
boot_res <- boot(data2ver2,bootfunc,R=10000)
boot_RR <- boot.ci(boot_res,index=2)

as.matrix(exp(tidy(modelRR)[2,2]))
as.matrix(exp(tidy(modelRR)[2,2] - 1.96*sqrt(sandwich(modelRR)[2,2])))
as.matrix(exp(tidy(modelRR)[2,2] + 1.96*sqrt(sandwich(modelRR)[2,2])))

marg_stand_RR
boot_RR$bca[4]
boot_RR$bca[5]

##結果

#estimate   　ポアソン分布の推定値
#[1,] 1.122911
#> as.matrix(exp(tidy(modelRR)[2,2] - 1.96*sqrt(sandwich(modelRR)[2,2])))
#estimate　　信頼区間下限
#[1,] 0.7699714
#> as.matrix(exp(tidy(modelRR)[2,2] + 1.96*sqrt(sandwich(modelRR)[2,2])))
#estimate　　信頼区間上限
#[1,] 1.637631
#> 
#  > marg_stand_RR
#[1] 0.9512871　 Marginal standardization推定値
#> boot_RR$bca[4]
#[1] 0.7249492　信頼区間下限
#> boot_RR$bca[5]
#[1] 1.139688　 信頼区間上限


