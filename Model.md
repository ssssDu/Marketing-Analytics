---
title: "Marketing_Project"
output: html_document
---

## Fitting a explanatory model for email open rate based on demographic information of respondent


#### The data is highly skewed and cannot be transformed directly with BOXCOX since dependent variables contain zeros. We first tried fitting a regression model with all variables and yielded R-sq of 0.5%.The poor performance may due to the violation of residual assumption. To transform and fix the assumption violation, we seek to exclude zeros in the depedent variable.<br> 

#### The concept is to use the two-stage tobit model. We first used the choice-based logistic model to predict open rate. As can be seen from the result in conversion matrix, the model performs poorly. After that, we exclude customers that have zero open rates and run linear regression on the rest of customers to predict their open rate. We compared our results of linear regression with and without box-cox transformation. With the box-cox transformation before the linear model, the R^2 increased and is now 14% and 8% for active and unsubscribe customers respectively. <br>


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, message = FALSE, warning = FALSE}
library("EnvStats")
library(forecast)
library(caret)
```

## Data cleaning, basic intergrity check and dummy-coding

```{r, warning = FALSE}

data_main<- read.csv(file="active_unsub.csv", header=TRUE, sep=",")
data_stage<- subset(data_main, select=-c(Last_order_date_historic))

# exclude zeros 
fil<- which(data_stage$Average_Order_Value >= 0 & data_stage$Customer_Metrics_Orders >= 0 & data_stage$Customer_Metrics_Orders %%1 == 0)
data_stage <- data_stage[fil,]

# pragma
data_stage["state"] =as.numeric(data_stage["state"] =="tx") 
act_data = data_stage[which(data_stage$Status =="active"),]
act_data = act_data[,c(-1,-2)]
unsub_data = data_stage[which(data_stage$Status =="unsub"),]
unsub_data = unsub_data[,c(-1,-2)]

```

### Two steps logit-OLS model<p>

#### Choise-based Logistic Regression<br>
Model onto two subset: active respondent and unsubscribe respondent

```{r,  warning = FALSE}

#Choise based logistic with logit transformation - active
act_data_logit = act_data
act_data_logit["Open.Rate"] = 1-(as.numeric(act_data_logit["Open.Rate"] ==0))

#50/50 dependent variabe for choise-based selection
act_data_logit_1 = act_data_logit[which(act_data_logit["Open.Rate"]!= 0),]
act_data_logit_0 = act_data_logit[which(act_data_logit["Open.Rate"]== 0),]
cb_act <- rbind(act_data_logit_1[sample(1:nrow(act_data_logit_1),8000),],act_data_logit_0[sample(1:nrow(act_data_logit_0),8000),])
logit_act <- glm(Open.Rate~.,family=binomial(link='logit'),data=cb_act)

#intercept correction
popu_cor = mean(act_data_logit$Open.Rate)
samp_cor = mean(cb_act$Open.Rate)
offset = log((1-popu_cor)*(samp_cor)/(1-samp_cor)/(popu_cor))
logit_act$coefficients[1] = logit_act$coefficients[1] -offset

#Choise based logistic with logit transformation - unsubscribe
unsub_data_logit = unsub_data
unsub_data_logit["Open.Rate"] = 1-(as.numeric(unsub_data_logit["Open.Rate"] ==0))

#50/50 dependent variabe for choise-based selection
unsub_data_logit_1 = unsub_data_logit[which(unsub_data_logit["Open.Rate"]!= 0),]
unsub_data_logit_0 = unsub_data_logit[which(unsub_data_logit["Open.Rate"]== 0),]
cb_unsub <- rbind(unsub_data_logit_1[sample(1:nrow(unsub_data_logit_1),83),],unsub_data_logit_0[sample(1:nrow(unsub_data_logit_0),83),])
logit_unsub <- glm(Open.Rate~.,family=binomial(link='logit'),data=cb_unsub)

#intercept correction
popu_cor = mean(unsub_data_logit$Open.Rate)
offset = log((1-popu_cor)*(samp_cor)/(1-samp_cor)/(popu_cor))
logit_unsub$coefficients[1] = logit_unsub$coefficients[1] -offset

```

#### Cut-off Threshold selection to test choiced-based logistic model predictive accuracy
```{r, warning = FALSE}

#active
#baseline: 0.847
for (thres in c(0.1,0.3,0.5,0.7,0.9)){
  pred = vector()
  for (num in predict(logit_act,act_data_logit[,-1])){
    if (num > thres){pred = c(1,pred)
    }else{
      pred = c(0,pred)
    }
  }
  print('act')
  print(thres)
  print(confusionMatrix(factor(pred),factor(unlist(act_data_logit["Open.Rate"]))))
}

##unsub
#baseline: 0.9850099
for (thres in c(0.1,0.3,0.5,0.7,0.9)){
  pred = vector()
  for (num in predict(logit_unsub,unsub_data_logit[,-1])){
    if (num > thres){pred = c(1,pred)
    }else{
      pred = c(0,pred)
    }
  }
  print('unsub')
  print(thres)
  print(confusionMatrix(factor(pred),factor(unlist(unsub_data_logit["Open.Rate"]))))
}

```

### Linear Regression in condition of logistic model<br>
#### Fit linear model onto active and unsubscribe datasets after boxcox transformation to fit residual assumption. 
```{r,warning = FALSE}

#Active - no boxcox Linear Model
act_data_linear = act_data[which(act_data$Open.Rate != 0),]
pre_lm = lm(Open.Rate~., data= act_data_linear)
summary(pre_lm)
#Active - boxcox Linear Model
bc<-boxcox(pre_lm, plotit = F)
lambda <- bc$lambda[which.max(bc$objective)]
act_data_linear$Open.Rate = BoxCox(act_data_linear$Open.Rate, lambda)
bc_lm = lm(Open.Rate~., data= act_data_linear)
summary(bc_lm)

#unsubscribe - no boxcox Linear Model
unsub_data_linear = unsub_data[which(unsub_data$Open.Rate != 0),]
pre_lm = lm(Open.Rate~., data= unsub_data_linear)
summary(pre_lm)
#unsubscribe - boxcox Linear Model
bc<-boxcox(pre_lm, plotit = F)
lambda <- bc$lambda[which.max(bc$objective)]
unsub_data_linear$Open.Rate = BoxCox(unsub_data_linear$Open.Rate, lambda)
bc_lm = lm(Open.Rate~., data= unsub_data_linear)
summary(bc_lm)

```


