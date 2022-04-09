# variables in imputation model

varlist <- c("ID", "B1", "B2", "B3", "B4", "C1", "C2", "C3", "Status_event")

# create a new dataset for imputation
data.impu = data[varlist]

# categorical variable as factor
data.impu$B1 <- factor(data.impu$B1)
data.impu$B2 <- factor(data.impu$B2)
data.impu$B3 <- factor(data.impu$B3)
data.impu$B4 <- factor(data.impu$B4)

data.impu$Status_event <- factor(data.impu$Status_event)

# see all the default settings for imputation
impu_default <- mice(data.impu, maxit = 0)
summary(impu_default)

# see the predictor structure
pred <- quickpred(data.impu, exclude = c("ID","Time_event"))
pred

# imputation method
meth <- impu_default$meth
meth    # imputation method can be changed manually

# imputation

# single imputation (m=1)
imputation <- mice(data.impu, maxit = 25, m = 1, seed = 1234, pred = pred, meth = meth, print = TRUE)

data_single <- mice::complete(imputation, 1)

nrow(data_single)
summary(data_single)


# predictors selection and fit the model
# full model

# define outcome and predictors
Outcome <- "Status_event"
CandidateVariables <- c("B1", "B2", "B3", "B4", "C1", "C2", "C3")

# create a formula
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(CandidateVariables, collapse=" + ")))

# fit a model with all candidate varaibles
model.full <- glm(Formula, data=data,family=binomial)

summary(model.full)

# Lasso

# Make data frame into matrix
tmp.y <- data$Status_event
tmp.x <- model.matrix(~.,data[CandidateVariables])

# Fit the model
model.lasso <-  glmnet(tmp.x, tmp.y, family="binomial", nlambda=50, alpha=1, standardize=TRUE)

plot(model.lasso,xvar="lambda",label=TRUE)

# find the optimal model via cross-validation
cv.model <- cv.glmnet(tmp.x, tmp.y, family="binomial", nlambda=50, alpha=1, standardize=TRUE)
plot(cv.model)
cv.model$lambda.min
coef(cv.model, s=cv.model$lambda.min) 

# increase lambda for further shrinkage
cv.model$lambda.1se
coef(cv.model, s=cv.model$lambda.1se) 

# fit a model with all selected varaibles
FinalVariables <- c("B1","B2", "B3", "C1" )
Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                         paste(FinalVariables, collapse=" + ")))

model.final <- glm(Formula, data=data,family=binomial)

summary(model.final)

# ROC curve
data$p_prediction <- predict(model.final, type="response")

roc.final <- roc(data$Status_event, data$p_prediction)

plot(roc.final)

# calibration plot
val.prob(p = fitted(model.final), y = data$Status_event,logistic.cal=F)


#Bootstrap

# first calculate apparent performance
model.train <- glm(Formula, data=data,family=binomial)
# calculate model performance in bootstrap dataset
data$p_prediction <- predict(model.train, data, type="response")
roc.apparent <- roc(data$Status_event, data$p_prediction)
c_apparent <- roc.apparent$auc
brier_apparent <- mean((data$p_prediction-data$Status_event)^2)

# then use bootstrap to calculate "optimism"

N_bootstrap <- 100      # or 1000 if needed

c_resample <- 0       # C statistics in resample data
c_original <- 0       # C statistics in original data
brier_resample <- 0   # brier score in resample data
brier_original <- 0    # brier score in original data

for (i in 1:N_bootstrap){
  
  
  #data.train <- data[sample(1:nrow(data), replace=TRUE),]
  data.train <- data.impu[sample(1:nrow(data.impu), replace=TRUE),]
  
  # imputation
  impu_default <- mice(data.train, maxit = 0)
  summary(impu_default)
  
  # see the predictor structure
  pred <- quickpred(data.train, exclude = c("ID"))
  pred
  
  # imputation method
  meth <- impu_default$meth
  meth    # imputation method can be changed manually
  
  # single imputation (m=1)
  imputation <- mice(data.train, maxit = 25, m = 1, seed = 1234, pred = pred, meth = meth, print = TRUE)
  data_single <- mice::complete(imputation, 1)
  nrow(data_single)
  summary(data_single)
  data.train <- data_single
  
  # train the model with the bootstrap dataset
  
  # define outcome and predictors
  Outcome <- "Status_event"
  CandidateVariables <- c("B1", "B2", "B3", 
                          "C1", "C2")
  
  # create a formula
  Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                           paste(CandidateVariables, collapse=" + ")))
  
  # fit a model with all candidate varaibles
  
  model.full <- glm(Formula, data= data.train,family=binomial)
  
  # LASSO selection and fit model
  
  tmp.y <- data.train$Status_event
  tmp.x <- model.matrix(~.,data.train[CandidateVariables])
  model.lasso <-  glmnet(tmp.x, tmp.y, family="binomial", nlambda=50, alpha=1, standardize=TRUE)
  
  plot(model.lasso,xvar="lambda",label=TRUE)
  cv.model <- cv.glmnet(tmp.x, tmp.y, family="binomial", nlambda=50, alpha=1, standardize=TRUE)
  plot(cv.model)
  cv.model$lambda.1se
  coef(cv.model, s=cv.model$lambda.1se)  
  
  select <-coef(cv.model, s=cv.model$lambda.1se) 
  CandidateVariables[summary(select)$i[-1]-2]
  Formula <- formula(paste(paste(Outcome,"~", collapse=" "), 
                           paste(FinalVariables, collapse=" + ")))
  
  model.train <- glm(Formula, data=data,family=binomial)
  
  # calculate model performance in bootstrap dataset
  
  data.train$p_prediction <- predict(model.train, data.train, type="response")
  
  roc.train <- roc(data.train$Status_event, data.train$p_prediction)
  
  c_resample[i] <- roc.train$auc
  
  brier_resample[i] <- mean((data.train$p_prediction-data.train$Status_event)^2)
  
  
  # evaluate the model in original dataset
  
  data.test <- data
  
  data.test$p_prediction <- predict(model.train, data.test, type="response")
  
  roc.test <- roc(data.test$Status_event, data.test$p_prediction)
  
  c_original[i] <- roc.test$auc
  
  brier_original[i] <- mean((data.test$p_prediction-data.test$Status_event)^2)
  
}

# calculate the average optimism

c_optimism <- mean(c_resample - c_original)
brier_optimism <- mean(brier_resample - brier_original)


# optimism adjusted C statistics
c_apparent - c_optimism

# optimism adjusted brier score
brier_apparent - brier_optimism

# publish prediction calcualtor

# install rsconnect first
# registry at https://www.shinyapps.io/
# get token,secret
# run code

DNbuilder(model.final, covariate = "numeric", DNtitle = "LMpredictors")

rsconnect::setAccountInfo(name='...', token='...', secret='...')
rsconnect::deployApp('DynNomapp')


# Shiny tutor: https://shiny.rstudio.com/tutorial/

# static nomogram
plot(nomogram(model.final, fun = plogis, lp = FALSE), xfrac=.15, cex.axis=.7)


# Decision Curve Analysis

dca <- function(data, outcome, predictors, xstart=0.01, xstop=0.99, xby=0.01, 
                ymin=-0.05, probability=NULL, harm=NULL,graph=TRUE, intervention=FALSE, 
                interventionper=100, smooth=FALSE,loess.span=0.10) {
  
  # LOADING REQUIRED LIBRARIES
  require(stats)
  
  # data MUST BE A DATA FRAME
  if (class(data)!="data.frame") {
    stop("Input data must be class data.frame")
  }
  
  #ONLY KEEPING COMPLETE CASES
  data=data[complete.cases(data[append(outcome,predictors)]),append(outcome,predictors)]
  
  # outcome MUST BE CODED AS 0 AND 1
  if (max(data[[outcome]])>1 | min(data[[outcome]])<0) {
    stop("outcome cannot be less than 0 or greater than 1")
  }
  # xstart IS BETWEEN 0 AND 1
  if (xstart<0 | xstart>1) {
    stop("xstart must lie between 0 and 1")
  }
  
  # xstop IS BETWEEN 0 AND 1
  if (xstop<0 | xstop>1) {
    stop("xstop must lie between 0 and 1")
  }
  
  # xby IS BETWEEN 0 AND 1
  if (xby<=0 | xby>=1) {
    stop("xby must lie between 0 and 1")
  }
  
  # xstart IS BEFORE xstop
  if (xstart>=xstop) {
    stop("xstop must be larger than xstart")
  }
  
  #STORING THE NUMBER OF PREDICTORS SPECIFIED
  pred.n=length(predictors)
  
  #IF probability SPECIFIED ENSURING THAT EACH PREDICTOR IS INDICATED AS A YES OR NO
  if (length(probability)>0 & pred.n!=length(probability)) {
    stop("Number of probabilities specified must be the same as the number of predictors being checked.")
  }
  
  #IF harm SPECIFIED ENSURING THAT EACH PREDICTOR HAS A SPECIFIED HARM
  if (length(harm)>0 & pred.n!=length(harm)) {
    stop("Number of harms specified must be the same as the number of predictors being checked.")
  }
  
  #INITIALIZING DEFAULT VALUES FOR PROBABILITES AND HARMS IF NOT SPECIFIED
  if (length(harm)==0) {
    harm=rep(0,pred.n)
  }
  if (length(probability)==0) {
    probability=rep(TRUE,pred.n)
  }
  
  
  #CHECKING THAT EACH probability ELEMENT IS EQUAL TO YES OR NO, 
  #AND CHECKING THAT PROBABILITIES ARE BETWEEN 0 and 1
  #IF NOT A PROB THEN CONVERTING WITH A LOGISTIC REGRESSION
  for(m in 1:pred.n) { 
    if (probability[m]!=TRUE & probability[m]!=FALSE) {
      stop("Each element of probability vector must be TRUE or FALSE")
    }
    if (probability[m]==TRUE & (max(data[predictors[m]])>1 | min(data[predictors[m]])<0)) {
      stop(paste(predictors[m],"must be between 0 and 1 OR sepcified as a non-probability in the probability option",sep=" "))  
    }
    if(probability[m]==FALSE) {
      model=NULL
      pred=NULL
      model=glm(data.matrix(data[outcome]) ~ data.matrix(data[predictors[m]]), family=binomial("logit"))
      pred=data.frame(model$fitted.values)
      pred=data.frame(pred)
      names(pred)=predictors[m]
      data=cbind(data[names(data)!=predictors[m]],pred)
      print(paste(predictors[m],"converted to a probability with logistic regression. Due to linearity assumption, miscalibration may occur.",sep=" "))
    }
  }
  
  # THE PREDICTOR NAMES CANNOT BE EQUAL TO all OR none.
  if (length(predictors[predictors=="all" | predictors=="none"])) {
    stop("Prediction names cannot be equal to all or none.")
  }  
  
  # CALCULATING NET BENEFIT  
  N=dim(data)[1]
  event.rate=colMeans(data[outcome])
  
  # CREATING DATAFRAME THAT IS ONE LINE PER THRESHOLD PER all AND none STRATEGY
  nb=data.frame(seq(from=xstart, to=xstop, by=xby))
  names(nb)="threshold"
  interv=nb
  
  nb["all"]=event.rate - (1-event.rate)*nb$threshold/(1-nb$threshold)
  nb["none"]=0
  
  # CYCLING THROUGH EACH PREDICTOR AND CALCULATING NET BENEFIT
  for(m in 1:pred.n){
    for(t in 1:length(nb$threshold)){
      # COUNTING TRUE POSITIVES AT EACH THRESHOLD
      tp=mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome])*sum(data[[predictors[m]]]>=nb$threshold[t])
      # COUNTING FALSE POSITIVES AT EACH THRESHOLD
      fp=(1-mean(data[data[[predictors[m]]]>=nb$threshold[t],outcome]))*sum(data[[predictors[m]]]>=nb$threshold[t])
      #setting TP and FP to 0 if no observations meet threshold prob.
      if (sum(data[[predictors[m]]]>=nb$threshold[t])==0) {
        tp=0
        fp=0
      }
      
      # CALCULATING NET BENEFIT
      nb[t,predictors[m]]=tp/N - fp/N*(nb$threshold[t]/(1-nb$threshold[t])) - harm[m]
    }
    interv[predictors[m]]=(nb[predictors[m]] - nb["all"])*interventionper/(interv$threshold/(1-interv$threshold))
  }
  
  # CYCLING THROUGH EACH PREDICTOR AND SMOOTH NET BENEFIT AND INTERVENTIONS AVOIDED 
  for(m in 1:pred.n) {
    if (smooth==TRUE){
      lws=loess(data.matrix(nb[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(nb[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      nb[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
      
      lws=loess(data.matrix(interv[!is.na(nb[[predictors[m]]]),predictors[m]]) ~ data.matrix(interv[!is.na(nb[[predictors[m]]]),"threshold"]),span=loess.span)
      interv[!is.na(nb[[predictors[m]]]),paste(predictors[m],"_sm",sep="")]=lws$fitted
    }
  }
  
  # PLOTTING GRAPH IF REQUESTED
  if (graph==TRUE) {
    require(graphics)
    
    # PLOTTING INTERVENTIONS AVOIDED IF REQUESTED
    if(intervention==TRUE) {
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- NULL
      legendcolor <- NULL
      legendwidth <- NULL
      legendpattern <- NULL
      
      #getting maximum number of avoided interventions
      ymax=max(interv[predictors],na.rm = TRUE)
      
      #INITIALIZING EMPTY PLOT WITH LABELS
      plot(x=nb$threshold, y=nb$all, type="n" ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab=paste("Net reduction in interventions per",interventionper,"patients"))
      
      #PLOTTING INTERVENTIONS AVOIDED FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(interv$threshold,data.matrix(interv[paste(predictors[m],"_sm",sep="")]),col=m,lty=2)
        } else {
          lines(interv$threshold,data.matrix(interv[predictors[m]]),col=m,lty=2)
        }
        
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    } else {
      # PLOTTING NET BENEFIT IF REQUESTED
      
      # initialize the legend label, color, and width using the standard specs of the none and all lines
      legendlabel <- c("None", "All")
      legendcolor <- c(17, 8)
      legendwidth <- c(2, 2)
      legendpattern <- c(1, 1)
      
      #getting maximum net benefit
      ymax=max(nb[names(nb)!="threshold"],na.rm = TRUE)
      
      # inializing new benfit plot with treat all option
      plot(x=nb$threshold, y=nb$all, type="l", col=8, lwd=2 ,xlim=c(xstart, xstop), ylim=c(ymin, ymax), xlab="Threshold probability", ylab="Net benefit")
      # adding treat none option
      lines(x=nb$threshold, y=nb$none,lwd=2)
      #PLOTTING net benefit FOR EACH PREDICTOR
      for(m in 1:pred.n) {
        if (smooth==TRUE){
          lines(nb$threshold,data.matrix(nb[paste(predictors[m],"_sm",sep="")]),col=m,lty=2) 
        } else {
          lines(nb$threshold,data.matrix(nb[predictors[m]]),col=m,lty=2)
        }
        # adding each model to the legend
        legendlabel <- c(legendlabel, predictors[m])
        legendcolor <- c(legendcolor, m)
        legendwidth <- c(legendwidth, 1)
        legendpattern <- c(legendpattern, 2)
      }
    }
    # then add the legend
    legend("topright", legendlabel, cex=0.8, col=legendcolor, lwd=legendwidth, lty=legendpattern)
    
  }
  
  #RETURNING RESULTS
  results=list() 
  results$N=N
  results$predictors=data.frame(cbind(predictors,harm,probability))
  names(results$predictors)=c("predictor","harm.applied","probability")
  results$interventions.avoided.per=interventionper
  results$net.benefit=nb
  results$interventions.avoided=interv
  
  return(results)
  
}  

# Univariate Decision Curve Analysis
# Test whether family history is associated with cancer
summary(glm(Status_event ~ B1, family=binomial(link="logit")))
# Run the decision curve: family history is coded as 0 or 1, i.e. a probability
dca(data=data, outcome="Status_event", predictors="B1")

# Restricting Threshold Probability
dca(data=data.set, outcome="Status_event", predictors="famhistory", xstop=0.7) 

# Multivariable Decision Curve Analysis
# run the multivariable model
model = glm(Status_event ~ B1 + B2 + B3 +C1 +C2, family=binomial(link="logit"))
# save out predictions in the form of probabilities
data$p_prediction = predict(model, type="response")

# Run decision curve
dca(data=data.set, outcome="Status_event", predictors=c("p_prediction","B1"), 
    xstop=0.7)

# Saving out Net Benefit Values
#Run the decision curve, specify xby=0.05 since we want 5% increments
output = dca(data=data.set, outcome="cancer", predictors="marker", probability=F, 
             xstart=0.05, xstop=0.35, xby=0.05, graph=F) 
# Calculate difference between marker and treat all
# Our standard approach is to biopsy everyone so this tells us
# how much better we do with the marker
output$net.benefit$advantage=output$net.benefit$marker-output$net.benefit$all 

output

# Interventions Avoided
dca(data=data.set, outcome="Status_event", predictors="p_prediction", probability=FALSE, 
    intervention=TRUE, xstart=0.4, xstop=1)
