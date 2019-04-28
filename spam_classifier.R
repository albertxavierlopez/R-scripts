# The spam filter that has been 'learned' for you
spam_classifier <- function(x){
  prediction <- rep(NA, length(x)) # initialize prediction vector
  prediction[x > 4] <- 1 
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(factor(prediction, levels = c("1", "0"))) # prediction is either 0 or 1
}


spam_classifier_simple <- function(x){
  prediction <- rep(NA, length(x)) # initialize prediction vector
  prediction[x > 4] <- 1 
  prediction[x <= 4] <- 0
  return(factor(prediction, levels = c("1", "0"))) # prediction is either 0 or 1
}

a<-c(1:10)

spam_classifier_simple(a)


# Apply spam_classifier to emails_full: pred_full
pred_full<-spam_classifier(a)

# Build CONFUSION MATRIX
conf_full <- table(a, pred_full)

# Calculate the ACCURACY 
acc_full <- sum(diag(conf_full)) / sum(conf_full)

# Print out ACCURACY
acc_full

