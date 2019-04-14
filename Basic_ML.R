##############################
### CLUSTERING with KMEANS ###
##############################

      # Set random seed. Don't remove this line.
      set.seed(1)

      # Chop up iris in my_iris and species
      my_iris <- iris[-5]
      species <- iris$Species

      # Perform k-means clustering on my_iris in 3 clusters: kmeans_iris
      kmeans_iris<-kmeans(my_iris, 3)

      # Compare the actual Species to the clustering using table() // The same for a confussion matrix
      table(species, kmeans_iris$cluster)

      # Plot Petal.Width against Petal.Length, coloring by cluster
      plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)

      # MEASURING ACCURACY ratio of the WSS to the BSS
      # Print out the ratio of the within sum of squares to the between cluster sum of squares, so WSS/BSS. 
      # Print out the ratio of the WSS to the BSS
      kmeans_iris$tot.withinss/kmeans_iris$betweenss



##############################
### CLASSIFICATION by hand ###
##############################

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



########################
###### Tree Model ######
########################

      # Set random seed. Don't remove this line.

      library(rpart)
      library(rattle)
      library(rpart.plot)
      library(RColorBrewer)

      set.seed(1)

      # Take a look at the iris dataset
      str(iris)
      summary(iris)

      # A decision tree model has been built for you
      tree <- rpart(Species ~ .,
                    data = iris, method = "class") #classification method

      # A dataframe containing unseen observations
      unseen <- data.frame(Sepal.Length = c(5.3, 7.2),
                           Sepal.Width = c(2.9, 3.9),
                           Petal.Length = c(1.7, 5.4),
                           Petal.Width = c(0.8, 2.3))

      # Predict the label of the unseen observations. Print out the result.
      predict(tree, unseen, type="class")

      head(iris)

      # Plot rpart
      fancyRpartPlot(tree)

      # Prune the tree:method to shrink tree to a more compact tree, 
      pruned<-prune(tree, cp=0.01)

      # Visualize the new pruned tree (this is a simple case but should be a shrinked version)
      fancyRpartPlot(pruned)

