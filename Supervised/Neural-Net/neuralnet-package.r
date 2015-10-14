# Neural Networks
# http://www.di.fc.ul.pt/~jpn/r/neuralnets/neuralnets.html

#  neuralnet package
# install.packages("neuralnet")

      library(neuralnet)

# Example using simulated data
      # create a neural network to perform square rooting using backpropagation
      traininginput <- as.data.frame(runif(50, min = 0, max = 100))
      trainingoutput <- sqrt(traininginput)
      
      trainingdata <- cbind(traininginput, trainingoutput)
      colnames(trainingdata) <- c("Input", "Output")
      
      # train the neural network with backpropagation, 10 hidden layers
      # threshold is a numeric value specifying th threshold for the
      # partial derivatives of th error function as a stopping criteria.
      net.sqrt <- neuralnet(Output ~ Input, trainingdata, hidden = 10, threshold = 0.01)
      print(net.sqrt)
      
      # Plot it
      plot(net.sqrt)
      
      # Test it on test data
      testdata <- as.data.frame((1:10) ^ 2)
      net.results <- compute(net.sqrt, testdata)
      
      ls(net.results)
      
      # Display results
      cleanoutput <- cbind(testdata, sqrt(testdata),
                           as.data.frame(net.results$net.result))
      colnames(cleanoutput) <- c("Input", "Expected Output", "Neural Net Output")
      print(cleanoutput)
      
      
      
# Example using iris data
      set.seed(802)
      size.sample <- 50
      train <- sample(1:nrow(iris), size.sample)
      nnet_iristrain <- iris[train,]
      
      
      # Binarize the categorical output
      nnet_iristrain <- cbind(nnet_iristrain, iristrain$Species == 'setosa')
      nnet_iristrain <- cbind(nnet_iristrain, iristrain$Species == 'versicolor')
      nnet_iristrain <- cbind(nnet_iristrain, iristrain$Species == 'virginica')
      
      names(nnet_iristrain)[6:8] <- c("setosa", "versicolor", "virginica")
      
      # create model
      nn <- neuralnet(setosa + versicolor + virginica ~
                        Sepal.Length + Sepal.Width +
                        Petal.Length + Petal.Width,
                      data = nnet_iristrain,
                      hidden = c(3))
      
      plot(nn)
      
      # evaluate
      test <- iris[-train,]
      
      mypredict <- compute(nn, test[-5])$net.result
      maxidx <- function(arr){
        return(which(arr == max(arr)))
      }
      
      idx <- apply(mypredict, c(1), maxidx)
      prediction <- c('setosa', 'versicolor', 'virginica')[idx]
      table(prediction, test$Species)
