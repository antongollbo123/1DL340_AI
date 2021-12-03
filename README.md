# 1DL340_AI
Coursework in Artificial Intelligence course at Uppsala University

ASSIGNMENT 2: Wheres Croc? 
Your task is to implement a control system to compete on the Where’s Croc game. You will use hidden Markov models and associated algorithms to work out where Croc is given the sequence of observable variables given to you in the game. Read the runWheresCroc help documentation in the R package for more details. The hidden Markov model will permit you to model the dynamic probabilities of Croc being at different water holes. This is important, but not the only important matter in performing well in the game. You will have to think about what else is important and how you can perform as well as possible

ASSIGNMENT 3 (NOT COMPLETE): Diagnostics
Your task is to set up this Bayesian network, train it from a set of 10000 historical cases and be able to use it to estimate the probability of the four illness variables when all other variables are observed using Metropolis in Gibbs MCMC sampling. This will mean creating one function that trains the network from the historical cases, and one function that is used to predict the probabilities of illness variables given observed values of the other variables. 


ASSIGNMENT 4: CIFAR-10 Image Classification
This assignment is a simple image classification task: you need to build and train a convolutional neural network so that it can classify images according to their content. We will use the CIFAR10 dataset. This contains small (32x32) color images, consisting of 10 classes. A data wrapping CIFAR class has been created for use in this project (it is included in the project code file). This class will divide the data into training, validation and test cases. You will train a CNN using the training/validation data and we will evaluate your network’s performance on the test data. The division of data into training, validation and test depends on a random seed. When we evaluate your code, we will choose a random seed to make this division. 
