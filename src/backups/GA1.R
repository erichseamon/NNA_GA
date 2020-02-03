library(GA)
library(parallel)
library(doParallel)
# data_x: input data frame
# data_y: target variable (factor)
# GA parameters

data <- read.csv("./data/wheat_transformed.csv", header=TRUE)
data_x <- data[-c(940)]
data_y <- data[940]
#vars<- colnames(data_x)





param_nBits=ncol(data_x)
col_names=colnames(data_x)
# Executing the GA 
ga_GA_1 = GA::ga(fitness = function(vars) custom_fitness(vars = vars, 
                                                     data_x =  data_x, 
                                                     data_y = data_y, 
                                                     p_sampling = 0.7), # custom fitness function
             type = "binary", # optimization data type
             crossover=gabin_uCrossover,  # cross-over method
             elitism = 3, # best N indiv. to pass to next iteration
             pmutation = 0.03, # mutation rate prob
             popSize = 50, # the number of indivduals/solutions
             nBits = param_nBits, # total number of variables
             names=col_names, # variable name
             run=5, # max iter without improvement (stopping criteria)
             maxiter = 50, # total runs or generations
             monitor=plot, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = T, # allow parallel procesing
             seed=84211 # for reproducibility purposes
)
# Checking the results
summary(ga_GA_1)
── Genetic Algorithm ─────────────────── 
GA settings: 
  Type                  =  binary 
Population size       =  50 
Number of generations =  50 
Elitism               =  3 
Crossover probability =  0.8 
Mutation probability  =  0.03 
GA results: 
  Iterations             = 17 
Fitness function value = 0.2477393 
Solution = 
  radius_mean texture_mean perimeter_mean area_mean smoothness_mean compactness_mean
[1,]           0            1              0         0               0                1
concavity_mean concave points_mean symmetry_mean fractal_dimension_mean  ... 
[1,]              0                   0             0                      0      
symmetry_worst fractal_dimension_worst
[1,]              0                       0
# Following line will return the variable names of the final and best solution
best_vars_ga=col_names[ga_GA_1@solution[1,]==1]
# Checking the variables of the best solution...
best_vars_ga
[1] "texture_mean"     "compactness_mean" "area_worst"       "concavity_worst"





get_accuracy_metric(data_tr_sample = data_x, target = data_y, best_vars_ga)