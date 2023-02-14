
## The ‘MDPtoolbox’ package in R is a simple Markov decision process package which uses the Markov process to learn reinforcement.
### Refer to Slide 13
###  install packages
#install.packages("MDPtoolbox")
library(MDPtoolbox)

#Let’s define the actions.
#This will be a probability matrix, so we will use the matrix() function such that the sum of probabilities in each row is 1
##Refer to Slide 14
#Up Action
up=matrix(c( 1, 0, 0, 0,
             0.7, 0.2, 0.1, 0,
             0, 0.1, 0.2, 0.7,
             0, 0, 0, 1),
          nrow=4,ncol=4,byrow=TRUE)

#Down Action
down=matrix(c(0.3, 0.7, 0, 0,
              0, 0.9, 0.1, 0,
              0, 0.1, 0.9, 0,
              0, 0, 0.7, 0.3),
            nrow=4,ncol=4,byrow=TRUE)

#Left Action
left=matrix(c( 0.9, 0.1, 0, 0,
               0.1, 0.9, 0, 0,
               0, 0.7, 0.2, 0.1,
               0, 0, 0.1, 0.9),
            nrow=4,ncol=4,byrow=TRUE)

#Right Action
right=matrix(c( 0.9, 0.1, 0, 0,
                0.1, 0.2, 0.7, 0,
                0, 0, 0.9, 0.1,
                0, 0, 0.1, 0.9),
             nrow=4,ncol=4,byrow=TRUE)
##Refer to Slide 15
# Combined Actions matrix
Actions=list(up=up, down=down, left=left, right=right)


# Defining the rewards and penalties
Rewards=matrix(c( -1, -1, -1, -1,
                  -1, -1, -1, -1,
                  -1, -1, -1, -1,
                  10, 10, 10, 10),
               nrow=4,ncol=4,byrow=TRUE)

## Refer to slide 16
# Solving the navigation
solver=mdp_policy_iteration(P=Actions, R=Rewards, discount = 0.1)

# Getting the policy
solver$policy #2 4 1 1
names(Actions)[solver$policy] #"down"  "right" "up" "up"

### Refer to slide 17
# Getting the Values at each step. These values can be different in each step
solver$V 

#Additional information: Number of iterations
solver$iter

## Additional information: Time taken. This time can be different in each step
solver$time 


# Getting into rough games - ReinforcementLearning github package
##Refer to slide 18
#install.packages("devtools")
library(devtools)

# Download and install latest version from GitHub
install_github("nproellochs/ReinforcementLearning")
force = TRUE
library(ReinforcementLearning)

# Viewing the pre-built function for each state, action and reward

print(gridworldEnvironment)

function (state, action) 
{
  next_state <- state
  if (state == state("s1") && action == "down") 
    next_state <- state("s2")
  if (state == state("s2") && action == "up") 
    next_state <- state("s1")
  if (state == state("s2") && action == "right") 
    next_state <- state("s3")
  if (state == state("s3") && action == "left") 
    next_state <- state("s2")
  if (state == state("s3") && action == "up") 
    next_state <- state("s4")
  if (next_state == state("s4") && state != state("s4")) {
    reward <- 10
  }
  else {
    reward <- -1
  }
  out <- list(NextState = next_state, Reward = reward)
  return(out)
}

  # Define names for state and action
  states <- c("s1", "s2", "s3", "s4")
actions <- c("up", "down", "left", "right")

# Generate 1000 iterations
sequences <- sampleExperience(N = 2000, env = gridworldEnvironment, states = states, actions = actions)

#Solve the problem
solver_rl <- ReinforcementLearning(sequences, s = "State", a = "Action", r = "Reward", s_new = "NextState")

#Getting the policy; this may be different for each run
solver_rl$Policy


#Getting the Reward; this may be different for each run
solver_rl$Reward #-351

# Conclusion: Adapting to the changing environment
#Refer to slide 20
############################################
# Load dataset
data("tictactoe")
tictactoe
# Perform reinforcement learning on tictactoe data
model_tic_tac <- ReinforcementLearning(tictactoe, s = "State", a = "Action", r = "Reward", s_new = "NextState", iter = 1)

#Since the data is very large, it will take some time to learn. We can then see the model policy and reward. 

# Optimal policy; this may be different for each run
model_tic_tac$Policy #This will print a very large matrix of the possible step in each state

# Reward; this may be different for each run
model_tic_tac$Reward 



