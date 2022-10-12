# R-P05-OptimizationAlgorithms

# Section 0 ----

library(lpSolve)

setwd("E:/Mine/Maktab Khooneh/Data Analysis/5 - Optimization/Final Project")
getwd()

# Section 1 - Decision Variables ----

  # Decision Variables
    # x[i,j]
      # i: 7 nodes which are the start points of transportation routes.
      # j: 7 nodes which are the end points of transportation routes.
        # There are 49 decision variables

# Section 2 - Objective Function ----
  # Objective function
    # Minimize the transportation cost
      # Min: sum(x[i,j]*cost[i,j])

# Section 3 - Coefficients' Matrix ----

M = 1000000
  # M is an extra large number

cost = matrix(c(M, 5, 3, 5, 5, 20, 20,
                9, M, 9, 1, 1, 8, 15,
                0.4, 8, M, 1, 0.5, 10, 12,
                M, M, M, M, 1.2, 2, 12,
                M, M, M, 0.8, M, 2, 12,
                M, M, M, M, M, M, 1,
                M, M, M, M, M, 7, M),
              nrow = 7, ncol = 7, byrow = T)


obj_coef = as.vector(t(cost))

obj_coef

length(obj_coef)

# Section 4 - Constraints ----

  # There are 4 types of constraints.

  # 1st type
    # Maximum transportation volume in a route should be limited to 200 tons.

const1 = matrix(0, nrow = 49, ncol = 49)

for (i in 1:49){
    const1[i,i] = 1
}

const1

  # 2nd type
    # Suppliers (nodes 1, 2, and 3) should eventually have less supplies compared to their current inventory
      # x[1,j] - x[i,1] <= current inventory[i]

const2 = matrix(0, nrow = 3, ncol = 49)

for (i in 1:3){
    const2[i,i] = -1
    const2[i,i+7] = -1
    const2[i,i+14] = -1
}

for (i in 1:3){
  for (j in 1:7){
    const2[i,(i-1)*7 + j] = 1
  }
}

const2

  # 3rd type
    # Middle storage (nodes 4, and 5) should eventually have inventory of zero supplies.

const3 = matrix(0, nrow = 2, ncol = 49)

for (i in 1:2){
  for(j in 1:7){
    const3[i,i+3 + (j-1)*7] = 1
    const3[i,(i+2)*7+j] = -1
  }
}

const3

  # 4th type
    # The demand in nodes 6, and 7 should be met.

const4 = matrix(0, nrow = 2, ncol = 49)

for (i in 1:2){
  for (j in 1:7){
    const4[i, j*7-(2-i)] = 1
  }
}

for (i in 1:2){
  for (j in 1:7){
    const4[i,7*(i+4)+j] = -1
  }
}

const4

const = rbind(const1, const2, const3, const4)
dim(const)


# Section 5 - RHS ----

rhs = c(rep(200,49),
        200,300,100,
        0,0,
        400,180)

rhs

length(rhs)

# Section 6 - Signs ----

signs = c(rep("<=",52), rep("=",4))

signs

length(signs)

# Section 7 - Solution ----

sol = lp(direction = "min",
         objective.in = obj_coef,
         const.mat = const,
         const.dir = signs,
         const.rhs = rhs,
         compute.sens = 1)

sol$solution
sol$objval

solmat = matrix(sol$solution, nrow = 7, ncol = 7, byrow = T)
solmat

# Section 8 - Sensitivity Analysis ----

nodefrom = c(rep(1,7), rep(2,7), rep(3,7), rep(4,7), rep(5,7), rep(6,7), rep(7,7))
nodeto = c(rep(c(1,2,3,4,5,6,7),7))

sensivity_var = cbind(nodefrom, nodeto, sol$sens.coef.from, sol$sens.coef.to)


  # If variables change in this range, the optimization results will be constant.

sensivity_rhs = cbind(sol$duals.from, sol$duals.to)
  # These are the feasible range of rhs.
    # 56 constraints (ours) + 49 condition for 49 variables being positive.
    # Total 105 constraints.

shadowprice = matrix(sol$duals)
