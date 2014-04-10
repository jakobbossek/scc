library(methods)
library(devtools)
library(testthat)

load_all(".")

set.seed(4334)

# generate some random blocks
blocks = generate_random_blocks(40, 20)

ewmaChart = generate_ewma_chart(
  blocks = blocks,
  center = 0,
  sd = 1,
  parameters = list(lambda=0.2),
  description = "first EWMA chart",
  y_value_name = "process mean",
  desired_value_name = "mean of great process")

print(ewmaChart)
print(number_of_blocks(ewmaChart))
print(number_of_outliers(ewmaChart))
print(longest_outlier_sequence(ewmaChart))

print(autoplot(ewmaChart))
