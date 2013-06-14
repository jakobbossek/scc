library(ggplot2)
library(gridExtra)
library(devtools)

load_all("skel")
options(warn=2)

set.seed(4334)
# generate some random blocks
l = generate_random_blocks(40, 20)

# compute y-values (blockwise mean here)
y_value = as.numeric(lapply(l, mean))
bounds = list(UCB = 0.4, LCB = -0.4, UWB = 0.34, LWB = -0.34)

# 'manually' generate some charts
x1 = generate_chart(
	blocks = l,
	type = "ewma",
	parameters = NULL,
	y_value = y_value,
	y_value_name = "mean",
	bounds = bounds,
	description = "X-bar chart for examplary quantity\n (constant desired value)",
	desired_value = 0,
	desired_value_name = "desired_value")

ewma = generate_ewma_chart(
	blocks=l,
	center=0,
	sd=1,
	parameters=list(lambda=0.2),
	description="first EWMA chart",
	y_value_name="process mean",
	desired_value_name="mean of great process")

print(ewma)
print(x1)
print(number_of_blocks(x1))
print(number_of_outliers(x1))
print(longest_outlier_sequence(x1))



# old_pars = par()
# par(cex.axis=0.7, cex.lab=0.8, mar=c(5,4,4,5)+0.1, mfrow=c(1,2))
# plot(chartA)
# plot(chartB)
# par(old_pars)

# ggplot2 test
#pl = autoplot(x1)

# plot an very simple x-chart
# xs = as.numeric(lapply(l, mean))
# plot(1:10, xs, type="l", lty=5, ylim=c(-1.2,1.2))
# grid()
# points(1:10, xs, pch=19)
# abline(h=0, lwd=2, col="darkgray")
# mtext("mu", side=4, adj=0, at=c(10,0))

