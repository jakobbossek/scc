library(devtools)
load_all("skel")

options(warn=2)

# generate some random blocks
l = generate_random_blocks(10, 20)
print(l)

# plot an very simple x-chart
xs = as.numeric(lapply(l, mean))
plot(1:10, xs, type="l", lty=5, ylim=c(-1.2,1.2))
points(1:10, xs, pch=19)
abline(h=0, lty=3, col="darkgray")
mtext("mu", side=4, adj=0, at=c(10,0))

