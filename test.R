library(devtools)
load_all("skel")

options(warn=2)

l = generate_random_blocks(10, 20)
print(l)