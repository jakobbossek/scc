context("generate_random_blocks")

blocks = generate_random_blocks(10, 5)

test_that("Random generation of blocks works", {
	expect_equal(length(blocks), 10)
	expect_equal(all(lapply(blocks, length) == 5), TRUE)
})