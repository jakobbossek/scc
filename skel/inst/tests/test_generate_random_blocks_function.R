context("generate_random_blocks")

blocks = generate_random_blocks(10, 5)

test_that("Random generation of blocks works", {
	expect_equal(ncol(blocks), 10)
	expect_equal(nrow(blocks), 5)
})