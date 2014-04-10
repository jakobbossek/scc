context("generate_random_blocks")

test_that("Random generation of blocks works", {
  blocks = generate_random_blocks(10, 5)
	expect_equal(length(blocks), 10)
	expect_true(all(lapply(blocks, length) == 5))
})
