test_that("parallel linear model runner works with one worker", {
  sample_ids <- paste0("sample_", seq_len(6))
  mat1 <- matrix(
    c("control", "control", "control", "case", "case", "case"),
    nrow = 1,
    dimnames = list("condition", sample_ids)
  )
  mat2 <- matrix(
    c(1, 2, 3, 4, 5, 6),
    nrow = 1,
    dimnames = list("feature", sample_ids)
  )
  meta <- data.frame(
    Sample_ID = sample_ids,
    randomEffFac = "single_group",
    row.names = sample_ids
  )

  result <- f_run_linear_models_parallel(
    mat1 = mat1,
    mat2 = mat2,
    meta = meta,
    n_cores_max = 1
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$feat1, "condition_case")
  expect_equal(result$feat2, "feature")
})

test_that("parallel Fisher runner works with one worker", {
  sample_ids <- paste0("sample_", seq_len(8))
  mat1 <- matrix(
    c("control", "control", "control", "control", "case", "case", "case", "case"),
    nrow = 1,
    dimnames = list("condition", sample_ids)
  )
  mat2 <- matrix(
    c(-4, -4, -4, -2, -4, -2, -2, -2),
    nrow = 1,
    dimnames = list("feature", sample_ids)
  )

  result <- f_run_fisher_test_parallel(
    mat1 = mat1,
    mat2 = mat2,
    n_cores_max = 1
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$feat1, "condition_case")
  expect_equal(result$feat2, "feature")
})
