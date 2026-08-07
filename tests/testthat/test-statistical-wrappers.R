test_that("linear model wrapper returns the expected x effect", {
  sample_ids <- paste0("sample_", seq_len(6))
  x <- setNames(c("control", "control", "control", "case", "case", "case"), sample_ids)
  y <- setNames(c(1, 2, 3, 4, 5, 6), sample_ids)
  meta <- data.frame(Sample_ID = sample_ids, row.names = sample_ids)

  result <- f_lm(
    x = x,
    y = y,
    formula = y ~ x,
    meta = meta,
    feat_name_x = "condition",
    feat_name_y = "feature"
  )

  ordered_x <- factor(x, levels = c(result["Group1"], result["Group2"]))
  expected <- coef(summary(lm(y ~ ordered_x)))[2, ]

  expect_equal(as.numeric(result["effect_size"]), unname(expected["Estimate"]))
  expect_equal(as.numeric(result["p_value"]), unname(expected["Pr(>|t|)"]))
  expect_equal(as.numeric(result["N_Group1"]) + as.numeric(result["N_Group2"]), 6)
})

test_that("logistic model wrapper returns the expected odds ratio", {
  sample_ids <- paste0("sample_", seq_len(12))
  x <- setNames(rep(c("control", "case"), each = 6), sample_ids)
  y <- setNames(c(0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1), sample_ids)
  meta <- data.frame(Sample_ID = sample_ids, row.names = sample_ids)

  result <- f_glm(x = x, y = y, formula = y ~ x, meta = meta)

  ordered_x <- factor(x, levels = c(result["Group1"], result["Group2"]))
  expected <- glm(y ~ ordered_x, family = binomial())
  expect_equal(as.numeric(result["odds_ratio_glm"]), unname(exp(coef(expected)[2])))
})

test_that("Spearman wrapper reports correlation and sample count", {
  result <- f_spearman(
    x = seq_len(5),
    y = seq_len(5),
    feat_name_x = "x_feature",
    feat_name_y = "y_feature"
  )

  expect_equal(as.numeric(result["effect_size"]), 1)
  expect_equal(as.numeric(result["N_samples"]), 5)
  expect_lt(as.numeric(result["p_value"]), 0.05)
})

test_that("Fisher wrapper matches base fisher.test for a binary comparison", {
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

  result <- f_single_run_fisher_test(
    i = 1,
    j = 1,
    mat1 = mat1,
    mat2 = mat2,
    threshold_for_prev = -3,
    prevalence_threshold = FALSE
  )

  expected_table <- table(mat1[1, ], mat2[1, ] > -3)
  expected <- fisher.test(expected_table[c(result$Group1, result$Group2), ])
  expect_equal(as.numeric(result$p.val_fisher), expected$p.value)
  expect_equal(as.numeric(result$odds_ratio), unname(expected$estimate))
})

test_that("paired Wilcoxon wrapper handles complete pairs", {
  sample_ids <- paste0("sample_", seq_len(6))
  x <- setNames(rep(c("control", "case"), times = 3), sample_ids)
  y <- setNames(c(-4, -2, -3, -1, -2, 0), sample_ids)
  meta <- data.frame(
    Sample_ID = sample_ids,
    pair_id = rep(paste0("pair_", seq_len(3)), each = 2),
    row.names = sample_ids
  )

  result <- f_wilcox(
    x = x,
    y = y,
    meta = meta,
    feat_name_x = "condition",
    feat_name_y = "feature",
    paired_wilcox_by = "pair_id"
  )

  expect_equal(as.numeric(result["N_Pairs"]), 3)
  expect_false(is.na(as.numeric(result["p.val_wilcox"])))
})
