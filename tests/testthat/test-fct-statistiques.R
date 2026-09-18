test_that("tablePourcent computes proportions sorted ascending", {
  data <- tablePourcent(c("a", "a", "b"))
  expect_equal(data$pourcent, c(1/3, 2/3))
  expect_equal(data$nom, c("b", "a"))
})

test_that("reorder_factor_levels reorders levels keeping unknown ones last", {
  f <- factor(c("c", "a", "b"))
  g <- reorder_factor_levels(f, new.order = c("b", "a"))
  expect_equal(levels(g), c("b", "a", "c"))
  # values are preserved
  expect_equal(as.character(g), c("c", "a", "b"))
})

test_that("rdpv formats p-values", {
  expect_equal(rdpv(0.001), "<0.01")
  expect_equal(rdpv(0.1234), 0.123)
  expect_equal(rdpv(NA), NA)
})

test_that("cs standardizes a vector", {
  expect_equal(cs(c(1, 2, 3)), c(-1, 0, 1))
})

test_that("IC.diff.prop estimates the difference of two proportions", {
  res <- IC.diff.prop(x1 = 20, n1 = 100, x2 = 30, n2 = 100)
  expect_equal(res$Estimation, 0.1)
  expect_equal(dim(res$IC), c(4, 2))
  expect_true(all(res$IC[3, ] > -1 & res$IC[3, ] < 1))
})

test_that("tests_autoGMRC selects a test for two groups", {
  set.seed(1)
  y <- rnorm(40)
  grp <- factor(rep(c("A", "B"), each = 20))
  f <- tests_autoGMRC(y, grp)
  expect_s3_class(f, "formula")
  expect_true(any(c("t.testVarEgal", "t.test", "wilcox.test") %in% all.vars(f)))
})

test_that("tests_autoGMRC selects chi2 or fisher for factors", {
  x <- factor(rep(c("yes", "no"), each = 10))
  grp <- factor(rep(c("A", "B"), each = 20))
  f <- tests_autoGMRC(x, grp)
  expect_s3_class(f, "formula")
  expect_true(any(c("chisq.test", "fisher.test") %in% all.vars(f)))
})

test_that("tests_autoGMRC returns no.test for a single group", {
  f <- tests_autoGMRC(rnorm(10), rep("A", 10))
  expect_true(all.vars(f) == "no.test")
})

test_that("descr1 returns a descriptive matrix for a numeric vector", {
  set.seed(1)
  res <- descr1(rnorm(50))
  expect_true(is.list(res))
  expect_true(all(c("Descriptif", "TestNormalite") %in% names(res)))
  expect_equal(dim(res$Descriptif), c(24, 1))
})

test_that("desql returns counts and proportions for a factor", {
  x <- factor(c("a", "a", "b", NA))
  res <- desql(x)
  expect_equal(dim(res), c(5, 2))
  expect_equal(res[1, 1], 2)
  expect_equal(res[2, 1], 1)
  expect_equal(res[3, 1], 3)
  expect_equal(res[5, 1], 1)
})

test_that("descr3 returns named test fields that match their UI labels", {
  set.seed(1)
  y <- rnorm(90)
  grp <- factor(rep(c("A", "B", "C"), each = 30))
  res <- descr3(y, grp)
  expect_true(all(c("TestNormalite", "Testpv", "TestsNPv",
                    "Tests_de_Student", "TestsNP") %in% names(res)))
  expect_true(grepl("Bartlett", res$Testpv))
  expect_true(grepl("Fligner", res$TestsNPv))
  expect_true(grepl("Variance", res$Tests_de_Student))
  expect_true(grepl("Kruskal", res$TestsNP))
})

test_that("tab_survie returns the Kaplan-Meier detail table", {
  set.seed(1)
  x <- rpois(50, 30)
  x[x == 0] <- 1
  y <- c(0, 1, 0, 1, rbinom(46, 1, 0.5))
  res <- tab_survie(x, y)
  expect_s3_class(res, "data.frame")
  expect_true(all(c("Delai", "nrisque", "evenements", "censures",
                    "survie", "ecart_type", "IC95_sup", "IC95_inf") %in% names(res)))
})

test_that("lire_bdd_csv reads a CSV, applies dec/NA, strips empty rows, NULL on error", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a;b", "1,5;*", "2;3", ";", "3,25;4"), tmp)
  dd <- lire_bdd_csv(tmp, header = TRUE, sep = ";",
                     manquants = "*", decimale = ",", encodage = "utf-8")
  expect_s3_class(dd, "data.frame")
  expect_equal(nrow(dd), 3)
  expect_equal(dd$a[1], 1.5)
  expect_true(is.na(dd$b[1]))
  expect_null(lire_bdd_csv(tempfile(fileext = ".csv")))
})
