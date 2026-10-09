# Tests for interprobe()

#interprobe_001
test_that("interprobe works with lm() model input and returns expected structure", {
  skip_if_not_installed("marginaleffects")

  set.seed(111)
  n <- 600
  x1 <- rnorm(n)
  z1 <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x1 * z1
  e <- rnorm(n, sd = sd(y.raw))
  y1 <- y.raw + e

  df <- data.frame(x1, y1, z1)
  lm1 <- lm(y1 ~ x1 * z1, data = df)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    model = lm1,
    x = "x1",
    z = "z1",
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30,
    moderator.on.x.axis = TRUE
  )

  expect_true(is.list(res))
  expect_true(all(c("simple.slopes", "johnson.neyman", "frequencies") %in% names(res)))
  expect_true(is.data.frame(res$johnson.neyman))
  expect_true(nrow(res$johnson.neyman) > 0)

  # For a linear model, dy/dx does not depend on x itself (only on z due to interaction),
  # so the marginal effects curves should be (nearly) identical across the x-spotlights.
  jn <- res$johnson.neyman
  expect_true("marginal.effect" %in% names(jn))
  expect_true("x1" %in% names(jn))
  expect_true("z1" %in% names(jn))

  eff_by_x <- split(jn$marginal.effect, jn$x1)
  expect_true(length(eff_by_x) == 3)
  expect_equal(unname(eff_by_x[[1]]), unname(eff_by_x[[2]]), tolerance = 1e-8)
  expect_equal(unname(eff_by_x[[2]]), unname(eff_by_x[[3]]), tolerance = 1e-8)
})

#interprobe_002
test_that("interprobe returns fitted model when estimating internally", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("estimatr")

  set.seed(333)
  n <- 250
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true("gam_results" %in% names(res))
  expect_s3_class(res$gam_results, "gam")
  expect_true("lm2_results" %in% names(res))
  expect_s3_class(res$lm2_results, "lm2")

  cl <- attr(res$lm2_results, "lm2_call")
  expect_true(!is.null(cl))
  call_txt <- gsub("\\s+", " ", paste(deparse(cl), collapse = " "))
  expect_equal(call_txt, "lm2(y ~ x * z)")
})

#interprobe_003
test_that("binary x returns gam_results_testing (ti-based testing GAM)", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("estimatr")

  set.seed(555)
  n <- 200
  z <- rnorm(n)
  x <- rep(c(0, 1), each = n / 2)
  y <- x + z + rnorm(n, sd = 2)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true("gam_results_testing" %in% names(res))
  expect_s3_class(res$gam_results_testing, "gam")
})

#interprobe_004
test_that("quiet=TRUE suppresses interaction APA printing", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(444)
  n <- 200
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  out <- capture.output(
    interprobe(
      x, z, y,
      quiet = TRUE,
      draw = "jn",
      histogram = FALSE,
      spotlights = c(-1, 0, 1),
      probe.bins = 30
    )
  )

  expect_false(any(grepl("p-value for the interaction", out, fixed = TRUE)))
})

#interprobe_005
test_that("interprobe works with lm2() model input", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 600
  x1 <- rnorm(n)
  z1 <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x1 * z1
  e <- rnorm(n, sd = sd(y.raw))
  y1 <- y.raw + e

  df <- data.frame(x1, y1, z1)
  lm2_1 <- lm2(y1 ~ x1 * z1, data = df)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    model = lm2_1,
    x = "x1",
    z = "z1",
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30,
    moderator.on.x.axis = TRUE
  )

  expect_true(is.list(res))
  expect_true(all(c("simple.slopes", "johnson.neyman", "frequencies") %in% names(res)))
  expect_true(nrow(res$johnson.neyman) > 0)
  jn <- res$johnson.neyman
  eff_by_x <- split(jn$marginal.effect, jn$x1)
  expect_true(length(eff_by_x) == 3)
  expect_equal(unname(eff_by_x[[1]]), unname(eff_by_x[[2]]), tolerance = 1e-8)
  expect_equal(unname(eff_by_x[[2]]), unname(eff_by_x[[3]]), tolerance = 1e-8)
})

#interprobe_006
test_that("interprobe with data fits lm2 when model = linear or lm sentinel", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 600
  x1 <- rnorm(n)
  z1 <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x1 * z1
  e <- rnorm(n, sd = sd(y.raw))
  y1 <- y.raw + e

  df <- data.frame(x1, y1, z1)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res_linear <- interprobe(
    data = df,
    x = "x1",
    z = "z1",
    y = "y1",
    model = "linear",
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30,
    moderator.on.x.axis = TRUE
  )

  expect_true(is.list(res_linear))
  expect_true(nrow(res_linear$johnson.neyman) > 0)
  jn <- res_linear$johnson.neyman
  eff_by_x <- split(jn$marginal.effect, jn$x1)
  expect_true(length(eff_by_x) == 3)
  expect_equal(unname(eff_by_x[[1]]), unname(eff_by_x[[2]]), tolerance = 1e-8)
  expect_equal(unname(eff_by_x[[2]]), unname(eff_by_x[[3]]), tolerance = 1e-8)

  res_lm_sentinel <- interprobe(
    data = df,
    x = "x1",
    z = "z1",
    y = "y1",
    model = lm,
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_equal(nrow(res_lm_sentinel$johnson.neyman), nrow(res_linear$johnson.neyman))
})

#interprobe_007
test_that("interprobe(x, z, y vectors) works with model = linear", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 600
  x <- rnorm(n)
  z <- rnorm(n, mean = 10, sd = 2)
  y.raw <- x * z
  e <- rnorm(n, sd = sd(y.raw))
  y <- y.raw + e

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    model = "linear",
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

#interprobe_008
test_that("bare model = linear is keyword when linear not in calling env", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  # Child of .GlobalEnv: no `linear` binding here; interprobe's parent.frame() is this env.
  res <- local(
    {
      set.seed(111)
      n <- 200
      x <- rnorm(n)
      z <- rnorm(n)
      y <- x * z + rnorm(n)
      grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)
      interprobe(
        x, z, y,
        model = linear,
        quiet = TRUE,
        draw = "jn",
        histogram = FALSE,
        spotlights = c(-1, 0, 1),
        probe.bins = 30
      )
    },
    envir = new.env(parent = globalenv())
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

#interprobe_009
test_that("bare model = linear uses fitted object when linear exists", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  res <- local(
    {
      set.seed(111)
      n <- 200
      x1 <- rnorm(n)
      z1 <- rnorm(n)
      y1 <- x1 * z1 + rnorm(n)
      df <- data.frame(x1, z1, y1)
      linear <- lm2(y1 ~ x1 * z1, data = df)
      grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)
      interprobe(
        model = linear,
        x = "x1",
        z = "z1",
        draw = "jn",
        histogram = FALSE,
        quiet = TRUE,
        spotlights = c(-1, 0, 1),
        probe.bins = 30
      )
    },
    envir = new.env(parent = globalenv())
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

#interprobe_010
test_that("bare model = lm on vectors uses stats::lm as keyword", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(111)
  n <- 200
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    model = lm,
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

#interprobe_011
test_that("error message shows original model name (linear)", {
  set.seed(123)
  x <- rnorm(200)
  z <- rnorm(200)
  y <- x * z + rnorm(200)
  x2 <- rnorm(200)

  linear <- lm(y ~ x2 * z)

  msgs <- character(0)
  out <- withCallingHandlers(
    withRestarts(
      interprobe(x, z, y, model = linear, quiet = TRUE),
      abort = function(...) "aborted"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_identical(out, "aborted")
  expect_true(any(grepl("model 'linear'", msgs, fixed = TRUE)))
})

#interprobe_012
test_that("bare x/z/y names work with data= when columns are not in calling env", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  # Isolate so value/interest/gone are only columns of df, not env bindings
  res <- local(
    {
      set.seed(222)
      n <- 200
      df <- data.frame(
        value = rnorm(n),
        interest = rnorm(n),
        gone = rnorm(n)
      )
      df$gone <- df$value * df$interest + rnorm(n)

      grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)

      interprobe(
        x = value,
        z = interest,
        y = gone,
        data = df,
        model = "linear",
        quiet = TRUE,
        draw = "jn",
        histogram = FALSE,
        spotlights = c(-1, 0, 1),
        probe.bins = 30
      )
    },
    envir = new.env(parent = globalenv())
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

#interprobe_013
test_that("quoted x/z/y strings work with data=", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("estimatr")

  set.seed(223)
  n <- 200
  df <- data.frame(
    value = rnorm(n),
    interest = rnorm(n),
    gone = rnorm(n)
  )
  df$gone <- df$value * df$interest + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x = "value",
    z = "interest",
    y = "gone",
    data = df,
    model = "linear",
    quiet = TRUE,
    draw = "jn",
    histogram = FALSE,
    spotlights = c(-1, 0, 1),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

#interprobe_014
test_that("missing column with data= reports requested error wording", {
  set.seed(224)
  n <- 50
  df <- data.frame(
    interest = rnorm(n),
    gone = rnorm(n)
  )

  msgs <- character(0)
  out <- withCallingHandlers(
    withRestarts(
      interprobe(x = value, z = interest, y = gone, data = df, quiet = TRUE),
      abort = function(...) "aborted"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_identical(out, "aborted")
  expect_true(any(grepl("'value' is not a variable in dataframe 'df'", msgs, fixed = TRUE)))
})

#interprobe_015
test_that("bare x/z names work with fitted model when columns not in calling env", {
  skip_if_not_installed("marginaleffects")

  res <- local(
    {
      set.seed(225)
      n <- 200
      df <- data.frame(
        value = rnorm(n),
        interest = rnorm(n)
      )
      df$gone <- df$value * df$interest + rnorm(n)
      lm1 <- lm(gone ~ value * interest, data = df)

      grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
      on.exit(grDevices::dev.off(), add = TRUE)

      interprobe(
        model = lm1,
        x = value,
        z = interest,
        quiet = TRUE,
        draw = "jn",
        histogram = FALSE,
        spotlights = c(-1, 0, 1),
        probe.bins = 30
      )
    },
    envir = new.env(parent = globalenv())
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
})

#interprobe_016
test_that("binary x allows cols of length 2", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(616)
  n <- 200
  z <- rnorm(n)
  x <- rep(c(0, 1), each = n / 2)
  y <- x + z + x * z + rnorm(n, sd = 2)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    quiet = TRUE,
    draw = "both",
    histogram = FALSE,
    cols = c("red4", "dodgerblue"),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(nrow(res$johnson.neyman) > 0)
  expect_true(nrow(res$simple.slopes) > 0)
})

#interprobe_017
test_that("continuous x rejects cols of length 2", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(617)
  n <- 200
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  msgs <- character(0)
  out <- withCallingHandlers(
    withRestarts(
      {
        grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
        on.exit(grDevices::dev.off(), add = TRUE)
        interprobe(
          x, z, y,
          quiet = TRUE,
          draw = "jn",
          histogram = FALSE,
          cols = c("red4", "dodgerblue"),
          spotlights = c(-1, 0, 1),
          probe.bins = 30
        )
      },
      abort = function(...) "aborted"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_identical(out, "aborted")
  expect_true(any(grepl("cols' must have length 3", msgs, fixed = TRUE)))
})

#interprobe_018
test_that("Johnson-Neyman regions of significance report sample percentage", {
  df <- data.frame(z = 1:10)
  expect_equal(statuser:::ip_pct_in_range(df, "z", 4, 6), 30, tolerance = 1e-8)

  j <- data.frame(
    z = seq(4, 6, length.out = 5),
    marginal.effect = rep(1, 5),
    conf.low = rep(1, 5),
    conf.high = rep(2, 5),
    contrast = rep("mean(1) - mean(0)", 5)
  )
  txt <- statuser:::ip_get_regions_jn(j, "x", "z", "categorical", 5, df)
  expect_match(txt, "\\(30% of the sample\\)")

  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(618)
  n <- 400
  z <- rnorm(n, mean = 5, sd = 2)
  x <- rep(c(0, 1), each = n / 2)
  y <- x + z + 2 * x * z + rnorm(n, sd = 0.5)
  analysis_df <- data.frame(x, z, y)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    draw = "jn",
    histogram = FALSE,
    quiet = TRUE,
    cols = c("red4", "dodgerblue"),
    probe.bins = 50
  )

  regions_txt <- statuser:::ip_get_regions_jn(
    res$johnson.neyman, "x", "z", "categorical", 50, analysis_df
  )
  region_lines <- grep("% of the sample\\)", strsplit(regions_txt, "\n")[[1]], value = TRUE)
  expect_true(length(region_lines) > 0)
})

#interprobe_019
test_that("ip_resolve_moderator_on_x_axis resolves auto from nux", {
  expect_true(statuser:::ip_resolve_moderator_on_x_axis("auto", 2, 11))
  expect_true(statuser:::ip_resolve_moderator_on_x_axis("auto", 3, 11))
  expect_false(statuser:::ip_resolve_moderator_on_x_axis("auto", 4, 11))
  expect_false(statuser:::ip_resolve_moderator_on_x_axis("auto", 8, 11))
  expect_false(statuser:::ip_resolve_moderator_on_x_axis("auto", 12, 11))
  expect_true(statuser:::ip_resolve_moderator_on_x_axis(TRUE, 2, 11))
  expect_false(statuser:::ip_resolve_moderator_on_x_axis(FALSE, 50, 11))
})

#interprobe_019b
test_that("continuous JN with covariate matches rename-before-covariates newdata", {
  skip_if_not_installed("marginaleffects")

  set.seed(419)
  n <- 300
  x1 <- rnorm(n)
  z1 <- rnorm(n)
  w <- rnorm(n, mean = 5)
  y <- 0.5 * x1 + 0.3 * z1 + 0.4 * x1 * z1 + 0.2 * w + rnorm(n)
  dat <- data.frame(x1 = x1, z1 = z1, w = w, y = y)
  m <- lm(y ~ x1 * z1 + w, data = dat)

  xs <- seq(min(x1), max(x1), length.out = 20)
  zj <- 0
  jn_list <- statuser:::ip_compute_jn_continuous(
    spotlights = zj,
    data = dat,
    xs = xs,
    zs = sort(unique(z1)),
    model = m,
    xvar = "x1",
    zvar = "z1",
    moderator.on.x.axis = FALSE
  )
  jn <- jn_list[[1]]

  ndj_ok <- expand.grid(z = zj, x = xs)
  names(ndj_ok)[1:2] <- c("z1", "x1")
  ndj_ok <- statuser:::ip_add_covariates_at_mean(ndj_ok, dat)
  expect_equal(anyDuplicated(names(ndj_ok)), 0L)

  slopes_ok <- data.frame(
    marginaleffects::slopes(m, newdata = ndj_ok, variables = "x1")
  )
  expect_equal(jn$estimate, slopes_ok$estimate, tolerance = 1e-8)
})

#interprobe_020
test_that("moderator.on.x.axis validation accepts auto, TRUE, and FALSE", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(620)
  n <- 200
  x <- rnorm(n)
  z <- rnorm(n)
  y <- x * z + rnorm(n)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  expect_error(
    interprobe(x, z, y, moderator.on.x.axis = "auto", quiet = TRUE, draw = "jn", histogram = FALSE, probe.bins = 30),
    NA
  )
  expect_error(
    interprobe(x, z, y, moderator.on.x.axis = TRUE, quiet = TRUE, draw = "jn", histogram = FALSE, probe.bins = 30),
    NA
  )
  expect_error(
    interprobe(x, z, y, moderator.on.x.axis = FALSE, quiet = TRUE, draw = "jn", histogram = FALSE, probe.bins = 30),
    NA
  )

  msgs <- character(0)
  out <- withCallingHandlers(
    withRestarts(
      interprobe(x, z, y, moderator.on.x.axis = "maybe", quiet = TRUE),
      abort = function(...) "aborted"
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(out, "aborted")
  expect_true(any(grepl("moderator.on.x.axis", msgs, fixed = TRUE)))
})

#interprobe_021
test_that("binary x with default auto matches explicit moderator on x-axis", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(621)
  n <- 200
  z <- rnorm(n)
  x <- rep(c(0, 1), each = n / 2)
  y <- x + z + x * z + rnorm(n, sd = 2)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res_auto <- interprobe(
    x, z, y,
    quiet = TRUE,
    draw = "both",
    histogram = FALSE,
    cols = c("red4", "dodgerblue"),
    probe.bins = 30
  )
  res_true <- interprobe(
    x, z, y,
    moderator.on.x.axis = TRUE,
    quiet = TRUE,
    draw = "both",
    histogram = FALSE,
    cols = c("red4", "dodgerblue"),
    probe.bins = 30
  )

  expect_true(is.list(res_auto))
  expect_equal(res_auto$simple.slopes, res_true$simple.slopes)
  expect_equal(res_auto$johnson.neyman, res_true$johnson.neyman)
})

#interprobe_022
test_that("binary x with explicit FALSE is not overridden to TRUE", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(622)
  n <- 200
  z <- rnorm(n)
  x <- rep(c(0, 1), each = n / 2)
  y <- x + z + rnorm(n, sd = 2)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  msgs <- character(0)
  res <- withCallingHandlers(
    interprobe(
      x, z, y,
      moderator.on.x.axis = FALSE,
      quiet = TRUE,
      draw = "both",
      histogram = FALSE,
      cols = c("red4", "dodgerblue"),
      probe.bins = 30
    ),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )

  expect_true(is.list(res))
  expect_false(any(grepl("Will ignore request to have it on the x-axis", msgs, fixed = TRUE)))
})

#interprobe_023
test_that("binary x with explicit TRUE still works", {
  skip_if_not_installed("marginaleffects")
  skip_if_not_installed("mgcv")

  set.seed(623)
  n <- 200
  z <- rnorm(n)
  x <- rep(c(0, 1), each = n / 2)
  y <- x + z + x * z + rnorm(n, sd = 2)

  grDevices::pdf(file = tempfile(fileext = ".pdf"), width = 7, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)

  res <- interprobe(
    x, z, y,
    moderator.on.x.axis = TRUE,
    quiet = TRUE,
    draw = "both",
    histogram = FALSE,
    cols = c("red4", "dodgerblue"),
    probe.bins = 30
  )

  expect_true(is.list(res))
  expect_true(nrow(res$simple.slopes) > 0)
  expect_true(nrow(res$johnson.neyman) > 0)
})

