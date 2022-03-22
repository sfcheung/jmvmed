
# This file is a generated template, your changes will not be overwritten

medxmyClass <- if (requireNamespace('jmvcore', quietly = TRUE)) R6::R6Class(
    "medxmyClass",
    inherit = medxmyBase,
    private = list(
        .run = function() {
            if (is.null(self$options$X) ||
                is.null(self$options$Y) ||
                is.null(self$options$M)) {
                  return()
              }

            basic_table <- self$results$basic
            coef_table_m <- self$results$mmodel
            coef_table_y <- self$results$ymodel
            rsq_table <- self$results$rsq
            ind_table <- self$results$indirect

            formula_m <- jmvcore::constructFormula(
                                      self$options$M,
                                      c(self$options$X,
                                        self$options$covariates)
                                    )
            formula_m <- as.formula(formula_m)
            formula_y <- jmvcore::constructFormula(
                                      self$options$Y,
                                      c(self$options$M,
                                        self$options$X,
                                        self$options$covariates)
                                    )
            formula_y <- as.formula(formula_y)

            dat <- self$data[, all.vars(formula_y)]
            dat <- dat[complete.cases(dat), ]
            n <- nrow(dat)

            basic_table$setRow(rowNo = 1,
                values = list(
                            term = "Number of valid cases",
                            data = n
                  )
              )
            basic_table$setRow(rowNo = 2,
                values = list(
                            term = "Outcome (Y)",
                            data = self$options$Y
                  )
              )
            basic_table$setRow(rowNo = 3,
                values = list(
                            term = "Predictor (X)",
                            data = self$options$X
                  )
              )
            basic_table$setRow(rowNo = 4,
                values = list(
                            term = "Mediator (M)",
                            data = self$options$M
                  )
              )
            basic_table$setRow(rowNo = 5,
                values = list(
                            term = "Covariate(s)",
                            data = paste0(self$options$covariates,
                                          collapse = ",")
                  )
              )
            basic_table$setRow(rowNo = 6,
                values = list(
                            term = "Number of Bootstrap Samples",
                            data = self$options$R
                  )
              )
            basic_table$setRow(rowNo = 7,
                values = list(
                            term = "Random seed",
                            data = self$options$boot_seed
                  )
              )

            results_m <- stats::lm(formula_m, dat)
            summary_m <- summary(results_m)
            summary_m_str <- capture.output(print(summary_m))

            results_y <- stats::lm(formula_y, dat)
            summary_y <- summary(results_y)
            summary_y_str <- capture.output(print(summary_y))

            fct_i <- function(dat, i) {
                dat_i <- dat[i, ]
                outm <- stats::lm(formula_m, dat_i)
                outy <- stats::lm(formula_y, dat_i)
                a_ustd <- stats::coef(outm)[self$options$X]
                b_ustd <- stats::coef(outy)[self$options$M]
                ab_ustd <- a_ustd * b_ustd
                x_sd <- sd(outy$model[, self$options$X])
                y_sd <- sd(outy$model[, self$options$Y])
                ab_std <- ab_ustd * x_sd / y_sd
                return(c(ab_ustd, ab_std))
              }

            set.seed(self$options$boot_seed)
            boot_out <- boot::boot(dat, fct_i, R = self$options$R)

            boot_ci_ustd <- boot::boot.ci(boot_out, type = "perc", index = 1,
                                          conf = self$options$conf / 100)
            boot_ci_ustd_str <- capture.output(print(boot_ci_ustd))
            boot_ci_std <- boot::boot.ci(boot_out, type = "perc", index = 2,
                                         conf = self$options$conf / 100)
            boot_ci_std_str <- capture.output(print(boot_ci_std))

            coef_m <- summary_m$coefficients
            term_m <- rownames(coef_m)
            names(term_m) <- rownames(coef_m)
            coef_m_std <- summary(lm.beta::lm.beta(results_m))$coefficients

            coef_y <- summary_y$coefficients
            term_y <- rownames(coef_y)
            names(term_y) <- rownames(coef_y)
            coef_y_std <- summary(lm.beta::lm.beta(results_y))$coefficients

            # self$results$text$setContent(c(summary_m_str,
            #                                "\n",
            #                                summary_y_str,
            #                                "\n",
            #                                boot_ci_ustd_str,
            #                                "\n",
            #                                boot_ci_std_str))

            out_str <- paste0("This module is merely for learning.\n",
                              "It should not be used for serious research.")

            self$results$text$setContent(out_str)

            coef_table_m$setTitle(paste0("Model M: Predict ",
                                         self$options$M,
                                         " (M) by ",
                                         self$options$X,
                                         " (X) and covariate(s)", collapse = ""))

            coef_table_m$setRow(rowNo = 1,
                values = list(
                            term = "Constant",
                            coef_ustd = coef_m["(Intercept)", "Estimate"],
                            se = coef_m["(Intercept)", "Std. Error"],
                            t = coef_m["(Intercept)", "t value"],
                            p = coef_m["(Intercept)", "Pr(>|t|)"]
                          )
              )
            for (i in term_m[-1]) {
                    coef_table_m$addRow(
                        rowKey = i,
                        values = list(
                                term = i,
                                coef_ustd = coef_m[i, "Estimate"],
                                coef_std = coef_m_std[i, "Standardized"],
                                se = coef_m[i, "Std. Error"],
                                t = coef_m[i, "t value"],
                                p = coef_m[i, "Pr(>|t|)"]
                              )
                          )
              }

            coef_table_y$setTitle(paste0("Model Y: Predict ",
                                         self$options$Y,
                                         " (Y) by ",
                                         self$options$X,
                                         " (X), ",
                                         self$options$M,
                                         " (M), and covariate(s)", collapse = ""))

            coef_table_y$setRow(rowNo = 1,
                values = list(
                            term = "Constant",
                            coef_ustd = coef_y["(Intercept)", "Estimate"],
                            se = coef_m["(Intercept)", "Std. Error"],
                            t = coef_m["(Intercept)", "t value"],
                            p = coef_m["(Intercept)", "Pr(>|t|)"])
              )
            for (i in term_y[-1]) {
                    coef_table_y$addRow(
                        rowKey = i,
                        values = list(
                                term = i,
                                coef_ustd = coef_y[i, "Estimate"],
                                coef_std = coef_y_std[i, "Standardized"],
                                se = coef_y[i, "Std. Error"],
                                t = coef_y[i, "t value"],
                                p = coef_y[i, "Pr(>|t|)"])
                          )
              }

            f_m <- summary_m$fstatistic
            rsq_table$setRow(rowNo = 1,
                values = list(
                            model = "Model M",
                            rsq = summary_m$r.squared,
                            adjrsq = summary_m$adj.r.squared,
                            df1 = f_m[2],
                            df2 = f_m[3],
                            f = f_m[1],
                            p = stats::pf(f_m[1],
                                          f_m[2],
                                          f_m[3],
                                          lower.tail = FALSE)
                  )
              )

            f_y <- summary_y$fstatistic
            rsq_table$setRow(rowNo = 2,
                values = list(
                            model = "Model Y",
                            rsq = summary_y$r.squared,
                            adjrsq = summary_y$adj.r.squared,
                            df1 = f_y[2],
                            df2 = f_y[3],
                            f = f_y[1],
                            p = stats::pf(f_y[1],
                                          f_y[2],
                                          f_y[3],
                                          lower.tail = FALSE)
                  )
              )

            a_ustd <- stats::coef(results_m)[self$options$X]
            b_ustd <- stats::coef(results_y)[self$options$M]
            ab_ustd <- a_ustd * b_ustd
            x_sd <- sd(results_y$model[, self$options$X])
            y_sd <- sd(results_y$model[, self$options$Y])
            ab_std <- ab_ustd * x_sd / y_sd

            ind_table$setRow(rowNo = 1,
                values = list(
                            effect = "Unstandardized",
                            est = ab_ustd,
                            lb = boot_ci_ustd$percent[1, 4],
                            ub = boot_ci_ustd$percent[1, 5],
                            lvl = self$options$conf
                  )
              )
            ind_table$setRow(rowNo = 2,
                values = list(
                            effect = "Standardized",
                            est = ab_std,
                            lb = boot_ci_std$percent[1, 4],
                            ub = boot_ci_std$percent[1, 5],
                            lvl = self$options$conf
                  )
              )
            imageustd <- self$results$plotustd
            imageustd$setState(boot_out)
            imagestd <- self$results$plotstd
            imagestd$setState(boot_out)
            imageqqustd <- self$results$qqustd
            imageqqustd$setState(boot_out)
            imageqqstd <- self$results$qqstd
            imageqqstd$setState(boot_out)
        },
        .plotustd = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            boot_out <- image$state
            # plot(boot_out, index = 1)
            dat <- as.data.frame(boot_out$t)
            colnames(dat) <- c("t1", "t2")
            p <- ggplot(dat, aes(x = t1)) +
                    geom_histogram(aes(y =..density..),
                                   fill = "blue",
                                   color = "black") +
                    stat_function(fun = stats::dnorm,
                                  args = list(mean = mean(dat$t1),
                                              sd = stats::sd(dat$t1)),
                                  color = "red",
                                  size = 2, alpha = .75) +
                    labs(x = "Estimate (Unstandardized)",
                         y = "Density")
            print(p)
            TRUE
        },
        .plotstd = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            boot_out <- image$state
            # plot(boot_out, index = 2)
            dat <- as.data.frame(boot_out$t)
            colnames(dat) <- c("t1", "t2")
            p <- ggplot(dat, aes(x = t2)) +
                    geom_histogram(aes(y =..density..),
                                   fill = "darkgreen",
                                   color = "black") +
                    stat_function(fun = stats::dnorm,
                                  args = list(mean = mean(dat$t2),
                                              sd = stats::sd(dat$t2)),
                                  color = "red",
                                  size = 2, alpha = .75) +
                    labs(x = "Estimate (Standardized)",
                         y = "Density")
            print(p)
            TRUE
        },
        .qqstd = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            boot_out <- image$state
            dat <- as.data.frame(boot_out$t)
            colnames(dat) <- c("t1", "t2")
            p <- ggplot(dat, aes(sample = t2)) +
                  stat_qq(colour = "darkgreen") +
                  stat_qq_line(color = "red")
            print(p)
            TRUE
        },
        .qqustd = function(image, ...) {
            if (is.null(image$state)) return(FALSE)
            boot_out <- image$state
            dat <- as.data.frame(boot_out$t)
            colnames(dat) <- c("t1", "t2")
            p <- ggplot(dat, aes(sample = t1)) +
                  stat_qq(colour = "blue") +
                  stat_qq_line(color = "red")
            print(p)
            TRUE
        }
        )
)
