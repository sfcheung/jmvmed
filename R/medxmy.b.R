
# This file is a generated template, your changes will not be overwritten

medxmyClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "medxmyClass",
    inherit = medxmyBase,
    private = list(
        .run = function() {

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

            coef_table_m <- self$results$mmodel
            coef_table_y <- self$results$ymodel

            formula_m <- jmvcore::constructFormula(
                                      self$options$m,
                                      c(self$options$x,
                                        self$options$covariates)
                                    )
            formula_m <- as.formula(formula_m)
            results_m <- stats::lm(formula_m, self$data)
            summary_m <- summary(results_m)
            summary_m_str <- capture.output(print(summary_m))
            formula_y <- jmvcore::constructFormula(
                                      self$options$y,
                                      c(self$options$m,
                                        self$options$x,
                                        self$options$covariates)
                                    )
            formula_y <- as.formula(formula_y)
            results_y <- stats::lm(formula_y, self$data)
            summary_y <- summary(results_y)
            summary_y_str <- capture.output(print(summary_y))
            fct_i <- function(dat, i) {
                dat_i <- dat[i, ]
                outm <- stats::lm(formula_m, dat_i)
                outy <- stats::lm(formula_y, dat_i)
                a_ustd <- stats::coef(outm)[self$options$x]
                b_ustd <- stats::coef(outy)[self$options$m]
                ab_ustd <- a_ustd * b_ustd
                x_sd <- sd(outy$model[, self$options$x])
                y_sd <- sd(outy$model[, self$options$y])
                ab_std <- ab_ustd * x_sd / y_sd
                return(c(ab_ustd, ab_std))
              }

            set.seed(self$options$boot_seed)
            boot_out <- boot::boot(self$data, fct_i, R = self$options$R)

            boot_ci_ustd <- boot::boot.ci(boot_out, type = "perc", index = 1,
                                          conf = self$options$conf)
            boot_ci_ustd_str <- capture.output(print(boot_ci_ustd))
            boot_ci_std <- boot::boot.ci(boot_out, type = "perc", index = 2,
                                         conf = self$options$conf)
            boot_ci_std_str <- capture.output(print(boot_ci_std))

            coef_m <- summary_m$coefficients
            term_m <- rownames(coef_m)
            names(term_m) <- rownames(coef_m)

            coef_y <- summary_y$coefficients
            term_y <- rownames(coef_y)
            names(term_y) <- rownames(coef_y)

            self$results$text$setContent(c(summary_m_str,
                                           "\n",
                                           summary_y_str,
                                           "\n",
                                           boot_ci_ustd_str,
                                           "\n",
                                           boot_ci_std_str))
            coef_table_m$setRow(rowNo = 1,
                values = list(
                            term = "Constant",
                            coef_ustd = coef_m["(Intercept)", "Estimate"]
                          )
              )
            for (i in term_m[-1]) {
                    coef_table_m$addRow(
                        rowKey = i,
                        values = list(
                                term = i,
                                coef_ustd = coef_m[i, "Estimate"]
                              )
                          )
              }

            coef_table_y$setRow(rowNo = 1,
                values = list(
                            term = "Constant",
                            coef_ustd = coef_y["(Intercept)", "Estimate"]
                          )
              )
            for (i in term_y[-1]) {
                    coef_table_y$addRow(
                        rowKey = i,
                        values = list(
                                term = i,
                                coef_ustd = coef_y[i, "Estimate"]
                              )
                          )
              }

        })
)
