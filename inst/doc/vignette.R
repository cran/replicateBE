## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

## ----eval = FALSE-------------------------------------------------------------
#  modA <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
#                       data = data)

## ----show_mod1, eval = FALSE--------------------------------------------------
#  modB <- lmer(log(PK) ~ sequence + period + treatment + (1|subject),
#                         data = data)

## ----show_mod2, eval = FALSE--------------------------------------------------
#  modB <- lme(log(PK) ~ sequence +  period + treatment, random = ~1|subject,
#                        data = data)

## ----show_mod3, eval = FALSE--------------------------------------------------
#  modB <- lmer(log(PK) ~ sequence + period + treatment + (1|subject),
#                         data = data)

## ----help, eval = FALSE-------------------------------------------------------
#  help("data", package = "replicateBE")
#  ?replicateBE::data

## ----mod_CV, eval = FALSE-----------------------------------------------------
#  modCV <- lm(log(PK) ~ sequence + subject%in%sequence + period,
#                        data = data[data$treatment = "R", ])

## ----expl1--------------------------------------------------------------------
# Estimate sample sizes of full replicate designs (theta0 0.90,
# target power 0.80) and CI of the CV with package PowerTOST
CV     <- 0.30
design <- c("2x2x4", "2x2x3") # 4- and 3-period full replicate designs
res    <- data.frame(design = rep(design, 2), n = c(rep(NA, 2), rep(12, 2)),
                     df = NA, CV = CV, lower = NA, upper = NA)
# 95% confidence interval of the CV
for (i in 1:nrow(res)) {
  if (is.na(res$n[i])) {
    res$n[i] <- PowerTOST::sampleN.scABEL(CV = CV,
                                          design = res$design[i],
                                          details = FALSE,
                                          print = FALSE)[["Sample size"]]
  }
  if (i > 2 ) {
    n.1 <- res$n[i-2] / 2 # no dropouts in one sequence
    n.2 <- 12             # only 12 eligible subjects in the other
    res$n[i] <- n.1 + n.2
  }
  if (res$design[i] == "2x2x4") {
    res$df[i] <- 3 * res$n[i] - 4
  } else {
    res$df[i] <- 2 * res$n[i] - 3
  }
  res[i, 5:6] <- round(PowerTOST::CVCL(CV = CV,
                                       df = res$df[i],
                                       side = "2-sided",
                                       alpha = 0.05), 3)
}
print(res, row.names = FALSE)
# Rows 1-2: Sample sizes for target power
# Rows 3-4: Only 12 eligible subjects in one sequence

## ----typeIII------------------------------------------------------------------
library(replicateBE) # attach the library to run example scripts
method.A(data = rds01, print = FALSE, verbose=TRUE)

## ----expl2--------------------------------------------------------------------
# Calculate limits with package PowerTOST
CV <- c(30, 50, 57.382)
df <- data.frame(CV = CV,
                 reg1 = "EMA", L1 = NA, U1 = NA, cap1 = "",
                 reg2 = "HC",  L2 = NA, U2 = NA, cap2 = "",
                 reg3 = "GCC", L3 = NA, U3 = NA, cap3 = "",
                 stringsAsFactors = FALSE)
for (i in seq_along(CV)) {
  df[i, 3:4]   <- sprintf("%.3f",
                          PowerTOST::scABEL(CV[i]/100,
                                            regulator = df$reg1[i])*100)
  df[i, 7:8]   <- sprintf("%.1f",
                          PowerTOST::scABEL(CV[i]/100,
                                            regulator = df$reg2[i])*100)
  df[i, 11:12] <- sprintf("%.3f",
                          PowerTOST::scABEL(CV[i]/100,
                                            regulator = df$reg3[i])*100)
}
df$cap3[df$CV <= 30] <- df$cap2[df$CV <= 30] <- df$cap1[df$CV <= 30] <- "lower"
df$cap1[df$CV >= 50 & df$reg1 == "EMA"]    <- "upper"
df$cap2[df$CV >= 57.382 & df$reg2 == "HC"] <- "upper"
names(df) <- c("CV(%)", rep(c("reg", "L(%)", "U(%)", "cap"), 3))
print(df, row.names = FALSE)

## ----expl3--------------------------------------------------------------------
# Compare Method B acc. to the GL with Method A for all reference data sets.
ds <- substr(grep("rds", unname(unlist(data(package = "replicateBE"))),
                  value = TRUE), start = 1, stop = 5)
for (i in seq_along(ds)) {
  A <- method.A(print = FALSE, details = TRUE, data = eval(parse(text = ds[i])))$BE
  B <- method.B(print = FALSE, details = TRUE, data = eval(parse(text = ds[i])))$BE
  r <- paste0("A ", A, ", B ", B, " - ")
  cat(paste0(ds[i], ":"), r)
  if (A == B) {
    cat("Methods agree.\n")
  } else {
    if (A == "fail" & B == "pass") {
      cat("Method A is conservative.\n")
    } else {
      cat("Method B is conservative.\n")
    }
  }
}

## ----expl4--------------------------------------------------------------------
A  <- method.A(print = FALSE, details = TRUE, data = rds14)
B1 <- method.B(print = FALSE, details = TRUE, data = rds14, option = 1)
B2 <- method.B(print = FALSE, details = TRUE, data = rds14) # apply default option
B3 <- method.B(print = FALSE, details = TRUE, data = rds14, option = 3)
# Rounding of CI according to the GL
A[17:21]  <- round(A[17:21],  2) # all effects fixed
B1[17:21] <- round(B1[17:21], 2) # Satterthwaite's df
B2[17:21] <- round(B2[17:21], 2) # df acc. to Q&A
B3[17:21] <- round(B3[17:21], 2) # Kenward-Roger df
cs <- c(2, 10, 17:25)
df <- rbind(A[cs], B1[cs], B2[cs], B3[cs])
names(df)[c(1, 3:6, 11)] <- c("Meth.", "L(%)", "U(%)",
                              "CL.lo(%)", "CL.hi(%)", "hw")
df[, c(2, 11)] <- signif(df[, c(2, 11)], 5)
print(df[order(df$hw, df$BE, decreasing = c(FALSE, TRUE),
               method = "radix"), ], row.names = FALSE)

## ----expl5--------------------------------------------------------------------
# Compare different types with some random data
set.seed(123456)
x <- rnorm(48)
p <- c(25, 50, 75)/100
q <- matrix(data = "", nrow = 9, ncol = 4,
            dimnames = list(paste("type =", 1:9),
                            c("1st quart.", "median", "3rd quart.",
                              "software / default")))
for (i in 1:9) {
  q[i, 1:3] <- sprintf("%.5f", quantile(x, prob = p, type = i))
}
q[c(2, 4, 6:8), 4] <- c("SAS, Stata", "SciPy", "Phoenix, Minitab, SPSS",
                        "R, S, MATLAB, Octave, Excel", "Maple")
print(as.data.frame(q))

