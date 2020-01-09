## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

## ----setup--------------------------------------------------------------------
library(replicateBE) # attach the library

## ----eval=FALSE---------------------------------------------------------------
#  modA <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
#                       data = data)

## ----eval=FALSE---------------------------------------------------------------
#  modB <- lmer(log(PK) ~ sequence + period + treatment + (1|subject),
#                         data = data)

## ----eval=FALSE---------------------------------------------------------------
#  modB <- lme(log(PK) ~ sequence +  period + treatment, random = ~1|subject,
#                        data = data)

## ----eval=FALSE---------------------------------------------------------------
#  modB <- lmer(log(PK) ~ sequence + period + treatment + (1|subject),
#                         data = data)

## ----eval=FALSE---------------------------------------------------------------
#  help("data", package = "replicateBE")
#  ?replicateBE::data

## ----eval=FALSE---------------------------------------------------------------
#  modCV <- lm(log(PK) ~ sequence + subject%in%sequence + period,
#                        data = data[data$treatment = "R", ])

## -----------------------------------------------------------------------------
# Estimate sample sizes of full replicate designs (theta0 0.90, target
# power 0.80) and CI of the CV with library PowerTOST
CV <- 0.30
n4 <- PowerTOST::sampleN.scABEL(CV = CV, design = "2x2x4", details = FALSE,
                                print = FALSE)[["Sample size"]] # 4-period
n3 <- PowerTOST::sampleN.scABEL(CV = CV, design = "2x2x3", details = FALSE,
                                print = FALSE)[["Sample size"]] # 3-period
# 95% CI of CVs in %
round(100*PowerTOST::CVCL(CV = CV, df = 3*n4-4, "2-sided"), 2) # 4-period
round(100*PowerTOST::CVCL(CV = CV, df = 2*n3-3, "2-sided"), 2) # 3-period
# As above but assume that only 12 subjects remain in each sequence
round(100*PowerTOST::CVCL(CV = CV, df = 3*24-4, "2-sided"), 2) # 4-period
round(100*PowerTOST::CVCL(CV = CV, df = 2*24-3, "2-sided"), 2) # 3-period

## -----------------------------------------------------------------------------
# Calculate limits with library PowerTOST
CV <- c(30, 40, 49.6, 50, 50.4)
df <- data.frame(CV = CV, L = NA, U = NA, cap = "",
                 stringsAsFactors = FALSE)
for (i in seq_along(CV)) {
  df[i, 2:3] <- sprintf("%.8f", PowerTOST::scABEL(CV[i]/100)*100)
}
df$cap[df$CV <= 30] <- "lower"
df$cap[df$CV >= 50] <- "upper"
names(df)[1:3] <- c("CV(%)", "L(%)", "U(%)")
print(df, row.names = FALSE)

## -----------------------------------------------------------------------------
# Compare Method B acc. to the GL with Method A for all reference datasets.
ds <- substr(grep("rds", unname(unlist(data(package = "replicateBE"))),
                  value = TRUE), start = 1, stop = 5)
for (i in seq_along(ds)) {
  A <- method.A(print = FALSE, details = TRUE, data = eval(parse(text = ds[i])))$BE
  B <- method.B(print = FALSE, details = TRUE, data = eval(parse(text = ds[i])))$BE
  r <- paste0("A ", A, ", B ", B, " \u2013 ")
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

## -----------------------------------------------------------------------------
A  <- method.A(print = FALSE, details = TRUE, data = rds14)
B1 <- method.B(print = FALSE, details = TRUE, data = rds14, option = 1)
B2 <- method.B(print = FALSE, details = TRUE, data = rds14) # apply default option
B3 <- method.B(print = FALSE, details = TRUE, data = rds14, option = 3)
# Rounding of CI according to the GL
A[15:19]  <- round(A[15:19],  2) # all effects fixed
B1[15:19] <- round(B1[15:19], 2) # Satterthwaite's df
B2[15:19] <- round(B2[15:19], 2) # df acc. to Q&A
B3[15:19] <- round(B3[15:19], 2) # Kenward-Roger df
cs <- c(2, 10, 15:23)
df <- rbind(A[cs], B1[cs], B2[cs], B3[cs])
names(df)[c(1, 3:6, 11)] <- c("Meth.", "L(%)", "U(%)",
                              "CL.lo(%)", "CL.hi(%)", "hw")
df[, c(2, 11)] <- signif(df[, c(2, 11)], 5)
print(df[order(df$BE, df$hw, decreasing = c(FALSE, TRUE)), ],
      row.names = FALSE)

## -----------------------------------------------------------------------------
### Compare different types with some random data
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

## ---- sessioninfo-------------------------------------------------------------
options(width = 80)
devtools::session_info()

