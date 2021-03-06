#########################################################
# Conventional (unscaled) ABE by ANOVA                  #
# fixed: sequence, subject(sequence), period, treatment #
#########################################################
ABE <- function(alpha = 0.05, path.in, path.out = tempdir(), file, set = "",
                ext, na = ".", sep = ",", dec = ".",
                logtrans = TRUE, print = TRUE, details = FALSE,
                verbose = FALSE, ask = FALSE, data = NULL,
                theta1, theta2) {
  exec <- strftime(Sys.time(), usetz=TRUE)
  if (!missing(ext)) ext <- tolower(ext) # case-insensitive
  if (missing(theta1) & missing(theta2)) theta1 <- 0.80
  if (missing(theta1)) theta1 = 1/theta2
  if (missing(theta2)) theta2 = 1/theta1
  ret  <- CV.calc(alpha=alpha, path.in=path.in, path.out=path.out,
                  file=file, set=set, ext=ext, na=na, sep=sep,
                  dec=dec, logtrans=logtrans, ola=FALSE,
                  print=print, verbose=verbose, ask=ask,
                  theta1=theta1, theta2=theta2, data=data)
  logtrans <- ret$logtrans
  if (print) { # change from "ABEL" to "ABE"
    if (theta1 <= 0.8) {
      results <- paste0(substr(ret$res.file, 1,
                        which(strsplit(ret$res.file, "")[[1]]=="_")),
                        "ABE.txt")
    } else {
      results <- paste0(substr(ret$res.file, 1,
                               which(strsplit(ret$res.file, "")[[1]]=="_")),
                        "ABE_NTID.txt")
    }
  }
  # generate variables based on the attribute
  # 2nd condition: Otherwise, the header from a CSV file will be overwritten
  if (!is.null(data) | missing(ext)) {
    info  <- info.data(data)
    file  <- info$file
    set   <- info$set
    ref   <- info$ref
    descr <- info$descr
    ext   <- ""
  }
  os <- Sys.info()[[1]]   # get OS for line-endings in output (Win: CRLF)
  ow <- options("digits") # save options
  options(digits=12)      # increase digits for anova()
  on.exit(ow)             # ensure that options are reset if an error occurs
  if (logtrans) { # use the raw data and log-transform internally
    mod <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
                        data=ret$data)
  } else {        # use the already log-transformed data
    mod <- lm(logPK ~ sequence + subject%in%sequence + period + treatment,
                      data=ret$data)
  }
  if (verbose) {
    name <-  paste0(file, set)
    len  <- max(27+nchar(name), 35)
    cat(paste0("\nData set ", name, ": ABE by lm()"),
        paste0("\n", paste0(rep("\u2500", len), collapse = "")), "\n")
    # change from type I (default as in versions up to 1.0.17)
    # to type III to get the correct carryover test
    typeIII <- stats::anova(mod) # otherwise summary of lmerTest is used
    attr(typeIII, "heading")[1] <- "Type III Analysis of Variance Table\n"
    MSdenom <- typeIII["sequence:subject", "Mean Sq"]
    df2     <- typeIII["sequence:subject", "Df"]
    fvalue  <- typeIII["sequence", "Mean Sq"] / MSdenom
    df1     <- typeIII["sequence", "Df"]
    typeIII["sequence", 4] <- fvalue
    typeIII["sequence", 5] <- pf(fvalue, df1, df2, lower.tail = FALSE)
    print(typeIII, digits = 6, signif.stars = FALSE)
    cat("\ntreatment T \u2013 R:\n")
    print(signif(summary(mod)$coefficients["treatmentT", ]), 6)
    cat(anova(mod)["Residuals", "Df"], "Degrees of Freedom\n\n")
  }
  PE  <- exp(coef(mod)[["treatmentT"]])
  CI  <- as.numeric(exp(confint(mod, "treatmentT", level=1-2*alpha)))
  DF  <- aov(mod)[[8]]
  res <- data.frame(ret$type, "ABE", ret$n, ret$nTT, ret$nRR,
                    paste0(ret$Sub.Seq, collapse="|"),
                    paste0(ret$Miss.seq, collapse="|"),
                    paste0(ret$Miss.per, collapse="|"), alpha,
                    DF, ret$CVwT, ret$CVwR,
                    100*theta1, 100*theta2, CI[1], CI[2], PE, "fail",
                    stringsAsFactors=FALSE)
  names(res)<- c("Design", "Method", "n", "nTT", "nRR", "Sub/seq",
                 "Miss/seq", "Miss/per", "alpha", "DF", "CVwT(%)",
                 "CVwR(%)", "BE.lo(%)", "BE.hi(%)", "CL.lo(%)",
                 "CL.hi(%)", "PE(%)", "BE")
  # Convert CVs, limits, PE, and CI (till here as fractions) to percent
  res$"CVwT(%)"  <- 100*res$"CVwT(%)"
  res$"CVwR(%)"  <- 100*res$"CVwR(%)"
  res$"PE(%)"    <- 100*res$"PE(%)"
  res$"CL.lo(%)" <- 100*res$"CL.lo(%)"
  res$"CL.hi(%)" <- 100*res$"CL.hi(%)"
  if (round(res$"CL.lo(%)", 2) >= 100*theta1 &
      round(res$"CL.hi(%)", 2) <= 100*theta2)
    res$BE <- "pass" # CI within acceptance range
  options(ow) # restore options
  if (details) { # results in full numeric precision
    ret <- res   # and remove superfluous columns
    #class(ret) <- "repBE"
    return(ret)
  }
  # Round percents to two decimals according to the GL
  res$"CVwT(%)"  <- round(res$"CVwT(%)", 2)
  res$"CVwR(%)"  <- round(res$"CVwR(%)", 2)
  res$"BE.lo(%)" <- round(res$"BE.lo(%)", 2)
  res$"BE.hi(%)" <- round(res$"BE.hi(%)", 2)
  res$"PE(%)"    <- round(res$"PE(%)", 2)
  res$"CL.lo(%)" <- round(res$"CL.lo(%)", 2)
  res$"CL.hi(%)" <- round(res$"CL.hi(%)", 2)
  overwrite <- TRUE # default
  if (print) { # to file in UTF-8
    if (ask & file.exists(results)) {
      answer <- tolower(readline("Results already exists. Overwrite the file [y|n]? "))
      if(answer != "y") overwrite <- FALSE
    }
    if (overwrite) { # either the file does not exist or should be overwritten
      # only binary mode supports UTF-8 and different line endings
      res.file <- file(description=results, open="wb")
      res.str  <- info.env(fun="ABE", option=NA, path.in, path.out,
                           file, set, ext, exec, data)
      if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
      if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
      writeBin(charToRaw(res.str), res.file)
      close(res.file)
    }
  }
  txt <- paste0(ret$txt, "\nalpha              :   ", alpha,
                " (", 100*(1-2*alpha), "% CI)")
  txt <- paste0(txt,
                "\nConfidence interval: ", sprintf("%6.2f%% ... %6.2f%%",
                res$"CL.lo(%)", res$"CL.hi(%)"), "  ", res$BE,
                "\nPoint estimate     : ", sprintf("%6.2f%%", res$"PE(%)"), "\n")
  txt <- paste0(txt,
                repBE.draw.line(called.from="ABE", L=ret$BE1, U=ret$BE2,
                                lo=CI[1], hi=CI[2], PE=PE,
                                theta1=theta1, theta2=theta2), "\n")
  if (res$Design == "TRR|RTR")
    txt <- paste0(txt, "\nNote: The extra-reference design assumes lacking period effects. ",
                  "The treatment\ncomparison will be biased in the presence of a ",
                  "true period effect.")
  if (res$Design %in% c("TRTR|RTRT|TRRT|RTTR", "TRRT|RTTR|TTRR|RRTT"))
    txt <- paste0(txt, "\nNote: Confounded effects; design not recommended.")
  txt <- paste0(txt, "\n")
  if (print & overwrite) {
    res.file <- file(results, open="ab")                        # line endings
    res.str  <- txt                                             # LF (UNIXes, Solaris)
    if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF (Windows)
    if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR (OSX)
    writeBin(charToRaw(res.str), res.file)
    close(res.file)
  }
} # end of function ABE()
