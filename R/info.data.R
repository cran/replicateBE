###########################################
# return information of the internal data #
# containg variables as required by other #
# functions. Don't forget to update for   #
# new reference data sets!                #
############################################
info.data <- function(data = NULL) {
  if (missing(data) | is.null(data)) stop()
  sets     <- 30
  descr    <- c("Dataset I given by the EMA available at
 https://www.ema.europa.eu/en/documents/other/31-annex-ii-statistical-analysis-bioequivalence-study-example-data-set_en.pdf",
                "Dataset II given by the EMA (Q&A document) available at https://www.ema.europa.eu/en/documents/other/statistical-method-equivalence-studies-annex-iii_en.pdf",
                "Modified dataset I given by the EMA: Period\u00A03 removed.",
                "Cmax data of Table II from Patterson SD, Jones B. Viewpoint: observations on scaled average bioequivalence. Pharm Stat. 2012:11(1):1\u20137. doi:10.1002/pst.498",
                "Cmax data of the Appendix from Metzler CM, Shumaker RC. The Phenytoin Trial is a Case Study of \u2018Individual\u2019 Bioequivalence. Drug Inf J. 1998:32:1063\u201372.",
                "Modified dataset I given by the EMA: T and R switched.",
                "Dataset simulated with CVwT\u00A0=\u00A0CVwR\u00A0=\u00A035%, GMR\u00A0=\u00A00.90.",
                "Dataset simulated with CVwT\u00A0=\u00A070%, CVwR\u00A0=\u00A080%, CVbT\u00A0=\u00A0CVbR\u00A0=\u00A0150%, GMR\u00A0=\u00A00.85.",
                "Dataset with wide numeric range (based of rds08: Data of last 37 subjects multiplied by 1,000,000).",
                "Table 9.3.3 (AUC) from: Chow SC, Liu JP. Design and Analysis of Bioavailability and Bioequivalence Studies. Boca Raton: CRC Press; 3rd edition 2009. p275.",
                "Table 9.6 (Cmax) from: Hauschke D, Steinijans VW, Pigeot I. Bioequivalence Studies in Drug Development. Chichester: John Wiley: 2007. p216. (Drug\u00A017a of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip).",
                "Dataset simulated with extreme intra- and intersubject variability, GMR\u00A0= 1.6487.",
                "Highly incomplete dataset (based of rds08: Approx. 50% of period\u00A04 data deleted).",
                "Dataset simulated with extreme intra- and intersubject variability, GMR\u00A0= 1. Dropouts as a hazard function growing with period.",
                "Highly incomplete data set (based of rds08: Approx. 50% of period\u00A04 data are coded as missing '.').",
                "Drug 14a, Cmax data: MAO inhibitor\u00A0- IR of the FDA\u2019s bioequivalence study files: available at https://wayback.archive-it.org/7993/20170723175533/https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip.",
                "Highly unbalanced dataset (based on rds03: 12 subjects in RTR and 7 in TRT).",
                "Highly incomplete dataset (based on rds14: T data of subjects 63\u201378 removed).",
                "Highly incomplete dataset (based on rds18: Data of subjects 63\u201378 removed).",
                "Highly incomplete dataset (based on rds19: Outlier of R (subject\u00A01) introduced: original value \u00D7100).",
                "Modified dataset I given by the EMA: One extreme result of subjects 45
& 52 set to NA.",
                "Dataset simulated with CVwT\u00A0= CVwR\u00A0=\u00A045%, CVbT\u00A0= CVbR\u00A0=\u00A0100%, GMR\u00A0=\u00A00.90.",
                "Drug 7a, Cmax data: Beta-adrenergic blocking agent\u00A0- IR of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip.",
                "Drug 1, Cmax data: Antianxiety agent\u00A0- IR of the FDA\u2019s bioequivalence study files: available at https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip.",
                "Dataset simulated with CVwT\u00A0=\u00A050%, CVwR\u00A0=\u00A080%, CVbT\u00A0=\u00A0CVbR\u00A0=\u00A0130%, GMR\u00A0=\u00A00.90.",
                "Example 4.4 (Cmax) from: Patterson SD, Jones B. Bioequivalence and Statistics in Clinical Pharmacology. Boca Raton: CRC Press; 2nd edition 2016. p105\u20136.",
                "Dataset simulated with CVwT\u00A0= CVwR\u00A0=\u00A035%, CVbT = CVbR\u00A0=\u00A075%, GMR\u00A0=\u00A00.90.",
                "Dataset simulated with CVwT\u00A0= CVwR\u00A0=\u00A035%, CVbT = CVbR\u00A0=\u00A075%, GMR\u00A0=\u00A00.90.",
                "Highly imbalanced & incomplete dataset simulated with CVwT\u00A0=\u00A014%, CVwR\u00A0=\u00A028%, CVbT\u00A0=\u00A028% CVbR\u00A0=\u00A056%, GMR\u00A0=\u00A00.90.",
                "Highly imbalanced & incomplete dataset simulated with CVwT\u00A0=\u00A014%, CVwR\u00A0=\u00A028%, CVbT\u00A0=\u00A028% CVbR\u00A0=\u00A056%, GMR\u00A0=\u00A00.90.")
  file     <- rep("DS", sets)
  set      <- sprintf("%02i", 1:sets)
  ref      <- paste0("rds", set)
  id       <- data.frame(file, set, ref, descr, stringsAsFactors = FALSE)
  act      <- attr(data, "rset")
  if (!act %in% ref) {
    info <- NULL
  } else {
    info <- id[as.integer(substr(act, 4, 5)), ]
  }
  return(info)
}
