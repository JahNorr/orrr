

#' Pearson's Chi-squared test
#'
#' Convenience function for stats::chisq.test(). Returns only the p-value if pvalue is set to TRUE (default),
#' otherwise returns a list with class "htest"
#'
#' @param vals an integer vector of numerator (odd elements), denominator (even elements) pairs
#' @param pvalue a logical TRUE: return only p-value | FALSE: return list from chisq.test
#' @param correct a logical indicating whether to apply continuity correction when computing the test statistic for 2 by 2 tables
#'
#' @return numeric p-value or list with class "htest"
#' @export
#'
#' @examples
#' v<-c(c(100,144),c(200,344),c(75,130))
#' orrr::chi_square(v)
#'

chi_square<-function(vals, pvalue=T,correct=FALSE) {
#  require(pwr,quietly = TRUE, warn.conflicts = FALSE)

  ev<-seq(2,length(vals),2)
  od<-ev-1
  vals[ev]<-vals[ev]-vals[od]

  mat<-matrix(vals, nrow = 2)

  chsq<-chisq.test(mat,correct=correct)

  if(pvalue) return (chsq$p.value)  else return (chsq)
}
