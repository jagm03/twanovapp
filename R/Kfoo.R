#	Kfoo.R		Estimation of K function as data frame with numerator 
#           and denominator separately
#
#	$Revision: 1 $	$Date: 2017/11/01 20:52:01 $
#
# -------- standard arguments ------------------------------	
#	pattern		point pattern (of class 'ppp')
#
#	r		      distance values at which to compute K	
# and denominator separately
#
#	$Revision: 1 $	$Date: 2017/11/01 20:52:01 $
#
# -------- standard output ------------------------------
#      A data frame with columns named
#
#	num:		numerator of estimated K function
#
#	den:		denominator of estimated K function
#
#	val:		K function estimated by the given correction
# ------------------------------------------------------------------------

Kfoo <- function (pattern, r = NULL, correction = "isotropic",...)
  {
  verifyclass(pattern, "ppp")
  K <- Kest(pattern, r = r, correction = correction, ratio = T, ...)
  num <- attr(K, which = "numerator")
  num <- num[, names(num)!= "theo"]
  den <- attr(K, which = "denominator")
  den <- den[, names(den) != "theo"]
  inden <- attr(K, "valu")
  val <- K[[inden]]
  return(K = data.frame(num = num[[inden]], den = den[[inden]], val = val))
  }
