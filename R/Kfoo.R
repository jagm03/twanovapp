#	Kfoo.R		Estimation of K function as data frame with numerator 
# and denominator separately
#
#	$Revision: 1 $	$Date: 2017/11/01 20:52:01 $
#
#
# -------- standard arguments ------------------------------	
#	pattern		point pattern (of class 'ppp')
#
#	r		distance values at which to compute K	
#
# -------- standard output ------------------------------
#      A data frame with columns named
#
#	r:		same as input
#
#	trans:		K function estimated by translation correction
#
#	iso:		K function estimated by Ripley isotropic correction
#
#	theo:		K function for Poisson ( = pi * r ^2 )
#
#	border:		K function estimated by border method
#			using standard formula (denominator = count of points)
#
#       bord.modif:	K function estimated by border method
#			using modified formula 
#			(denominator = area of eroded window
#
# ------------------------------------------------------------------------

Kfoo <- function (pattern, r, correction = "isotropic"){
  verifyclass(pattern, "ppp")
  K <- Kest(pattern, r = r, correction = correction, ratio = T)
  num <- attr(K, which = "numerator")
  num <- num[, names(num)!= "theo"]
  den <- attr(K, which = "denominator")
  den <- den[, names(den) != "theo"]
  return(K = data.frame(num = num$iso, den = den$iso, val = K$iso))
}
