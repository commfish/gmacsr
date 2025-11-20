#' Calculate Growth Transition Matrix from Parameters
#'
#' Produces growth transition matrix provided parameters and dimensions
#' @param gpars Growth parameters list in same order as .ctl file.
#' @param n_size_bins Number of size classes.
#' @param size_breaks Breaks between size classes, vector of length `n_size_bins + 1`.
#' @param type Growth parameterization corresponding to GMACS option. Currently only type = 7 is available.

#' @return Growth transition matrix
#' @examples gmacs_calc_growth_trans(gpars = list(161, 0.35, 0.05, 0.05), n_size_bins = 13, size_breaks = seq(40, 170, 10), type = 7)
#'
#' @export
#'
gmacs_calc_growth_trans <- function(gpars, n_size_bins, size_breaks, type) {
  
  if(type == 7){
    
    # unlist parameters
    Linf = gpars[[1]]
    Kappa = gpars[[2]]
    SigmaLinf  = gpars[[3]]
    SigmaLKappa = gpars[[4]]
    
    # translated from GMACS 
    
    # number of quadrature points
    nvar <- 15
    nquad <- 32   # loops
    
    # quadrature points and weights for [-1,1]
    quad <- statmod::gauss.quad(nquad, kind = "legendre")
    xg <- quad$nodes
    wg <- quad$weights
    
    
    # parameter setup
    sigmaL2 <- SigmaLinf^2
    sigmaK2 <- SigmaKappa^2
    
    tempL1 <- sqrt(2*pi*sigmaL2)
    tempL2 <- 2*sigmaL2
    tempk1 <- sqrt(2*pi*sigmaK2)
    tempk2 <- 2*sigmaK2
    
    temp <- sqrt(exp(2*log(Kappa) + sigmaK2) * (exp(sigmaK2) - 1)) * nvar
    rangU <- Kappa + temp
    rangL <- max(0, Kappa - temp)
    Kr <- (rangU - rangL)/2
    
    # initialize growth transition matrix
    growth_transition <- matrix(0, nrow = n_size_bins, ncol = n_size_bins)
    growth_transition[n_size_bins, n_size_bins] <- 1   # terminal bin stays terminal
    rownames(growth_transition) <- size_breaks[1:n_size_bins] + (size_breaks[2] - size_breaks[1])/2
    colnames(growth_transition) <- size_breaks[1:n_size_bins] + (size_breaks[2] - size_breaks[1])/2
    
    # nested integration
    for (l in 1:(n_size_bins-1)) {
      Len1Low <- size_breaks[l]
      Len1Hi  <- size_breaks[l+1]
      
      temp <- Len1Low
      
      scale <- 1.0 / (Len1Low - Len1Hi)
      
      total <- 0
      for (l2c in l:(n_size_bins+20)) {
        if (l2c <= n_size_bins) {
          step <- size_breaks[l2c+1] - size_breaks[l2c]
        } else {
          step <- size_breaks[n_size_bins+1] - size_breaks[n_size_bins]
        }
        l1r <- step/2
        
        templ11 <- scale * Kr * l1r
        Len2Low = temp
        Len2Hi = temp + step
        temp = Len2Hi
        prob_val = 0
        Len2Hi  <- Len2Low + step
        
        prob_val <- 0
        
        #outer quadrature over initial size l1
        for (evl1 in 1:nquad) {
          #l1 <- exp(log(Linf) + SigmaLinf * xg[evl1])   # transform node
          l1 <- ((xg[evl1] + 1)/2) * (Len1Hi - Len1Low) + Len1Low
          temp2 <- (log(l1) - log(Linf))/SigmaLinf
          temp4 <- 1 - pnorm(temp2)
          templ12 <- wg[evl1] * templ11 / temp4
          
          # middle quadrature over kappa
          for (evk in 1:nquad) {
            kval <- ((xg[evk] + 1)/2) * (rangU - rangL) + rangL
            LinfU <- l1 + (Len2Hi - l1)/(1 - exp(-kval))
            LinfL <- l1 + (Len2Low - l1)/(1 - exp(-kval))
            Linfr <- (LinfU - LinfL)/2
            
            temp6 <- exp(-((log(kval) - log(Kappa))^2)/tempk2)/(kval*tempk1)
            temp68 <- wg[evk] * temp6 * templ12
            
            # inner quadrature over Linf
            for (evL in 1:nquad) {
              Linfval <- ((xg[evL] + 1)/2) * (LinfU - LinfL) + LinfL
              if(Linfval <= 0) {next}
              temp5 <- exp(-((log(Linfval) - log(Linf))^2) / tempL2) / (Linfval*tempL1)
              prob_val <- prob_val + Linfr * wg[evL] * temp5 * temp68
            }
          }
        }
        
        total <- total + prob_val
        if (l2c < n_size_bins) {
          growth_transition[l, l2c] <- prob_val
        } else {
          growth_transition[l, n_size_bins] <- growth_transition[l, n_size_bins] + prob_val
        }
      }
      
      # normalization
      growth_transition[l, ] <- growth_transition[l, ] / total
    }
    
  }
  
  return(growth_transition)
  
}





