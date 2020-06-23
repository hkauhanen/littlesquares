# Preprocessing code to prepare the hash table that may be used to convert
# between tau and H(tau).

require(pracma) # to compute arithmetic-geometric mean


# H(tau)
Htau <- function(tau) {
  (pi*(1 + tau))/(2*ceifk(1/(1 + tau))) - tau
}

# the complete elliptic integral of the first kind
ceifk <- function(k) {
  (0.5*pi)/pracma::agmean(1, sqrt(1 - k^2))$agm
}

# performs the actual task of constructing the tau hash
make_tauhash <- function(tau_min,
                         tau_max,
                         tau_res) {
  tau <- exp(seq(from=log(tau_min), to=log(tau_max), length.out=tau_res))
  hash <- data.frame(tau=tau, Htau=Htau(tau))
  
  # check each Htau is unique. If not, issue a warning
  if (length(unique(hash$Htau)) < tau_res) {
    warning("Some Htau values non-unique. Consider rerunning with a smaller value of tau_res")
  }

  hash
}



# Prepare the hash
tau_hash <- make_tauhash(tau_min=0.000001, tau_max=10000, tau_res=10000)
write.csv(tau_hash, file="../conf/tau-hash.csv", row.names=FALSE)


