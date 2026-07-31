if (R::dbinom(x, x, pmax, false) > tol) {
  stn = x;
} else {
  stn = x + 1;
  while (R::dbinom(x, stn, pmax, false) < tol) {
    stn++;
  }
}

endn = stn + 1;
while (R::dbinom(x, endn, pmax, false) > tol) {
  endn++;
}
k <- 18
pvec <- samples[,k]
pmax <- max(pvec)
pmin <- min(pvec)

x <- analysedata$n[k]

dbinom(x, x, pmax)

p <- .5;ns <- 10000;x <- 100

ls <- f_N_vec(x,rep(p,ns))
       
ls2 <- rpois(ns,rgamma(ns,x+1,1)/p)

median(ls2)
ls |> quantile(probs = c(.5,.025,.0975))
stn <- x+1
endn = x + 1
tol = 1e-20

while (dbinom(x, stn, pmax) < tol) {
    stn <- stn+1
  }
  
endn = stn + 1;
while (dbinom(x, endn, pmin) < tol) {
  endn <- endn + 1
}

k <- 19;l <- 20
ls <- f_N_vec(x,rep(pmax,2))

sourceCpp("~/Projects/GitHub/RDNowcast/Junk/TestNowcastProb.cpp")
sourceCpp("~/Projects/GitHub/RDNowcast/Junk/TestNowcastTest.cpp")

data <- RDNowcast::RDdata
datum1 <- today()-5- 26*7

NCdates <- seq.Date(datum1-14,length.out = 4,by = "weeks")

NCsize = 10  
reference_date = "reference_date"; report_date = "report_date" 

obs <- Nowcastobs(data = data,NCdates = NCdates)
full <- Nowcastfull(data = data,NCdates = NCdates)



t1 <- Sys.time()
pmat <- pSampleMult(NC)
t2 <- Sys.time();t2-t1

t1 <- Sys.time()
pmatC <- pSampleMultC(NC)
t2 <- Sys.time();t2-t1
