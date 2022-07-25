# DB-efficient designs
# 3 Attributes, all dummy coded. 1 alternative specific constant = 7 parameters
cand.set <- Profiles(lvls = c(3, 3, 3), coding = c("D", "D", "D"))
mu <- c(0.5, 0.8, 0.2, -0.3, -1.2, 1.6, 2.2) # Prior parameter vector
v <- diag(length(mu)) # Prior variance.
set.seed(123)

pd1 <- MASS::mvrnorm(n = 10, mu = mu, Sigma = v) # 10 draws.
p.d1 <- list(matrix(pd1[,1], ncol = 1), pd1[,2:7])

pd1
p.d1

model2 <- Modfed(cand.set = cand.set, n.sets = 8, n.alts = 2,
       alt.cte = c(1, 0), parallel = FALSE, par.draws = p.d1, best = FALSE)

model2

cand.set1 <- cand.set[!cand.set[,1] !=1 | !cand.set[,3] !=1, ]
cand.set2 <- cand.set[cand.set[,1] !=1 & cand.set[,3] !=1, ]
cand.set3 <- rbind(cand.set1,cand.set2)
cand.set3

cand.set2
