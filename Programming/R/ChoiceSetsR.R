install.packages('idefix')

library(idefix)

example_design

at.levels <- c(4,6,2,2,2)
c.type  <- c("E","E","E","E","E")
prof <- Profiles(lvls = at.levels, coding = c.type)

prof

prof1 <- prof[(prof[,1]==1) & prof[,5]==(-1), ]
prof2 <- prof[(prof[,2]==1 | prof[,2]==(-1)) & prof[,5]!=(-1), ]
prof3 <- prof[(prof[,3]==1 | prof[,3]==(-1)) & prof[,5]!=(-1), ]
prof3
prof4 <- rbind(prof1, prof2, prof3)
prof4

#prof1 <- prof1[!prof1[,1] !=0 | !prof1[,5] !=0, ]
#prof1 <- prof1[!prof1[,1] !=0 | !prof1[,6] !=0, ]
#prof1 <- prof1[!prof1[,1] !=0 | !prof1[,7] !=0, ]
#prof1 <- prof1[!prof1[,1] !=0 | !prof1[,8] !=0, ]

#prof1 <- prof1[!prof1[,2] !=0 | !prof1[,4] !=0, ]
#prof1 <- prof1[!prof1[,2] !=0 | !prof1[,5] !=0, ]
#prof1 <- prof1[!prof1[,2] !=0 | !prof1[,6] !=0, ]
#prof1 <- prof1[!prof1[,2] !=0 | !prof1[,7] !=0, ]


prof1




# DB-efficient design

mu <- c(0.82,0.44,-0.41, 0.55,0,0.17,-0.15,0, 0.84, 0.23, 0.6) # Prior parameter vector
v <- diag(length(mu)) # Prior variance.

v[1,1] <- 11*(1.43^2) 
v[2,2] <- 11*(0.6^2)
v[3,3] <- 11*(0.54^2)
v[4,4] <- 11*(0.57^2)
v[5,5] <- 11*(0.48^2)
v[6,6] <- 11*(0.51^2) 
v[7,7] <- 11*(0.44^2)
v[8,8] <- 0.5 
v[9,9] <- 11*(0.17^2)
v[10,10] <- 11*(0.17^2) 
v[11,11] <- 11*(0.17^2)

set.seed(123)
pd <- MASS::mvrnorm(n = 10, mu = mu, Sigma = v) # 10 draws.
p.d <- list(matrix(pd[,1], ncol = 1), pd[,2:11])


modelt <- Modfed(cand.set = prof4, n.sets = 12, n.alts = 2,
       alt.cte = c(0,0), parallel = TRUE, par.draws = pd, best = TRUE)

modelt

modelt2 <- Modfed(cand.set = prof, n.sets = 12, n.alts = 2,
                  alt.cte = c(0,0), parallel = FALSE, par.draws = pd, best = TRUE)

modelt2$design

modelt2

mu1 <- c(0,0,0, 0,0,0,0,0, 0, 0, 0) # Prior parameter vector
v1 <- diag(length(mu)) # Prior variance.
set.seed(123)
pd2 <- MASS::mvrnorm(n = 10, mu = mu1, Sigma = v1) # 10 draws.
p.d2 <- list(matrix(pd2[,1], ncol = 1), pd2[,2:11])


modelt3 <- Modfed(cand.set = prof4, n.sets = 12, n.alts = 2,
                 alt.cte = c(0,0), parallel = FALSE, par.draws = pd2, best = TRUE)
modelt3 

## Not run:
# Example without continuous attributes.
design_final <- read.csv("/Users/dominikmecko/Work/Bachelor's Thesis/Data/design_final.csv")
design <- modelt$design
coded <- c("E", "E", "E","E","E") # Coding.
# Levels as they should appear in survey.
al <- list(
  c("$-5", "$0", "$2", "$5"), # Levels attribute 1.
  c("All RES", "wind", "solar","hydro","biomass","normal"), # Levels attribute 2.
  c("Local", "International"), # Levels attribute 3
  c("Can change","Cannot"),
  c("No chagne", "Need change")
)

# Decode

Decode(des = design, n.alts = 2, lvl.names = al, coding = coded)
decoded <-decoded$design
write.csv(decoded,"/Users/dominikmecko/Work/Bachelor's Thesis/Data/design_decoded.csv")


design3 <- data.frame(modelt$design)
write.csv(design3,"/Users/dominikmecko/Work/Bachelor's Thesis/Data/design_final.csv")









designpilot = read.csv("/Users/dominikmecko/Work/Bachelor's Thesis/Data/Pilot/design.csv")
designpilot1 <- as.matrix(designpilot[,-1])
## Not run:
# Example without continuous attributes.
coded <- c("E", "E", "E","E","E") # Coding.
# Levels as they should appear in survey.
al <- list(
  c("$-5", "$0", "$2", "$5"), # Levels attribute 1.
  c("All RES", "wind", "solar","hydro","biomass","normal"), # Levels attribute 2.
  c("Local", "International"), # Levels attribute 3
  c("Can change","Cannot"),
  c("No chagne", "Need change")
)

# Decode
Decode(des = design3, n.alts = 2, lvl.names = al, coding = coded)