#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
library(dplyr)
source("GitHub/src/stat133/hw6/bml_functions.R")
set.seed(1)

sim <- function(lengths, widths, densities, repititions) {
  results <- data.frame(r = NULL, c = NULL, p = NULL, iter = NULL)
  
  for (len in lengths) {
    for (wid in widths) {
      for (dens in densities) {
        print(dens)
        for (rep in repititions) {
          result <- bml.sim(len, wid, dens)
          results <- rbind(results, 
                           data.frame(r = len, c = wid, p = dens, iter = result[[2]]))
        }
      }
    }
  }
  return(results)
}


lengths <- 20
widths <- 20
densities <- seq(from = .05, to = .95, by = .05)
repititions <- 1

system.time(step1 <- sim(lengths, widths, densities, repititions))
plot(step1$iter~step1$p, xlab = "p", ylab = "Iterations",
     main = "20x20 Matrix")

write.csv(step1, "R/step1.csv")
step1 <- read.csv("R/step1.csv")

# Looks like the issues happen between p = .4 and p = .6

lengths <- 20
widths <- 20
densities <- seq(from = .4, to = .6, by = .01)
repititions <- 1:10

system.time(step2 <- sim(lengths, widths, densities, repititions))
step2 <- group_by(step2, p) %>%
  summarize(iter = mean(iter))

plot(step2$iter~step2$p, xlab = "p", ylab = "Iterations",
     main = "20x20 Matrix")

write.csv(step2, "R/step2.csv")
step2 <- read.csv("R/step2.csv")


# Try different shapes of matrix
lengths <- seq(from = 5, to = 20, by = 5)
widths <- seq(from = 5, to = 20, by = 5)
densities <- seq(from = .05, to = .95, by = .05)
repititions <- 1

system.time(step3 <- sim(lengths, widths, densities, repititions))
step3$col <- rep(1:16, each=19)
plot(step3[step3$col == 1, ]$iter~step3[step3$col == 1, ]$p, col = 1)
for (i in 2:16) {
  plot(step3[step3$col == i, ]$iter~step3[step3$col == i, ]$p, col = i)
}

step3$shape <- ifelse(step3$r == step3$c, "Square", "Rectangular")
plot(step3[step3$shape == "Square", ]$iter~step3[step3$shape == "Square", ]$p, col = 1)
plot(step3[step3$shape != "Square", ]$iter~step3[step3$shape != "Square", ]$p, col = 1)

write.csv(step3, "R/step3.csv")
step3 <- read.csv("R/step3.csv")

# Rectangular shapes tend to tolerate a higher p value before congestion. The more elongated, the higher p
# value is tolerable. for a 5x20 at p = 0.7 failure begins vs a 15x20 is at 0.6.
# Square shapes fail between 0.4 and 0.45. Let's look at a larger matrix for accuracy.

lengths <- 50
widths <- 50
densities <- seq(from = .4, to = .5, by = .01)
repititions <- 1:20

system.time(step4 <- sim(lengths, widths, densities, repititions))
step4 <- group_by(step4, p) %>%
  summarize(iter = mean(iter))

plot(step4$iter~step4$p, xlab = "p", ylab = "Iterations",
     main = "50x50 Matrix", type = 'l')

write.csv(step4, "R/step4.csv", row.names = FALSE)
step4 <- read.csv("R/step4.csv")

lengths <- 5
widths <- 50
densities <- seq(from = .55, to = .85, by = .01)
repititions <- 1:20

system.time(step5 <- sim(lengths, widths, densities, repititions))
step5 <- group_by(step5, p) %>%
  summarize(iter = mean(iter))

plot(step5$iter~step5$p, xlab = "p", ylab = "Iterations",
     main = "5x50 Matrix", type = 'l')

write.csv(step5, "R/step5.csv", row.names = FALSE)
step5 <- read.csv("R/step5.csv")

lengths <- 50
widths <- 5
densities <- seq(from = .55, to = .85, by = .01)
repititions <- 1:20

system.time(step6 <- sim(lengths, widths, densities, repititions))
step6 <- group_by(step6, p) %>%
  summarize(iter = mean(iter))

plot(step6$iter~step6$p, xlab = "p", ylab = "Iterations",
     main = "50x5 Matrix", type = 'l')

write.csv(step6, "R/step6.csv", row.names = FALSE)
step6 <- read.csv("R/step6.csv")

# Plot 50x5 vs 5x50
plot(step5$iter~step5$p, xlab = "p", ylab = "Iterations", type = 'l')
points(step6$iter~step6$p, xlab = "p", ylab = "Iterations",
     type = 'l', col = "dodgerblue")
legend("topright",
       c("50x5", "5x50"),
       col = c("dodgerblue", "black"), lwd = c(2.5, 2.5))

#lengths <- c(5,20)
#widths <- c(5,20)
#densities <- seq(from = .2, to = .8, by = .01)
#repititions <- 1:20

#system.time(step7 <- sim(lengths, widths, densities, repititions))
#write.csv(step7, "R/step7.csv")
#step7 <- read.csv("R/step7.csv")

# Try to find exact point where transition happens

lengths <- 100
widths <- 100
densities <- seq(from = .4, to = .45, by = .001)
repititions <- 1:10

system.time(step8 <- sim(lengths, widths, densities, repititions))
step8 <- group_by(step8, p) %>%
  summarize(iter = mean(iter))

plot(step8$iter~step8$p, xlab = "p", ylab = "Iterations",
     main = "50x5 Matrix", type = 'l')

write.csv(step8, "R/step8.csv", row.names = FALSE)
