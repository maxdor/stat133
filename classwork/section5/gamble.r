# Implement the function betFun. Your function should take the following
# arguments:
#
# <current.wealth>: a numeric value indicating your wealth at the current
#   iteration of a simulation.
# <previous.winnings>: (optional) a numeric value indicating the amount won or
#   lost in the previous iteration of a simulation
#
# Your function should return the amount of your bet for the current iteration
# of a simulation. The bet must be between 0 and <current.wealth>.
betFun <- function(current.wealth,  previous.winnings=NA) {

    # your code here
    bet <- max(current.wealth / 3, 0)
    return(min(bet, current.wealth))
}

# Implement a function that runs a gambling simulation with the following
# parameters:
#
# <bet.FUN>: a betting function that returns a numeric value based on inputs of
#   current wealth and previous winnings
# <init.wealth>: a numeric value giving starting wealth
# <prob.win>: a numeric value between 0 and 1 giving the probability of winning
#   each round
# <max.turns>: an integer giving the maximum number of turns to be played
#
# Your function should return the difference between your initial and final
# wealth after playing a game for <max.turns> rounds. If your wealth falls below
# 0, the simulation should end.
gamble <- function(bet.FUN, init.wealth=50, prob.win=0.52, max.turns=25) {
  current.wealth <- init.wealth
  bet <- bet.FUN(current.wealth, 0)
  games <- rbinom(max.turns, 1, prob.win)
  
  for(turn in 1:max.turns) {
    if(games[turn] == 1) {
      current.wealth <- current.wealth + bet
      bet <- bet.FUN(current.wealth, bet)
    } else {
      current.wealth <- current.wealth - bet
      bet <- bet.FUN(current.wealth, -bet)
    }
    
    if(current.wealth < 0) break
    if(current.wealth > 2.5 * init.wealth) break
  }
  return(current.wealth - init.wealth)
}

money <- replicate(1000, gamble(betFun))


