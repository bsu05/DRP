This is my DRP Project. It is written in R. The basis of it is a Monte-Carlo Simulation of the NHL season. So far, it predicts the winner of the President's trophy. When the playoff bracket is set, I will have it predict the Stanley Cup Champion. Here I will log progress, and to-do.

The general model is quite basic. It iterates through the remaining schedule of the season. Each game is simulated with the Poisson Random Variable to model each team's number of goals. There is simple OT/SO logic as well based on the RV. It then generates variable amounts of seasons for the Monte Carlo portion. 

To-do:
- [ ] Implement Stanley Cup Prediction **(Date Completed)** 
