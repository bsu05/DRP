This is my DRP Project. It is written in R. The basis of it is a Monte-Carlo Simulation of the NHL season. So far, it predicts the winner of the President's trophy. When the playoff bracket is set, I will have it predict the Stanley Cup Champion. Here I will log progress, and to-do.

The general model is quite basic. It iterates through the remaining schedule of the season. Each game is simulated with the Poisson Random Variable to model each team's number of goals. There is simple OT/SO logic as well based on the RV. It then generates variable amounts of seasons for the Monte Carlo portion. 

To-do:
- [x] Implement Stanley Cup Prediction **(04/15)** 

4/15: Added Stanley Cup Monte Carlo, using same game logic, and then iterating through the playoff bracket. I want to fix the updating stats issue (right now it doesn't update as the simulate continues). And potentially add a webscraping feature, so I don't have to obtain and clean NHL standings data manually. 
