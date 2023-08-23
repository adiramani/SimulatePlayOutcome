# Simulate NFL Plays

Link to Shiny app: https://adiramani.shinyapps.io/SimulatedPlayOutcome/

#### todo: add detailed description of project. Brief description found in this twitter thread (and below): https://twitter.com/adiramani84/status/1401326521508241408?s=20

How does it work? 
For every simulation, it compares a random number with the Expected Pass % to determine whether it is a run or pass. If it is a run, it just calculates the Expected Rush Yards. If it is a pass, it then calculates Expected Air Yards. Using the Expected Air Yards, it can then calculate the Expected Completion %. Using another random number, it determines whether the pass was complete or not. Finally, if the pass it complete, it calculates the expected YAC.
