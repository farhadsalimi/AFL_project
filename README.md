# AFL_project
AFL_project
Take home challenge 
Build an AFL predictive model to predict match results for the 2017 season 
 
Data Available
•	afl_match_results.csv – date, round, home team, home goals, home behinds, home points, away team, away goals, away behinds, away points, venue, margin, season, round type 
o	It is not necessary to model/use finals data, just the regular home and away season will do
•	afl_player_stats.csv – date, season, round, venue, player, team, opposition, status, goal assists, contested possessions, uncontested possessions, effective disposals, disposal efficiency %, contested marks, marks inside 50, one percenters, bounces, time on ground %, kicks, handballs, disposals, marks, goals, behinds, tackles, hitouts, inside 50s, clearances, clangers,  rebound 50s, frees for, frees against, afl fantasy points, supercoach points, centre clearances, stoppage clearances, score involvements, metres gained, turnovers, intercepts, tackles inside 50 
o	Italicised columns are advanced stats, and aren’t available for all seasons. Feel free to not use them or discard them.
o	It is not a must to use player level stats, because it can be quite time consuming. Feel free to aggregate and use team level stats to model results. 
•	afl_odds.csv – date, match description, season, team names, Betfair odds 
o	Team names may not be consistent across matches and seasons as it is a manual process to create them. Might involve extra cleaning up to match other files as the systems for the previous two files and this one are different. 
o	If odds are not available for any particular match, feel free to ignore those matches. 
 
What is expected
•	Create a predictive model that predicts the winner of a particular AFL match. Use the 2012-2016 data to train your model, and 2017 data to test your model. 
•	Use R or Python or any statistical tool of your choice, whatever you are comfortable with, but please comment your code for readability.
•	Feel free to use any other external data if you think it could help your model, but it is not necessary to do so. 
•	Please cater your solution to show us your data wrangling, predictive modelling skills rather than spending too much time for fine tuning results to get an extra 1% of accuracy (like Kaggle for instance). Your solution will be evaluated on approach more than result. 
•	If you are not familiar with AFL please use these links to get an idea of the sport, the league, and the terms used in the sport 
