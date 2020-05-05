# Flight Landing Distance Prediction 
 Analysing variables which can help us predict the landing distance of flight on a runway.
 This project is divided in 3 parts.
 
Motivation: To reduce the risk of landing overrun.

Goal: To study what factors and how they would impact the landing distance of a commercial flight.
 
 
Data: Landing data (landing distance and other parameters) from 950 commercial flights (not real data set but simulated from statistical models). See two Excel files ‘FAA-1.xls’ (800 flights) and ‘FAA-2.xls’ (150 flights).


Variable dictionary:
Aircraft: The make of an aircraft (Boeing or Airbus).

Duration (in minutes): Flight duration between taking off and landing. The duration of a normal flight should always be greater than 40min.

No_pasg: The number of passengers in a flight.

Speed_ground (in miles per hour): The ground speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.

Speed_air (in miles per hour): The air speed of an aircraft when passing over the threshold of the runway. If its value is less than 30MPH or greater than 140MPH, then the landing would be considered as abnormal.

Height (in meters): The height of an aircraft when it is passing over the threshold of the runway. The landing aircraft is required to be at least 6 meters high at the threshold of the runway.

Pitch (in degrees): Pitch angle of an aircraft when it is passing over the threshold of the runway.
 
Distance (in feet): The landing distance of an aircraft. More specifically, it refers to the distance between the threshold of the runway and the point where the aircraft can be fully stopped. The length of the airport runway is typically less than 6000 feet.


1st Part
Initial exploration of the data
Data Cleaning and further exploration
Initial analysis for identifying important factors that impact the response variable “landing distance”
Regression using a single factor each time
Check collinearity
Variable selection based on our ranking
Variable selection based on automate algorithm.(AIC)

2nd Part
modeling a binary response using logistic regression.
Create binary responses
long.landing = 1 if distance > 2500; =0 otherwise 
risky.landing = 1 if distance > 3000; =0 otherwise.

Identifying important factors using the binary data of “long.landing”.
Identifying important factors using the binary data of “risky.landing”.
Compare the two models built for “long.landing” and “risky.landing”
Compare models with different link functions
Probit model
Hazard model with complementary log-log link


3rd Part
Create a multinomial variable and attach it to the data set.
Y = 1 if distance < 1000
Y = 2 if 1000 < = distance < 2500
Y = 3 otherwise
Discard the continuous data for “distance”, and assume we are given this multinomial response only without knowing its order.

What are risk factors in the landing process and how do they influence its occurrence?



Analysing if we have any variables in our dataset which can help us predict the number of passengers on board.
Which distributuion would be used to model this variable?
