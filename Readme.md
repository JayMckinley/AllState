# Proposal

For this project we wanted to find a data set that was both challenging and within the scope or a one-month project.  The Allstate Claim Severity challenge on Kaggle has been running since 10 October 2016 and is scheduled to end 12 December 2016.  This is a recruitment data set designed to challenge potential hires to get creative with the problem in order to climb to the top of the leaderboard.  This is a challenge our team is prepared to undertake.  

# The Problem is the Data

The training data (train.csv) for this challenge has 129 features and 188k observations.  There are 116 categorical features and 14 continuous features.  The outcome variable is the loss associated with each claim.  This is a continuous value which means this is a regression problem.  What makes this data really challenging is that there are no useful labels for the features.  The only columns that are strictly identified are the loss and id columns.  Using this data set we must train and validate our model and then use it to make predictions on another test data set (test.csv) which contains 125k observations.  Then we can submit this on Kaggle.com and we will be scored using *mean absolute area* (MAE).  

# The Solution

First, we want to really take some time and explore the data and try and determine what information each feature can bring and which ones are simply confounding the model.  There is no missingness in the data, but a cursory look showed some collinearity and many features having little to no variance.  The depdent variable is continuous and positively skewed so it may need to be transformed using the log function to make it more normal.    

Secondly, we plan to each take a different path in selecting features and choosing models.  Throughout this process we will meet to ensure we don’t end up on similar analysis tracks.  This will help ensure the goal of producing several diverse models.  At the end we hope to weigh and assemble an ensemble of these models to produce a regressor that out performs any single model.  The path we think might be interesting to take is to have each one of us focus on a different style of learning.  As of right now one of us will focus on *linear regressioin* combined with *elastic net*, another is focusing on *PCA* and way to incorporate alternative dimensions to the model, and one of us will focus primarily on tree based regression utilizing *Random Forrest* and *Gradient Boosted Machine*.
