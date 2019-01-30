# shiny-nonparametric
Ranked and non-ranked post hoc test using Shiny

This was the final project for Stat 464, Applied Non-Parametric Statistics.
The goal was to make an app using Shiny that applied something we learned throughout the semester.
I choose to create an app that performs 3 post hoc tests, HSD, LSD, and Bonferroni, and their respective
ranked test versions.
The app outputs a table, boxplot, and histogram of the data along with the conclusion 
if the groups are significantly difference using each post hoc method given user selected confidence level. 
The data can be separated using commas, semicolons, or tabs, however, a constraint however is 
the data has to be in three groups, each with equal sample size. 
