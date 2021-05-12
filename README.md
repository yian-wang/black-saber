# Bias in Black Saber's Hiring and Promotions
A project adapted from a Data Analysis course at the University of Toronto (STA303/1002)
#### Project Status: Active
#### Contributors: 
* [Claire Hsiung](https://github.com/claire-hsiung)
* [Yian Wang](https://github.com/yian-wang)

## Project Introduction
This is a data analysis project investigating any potential biases in a hypothetical company's hiring and promotion processes. 

Deliverable: Report for Black Saber's Board of Directors on gender disparity in hiring, wages, and promotion.
[Skip to Findings](https://github.com/yian-wang/black-saber/blob/main/README.md#findings-summary)

### Needs of this project
- Data Exploration/Descriptive Statistics
- Data Processing/Wrangling and Cleaning
- Statistical Modelling
- Reporting
- Ethical Consideration

### Technologies
* R (ggplot, tidyverse (dplyr))

## Project Description
*Some context: this is a hypothetical project in which the data is based from real company data. We are assuming the position of consultants and analysts for a hypothetical consultancy called The Hive. We have been contracted by hypothetical Chief People Officer Gideon Blake of a hypothetical company called Black Saber Software. See the emails attached below for more background on the needs of this project.*

<details>
<summary>Gideon's emails</summary>
<img src="https://github.com/yian-wang/black-saber/blob/main/images/email-1.png?raw=true" width="50%">
<img src="https://github.com/yian-wang/black-saber/blob/main/images/email-2.png?raw=true" width="50%">
</details>

A critical area of concern in today’s workplace is gender bias, and Black Saber Software’s culture is no exception. It is critical that a company shows that their workplace practices are not only unbiased, but also that they embrace diversity. As a result, we have been hired as an external, third-party consultancy to review Black Saber Software’s hiring and promotion processes, as well as their employee salaries in order to determine whether or not the company is biased in their practices.

### Research Questions
* Does the hiring algorithm favour a certain gender? Do the humans conducting the interviews have bias towards a certain gender?
* Does Black Saber as a company favour a certain gender when promoting employees? 
* Does Black Saber pay a certain gender more?


## Findings Summary

### Hiring
* The AI algorithm did not prefer any specific gender. That is, it chose who should move forward in the recruitment process fairly.
* More women than men initially applied to Black Saber, but only 2 of the 10 final hires were women. Black Saber did not hire anyone who identified as “prefer not to say” in 2020.
* The interviewers favoured men slightly more than women.

### Promotions 
While accounting for productivity and role seniority:
* Women were about 40% less likely to be promoted than men.
* There is significant evidence that gender affects whether an employee is expected to be promoted.
* There are no significant conclusions to be drawn about the group “Prefer not to say” most likely due to the small sample size of this group.

### Salary
While accounting for the variables gender, team, productivity, leadership and role seniority:
* Women received a salary of approximately $2248.20 less than men.
* Those in the “Prefer not to say” group received a salary of approximately $1104.10 less
than men.
* There is significant evidence that gender affects an employee’s salary

## Limitations
* We did not have enough data to determine whether the biases we found were necessarily due to a biased culture at Black Saber, or if it is due to the systemic barriers women (and those who prefer not to specify) face in the professional world.
* Because only 10 people were hired, it is diffcult to determine whether or not the findings in the final hires was statistically significant.
* The number of applicants and employees in the “Prefer not to say” group was too small to make meaningful conclusions.
* The data does not account for those who may reject promotions for personal reasons which could inflate the amount of non-promotions we see. This in turn indirectly impacts salary expectations as well.

## Code of Ethical Conduct
Not only is The Hive passionate about making actionable insights, but we also practice ethical statistics. Our main values lie in, but are not limited to:
* Confidentiality of client information and respecting their rights
* Using appropriate methods and interpreting them correctly and completely
* Reporting results impartially even if they may pose harm to the parties involved, so as to encourage action against our insights so as to not fall into trouble in the future
* Only using methods we have sufficient knowledge in to use in order to prevent any misinterpretation and summary of the data
* Only using data that is provided to us directly by Black Saber Software, and not to scrape other data that we do not have permission to access
* Declaring our relationship with the clients (ie. financial or other interests) to maintain transparency regarding the influence it may have on the outcomes

[Read Full Report](https://github.com/yian-wang/black-saber/files/6468678/black-saber-final.pdf)

