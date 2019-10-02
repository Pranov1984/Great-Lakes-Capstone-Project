# Great-Lakes-Capstone-Project
Reduction in Customer Complaints – Mortgage Servicing Industry

The presentation for the result can be found in my slideshare account:
https://www.slideshare.net/pranovmishra/reduction-in-customer-complaints-mortgage-industry-136825640


Project Overview: 
The project aims at analysis of Customer Complaints/Inquiries received by a US based mortgage (loan)
servicing company . The scope of the project is limited to complaints received with respect to the part of the servicing
life cycle that is related to Escrow Analysis and other related or subsidiary activities. 

Goal Statement
Identification of major contributors towards complaints/inquiries. Utilization of the identified significant
contributors and coming up with recommendations for changes/new implementations with the below goals 
Reducing  Re-work
Reducing Operational Cost
Improve Customer Satisfaction
Improve company preparedness to respond to customers

Data Considered
A few months of data of standard servicing loans of the organisation which comprises of circa 154,000
records was used for data exploration, visualization and hypothesis generation. The data had a lot of missing
values and the missing values were typically when the event corresponding to the variable concerned) did not 
occur for that observation. The data was cleaned by creating dummy variables with no missing values. 
The dataset provided constitutes the cleaned data.
Escalations typically lead to extra work, reputational damage, sometimes regulatory scrutiny and penalties. Preventing customer complaints and escalations is in the best interest of the company.

The data was highly imbalanced (Majority class: Minority Class = 96%:4%) and hence appropriate model evaluation metric was required to be chosen. A combination of Harmonic mean (F1 score) and Area Under the Curve (AUC) was used to finalize the best model.

Models tried to arrive at the best are 
Simple Model like Logistic Regression with different thresholds for classification
Random Forest after balancing the dataset using Synthetic Minority Oversampling Technique (SMOTE)
Stochastic Gradient Boosting technique after balancing the dataset as was the case with random forest

The key insights derived from the model with best results, indicate that the variables that significantly impact customer behaviour can  be broadly classified as below:
Waiver of escrow payments which could arise due to incorrect escrow analysis conducted resulting in customer requesting for waiver of extra charges levied.
Presence of “Initials” which comes into play when a customer is escrowed for the first time. So the customer could be escalating either because he is incorrectly escrowed or the initial payment calculation for the escrow services is incorrect.
Process of handling force escrowed loans have been inadequate leading to customer queries and complaints.

A Gains chart was prepared which gave a cumulative lift of 133% in the first 4 deciles. The customers  with highest probability of making a complaint were identified. The company could use this information to proactively review the operations performed on the customer’s account and correct any errors if found.

