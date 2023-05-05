Project name: Spatiotemporal Assocations of COVID-19 and Smoke Exposure in California

Project purpose: the purpose of the product is to explore the spatial assocations of COVID-19 and smoke in California. I am working on another project exploring fungal infections and smoke exposure. However, I still do not have access to the clinical data for fungal infections. This project serves as a model for how I can model the fungal infections when the data is available. 

Availability: The product can be accessed on my publicly available github. The html document that describes the data is avialable in the github as well as at the independt link: file:///Users/thomasmchale/Desktop/Wildfire-fungi-project/repository/smoke-infections/final-project_McHale.html.
There is also a shiny app associated with the project that is available at the link: https://mchal053.shinyapps.io/smoke-covid/


Product features: The main product is a document that describes that spatial trends of COVID-19, smoke exposure as well as population and demographic data in California. There is also an interactive shiny app that allows users to explore the temporal trends of COVID-19 and smoke exposure in California in 2020. 

Programming challenges: The first challenge was the Shiny app that allows for interactivity in exploring the temporal trends. The app took a long time to get functional as well as to make it publicly available. There were issues with the working directory on my local machine and being read when trying to render the app. 
The next challenge was the statistical analysis. In this document I include the first step in performing a spatial statistical analysis. This consists of semivariograms to determine if spatial aucorrelation exists in the data. I found that indeed both COVID-19 and smoke exposure were spatially autocorrelated in California for the year 2020.
The next step will be to perform universal krigeing. I did try extensively to achieve this, but I had difficulty converting the simple feature to a space-time data frame that can account for both the variable and the change over time. 
Ultimately, I plan to create a universal krige model that can explore the degree of correlation between COVID-19 and smoke exposure as well as account for time and the demographic varialbes displayed here. 

Division of labor: Thomas McHale is responsible for all features in this project.

Future work: See above, I hope to finish the statistical analysis. In addition, I hope to apply this to fungal infections.


Not for submission, but I have continued work on the project, if you are interested: https://majestic-syrniki-62ba33.netlify.app
