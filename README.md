# smoke-infections
Analyzing exposure to smoke and risk of infections

We will be analyzing the spatiotemporal risk of exposure to smoke and infections.
We hypothesize that exposure to smoke and incidence of wildfires may increase the risk of fungal infections such as aspergillosis, mucormycosis or cryptococcosis.

We will use COVID 2020 data as an example of how this analysis might be done. 

For the draft submission:
I am restricting my analysis to California because looking at the entire country was too large a dataset and caused R to freeze or take a really long time to process. 

1) I was initially using a repository in the UMN Github but because I am going to be adding large files and may want to simulate something, I switched over to github.com. So I created a submodule from the UMN github. Thus my work is all in the submodule that you can follow from my main repository.
2) In the data-cleaning file I was trying to create animations that would follow both covid and smoke exposure over time. But I have not gotten this code to work yet.
3) In the shiny-script.R file, I have created a Shiny app just for visualization. This allows you to select cases or deaths and a month of the year. I will plan to add more data to this and a hoveron feature to see more data when hovering over each county in California. 
