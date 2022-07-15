# How are COVID deaths correlated with Human Development Index? Whether high or low HDI has linear relationship with the fatality rate?

Motivation:

The COVID-19 pandemic has not only affected global economies in terms of GDP growth, but also on the path of human development. The core foundations of the concept of human development i.e. a healthy and long life has also been affected. Human development is measured using three core indicators which are; life expectancy (at birth,) knowledge (mean years of schooling) and standard of living (Gross National Income per capita.) The Human Development Index is a critique of the ‘economic growth before people’ model of development. In this project we would like to answer the question: How COVID-19 related deaths correlates with the Human Development Index (considering both low and high HDI countries?) This topic peaked our interests as the media has speculated that most developed countries who rank very high on the HDI topped the charts of corona virus deaths worldwide per million population, albeit having a strongest and most efficient healthcare facilities. In contrast, it is believed that developing countries like India and South Africa with middle-low ranked HDI Indices have managed to keep COVID-19 infections and death rates at some of the lowest levels in the world. To check the relevancy of these speculations, we will use the Our World In Data Covid data set to analyse Covid cases against the components of HDI and provide a summary of findings.

Analysis Plan:

To answer our research question, Firstly we perform data retrieval (identifying data sources, defining important variables and relevant formulas for calculating Human Development Index). Secondly, we will perform data processing which includes data transformation, data translation, checking consistency of data, data pre-processing, checking outliers and handling of missing values. Thirdly, we will perform Exploratory Data Analysis, make statistical summaries, visualization of results and correlation plots. Then we will perform a hypothesis testing of HDI vs total deaths in (all countries, both low & high countries, continent wave 1) and make a hypothesis comparison table as well as data normalization. To confirm the hypothesis test result we will perform linear regression for high HDI countries and identify the significant variables which contributes more to covid death.

<img width="974" alt="image" src="https://user-images.githubusercontent.com/63796480/179194561-1f181ec8-86b4-4ab8-9aba-c82a998ba0c6.png">

Conclusion:

From the overall analysis, we identified that there exist a linear relation between HDI and covid death. However, while exploring deep into separate countries with high HDI and low HDI we found that, high HDI countries has no relationship with covid death. Hence we performed a linear relation to identify what are the other factors contributing to death in countries with high HDI. It can be concluded from the linear model that median age variable (aged population) is one of the prominent factor which contributes to total death, which means high median age contributes to high covid deaths.

<img width="704" alt="image" src="https://user-images.githubusercontent.com/63796480/179194834-cfedaf61-607b-4191-8903-860b96b66116.png">
<img width="704" alt="image" src="https://user-images.githubusercontent.com/63796480/179194910-b48745ca-4fc3-4f20-92b0-2d1de4fe9579.png">


Critique:

From general observation, another probable reason why countries with high HDI had high death rates is due to the longer life expectancy as compared to low HDI countries. This means High HDI countries have a large population of elederly people which are more prone to risk of contracting diseases due to weakened immune systems thus the corona virus pandemic took a toll on high HDI countries. We can also notice that when vaccines where being administered, high priority was given to the old age since they were more at risk.
