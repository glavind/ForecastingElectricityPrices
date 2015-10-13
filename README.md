# Master Thesis

### thesis.pdf
My master thesis - "Evaluating Forecasts of Volatile Electricity Prices made by Competing Models". The main goals of the thesis are 

1. provide an introduction to the german electricity market with a focus on how fundamentals affect prices through supply/demand effects. 
2. give an overview of available data and data sources.
3. compare different machine learning and statistical methods for forecasting electricity prices.

### jonsson.R
Contains the code that implements the Jonsson et al. (2013) local linear approximation model for forecasting the epex spot day-ahead price. The code is completely raw, so it requires some understanding and tweaking. 

1. Optimize the model in the train period using modified BFGS to obtain values for the 3 tuning parameters (or skip this step and just use the values provided, which should be resonable if data is similiar). 
2. Run the model for train+test period with optimized parameters to get forecasts.

### tso-data.csv
The data included are only the publicly available data (TSO forecasts of fundamentals and EXAA prices), so anyone interested in testing electricity price forecasting models will have to get EPEX prices from another source (check <a href="www.epexspot.com">EPEX SPOT</a> or <a href="www.transparency.entsoe.eu">ENTSOE</a>).

The data covers forecast wind, forecast solar, and forecast consumption taken from the TSO's, and realized prices for the Austrian exchange EXAA. All for the period 01-11-2012 to 31-12-2014. For a list of sources and a description of the preprocessing applied see thesis.pdf. For newer data check ENTSOE and EXAA.

If you have proprietary forecast data, good for you. You will likely get marginally worse forecast performance as the TSO's have pretty good forecast aggregation schemes (to the best of my knowledge). And remember, when forecasting DA prices you are not interested in how precise your forecast is compared to the realized fundamentals. You want to forecast the market players expectations of fundamentals (which are unknown).
