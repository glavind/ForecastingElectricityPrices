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

### data.csv
The data included is only the publicly available data (TSO fundamentals and EXAA prices), so anyone interested in testing models will have to get EPEX prices from another source (check epexspot.com or transparency.entsoe.eu).
