## Forecasting Liquidity with Indicators of Financial Stability

This project aims to forecast bank funding liquidity as a proxy for financial system stability using a set of macroeconomic and financial indicators collected from the previous financial stability literature. Accurate forecasts of liquidity not only inform macroprudential and monetary policy, but also elucidate the economic variables that exert the largest effect on overall financial stability, providing a clear policy-based motivation for this analysis. Measuring liquidity via the TED spread in the United States, I utilize lasso regression to fit a forecast model of the spread based on its one-quarter lagged value and a high-dimensional vector of macro and financial indicators, producing an out-of-sample error about 15 percent lower than a baseline AR(1) model of the spread. In parallel, I perform natural language processing on the minutes of the Federal Open Market Committee to generate a Financial Stability Sentiment score meant to gauge policymaker opinions of market conditions and provide an alternative quantifiable measure of overall financial stability. Though modeling sentiment is difficult given the low signal-to-noise ratio of the information content in FOMC minutes, lasso models of the sentiment score produce a more modest 5 percent forecast improvement over a baseline AR(1) model. Fitting the liquidity model to a sparser dataset of economic indicators for the United Kingdom's economy, I again find a broad relationship between liquidity and financial stability, though the sparser set of indicators do not meaningfully improve forecast performance. These results contribute to the financial stability literature both by highlighting some of the short-run macroeconomic and financial determinants of liquidity, and by demonstrating the utility of machine learning techniques in forecasting within data-rich policy settings.

Here you'll find appendices describing data collection for the Financial Stability Indicators (`data_appendix.xlsx`), data sources (the "final datasets" folder), and code used to run the models and generate results (`figures0.r`); as well as the text of the FOMC minutes used to generate the Financial Stability Sentiment scores (`minutes1` and `minutes2`), and scripts used to parse the minutes and generate the scores (`FSS`). Feel free to reach out to eli.metzner28@gmail.com with any questions!
