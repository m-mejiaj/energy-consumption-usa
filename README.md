# Predicting electric energy consumption in the USA


An energy company wants to develop a model to predict annual electric energy consumption (in dollars) by households in the United States. Energy consumption change depending on a bundle of attributes, such as housing unit attributes (e.g. BEDROOMS, TOTSQFT_EN, TYPEHUQ, SWIMPOOL), location (e.g. REGIONC, BA_climate), demographics (e.g. INCOME, NHSLDMEM) and consumption habits (e.g. CWASHER,  DISHWASH, ELCOOL, ELWARM, ELWATER, TVCOLOR). Therefore, for a given household defined on a set of attributes, there is a (conditional) distribution of energy consumption. The management is interested in a prediction interval, defining this range as the difference between the 97.5th and 2.5th quantile of the conditional distribution of interest.
