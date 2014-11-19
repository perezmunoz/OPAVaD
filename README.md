# OPAVaD
### Outil de Prospection, d'Analyse et de Valorisation de la Donnée
----
OPAVaD is the repository of my Work realized during my 6 month internship from June to December 2014 at the **Innovation Lab** of the French leading bank **Crédit Agricole**.

My mission consists in developping a prospecting tool to the merchants affiliated to the Crédit Agricole bank. This tool is based on the data of the credit card transactions (data not provided due to privacy). The specifications are as follows :

* login module to propose merchant to access its personnel account 
* geographical comparison with other merchants in the same category (eg, restaurant) in terms of turnover during a certain period
* visualization of its sales
* profiling of its customers
* prospecting module for determining customers potentially interested in promotional offers

Technically, the prospecting tool is mainly developped in R, using the framework `shiny`. For further information about the Shiny Project developped by RStudio employees, click [here](https://github.com/rstudio/shiny).

====
#### New version : OPAVaD-Grid
----
I developped a second version of the OPAVaD project based on an implementation of the Gridster JavaScript library, the [ShinyDash](https://github.com/trestletech/ShinyDash) project. The aim is to offer to the merchant an interactive, easy to read dashbord. It is a new implementation of the user interface. The new project is entitled OPAVaD-Grid and you can find it [here](https://github.com/thidiff/OPAVaD-Grid).
