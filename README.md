# Marine Ships

## Brief Description 

### Introduction

In this folder, you can find two versions of the application. One was done with bootstrap and the other one with the `shiny.semantic` package. After some time developing the app with bootstrap, I realized that the requirement was to use the `shiny.semantic` package. Hence, the two versions.

The difference between the two versions is that I included a `plotly` plot in the bootstrap version. I was trying out the `shiny.router` package for the `semantic` version and would have liked to include the `plotly` plot in that version as well but was not able to make it work, during the required time frame, combining two UI modules in the `router()` function.

The application consists of two/three modules:

- select_inputs.R file
- map.R file 
- plot.R file (only in bootstrap version)

### Data Cleaning

I used the `geosphere` package and the `distHaversine()` function to calculate the distance between two lat and lon data pairs. The calculations were done before, not within the application, and the results were saved in a column, `distance_traveled`. 

I noticed that for the same `shipnames` and `ship_types`, the `ship_id` was sometimes different. Hence, I grouped the data by `shipnames`, `ship_types`, and `ship_id`, then arranged by `datetime` and calculated the distance between observations. I discovered that there are three ship names that only have one observation. I excluded these for the distance calculations and appended the three observations after the calculations were finished. In the map module, these three exceptions are handeled separately when plotting the leaflet map.

I saved the file as a `feather` file due to the large amount of observations. A better alternative would be to connect the application to a database and read in the data as needed. 

### Module 1 (`select_input.R`)

In this module, the user can select the ship type and ship name. When the user selects a ship type, the ship names are getting updated with `updateSelectInput`. I included an action button to submit the request for the longest distance traveled between two consecutive observations for the specified ship type and ship name. The module returns the filtered df and an action button.

### Module 2 (`map.R`)

In this module, I included three cards with information about the distance traveled, time elapsed, and total observations. In the semantic version, I used code form the Appsilon FIFA application to create the cards. Link to [repo](https://github.com/Appsilon/shiny.semantic-hackathon-2020). 

I also included a leaflet map and a `DT` data table. For the data table, I used a proxy to replace existing data instead of re-rendering the entire table every time. 

This modules uses the filtered data frame from module one and also the action button from module one. 

### Module 3 (`plots.R`) (only bootstrap version)

In this module, I re-used the `select_input.R` module and also created a time series plot with distance on the y axis that shows how far the ship traveled each observation. Below that, I included a DT data table with all observations. 

### Links to Application

- (Semantic)[https://pascalschmidt.shinyapps.io/shiny_semantic_marine/]
- (Bootstrap)[https://pascalschmidt.shinyapps.io/bootstrap_marine/]






