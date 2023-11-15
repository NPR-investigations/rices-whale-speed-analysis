 Read the story [here]()

## Code

The R scripts and notebooks that were used to run the analys can be found [here]()

## Methodology
NPR downloaded Automatic Identification Systems (AIS) ship transponder data from MarineCadastre.gov, a cooperative effort between the Bureau of Ocean Energy Management and the National Oceanic and Atmospheric Administration (NOAA). This AIS data is collected by approximately 200 land-based receiving stations from the U.S. Coast Guard's Nationwide Automatic Identification System. As the range of land receivers is usually limited to 40-50 miles off the coast, the data was combined with satellite-collected AIS data from Spire and processed by Global Fishing Watch to provide more comprehensive coverage of the Gulf of Mexico.

In NPR's analysis of transits through the Rice's whales' proposed critical habitat, a transit was defined as an instance when a ship entered or exited the area. A new transit was also started if there was more than a day's gap between two AIS broadcast points.

The speed of a transit through the critical habitat was calculated using a distance-weighted average speed, as detailed in a vessel speed rule assessment published by NOAA. This method corrects for variations in AIS transmission and reception rates, which can be influenced by the speed and type of the vessel.

Ships measuring less than 65 feet in length as well as law enforcement, search and rescue, and military vessels, were excluded from this analysis.