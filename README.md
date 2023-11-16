 Read the story [here](https://www.npr.org/2023/11/16/1212690111/only-51-of-these-u-s-whales-remain-little-has-been-done-to-prevent-their-extinct)

## Code

The R scripts and notebooks that were used to run the analysis can be found [here](https://github.com/NPR-investigations/rices-whale-speed-analysis/tree/main/analysis)

## Methodology
NPR downloaded [Automatic Identification Systems (AIS) ship transponder data](https://coast.noaa.gov/htdata/CMSP/AISDataHandler/2022/index.html) from [MarineCadastre.gov](MarineCadastre.gov), a cooperative effort between the Bureau of Ocean Energy Management and the National Oceanic and Atmospheric Administration (NOAA). This AIS data is collected by approximately 200 land-based receiving stations from the U.S. Coast Guard's Nationwide Automatic Identification System. As the range of land receivers is usually limited to 40-50 miles off the coast, the data was combined with satellite-collected AIS data from Spire and processed by [Global Fishing Watch](https://globalfishingwatch.org/) to provide more comprehensive coverage of the Gulf of Mexico.

In NPR's analysis of transits through the [proposed critical habitat of the Rice's whales](https://noaa.maps.arcgis.com/home/item.html?id=f85d90527131406489ce721b2b71960b), a transit was defined as an instance when a ship entered or exited the area. A new transit was also started if there was more than a day's gap between two AIS broadcast points.

The speed of a transit through the critical habitat was calculated using a distance-weighted average speed, as detailed in a [vessel speed rule assessment published by NOAA](https://media.fisheries.noaa.gov/2021-01/FINAL_NARW_Vessel_Speed_Rule_Report_Jun_2020.pdf?null). This method corrects for variations in AIS transmission and reception rates, which can be influenced by the speed and type of the vessel.

Ships measuring less than 65 feet in length as well as law enforcement, search and rescue, and military vessels, were excluded from this analysis.
