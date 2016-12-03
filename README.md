# occupancy
Supervised Classification for Office Room Occupancy

Implementation: R
Required packages: ggplot2, caret, e1071
Predictors:date(year-month-day hour:minute:second), temperature(celsius), humidity(%),light(Lux), carbon dioxide(ppm), humidity ratio- derived quantity from temperature and relative humidity(kgwater-vapor/kg-air)
Response:Occupancy(0 if the room is not occupied and 1 otherwise)
