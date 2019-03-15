# Hexagonalfan

We introduce an R script that generates a range of hexagonal fan designs. Hexagonal fans are triangular grids in which each successive row of triangles is larger than the previous, by some constant multiplier. Such designs can include a range of both (plant) densities and frequencies in a single plot, providing large economies in space and material for studying local interactions such as competition. However, in practice the fan design can be difficult to implement, hence why we have provided this script. 

## Description 

The main function is called hex(), which accepts a concise set of user-determined criteria on spacing and returns the key components necessary for implementation of the hexagonal fan design. The overall perspective of the fan is as a series of spokes, anchored at an origin point, and with individuals positioned at identified distances along the spokes. The output components include the positions of individuals along the two distinct and alternating spoke types (type 1 and type 2), as well as the minimum and maximum plant spacing between neighbouring spokes of the same type. The minimum and maximum plant spacing corresponds to the linear distance between the two individuals closest to the origin, and the two farthest from the origin, on two neighbouring spokes of the same type. This information allows the spokes to be implemented without the use of a compass (i.e. you can use measuring tape to determine the angle between spokes). The spokes as well as plant positions within the fan design can be incorporated into the graphical output, to help the user better visualize how to implement the fan in the field. 

Four input values are required by the hex() function: the minimum spacing, x, between plants in the first whorl of the fan, the increment of increase per whorl, delta, the number of spokes, S, and the number of whorls, N.

**Options**

When the hex() function is called, it automatically outputs a hexagonal fan figure. Other options include:

-*DotPlot=T*: This plots the position of individuals (plants) as dots. 

-*SpokePlot=T*: This plots the spokes (type 1 and type 2).

-*GridLines=F*: This will turn off the grid lines. 

-*Cartesian=T*: This will export XYPlantPositionTable.csv, the Cartesian coordinates of each individual (plant) in the working directory as well as in the Consol. 

## Examples

*hex(x=0.25,delta=1.27,S=5,N=7)* 

The above will create a fan with minimum spacing 0.25 on the first arc/whorl. Each consecutive whorl will increase by a factor of 1.27, there is a total of 7 spokes and 5 whorls. So, what you have produced will look like three hexagons, each larger than the previous, stacked on top of each other. 

*hex(x=0.25,delta=1.27,S=5,N=7,SpokePlot=T,DotPlot=T)*

This will produce the same fan as above, but now the figure will include dots signifying individual (plant) positions, as well as the spokes. 

