# CompilerLab
Simple solver for the searching of area of curvilinear triangle. To describe the task the following format is used:
1. In the first line are written the bounds of segment, in which vertices of the triangle should be searched.
2. In following lines are written curves, which constrain the triangle, in reverse polish notation.
Examples can be found in examples/ directory, samples of generated assembly code also are situated there.

To build:
````
make
````
Executables can be found in directory out/ or run with command:
````
make run
````
Test can be performed with the following command:
````
make tests
````
