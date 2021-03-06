# Shiny-lmfor
The purpose on this tool is to provide an easy application for estimating relationship between tree breast height diameter (DBH) and top height with the help of ‘lmfor’ R-package. 
This package provides 20 different tree height models.


#### Tree DBH-Height Modelling Tool Technical guide.
https://drive.google.com/file/d/1f9FvW-F8bVL_Fx9jbvVK6Luumj_Ff4Tg/view

#### The use of the tools requires that there are some sample plot data available for the estimation. Each sample plot should preferably contain at least one height sample tree.  
![image](https://user-images.githubusercontent.com/37068938/113997441-70af1b00-9858-11eb-9f3a-794ad0c3b80b.png)

#### The input data should be in a CSV format file, and it should contain the following fields: (cluster,) plot, tree_dbh,  tree_height. Field ‘cluster’ is optional if the sample plots are grouped into clusters. 
![image](https://user-images.githubusercontent.com/37068938/113997735-b835a700-9858-11eb-9754-ab755f53f47a.png)

#### After uploading the data, the program shows a DBH-Height scatterplot. In this chart a small hover box shows tree_dbh, tree_height and plot_id (which is the combination of “cluster” and “plot” codes). 
![image](https://user-images.githubusercontent.com/37068938/114035477-6bb19200-987f-11eb-9c51-4d3dbd4f51c7.png)

