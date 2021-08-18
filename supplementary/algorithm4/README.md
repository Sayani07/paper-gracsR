
# Algorithm 4

_Description:_  
Extension of Algorithm 1, but also accounts for consecutive differences of categories. If granularity $g$ has $g_{lev}$, it considers the $g_{lev -1}$ consecutive pairwise categories as variables. So we have a data frame with customers as rows, consecutive pairwise categories as variables and the JS distance computed on them for each customer as the cell value. Distances between the customers are computed by considering euclidean distances between the variables. 

The following table is an illustration of the distance metric for hour-of-day with levels $(0, 1, 2, \dots, 23)$. Here, $dd_3 (1,2)$ stands for the distributional difference (JS or Hellinger) between 1st and 2nd level of hour of the day for the third customer.

| customer-id 	|    (0,1)   	|   (1, 2)   	| ... 	| ... 	| ... 	|    (22, 23)   	|
|:-----------:	|:----------:	|:----------:	|:---:	|:---:	|:---:	|:-------------:	|
|      1      	| $dd_1 (0,1)$ 	| $dd_1 (1,2)$ 	|     	|     	|     	| $dd_1 (22, 23)$ 	|
|      2      	| $dd_2 (0,1)$ 	| $dd_2 (1,2)$ 	|     	|     	|     	| $dd_2 (22, 23)$ 	|
|      3      	| $dd_3 (0,1)$ 	| $dd_3 (1,2)$	|     	|     	|     	| $dd_3 (22, 23)$ 	|
|     ...     	|     ...    	|     ...    	|     	|     	|     	|               	|
|      n      	| $dd_n (0,1)$ 	| $dd_n (1,2)$ 	|     	|     	|     	| $dd_n (22, 23)$ 	|




