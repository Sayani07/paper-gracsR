
#  Supplementary  
<!-- badges: start -->
<!-- badges: end -->

_*contains the description of all tested algorithms for distances and pros and cons of each*_
<!-- badges: start -->
<!-- badges: end -->

### Algorithm 1  
_Description:_  
Choose one granularity and take the distribution for each category. Distance between customers is taken as the sum of JS distances between distributions of these categories.  
_Pro:_  
- distance metric makes sense to group different shapes together  
- simulation results look great on typical designs  
_Cons:_  
- Can only take one granularity at once  
- Clustering a big blob of points together whereas the aim is to groups these big blob into smaller ones  

### Algorithm 2  
_Description:_  
Choose all significant granularities and compute wpd for all these granularities for all customers. Distance between customers is taken as the euclidean distances between them with the granularities being the variables and wpd being the value under each variable for which Euclidean distance needs to be measured.  
_Pro:_  
- Can only take many granularities at once
- can apply variable selection PCP and other interesting clustering techniques
- simulation results look great on typical designs
- splitting the data into similar sized groups  
_Cons:_  
- distance metric does not make sense to split the data into similar shaped clusters 


### Algorithm 3  
_Description:_  
Extension of Algorithm 1, where instead of one granularity we take all significant granularities at once. This is done by summing the sum of JS distances for each granularity as a measure for the distance between different customers.