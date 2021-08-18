
# Algorithm 4

_Description:_  
Extension of Algorithm 1, accounts for multiple granularities at once. If granularity $g1$ has $g1_{lev}$ and $g2$ has $g2_{lev}$, 
- within each customer, get distribution of each $g1_{lev}$ levels
- distance between two customers wrt $g1$ is the sum of JS distances between $g1_{lev}$ levels for the customers.
- distance between two customers wrt $g2$ is the sum of JS distances between $g2_{lev}$ levels for the customers.
- total distance between two customers is the sum of distances wrt two (or #) granularities considered.



