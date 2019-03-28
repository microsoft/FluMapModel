# dbViewR

dbViewR is an R package that queries the Seattle Flu Study de-identified research database and caches various views of the data for downstream modeling and delivery to the api_service for web visualization.  

This version pulls from seattleflu/simulated-data to test workflows.  Must rewrite to hook into real database. 

I suspect this logic should eventually move over to the database itself, but it makes sense to keep it adjacent to model development while we're still figuring out what views we want. 



 