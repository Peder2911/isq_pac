
docker build  -t pac .
docker run --rm pac\
   -v $(pwd)/plots:/replication/plots
   -v $(pwd)/maps:/replication/maps
   -v $(pwd)/tables:/replication/tables
