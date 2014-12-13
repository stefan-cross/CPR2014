

Compile everything:
> c(vehicle), c(testing), c(manager), c(planner), c(client), c(orchestration), c(dispatcher) ,c(vehicle_sup), c(top_sup), observer:start(), orchestration:start_simulation(10000).
    
    
Kick everything off:
> orchestration:start_simulation().


Produce Docs
> edoc:files(["planner.erl"]).

For testing the vehicle_sup: 
> exit(whereis(van1), kill).