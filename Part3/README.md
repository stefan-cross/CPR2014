Practical Part 3: Implementing the Vehicles 
============================================

The vehicle implementation is based around a finite state machine design pattern. At this stage we do not concern ourselves with starting all the vehicles as specified in the config, instead creating a few individual ones and implementing the algorithm as per the specification. 

It is worth noting that if a vehicle is started in a location that is not valid then it will still be created but never have the opportunity to change its state or location to participate in the simulation. Also if identical vehicle names are registered an error will be returned which is left just as “badargs” due to time constraints.

It was also found that various formatting functions had to be created to handle the output of the manager and planner functions which was not ideal, where possible the API for the previous modules has been respected but there are some slight variations to avoid having to pattern match on some outputs where a simple empty list would suffice.

It is worth noting that the specification emphasised the priority of older orders and every effort was made to do so, another optimisation that I ran out of time to implement would have been to maximise vehicle capacity by giving the lower capacity vehicles priority on smaller packages, however this would conflict with the specification point about order timing priority. However this would be worth exploring as during tests it appeared that this was a bottleneck with inefficient vehicle loads.

At this point the ETS table structure has been altered to allow for a process to be noted next to an order, as simply updating a record to show it is reserved would not distinguish it from other process reservations. In order to still comply with the original specification API the manager functions were left relatively intact and instead you will find internal formatting functions to add in the additional process identifier to the order records in the manager ETS table. Although not optimal by any stretch it was felt that specification requested strict adherence to the API and so this was followed at detriment to performance.



Compile with:
> c(orchestrate), c(planner), c(manager), c(order), c(vehicle).

Run with:
> orchestrate:init(Number_of_Orders).

Visualise with:
> observer:start().
The following demonstrates the starting of two vehicles, with differing capacities:

Basic testing
-------------

> c(orchestrate), c(planner), c(manager), c(order), c(vehicle), orchestrate:init(100), observer:start().
Config imported. 
Digraph and ETS graph ref created as [{digraph,16404,20501,24598,true}].
Verticies have been created. 
Edges have been created. 
Manager started 
Order sent: 1418499962720441 "Gdynia" "Kraków" 444 
Order sent: 1418499962720543 [67,122,196,153,115,116,111,99,104,111,119,97] "Kraków" 312 
Order sent: 1418499962720639 [66,105,97,197,130,121,115,116,111,107] "Lublin" 478 
Order sent: 1418499962721048 [87,114,111,99,197,130,97,119] "Warszawa" 458 
…
> vehicle:start(van1, "Warszawa").
Vehicle - van1 , at location: "Warszawa" with capacity 221 
{ok,<0.80.0>}
Vehicle  van1 , in transit {"Warszawa","Radom",103}
> Vehicle - van1 , at location: "Radom" with capacity 221 
> ** MANAGER UPDATE ** Package delivered by van1 Ref: 1418499962725719
> Finding work for van1 , going to "Sosnowiec" 
> Vehicle  van1 , in transit {"Radom","Sosnowiec",221}
> Vehicle - van1 , at location: "Sosnowiec" with capacity 1000 
…
> vehicle:start(truck1, "Warszawa").
> {ok,<0.82.0>}
> Vehicle - truck1 , at location: "Warszawa" with capacity 18650 
> Vehicle  truck1 , in transit {"Warszawa",
                              [66,105,97,197,130,121,115,116,111,107],
                              198}
...
> Vehicle - truck1 , at location: [66,105,97,197,130,121,115,116,111,107] with capacity 15289 
> ** MANAGER UPDATE ** Package delivered by truck1 Ref: 1418499962727550
> Vehicle  truck1 , in transit {[66,105,97,197,130,121,115,116,111,107],
                              [71,100,97,197,132,115,107],
                              411}
> ** MANAGER UPDATE ** Package delivered by truck1 Ref: 1418499962727409
> Vehicle - truck1 , at location: [71,100,97,197,132,115,107] with capacity 7822 
> ** MANAGER UPDATE ** Package delivered by truck1 Ref: 1418499962727896
> ** MANAGER UPDATE ** Package delivered by truck1 Ref: 1418499962724367
...

