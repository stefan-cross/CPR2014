Practical Part 4: Implementing Supervisors
==========================================

This final part of the implementation is in two parts, first a vehicle supervisor which will handle the potential issues of vehicle nodes and the second part deals with higher level issues, dealing with the manager, planner and orchestration.  

Compile everything:
>  c(manager), c(planner), c(vehicle), c(orchestration) ,c(vehicle_sup), c(top_sup), observer:start(), orchestration:start_simulation(10000).
    
Kick everything off:
> orchestration:start_simulation().

For testing the vehicle_sup: 
> exit(whereis(van1), kill).

Part 4.1 Vehicle supervisor
---------------------------

The vehicle supervisor traps errors from the child vehicle processes and return their contents to the main manger ETS table, eligible for circulation again, and then restart the vehicle in the last confirmed location. Testing proves that the correct number of parcels remain in circulation irrelevant of how many time one might try and sabotage vehicles. Alterations to note here is that we have split out the ETS tables across the individual vehicle processes to try and reduce the load on just one ETS table. An important change to accommodate this was to make the vehicle table heirs of the manager, that way, in the event of an error the table is not destroyed. Disk Erlang Term Storage (DETS), a more persistent version of ETS was considered, however I opted for this change of heir to circumvent the issue of data loss on a process termination.  We have also fully automated the creation of all the vehicles from the configuration file. To add more, we can simply add them to the file.config.csv. 

The following demonstrates the vehicle_sup being able to handle the failure of a vehicle node, by manually intervening and calling the exit/2 command in the shell we can see the vehicle error message propagating to the vehicle_sup and then the vehicle being brought back into circulation at its last known location.  Even with relatively high throughput we can see on the termination of vehicles in transit and they are brought back to start from the last confirmed location, rather than where they are destined for.

    1> c(vehicle), c(manager), c(planner), c(orchestration) ,c(vehicle_sup), c(top_sup), observer:start(), orchestration:start_simulation(10000).
    Config imported. 
    Digraph and ETS graph ref created as [{digraph,16410,20507,24604,true}].
    Verticies have been created. 
    Edges have been created. 
    Manager started 
    All vans registered. 
    All trucks registered. 
    Vehicle - van1 , at location: "Kraków" with capacity 767 
    ...
    Vehicle - van10 , at location: "Katowice" with capacity 22 
    
    Vehicle - van1 , at location: "Kraków" with capacity 87 
    Vehicle  van1 , in transit {"Kraków","Lublin",306}
    ...
    
    
    2> exit(whereis(van1), kill).
    Vehicle Supervisor - ERROR: <0.279.0>, killed 
    true
    Vehicle - van1 , at location: "Kraków" with capacity 40 
    Vehicle  van1 , in transit {"Kraków","Radom",189}
    ...
    Vehicle  truck1 , in transit {[66,105,97,322,121,115,116,111,107],
                                  "Lublin",259}
    
    
    3> exit(whereis(truck1), kill).
    Vehicle Supervisor - ERROR: <0.283.0>, killed 
    true
    Vehicle - truck1 , at location: [66,105,97,322,121,115,116,111,107] with capacity 77 
    ...
    Vehicle - truck2 , at location: [66,105,97,322,121,115,116,111,107] with capacity 7022 
    ** MANAGER UPDATE ** Package delivered by truck2 Ref: 1418517138834672
    Vehicle  truck2 , in transit {[66,105,97,322,121,115,116,111,107],
                                  [84,111,114,117,324],
                                  357}
    
    
    4> exit(whereis(truck2), kill).
    Vehicle Supervisor - ERROR: <0.284.0>, killed 
    true
    Vehicle - truck2 , at location: [66,105,97,322,121,115,116,111,107] with capacity 18882 
    Vehicle  truck2 , in transit {[66,105,97,322,121,115,116,111,107],
                                  [84,111,114,117,324],
                                  357}


4.2 Top level supervisor
------------------------

This final part is the implementation of a top level supervisor. Open Telecommunication Protocol (OTP) modules are utilsed to good effect. (OTP is considered to be three main components, a set of modules, design patterns and principles and Erlang itself.) In the event that one of the major components such as the planner, manager of vehicle supervisor failing, then this supervisor can act accordingly. It would have been ideal to strategise the significance of each component and deal with different combination in different means, alas time escaped me and the relatively primitive strategy was implemented. 

In the event that the vehicles supervisor goes down, then they are all restarted again in a one to many fashion, should the planner go down then this is also started again in a one to one fashion. In the event that the manager terminates then we have lost all the orders, and therefore we will have to start all over again anyway.


    1>  c(manager), c(planner), c(vehicle), c(orchestration) ,c(vehicle_sup), c(top_sup),
    {ok,top_sup}
    2> top_sup:start_link().
    Manager started
    {ok,<0.75.0>}
    3> whereis(manager).
    <0.77.0>
    4> whereis(vehicle_sup).
    <0.76.0>
    5> whereis(planner).
    <0.78.0>
    6> exit(whereis(planner), kill).
    true
    7> whereis(planner).
    <0.83.0>
    8> exit(whereis(manager), kill).
    Manager started 
    true
    9> exit(whereis(manager), kill).
    Manager started 