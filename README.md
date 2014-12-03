
CPR coursework Oct 2014
=======================


**At a high level the coursework has themes of a Courier Delivery Problem, 
and simulation.**

Parallel and distributed discrete-event simulation (PDES) in the Erlang language.

Quick start commands:
--------------------

Compile everything:

> c(vehicle), c(testing), c(manager), c(planner), c(client), c(orchestration), c(dispatcher) ,c(vehicle_sup).
    
    
Kick everything off:

> orchestration:start_simulation().


playing with single vehicle pid data
> ets:insert(van1, {1416605936537825,reserved,"Kraków",[71,100,97,197,132,115,107],80}).
> orchestrate(van1, "Kraków").

- Input data is structured, can we use meta programming to change behaviour or params depending on the config?

- Van and Truck location data is likely to change and to actually be states
later in the exercises, Finite State Machine pattern

- Maybe use timers to simulate distances

- Allow for changing of algorithms on dispatch?

Ets for Towns and Distances

http://www.erlang.org/doc/man/ets.html#match-2
> ets:match(towns, '$1').
> ets:match(distances, '$1').


Q3.1 test data: 

A list of destination cities
> ["Lublin", "Warszawa", "Lublin", "Gdynia"]

Starting city and destination list
> {"Radom" , ["Lublin", "Warszawa", "Lublin", "Gdynia"]}



Notes on the journey planner algorithm, an alphabetical approach:
----------------------------------------------------------------
    1. Reorder list to group possible duplicates A = ets:usort(ToList)
    2. Check routes between entries in reordered list, From A1
     2.1. Take starting city and list head
     2.2. Search destination for match of start and head
      2.3. if no exact match, find match on start, run matches on corresponding city
             - find matches with least total distance cost and create temp list of these for later optimisation
             - select least distance from temp list, use as a candidate for path
            recursive match on the corresponding city,  
     3. Repeat process with head and tail, may need to repeat 2.3 to find route between indirect cities
    
    Notes on the journey planner algorithm, an optimal approach:
    1. Reorder list to group possible duplicates
    2. Check routes between entries in reordered list
     2.1. Take starting city and list head
     2.2. Search destination for match of start and head
      2.3. if no exact match, find match on start, run matches on corresponding city
            recursive match on the corresponding city, find matches with least total distance cost
     3. Repeat process with head and tail, may need to repeat 2.3 to find route between indirect cities
          
Consult the following for proven approach to shortest path algorithm:
http://en.wikipedia.org/wiki/Shortest_path_problem

Some research:
http://www-bcf.usc.edu/~maged/publications/solvingCDP.pdf

Visualising the locations:
https://www.google.com/maps/d/edit?mid=zZqQW5ER_nEU.k0nVn_MpDioI

https://www.google.com/fusiontables/data?docid=1Y4fldAT2TxjyZx61wH8KrcMBuboM7ZQNh3MSxRwQ#rows:id=1



Useful Syntax
-------------

List to String:

    io:format("~s~n", [[83, 97, 109, 112, 108, 101]]).
    
    
    14> ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', "Warszawa"}], ['$$']}]).
      > ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', "Szczecin"}], ['$$']}]).
    [["Warszawa","Radom",103],
     ["Warszawa",[321,243,100,378],130],
     ["Warszawa",[84,111,114,117,324],208],
     ["Warszawa","Lublin",170],
     ["Warszawa",[66,105,97,322,121,115,116,111,107],198]]
    15> ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', "Warszawa"}, {'==', '$2', "Radom"}], ['$$']}]).
    [["Warszawa","Radom",103]]
    
    
     ets:select(distances, [{{'$1','$2','$3'},[{'==','$1',"Szczecin"},['$$']]}])
     
 
Digraphs
--------
 
     420> Graph = digraph:new().   
     {digraph,307222,311319,315416,true}
     421> ets:new(graph, [set, named_table]).                            
     graph
     422> ets:insert(graph, Graph).          
     true
     423> ets:match(graph, '$1').                                        
     [[{digraph,307222,311319,315416,true}]]
     424> X = ets:lookup(graph, digraph).
    [{digraph,16400,20497,24594,true}]
     425> lists:nth(1, X).
    {digraph,16400,20497,24594,true}
    
    lists:nth(1, ets:lookup(graph, digraph))
    
    40> digraph:vertices(lists:nth(1, ets:lookup(graph, digraph))).
    41> digraph:edges(lists:nth(1, ets:lookup(graph, digraph))).  




Notes on distribution of Depots:
-------------------------------

For some basic testing it has come to light that the initial distribution
of depots is not optimal;

    140> manager:cargo("Gdynia").
    {ok,[321,243,100,378]}
    141> manager:cargo("Bydgoszcz").
    {ok,[321,243,100,378]}
    142> manager:cargo("Poznań").   
    {ok,[321,243,100,378]}
    143> manager:cargo("Katowice").
    {ok,"Kraków"}
    144> manager:cargo("Toruń").   
    {ok,[321,243,100,378]}
    145> manager:cargo("Radom").
    {ok,[321,243,100,378]}
    146> manager:cargo("Sosnowiec").
    {ok,[321,243,100,378]}
    147> manager:cargo("Lublin").   
    {ok,"Kraków"}
    148> 
    148> manager:cargo("Wrocław").
    {ok,[321,243,100,378]}
    149> manager:cargo("Białystok").
    {ok,"Warszawa"}

When cross referenced with a visual aid (see docs/maps/) one can see easily that 
the deployment of a depot in Ludz is only going to be utilised by a few cities,
Lublin and Katowice. It would be good to test the hypothesis that 'depots 
are less efficient when placed on the edge of a network' or 'optimal depots
are not on network perimeters'



Notes on Design Decisions:
--------------------------

It is implied on the implementation of manager:lookup that the from
data remains the same and the location is updated through the journey. 

manager:lookup(Ref) -> {error, instance} |
         {ok,{Ref,From,To,Kg,Loc|VehiclePid,OwnerPid}}.
         
I have instead opted for a state field to save on IO operations and the updating every record
as it passes through a destination. State can abstract away from this and
still give an adequate level of detail/insight as to the whereabouts of an order.

I am also assuming that vehicles are best represented as individual processes
with finite a state. This is referred to as VehiclePid in the specification.

The specification also makes reference to OwnerPid, it is assumed that this
is a client that initiates the simulations, the client that sends in orders for example...

On implementing the vehicle processes:

As of this commit I am now looking to move the manager est tab and
distribute it to the vehicle pid ets tabs to for efficiency reasons.
It will be easier to calc the vehicle capacity this way and
vehicle state is implicit in the FSM structure. This is also help
alleviate potential race conditions on all vehicle pids clamouring
for a central tab and instead attaining some degree of autonomy
by using a specific pid ets table for the intricacies of the delivery algorithm.

The details of the delivery mechanism is shifted into a orchestration/policy manager class
allowing us to sub the logic in and out and encapsulate it better.
