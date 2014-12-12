Practical Part 2: Implementing the Parcel Manager 
=================================================

The following module keeps track of parcels and provides vans and trucks with locations that require pickups. It can access the journey planner’s tables and retrieve information on cities and routes as required.

Note that the setup items have been refactored out of the planner and also combined them with the additional supporting set up operations in its own setup file where ETS tables are creates, the config is imported, the graphs created and the planner started in support preparation.

Compile with:
> c(setup), c(planner), c(manager).

Run with:
> manager:start_link().

Visualise with:
> observer:start().
> {ok,<0.47.0>}

The following comments demonstrate the design decisions and compromises made on each function as well as supporting test cases. Extensive testing was performed and the tests noted here a simplified for brevity and prove the basic test case coverage.

The implementation of the start_link is trivial can can easily be tested:

> manager:start_link().

Sending items allows a client to specify a starting point and destination as well as the weight of the item and is returned a unique reference. This unique reference makes use of Erlangs now() built in function as this has the desirable feature of being a monotonic function that always increases and thus requires a global lock to read. This will prevent multiple identical references. This was tested with multiple concurrent orders being placed to ensure that no two references were the same.

> manager:send("Szczecin", "Sosnowiec", 100).
Order sent: 1418420153536945 "Szczecin" "Sosnowiec" 100 
{ok,1418420153536945}
> manager:send("Radom", "Sosnowiec", 10).
Order sent: 1418420175204952 "Radom" "Sosnowiec" 10 
{ok,1418420175204952}


The deliver function returns a list of near by locations from the current location of a vehicle with the oldest parcel being listed first to ensure the older the item the higher the priority. The reverse ETS select facilitates this functionality as opposed to a standard select which returns the newest order reference first. The specification requests this function returns a location list of destinations that have orders to be delivered to or picked up from, although it is not clear why you would want the locations to be delivered to without the concept of ownership of an order at this point.

> manager:deliver("Szczecin").
{ok,["Sosnowiec"]}
> manager:deliver("Sosnowiec").
{ok,["Radom","Szczecin"]}


The two reserve functions are similar and differ in the arity where one accepts the addition of a “To” destination location. Older orders are prioritised with the reverse select and it is ensure that only one vehicle can can pick it up by updating the entry and stating the identification of the vehicle to which it is reserved.

> manager:reserve("Szczecin", "Sosnowiec", 10).
{ok,[]}

Note this returns an empty list as there are not items under the weight limit of 10 kg.

> manager:reserve("Szczecin", "Sosnowiec", 100).
{ok,[[1418420153536945,reserved,"Szczecin","Sosnowiec",
      100]]}


The pickup function was relatively straight forward to implement however the introduction of the return value of instance was somewhat ambiguous despite some clarification defining this as “The parcel you are trying to do something with does not exist. For example, trying to drop a parcel you have not picked up or which as a result of a race condition, has been reserved and picked up by someone else.” It was therefore decided to return the failed reference identified as this instance.

> manager:pick(1418421220225738).
ok
> manager:pick(1418420153536945).
{error,not_picked,1418420153536945}


The drop function is called when a vehicle is at its destination and follows the logical progression of pick/1 and given a reference will allow the update of the record to show that it has arrived at its destination. 

> manager:drop(1418421220225738).
ok
> manager:drop(1000000000000000).
{error,not_reserved,1000000000000000}

The transit function permits a vehicle to drop an order at a cargo station en route to its final destination. This updates the corresponding order by reference in the manager ETS table and updates the “From” location to be the current cargo station location.

> manager:pick(1418421229303352).
{error,not_picked,1418421229303352} // this fails because we haven't called reserve yet...
> manager:reserve("Radom","Sosnowiec",10).
{ok,[[1418421229303352,reserved,"Radom","Sosnowiec",10]]}
> manager:pick(1418421229303352).         
ok
> manager:transit(1418421229303352, "Warszawa").
ok

Our record now shows the following in the ETS manager table:
> {1418421229303352,indepot,"Warszawa","Sosnowiec",10}


The cargo function is called when deliveries are complete and provided with a location the function will return the nearest cargo station, by location hop count, not distance based…

> manager:cargo("Częstochowa").
{ok,[321,243,100,378]}
> manager:cargo("Łódź").    
{ok,[321,243,100,378]}
> manager:cargo("Białystok").
{ok,"Warszawa"}


Finally the lookup/1 function allows you to query the state of a package in the system by the provided reference:

> manager:lookup(1418421220225738).
{ok,[{1418421220225738,delivered,"Szczecin","Sosnowiec",
      100}],
    <0.32.0>,manager}

