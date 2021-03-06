Practical Part 1: Implementing a Journey Planner 
================================================

It was decided to return nested loop of From To so that distance might be able to be included at a later date and allow for optimised routing on link cost rather then hop count, however there was not enough time for this

Compile with:
> c(planner).

Run with:
> planner:start_link().


Although the routing technique is not efficient it is reliable and there were other areas to address so during testing we can see inefficiencies such as:

    >planner:route("Szczecin", ["Wrocław", "Radom"]).
    {ok,[["Szczecin",
          [87,114,111,99,322,97,119],
          [321,243,100,378],
          "Radom"],
         ["Radom",[321,243,100,378],[87,114,111,99,322,97,119]]]}

May not lead to the most efficient route, but lists:usort removes duplicates effectively.

Potentially this could be remedied by removing such duplicates but is also an particularly unfortunate result due to the lists:usort and that these locations are not alphabetically contiguous and I have opted simplicity over complexity with random efficiency. However the aim of this exercise is to flex the power of Erlangs actor concurrency model rather than weigh up hop and distance vector based routing algorithms. We can make gains by can simply test package eligibility to be dropped when in each city and calling manager:reserve whenever the vehicle is empty.

After running simulations with random weighted packages it also appears that the capacity of vans is such that they can not take significant load and likely to only have one destination, and equally larger loads in the trucks are often more efficient when we select deliveries by location and time so that we have a higher quantity to drop in just one location, thus rendering an optimised routing function less necessary.

The loop functionality was added later to permit for message passing to ascertain a route this change in invocation involved subsequent adaption of dispatcher to receive messages but it allows for more dexterity when deciding how to call the routing function.

    > planner:start_link().
    > {ok,planner}
    > planner:route("Wrocław", ["Katowice"]).
    {ok,[[[87,114,111,99,322,97,119],"Katowice"]]}
    > flush().
    ok
    > planner ! {route, {"Wrocław", ["Katowice"]}, self()}.
    Routing request received from <0.55.0> 
    {route,{[87,114,111,99,322,97,119],["Katowice"]},<0.55.0>}
    > flush().
    Shell got {ok,[[[87,114,111,99,322,97,119],"Katowice"]]}
    ok
    > 
    
We can also test that the planner wont accept locations not imported from the configuration:

    > planner:route("London", ["Oxford"]).
    {error,invalid}