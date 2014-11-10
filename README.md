
CPR coursework Oct 2014

At a high level the coursework has themes of a Courier Delivery Problem, 
and simulation. 

Parallel and distributed discrete-event simulation (PDES) in the Erlang language.

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
1 - Reorder list to group possible duplicates A = ets:usort(ToList)
2 - Check routes between entries in reordered list, From A1
 2.1 - Take starting city and list head
 2.2 - Search destination for match of start and head
  2.3 - if no exact match, find match on start, run matches on corresponding city
         - find matches with least total distance cost and create temp list of these for later optimisation
         - select least distance from temp list, use as a candidate for path
        recursive match on the corresponding city,  
 3 - Repeat process with head and tail, may need to repeat 2.3 to find route between indirect cities

Notes on the journey planner algorithm, an optimal approach:
1 - Reorder list to group possible duplicates
2 - Check routes between entries in reordered list
 2.1 - Take starting city and list head
 2.2 - Search destination for match of start and head
  2.3 - if no exact match, find match on start, run matches on corresponding city
        recursive match on the corresponding city, find matches with least total distance cost
 3 - Repeat process with head and tail, may need to repeat 2.3 to find route between indirect cities
      
Consult the following for proven approach to shortest path algorithm:
http://en.wikipedia.org/wiki/Shortest_path_problem

Some research:
http://www-bcf.usc.edu/~maged/publications/solvingCDP.pdf

Visualising the locations:
https://www.google.com/maps/d/edit?mid=zZqQW5ER_nEU.k0nVn_MpDioI

https://www.google.com/fusiontables/data?docid=1Y4fldAT2TxjyZx61wH8KrcMBuboM7ZQNh3MSxRwQ#rows:id=1



Useful Syntax

14> ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', "Warszawa"}], ['$$']}]).
[["Warszawa","Radom",103],
 ["Warszawa",[321,243,100,378],130],
 ["Warszawa",[84,111,114,117,324],208],
 ["Warszawa","Lublin",170],
 ["Warszawa",[66,105,97,322,121,115,116,111,107],198]]
15> ets:select(distances, [{{'$1', '$2', '$3'}, [{'==', '$1', "Warszawa"}, {'==', '$2', "Radom"}], ['$$']}]).
[["Warszawa","Radom",103]]