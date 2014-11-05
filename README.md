
CPR coursework Oct 2014

At a high level the coursework has themes of a Courier Delivery Problem, 
and simulation. 

Parallel and distributed discrete-event simulation (PDES) in the Erlang language.

- Input data is structured, can we use meta programming to change behaviour or params depending on the config?

- Van and Truck location data is likely to change and to actually be states
later in the exercises


Ets for Towns and Distances

http://www.erlang.org/doc/man/ets.html#match-2
> ets:match(towns, '$1').
> ets:match(distances, '$1').


Q3.1 test data: 

A list of destination cities
> ["Lublin", "Warszawa", "Lublin", "Gdynia"]

Starting city and destination list
> {"Radom" , ["Lublin", "Warszawa", "Lublin", "Gdynia"]}

