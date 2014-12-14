
CPR coursework Oct 2014
=======================

This paper deals with a Courier Delivery Problem (CDP) in a Discrete Event Simulation (DES)
addressing concurrency, fault tolerance and scalability issues inherent in the problem using
Erlangs soft real­time systems. The exercise implements a nationwide parcel delivery
simulator used to optimise delivery times and reduce transport costs. The simulator runs off a
configuration file that specifies a list of locations and populations, distances between these
locations, and the starting locations of a fleet of vehicles which have different capacities.
The simulator will consist of a dispatcher informing the vans and trucks where to pick up and
deliver parcels, as well as a journey planner, telling them which routes to take in­between
cities. When the simulator is up and running, the system is ready to take in new parcel
delivery orders. By testing different dispatch strategies, routes between cities and vehicle
behaviour will provide metrics which allows us to optimise the service in relation to delivery
times, capacity and fuel consumption.

All code and working is provided in the same folder that this document was delivered in and
will be available online with version control and suporting instructions in a README.md file.
This file will also include examples of test cases and also an explanation of each sections
implementation and design decisions.

+ Part1 - Contains the planner, responsible for routing and path calculation.
+ Part2 - Implements the manager with various functions to facilitate parcel distribution.
+ Part3 - Introduces vehicles and a delivery algorithm.
+ Part4 - Applies supervisors for fault tolerance and a fully automated DES run from one config.
+ docs  - Contains the original question paper and also some example data and map visualisations.

Everything is run from the file.conf.csv in the parent folder to each Part folder. 

Quick start commands:
--------------------
__(from each Part* folder and when in the Erlang shell)__

Compile everything:
>  c(manager), c(planner), c(vehicle), c(orchestration) ,c(vehicle_sup), c(top_sup).
    
Kick everything off:
> orchestration:start_simulation(No_of_Orders).

Observer with:
> observer:start().

Produce Docs
> edoc:files([Module_List],  [{dir,"./docs"}]).

Run tests (where applicable):
> c(manager_test), manager_test:test().


Please consult individual folder README files for more instructions.

Upon submission of the coursework the following resources will be made made available on request.

A dynamic map is available here:
https://www.google.com/maps/d/edit?mid=zZqQW5ER_nEU.k0nVn_MpDioI
And a Github account also contains this code here:
https://github.com/stefan-cross/CPR2014