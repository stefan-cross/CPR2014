%%%-------------------------------------------------------------------
%%% @author stefancross
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Nov 2014 21:33
%%%-------------------------------------------------------------------
-module(testing).
-author("stefancross").

%% API
-export([enterDeliveryData/0, enterRoutingData/1]).


% c(planner),c(manager),c(vehicle),c(testing),planner:start_link(), manager:start_link(), testing:enterDeliveryData().

enterDeliveryData() ->
  manager:send("Kraków", "Gdańsk", 20),
  manager:send("Toruń", "Gdańsk", 40),
  manager:send("Kraków", "Gdańsk", 60),
  manager:send("Gdańsk", "Wrocław", 10),
  manager:send("Kraków", "Lublin", 20),
  manager:send("Bydgoszcz", "Gdańsk", 110),
  manager:send("Kraków", "Gdańsk", 70),
  manager:send("Częstochowa", "Gdańsk", 90),
  manager:send("Kraków", "Gdańsk", 60),
  manager:send("Kraków", "Białystok", 20),
  manager:send("Toruń", "Gdańsk", 30),
  manager:send("Kraków", "Poznań", 40),
  manager:send("Kraków", "Gdańsk", 60),
  manager:send("Radom", "Łódź", 60),
  manager:send("Kraków", "Gdańsk", 10),
  manager:send("Kraków", "Łódź", 70),
  manager:send("Szczecin", "Gdańsk", 10),
  manager:send("Kraków", "Toruń", 90),
  manager:send("Gdynia", "Gdańsk", 100),
  manager:send("Kraków", "Wrocław", 20),
  manager:send("Łódź", "Gdańsk", 30),
  manager:send("Toruń", "Gdańsk", 70),
  manager:send("Kraków", "Łódź", 80),
  manager:send("Kraków", "Gdańsk",310),
  manager:send("Radom", "Gdańsk", 60),
  manager:send("Kraków", "Wrocław", 30),
  manager:send("Kraków", "Sosnowiec", 80),
  manager:send("Białystok", "Gdańsk", 80),
  manager:send("Kraków", "Toruń", 110).


enterRoutingData(Acc) when Acc < 10000 ->
  van10 ! {van10, a, b, 1500},
  van9 ! {van9, a, b, 1500},
  van8 ! {van8, a, b, 1500},
  van7 ! {van7, a, b, 1500},
  van6 ! {van6, a, b, 1500},
  van5 ! {van5, a, b, 1500},
  van4 ! {van4, a, b, 1500},
  van3 ! {van3, a, b, 1500},
  van2 ! {van2, a, b, 1500},
  van1 ! {van1, a, b, 1500},
  van10 ! {van10, b, c, 1500},
  van9 ! {van9, b, c, 1000},
  van8 ! {van8, b, c, 1000},
  van7 ! {van7, b, c, 1000},
  van6 ! {van6, b, c, 1000},
  van5 ! {van5, b, c, 1000},
  van4 ! {van4, b, c, 1000},
  van3 ! {van3, b, c, 1000},
  van2 ! {van2, b, c, 1000},
  van1 ! {van1, b, c, 1000},
  enterRoutingData(Acc + 1).





