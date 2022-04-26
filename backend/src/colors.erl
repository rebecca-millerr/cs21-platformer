% A simple utility for generating colors


-module(colors).

-export([random_rgb/0]).

random_rgb() -> 
    R = rand:uniform(255) - 1,
    G = rand:uniform(255) - 1,
    B = rand:uniform(255) - 1,
    #{r => R, g => G, b => B}.
    
