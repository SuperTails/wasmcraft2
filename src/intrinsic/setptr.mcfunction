scoreboard players operation %%ptr reg = %ptr reg
scoreboard players operation %%ptr reg /= %%4 reg
scoreboard players operation %z reg = %%ptr reg 
scoreboard players operation %z reg %= %%PAGE_SPAN_Z reg
scoreboard players operation %%ptr reg /= %%PAGE_SPAN_Z reg
scoreboard players operation %y reg = %%ptr reg
scoreboard players operation %y reg %= %%PAGE_SPAN_Y reg
scoreboard players operation %%ptr reg /= %%PAGE_SPAN_Y reg
execute store result storage wasm:scratch Pos[0] double 1 run scoreboard players get %%ptr reg
execute store result storage wasm:scratch Pos[1] double 1 run scoreboard players get %y reg
execute store result storage wasm:scratch Pos[2] double 1 run scoreboard players get %z reg
data modify entity 44453000-0-0-0-1 Pos set from storage wasm:scratch Pos