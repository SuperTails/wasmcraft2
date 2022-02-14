scoreboard players operation %%align wasm = %ptr reg
scoreboard players operation %%align wasm %= %%4 reg
execute if score %%align wasm matches 0..0 run execute at @e[tag=memoryptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %param0%0 reg
execute unless score %%align wasm matches 0..0 run function intrinsic:store_word_unaligned