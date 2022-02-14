scoreboard players operation %%align wasm = %ptr reg
scoreboard players operation %%align wasm %= %%4 reg
execute if score %%align wasm matches 0..0 run execute at @e[tag=memoryptr] store result score %return%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute unless score %%align wasm matches 0..0 run function intrinsic:load_word_unaligned