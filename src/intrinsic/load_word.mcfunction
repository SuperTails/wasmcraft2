scoreboard players operation %%align reg = %ptr reg
scoreboard players operation %%align reg %= %%4 reg
execute if score %%align reg matches 0..0 run execute at 44453000-0-0-0-1 store result score %return%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute unless score %%align reg matches 0..0 run function intrinsic:load_word_unaligned