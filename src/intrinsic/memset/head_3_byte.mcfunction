execute store result score %mst_temp_word reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %mst_temp_byte reg = %param1%0 reg
scoreboard players operation %mst_temp_byte reg *= %%256 reg

scoreboard players operation %mst_temp_word reg %= %%256 reg
scoreboard players operation %mst_temp_word reg += %mst_temp_byte reg
scoreboard players operation %mst_temp_byte reg *= %%256 reg
scoreboard players operation %mst_temp_word reg += %mst_temp_byte reg
scoreboard players operation %mst_temp_byte reg *= %%256 reg
scoreboard players operation %mst_temp_word reg += %mst_temp_byte reg

execute store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %mst_temp_word reg

scoreboard players add %param0%0 reg 3
scoreboard players remove %mst_length reg 3