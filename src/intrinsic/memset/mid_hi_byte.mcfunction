# 00_NN_00_00

execute at @e[tag=memoryptr] store result score %mst_temp_word reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

# mst_low_half = mst_temp_word & 0x00_FF_00_00
scoreboard players operation %mst_low_half reg = %mst_temp_word reg
scoreboard players operation %mst_low_half reg %= %%16777216 reg
scoreboard players operation %mst_low_byte reg = %mst_temp_word reg
scoreboard players operation %mst_low_byte reg %= %%65536 reg
scoreboard players operation %mst_low_half reg -= %mst_low_byte reg

scoreboard players operation %mst_temp_word reg -= %mst_low_half reg

scoreboard players operation %mst_temp_value reg = %param1%0 reg
scoreboard players operation %mst_temp_value reg *= %%65536 reg

scoreboard players operation %mst_temp_word reg += %mst_temp_value reg

execute at @e[tag=memoryptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %mst_temp_word reg