# 00_00_NN_00

execute store result score %mst_temp_word reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

# mst_low_half = mst_temp_word & 0x00_00_FF_00
scoreboard players operation %mst_low_half reg = %mst_temp_word reg
scoreboard players operation %mst_low_half reg %= %%65536 reg
scoreboard players operation %mst_low_byte reg = %mst_temp_word reg
scoreboard players operation %mst_low_byte reg %= %%256 reg
scoreboard players operation %mst_low_half reg -= %mst_low_byte reg

scoreboard players operation %mst_temp_word reg -= %mst_low_half reg

scoreboard players operation %mst_temp_value reg = %param1%0 reg
scoreboard players operation %mst_temp_value reg *= %%256 reg

scoreboard players operation %mst_temp_word reg += %mst_temp_value reg

execute store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %mst_temp_word reg