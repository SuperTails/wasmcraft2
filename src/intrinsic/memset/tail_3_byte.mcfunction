execute store result score %mst_temp_word reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %mst_low_bits reg = %mst_temp_word reg
scoreboard players operation %mst_low_bits reg %= %%16777216 reg

scoreboard players operation %mst_temp_word reg -= %mst_low_bits reg

scoreboard players operation %mst_value_word reg %= %%16777216 reg
scoreboard players operation %mst_temp_word reg += %mst_value_word reg

execute store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %mst_temp_word reg