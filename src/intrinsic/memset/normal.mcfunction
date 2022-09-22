#execute if score %mst_bytes_left reg > %mst_length reg run say TODO: Only a very short memset

execute at @s if score %mst_bytes_left reg matches 1 run function intrinsic:memset/head_1_byte
execute at @s if score %mst_bytes_left reg matches 2 run function intrinsic:memset/head_2_byte
execute at @s if score %mst_bytes_left reg matches 3 run function intrinsic:memset/head_3_byte

scoreboard players operation %mst_value_word reg = %param1%0 reg
scoreboard players operation %mst_value_word reg *= %%256 reg
scoreboard players operation %mst_value_word reg += %param1%0 reg
scoreboard players operation %mst_temp reg = %mst_value_word reg
scoreboard players operation %mst_value_word reg *= %%65536 reg
scoreboard players operation %mst_value_word reg += %mst_temp reg

scoreboard players operation %ptr reg = %param0%0 reg
function intrinsic:setptr
scoreboard players operation %mst_x reg = %%ptr reg
scoreboard players operation %mst_y reg = %y reg
scoreboard players operation %mst_z reg = %z reg
execute if score %mst_length reg matches 4.. run function intrinsic:memset/body_words

# The pointer is already set properly by body_words,
# and %param0%0 is already updated.

execute at @s if score %mst_length reg matches 1 run function intrinsic:memset/tail_1_byte
execute at @s if score %mst_length reg matches 2 run function intrinsic:memset/tail_2_byte 
execute at @s if score %mst_length reg matches 3 run function intrinsic:memset/tail_3_byte