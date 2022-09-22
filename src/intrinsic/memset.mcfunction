# i8* dest == %param0%0
# i8 value == %param1%0
# i32 len  == %param2%0

# %return%0 is set to the return value 

# tellraw @a [{"score": {"name": "%param0%0", "objective": "reg" }}]
# tellraw @a [{"score": {"name": "%param1%0", "objective": "reg" }}]
# tellraw @a [{"score": {"name": "%param2%0", "objective": "reg" }}]

# !INTERPRETER: ASSERT if score %param1%0 reg matches 0..255
# !INTERPRETER: ASSERT if score %param2%0 reg matches 0..

scoreboard players operation %return%0 reg = %param0%0 reg

scoreboard players operation %mst_byte_offset reg = %param0%0 reg
scoreboard players operation %mst_byte_offset reg %= %%4 reg

scoreboard players operation %mst_length reg = %param2%0 reg

scoreboard players set %mst_bytes_left reg 4
scoreboard players operation %mst_bytes_left reg -= %mst_byte_offset reg
scoreboard players operation %mst_bytes_left reg %= %%4 reg

scoreboard players operation %ptr reg = %param0%0 reg
function intrinsic:setptr

# Handle special cases for a memset that occurs within a single word
execute as 44453000-0-0-0-1 at @s if score %mst_bytes_left reg matches 2 if score %mst_length reg matches 1 run function intrinsic:memset/mid_hi_byte
execute as 44453000-0-0-0-1 at @s if score %mst_bytes_left reg matches 3 if score %mst_length reg matches 1 run function intrinsic:memset/mid_lo_byte
execute as 44453000-0-0-0-1 at @s if score %mst_bytes_left reg matches 3 if score %mst_length reg matches 2 run function intrinsic:memset/mid_2_byte

# Otherwise, do a normal memset
execute as 44453000-0-0-0-1 if score %mst_bytes_left reg <= %mst_length reg run function intrinsic:memset/normal