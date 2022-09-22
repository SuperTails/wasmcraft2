# This assumes the memory pointer is already at the correct location
# arguments:
# %ptr - The location to write to
# %param2%0 - The byte to write

scoreboard players operation %tempsave_store_byte reg = %param2%0 reg
scoreboard players operation %param2%0 reg %= %%256 reg

execute at 44453000-0-0-0-1 store result score %param0%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %%temp0_store_byte reg = %ptr reg
scoreboard players operation %%temp0_store_byte reg %= %%4 reg

scoreboard players operation %return%0 reg = %param0%0 reg
# 0xFFFF_FF00
execute if score %%temp0_store_byte reg matches 0 run scoreboard players operation %param0%0 reg %= %%256 reg
execute if score %%temp0_store_byte reg matches 0 run scoreboard players operation %return%0 reg -= %param0%0 reg
# 0xFFFF_00FF
execute if score %%temp0_store_byte reg matches 1 run scoreboard players operation %param0%0 reg %= %%65536 reg
execute if score %%temp0_store_byte reg matches 1 run scoreboard players operation %param0%0 reg /= %%256 reg
execute if score %%temp0_store_byte reg matches 1 run scoreboard players operation %param0%0 reg *= %%256 reg
execute if score %%temp0_store_byte reg matches 1 run scoreboard players operation %return%0 reg -= %param0%0 reg
# 0xFF00_FFFF
execute if score %%temp0_store_byte reg matches 2 run scoreboard players operation %param0%0 reg %= %%16777216 reg
execute if score %%temp0_store_byte reg matches 2 run scoreboard players operation %param0%0 reg /= %%65536 reg
execute if score %%temp0_store_byte reg matches 2 run scoreboard players operation %param0%0 reg *= %%65536 reg
execute if score %%temp0_store_byte reg matches 2 run scoreboard players operation %return%0 reg -= %param0%0 reg
# 0x00FF_FFFF
execute if score %%temp0_store_byte reg matches 3 run scoreboard players operation %return%0 reg %= %%16777216 reg

# %param2%0 *= 1 << 0
#execute if score %%temp0_store_byte reg matches 0..0 run scoreboard players operation %param2%0 reg *= %%1 reg
# %param2%0 *= 1 << 8
execute if score %%temp0_store_byte reg matches 1 run scoreboard players operation %param2%0 reg *= %%256 reg
# %param2%0 *= 1 << 16
execute if score %%temp0_store_byte reg matches 2 run scoreboard players operation %param2%0 reg *= %%65536 reg
# %param2%0 *= 1 << 24
execute if score %%temp0_store_byte reg matches 3 run scoreboard players operation %param2%0 reg *= %%16777216 reg

scoreboard players operation %return%0 reg += %param2%0 reg

execute at 44453000-0-0-0-1 store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg

scoreboard players operation %param2%0 reg = %tempsave_store_byte reg