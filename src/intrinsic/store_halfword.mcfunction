# arguments:
# %ptr - The location to write to
# %param2%0 - The halfword to write

scoreboard players operation %tempsave_store_word reg = %param2%0 reg
scoreboard players operation %param2%0 reg %= %%65536 reg

scoreboard players operation %%temp0_store_byte reg = %ptr reg
scoreboard players operation %%temp0_store_byte reg %= %%2 reg
# !INTERPRETER: ASSERT if score %%temp0_store_byte reg matches 0..0

function intrinsic:setptr

execute at @e[tag=memoryptr] store result score %param0%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %%temp0_store_byte reg = %ptr reg
scoreboard players operation %%temp0_store_byte reg %= %%4 reg

# 0xFFFF_0000
execute if score %%temp0_store_byte reg matches 0..0 run scoreboard players set %param1%0 reg -65536
# 0x0000_FFFF
execute if score %%temp0_store_byte reg matches 2..2 run scoreboard players set %param1%0 reg 65535

function intrinsic:and

# %param2%0 *= 1 << 0
execute if score %%temp0_store_byte reg matches 0..0 run scoreboard players operation %param2%0 reg *= %%1 reg
# %param2%0 *= 1 << 16
execute if score %%temp0_store_byte reg matches 2..2 run scoreboard players operation %param2%0 reg *= %%65536 reg

scoreboard players operation %return%0 reg += %param2%0 reg

execute at @e[tag=memoryptr] store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg

scoreboard players operation %param2%0 reg = %tempsave_store_word reg