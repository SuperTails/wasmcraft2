function intrinsic:setptr

execute at 44453000-0-0-0-1 store result score %param0%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %%temp0_store_byte reg = %ptr reg
scoreboard players operation %%temp0_store_byte reg %= %%4 reg

scoreboard players operation %return%0 reg = %param0%0 reg
# %return%0 = %param0%0 & 0xFFFF_0000
execute if score %%temp0_store_byte reg matches 0..0 run scoreboard players operation %param0%0 reg %= %%65536 reg
execute if score %%temp0_store_byte reg matches 0..0 run scoreboard players operation %return%0 reg -= %param0%0 reg
# %return%0 = %param0%0 & 0x0000_FFFF
execute if score %%temp0_store_byte reg matches 2..2 run scoreboard players operation %return%0 reg %= %%65536 reg

# %param2%0 *= 1 << 0
execute if score %%temp0_store_byte reg matches 0..0 run scoreboard players operation %param2%0 reg *= %%1 reg
# %param2%0 *= 1 << 16
execute if score %%temp0_store_byte reg matches 2..2 run scoreboard players operation %param2%0 reg *= %%65536 reg

scoreboard players operation %return%0 reg += %param2%0 reg

execute at 44453000-0-0-0-1 store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %return%0 reg

scoreboard players operation %param2%0 reg = %tempsave_store_word reg