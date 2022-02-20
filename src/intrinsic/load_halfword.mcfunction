# %ptr - The location to read from
# %param0%0 - The return value
# Clobbers %param1%0

scoreboard players operation %param1%0 reg = %ptr reg
scoreboard players operation %param1%0 reg %= %%2 reg
# !INTERPRETER: ASSERT if score %param1%0 reg matches 0..0

function intrinsic:setptr

execute at @e[tag=memoryptr] store result score %param0%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %param1%0 reg = %ptr reg
scoreboard players operation %param1%0 reg %= %%4 reg

# 1 << (8 * 2)
execute if score %param1%0 reg matches 0..0 run scoreboard players set %param1%0 reg 65536
# 1 << (8 * 0)
execute if score %param1%0 reg matches 2..2 run scoreboard players set %param1%0 reg 1

scoreboard players operation %param0%0 reg *= %param1%0 reg

scoreboard players set %param1%0 reg 16

function intrinsic:lshr