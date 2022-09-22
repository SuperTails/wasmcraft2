# Assumes the memory pointer is already at the correct position
# %ptr - The location to read from
# %param0%0 - The return value
# Clobbers %param1%0

execute at 44453000-0-0-0-1 store result score %param0%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %param1%0 reg = %ptr reg
scoreboard players operation %param1%0 reg %= %%4 reg
# %param0%0 <<= (8 * 3)
execute if score %param1%0 reg matches 0..0 run scoreboard players operation %param0%0 reg *= %%16777216 reg
# %param0%0 <<= (8 * 2)
execute if score %param1%0 reg matches 1..1 run scoreboard players operation %param0%0 reg *= %%65536 reg
# %param0%0 <<= (8 * 1)
execute if score %param1%0 reg matches 2..2 run scoreboard players operation %param0%0 reg *= %%256 reg
# %param0%0 <<= (8 * 0)
# No-op:
# execute if score %param1%0 reg matches 3..3 run scoreboard players set %param1%0 reg 1

# -- %param0%0 >>= 24 --

execute store success score %%temp0_load_byte reg if score %param0%0 reg matches ..-1

# Have to split this in two because you can't actually subtract i32::MAX
execute if score %%temp0_load_byte reg matches 1..1 run scoreboard players remove %param0%0 reg 2147483647
execute if score %%temp0_load_byte reg matches 1..1 run scoreboard players remove %param0%0 reg 1

scoreboard players operation %param0%0 reg /= %%16777216 reg

execute if score %%temp0_load_byte reg matches 1..1 run scoreboard players add %param0%0 reg 128