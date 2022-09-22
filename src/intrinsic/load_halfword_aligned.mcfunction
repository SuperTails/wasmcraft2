# %ptr - The location to read from
# %param0%0 - The return value
# Clobbers %param1%0

function intrinsic:setptr

execute at 44453000-0-0-0-1 store result score %param0%0 reg run data get block ~ ~ ~ RecordItem.tag.Memory 1

scoreboard players operation %param1%0 reg = %ptr reg
scoreboard players operation %param1%0 reg %= %%4 reg

# %param0%0 <<= 16
execute if score %param1%0 reg matches 0 run scoreboard players operation %param0%0 reg *= %%65536 reg

# %param0%0 >>= 16 (logical)
execute store success score %%templshr_sign reg if score %param0%0 reg matches ..-1
execute if score %%templshr_sign reg matches 1 run scoreboard players operation %param0%0 reg -= %%-2147483648 reg
scoreboard players operation %%templshr_sign reg *= %%32768 reg
scoreboard players operation %param0%0 reg /= %%65536 reg
scoreboard players operation %param0%0 reg += %%templshr_sign reg

scoreboard players operation %return%0 reg = %param0%0 reg