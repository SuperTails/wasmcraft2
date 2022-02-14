# Arguments:
# %param0%0 
# %param0%1 - The value to be shifted (also the output)

scoreboard players operation %%temprotl64_param1_save reg = %param1%0 reg

# carry = (lhs & (1 << 63)) != 0
execute store success score %%temprotl64_carry reg if score %param0%1 reg matches ..-1
scoreboard players set %param1%0 reg 1
function intrinsic:shl_64
scoreboard players operation %param0%0 reg += %%temprotl64_carry reg

scoreboard players operation %param1%0 reg = %%temprotl64_param1_save reg