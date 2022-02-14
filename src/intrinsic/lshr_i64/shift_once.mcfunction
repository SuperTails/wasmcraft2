
# temp_carry = (param0_hi & 0x1) << 31;

scoreboard players operation %%temp_carry reg = %param0%1 reg
scoreboard players operation %%temp_carry reg %= %%2 reg
scoreboard players operation %%temp_carry reg *= %%-2147483648 reg

# ------- Shift top half ------

# was_neg = (param0_hi < 0)
execute store success score %%temp0_lshr_inner reg if score %param0%1 reg matches ..-1
# if (was_neg) { param0_1 += 1 << 31 };
execute if score %%temp0_lshr_inner reg matches 1..1 run scoreboard players operation %param0%1 reg += %%-2147483648 reg

scoreboard players operation %param0%1 reg /= %%2 reg

# if (was_neg) { param0_1 += 1 << 30 }
execute if score %%temp0_lshr_inner reg matches 1..1 run scoreboard players add %param0%1 reg 1073741824

# ------- Shift bottom half ------

# was_neg = (param0_lo < 0)
execute store success score %%temp0_lshr_inner reg if score %param0%0 reg matches ..-1
# if (was_neg) { param0_lo += 1 << 31 };
execute if score %%temp0_lshr_inner reg matches 1..1 run scoreboard players operation %param0%0 reg += %%-2147483648 reg

scoreboard players operation %param0%0 reg /= %%2 reg

# if (was_neg) { param0_lo += 1 << 30 }
execute if score %%temp0_lshr_inner reg matches 1..1 run scoreboard players add %param0%0 reg 1073741824

# ----- Carry -----

scoreboard players operation %param0%0 reg += %%temp_carry reg