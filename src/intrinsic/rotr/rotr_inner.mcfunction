# temp_carry = (param0_hi & 0x1) << 31;

scoreboard players operation %%temp_carry reg = %param0%0 reg
scoreboard players operation %%temp_carry reg %= %%2 reg
scoreboard players operation %%temp_carry reg *= %%-2147483648 reg

# ==== Logical shift right ====

# was_neg = (param0_hi < 0)
execute store success score %%temp0_rotr_inner reg if score %param0%0 reg matches ..-1
# if (was_neg) { param0_1 += 1 << 31 };
execute if score %%temp0_rotr_inner reg matches 1..1 run scoreboard players operation %param0%0 reg += %%-2147483648 reg

scoreboard players operation %param0%0 reg /= %%2 reg

# if (was_neg) { param0_1 += 1 << 30 }
execute if score %%temp0_rotr_inner reg matches 1..1 run scoreboard players add %param0%0 reg 1073741824

scoreboard players operation %param0%0 reg += %%temp_carry reg

scoreboard players remove %param1%0 reg 1