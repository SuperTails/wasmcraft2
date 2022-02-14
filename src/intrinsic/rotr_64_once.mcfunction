
scoreboard players operation %%temprotr64_carry reg = %param0%0 reg 
scoreboard players operation %%temprotr64_carry reg %= %%2 reg
scoreboard players operation %%temprotr64_carry reg *= %%-2147483648 reg

function intrinsic:lshr_i64/shift_once

scoreboard players operation %param0%1 reg += %%temprotr64_carry reg