scoreboard players operation %sdiv_lhs_lo_2 reg = %sdiv_lhs_lo reg
scoreboard players operation %sdiv_lhs_hi_2 reg = %sdiv_lhs_hi reg
scoreboard players operation %sdiv_rhs_lo_2 reg = %sdiv_rhs_lo reg
scoreboard players operation %sdiv_rhs_hi_2 reg = %sdiv_rhs_hi reg

function intrinsic:i64_sdivrem/div

# sdiv_trunc = %sdiv_q * %sdiv_rhs

scoreboard players operation %param0%0 reg = %sdiv_q_lo reg
scoreboard players operation %param1%0 reg = %sdiv_rhs_lo_2 reg
function intrinsic:mul_32_to_64
scoreboard players operation %sdiv_trunc_lo reg = %return%0 reg
scoreboard players operation %sdiv_trunc_hi reg = %return%1 reg

scoreboard players operation %temp%1000%lo reg = %sdiv_q_lo reg
scoreboard players operation %temp%1000%lo reg *= %sdiv_rhs_hi_2 reg
scoreboard players operation %sdiv_trunc_hi reg += %temp%1000%lo reg
scoreboard players operation %temp%1000%lo reg = %sdiv_q_hi reg
scoreboard players operation %temp%1000%lo reg *= %sdiv_rhs_lo_2 reg
scoreboard players operation %sdiv_trunc_hi reg += %temp%1000%lo reg

# sdiv_trunc *= -1

scoreboard players operation %sdiv_trunc_lo reg *= %%-1 reg
scoreboard players remove %sdiv_trunc_lo reg 1
scoreboard players operation %sdiv_trunc_hi reg *= %%-1 reg
scoreboard players remove %sdiv_trunc_hi reg 1
execute if score %sdiv_trunc_lo reg matches -1 run scoreboard players add %sdiv_trunc_hi reg 1
scoreboard players add %sdiv_trunc_lo reg 1

# sdiv_r = sdiv_lhs_2 + sdiv_trunc

scoreboard players operation %sdiv_r_lo reg = %sdiv_lhs_lo_2 reg
scoreboard players operation %sdiv_r_hi reg = %sdiv_lhs_hi_2 reg
scoreboard players operation %sdiv_r_lo reg += %sdiv_trunc_lo reg
scoreboard players operation %sdiv_r_hi reg += %sdiv_trunc_hi reg
scoreboard players set %sdiv_carry reg 0
execute if score %sdiv_lhs_lo_2 reg matches ..-1 if score %sdiv_trunc_lo reg matches ..-1 run scoreboard players set %sdiv_carry reg 1
execute if score %sdiv_lhs_lo_2 reg matches ..-1 if score %sdiv_trunc_lo reg matches 0.. if score %sdiv_r_lo reg matches 0.. run scoreboard players set %sdiv_carry reg 1
execute if score %sdiv_lhs_lo_2 reg matches 0.. if score %sdiv_trunc_lo reg matches ..-1 if score %sdiv_r_lo reg matches 0.. run scoreboard players set %sdiv_carry reg 1
scoreboard players operation %sdiv_r_hi reg += %sdiv_carry reg