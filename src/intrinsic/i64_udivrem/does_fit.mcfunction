
# r += neg_rhs;

scoreboard players operation %udiv_new_r_lo reg = %udiv_r_lo reg
scoreboard players operation %udiv_new_r_hi reg = %udiv_r_hi reg

scoreboard players operation %udiv_new_r_lo reg += %udiv_neg_rhs_lo reg
scoreboard players operation %udiv_new_r_hi reg += %udiv_neg_rhs_hi reg

scoreboard players set %udiv_carry reg 0

execute if score %udiv_r_lo reg matches ..-1 if score %udiv_neg_rhs_lo reg matches ..-1 run scoreboard players set %udiv_carry reg 1
execute if score %udiv_r_lo reg matches ..-1 if score %udiv_neg_rhs_lo reg matches 0.. if score %udiv_new_r_lo reg matches 0.. run scoreboard players set %udiv_carry reg 1
execute if score %udiv_r_lo reg matches 0.. if score %udiv_neg_rhs_lo reg matches ..-1 if score %udiv_new_r_lo reg matches 0.. run scoreboard players set %udiv_carry reg 1

scoreboard players operation %udiv_new_r_hi reg += %udiv_carry reg

scoreboard players operation %udiv_r_lo reg = %udiv_new_r_lo reg
scoreboard players operation %udiv_r_hi reg = %udiv_new_r_hi reg

# q |= q_bit;

scoreboard players operation %udiv_q_lo reg += %udiv_q_bit_lo reg
scoreboard players operation %udiv_q_hi reg += %udiv_q_bit_hi reg