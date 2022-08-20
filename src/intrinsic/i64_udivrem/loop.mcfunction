#	uint64_t q = 0;
#    uint64_t r = 0;
#    uint64_t q_bit = 1;

#    uint64_t neg_rhs = -rhs;

#	for (int i = 63; i >= 0; --i) {
#        r <<= 1;
#        r |= ((lhs & 1ull) != 0);
#        if (rhs <= r) {
#            r += neg_rhs;
#            q |= q_bit;
#        }
#
#        lhs >>= 1;
#        q_bit <<= 1;
#	}

# r <<= 1
execute store success score %udiv_r_lo_is_neg reg if score %udiv_r_lo reg matches ..-1
scoreboard players operation %udiv_r_lo reg += %udiv_r_lo reg
scoreboard players operation %udiv_r_hi reg += %udiv_r_hi reg
execute if score %udiv_r_lo_is_neg reg matches 1 run scoreboard players add %udiv_r_hi reg 1

# r |= ((lhs & (1ull << 63)) != 0)
execute if score %udiv_lhs_hi reg matches ..-1 run scoreboard players add %udiv_r_lo reg 1

# %udiv_does_fit = (rhs < r)

scoreboard players operation %udiv_ult_lhs reg = %udiv_rhs_hi reg
scoreboard players operation %udiv_ult_rhs reg = %udiv_r_hi reg
function intrinsic:i64_udivrem/unsigned_less_than
scoreboard players operation %udiv_hi_is_lesser reg = %udiv_ult_is_less reg

scoreboard players operation %udiv_ult_lhs reg = %udiv_r_hi reg
scoreboard players operation %udiv_ult_rhs reg = %udiv_rhs_hi reg
function intrinsic:i64_udivrem/unsigned_less_than
scoreboard players operation %udiv_hi_is_greater reg = %udiv_ult_is_less reg

execute store success score %udiv_hi_is_equal reg if score %udiv_r_hi reg = %udiv_rhs_hi reg

scoreboard players operation %udiv_ult_lhs reg = %udiv_rhs_lo reg
scoreboard players operation %udiv_ult_rhs reg = %udiv_r_lo reg
function intrinsic:i64_udivrem/unsigned_less_than
scoreboard players operation %udiv_lo_is_less_eq reg = %udiv_ult_is_less reg
execute if score %udiv_rhs_lo reg = %udiv_r_lo reg run scoreboard players set %udiv_lo_is_less_eq reg 1

execute if score %udiv_hi_is_lesser reg matches 1 run scoreboard players set %udiv_does_fit reg 1
execute if score %udiv_hi_is_greater reg matches 1 run scoreboard players set %udiv_does_fit reg 0
execute if score %udiv_hi_is_equal reg matches 1 run scoreboard players operation %udiv_does_fit reg = %udiv_lo_is_less_eq reg

#        if (rhs <= r) {
#            r += neg_rhs;
#            q |= q_bit;
#        }
execute if score %udiv_does_fit reg matches 1 run function intrinsic:i64_udivrem/does_fit


# lhs >>= 1;

scoreboard players operation %udiv_carry reg = %udiv_lhs_lo reg
scoreboard players operation %udiv_carry reg %= %%2 reg

#execute store success score %udiv_lhs_top_bit reg if score %udiv_lhs_lo reg matches ..-1
#execute if score %udiv_lhs_top_bit reg matches 1 run scoreboard players operation %udiv_lhs_lo reg += %%-2147483648 reg 
#scoreboard players operation %udiv_lhs_lo reg /= %%2 reg
#execute if score %udiv_lhs_top_bit reg matches 1 run scoreboard players operation %udiv_lhs_lo reg += %%1073741824 reg
#execute if score %udiv_carry reg matches 1 run scoreboard players operation %udiv_lhs_lo reg += %%-2147483648 reg

#execute store success score %udiv_lhs_top_bit reg if score %udiv_lhs_hi reg matches ..-1
#execute if score %udiv_lhs_top_bit reg matches 1 run scoreboard players operation %udiv_lhs_hi reg += %%-2147483648 reg 
#scoreboard players operation %udiv_lhs_hi reg /= %%2 reg
#execute if score %udiv_lhs_top_bit reg matches 1 run scoreboard players operation %udiv_lhs_hi reg += %%1073741824 reg

scoreboard players operation %udiv_lhs_hi reg += %udiv_lhs_hi reg
execute if score %udiv_lhs_lo reg matches ..-1 run scoreboard players add %udiv_lhs_hi reg 1
scoreboard players operation %udiv_lhs_lo reg += %udiv_lhs_lo reg

# q_bit >>= 1;

execute store success score %udiv_q_bit_max reg if score %udiv_q_bit_lo reg matches ..-1
scoreboard players operation %udiv_q_bit_lo reg /= %%2 reg
execute if score %udiv_q_bit_max reg matches 1 run scoreboard players set %udiv_q_bit_lo reg 1073741824

execute if score %udiv_q_bit_hi reg matches 1 run scoreboard players operation %udiv_q_bit_lo reg = %%-2147483648 reg

execute store success score %udiv_q_bit_max reg if score %udiv_q_bit_hi reg matches ..-1
scoreboard players operation %udiv_q_bit_hi reg /= %%2 reg
execute if score %udiv_q_bit_max reg matches 1 run scoreboard players set %udiv_q_bit_hi reg 1073741824

# rest of the loop

scoreboard players remove %udiv_iter reg 1
execute if score %udiv_iter reg matches 1.. run function intrinsic:i64_udivrem/loop