#    int is_neg = 0;
#
#    int64_t abs_lhs = lhs;
#    int64_t abs_rhs = rhs;
#
#    if (lhs < 0) {
#        abs_lhs *= -1;
#        is_neg = !is_neg;
#    }
#    if (rhs < 0) {
#        abs_rhs *= -1;
#        is_neg = !is_neg;
#    }
#
#    //udiv_result result = i64_udivrem(abs_lhs, abs_rhs);
#    int64_t quotient = i64_udiv(abs_lhs, abs_rhs);
#
#   if (is_neg) {
#       quotient *= -1;
#   }
#
#   return quotient;

# %sdiv_lhs ->
# %sdiv_rhs ->
# -> %sdiv_q

scoreboard players set %sdiv_sign reg 1

execute if score %sdiv_lhs_hi reg matches ..-1 run function intrinsic:i64_sdivrem/negate_lhs
execute if score %sdiv_rhs_hi reg matches ..-1 run function intrinsic:i64_sdivrem/negate_rhs

scoreboard players operation %udiv_lhs_lo reg = %sdiv_lhs_lo reg
scoreboard players operation %udiv_lhs_hi reg = %sdiv_lhs_hi reg
scoreboard players operation %udiv_rhs_lo reg = %sdiv_rhs_lo reg
scoreboard players operation %udiv_rhs_hi reg = %sdiv_rhs_hi reg
function intrinsic:i64_udivrem/main
scoreboard players operation %sdiv_q_lo reg = %udiv_q_lo reg
scoreboard players operation %sdiv_q_hi reg = %udiv_q_hi reg

execute if score %sdiv_sign reg matches -1 run function intrinsic:i64_sdivrem/negate_quot