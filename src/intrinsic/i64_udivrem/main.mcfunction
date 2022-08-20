#	uint64_t q = 0;
#    uint64_t r = 0;
#    uint64_t q_bit = 1;

#    uint64_t neg_rhs = -rhs;

#	for (int i = 63; i >= 0; --i) {
#        r <<= 1;
#        r |= ((lhs & (1ull << 63)) != 0);
#        if (rhs <= r) {
#            r += neg_rhs;
#            q += q_bit;
#        }
#
#        lhs <<= 1;
#        q_bit >>= 1;
#	}

# returns variables in (%udiv_q_lo reg %udiv_q_hi reg), (%udiv_r_lo reg %udiv_r_hi reg)

# Initialize loop variables

scoreboard players set %udiv_q_lo reg 0
scoreboard players set %udiv_q_hi reg 0

scoreboard players set %udiv_r_lo reg 0
scoreboard players set %udiv_r_hi reg 0

scoreboard players set %udiv_q_bit_lo reg 0
scoreboard players operation %udiv_q_bit_hi reg = %%-2147483648 reg

# udiv_neg_rhs = -udiv_rhs

scoreboard players operation %udiv_neg_rhs_lo reg = %udiv_rhs_lo reg
scoreboard players operation %udiv_neg_rhs_hi reg = %udiv_rhs_hi reg

scoreboard players operation %udiv_neg_rhs_lo reg *= %%-1 reg
scoreboard players remove %udiv_neg_rhs_lo reg 1
scoreboard players operation %udiv_neg_rhs_hi reg *= %%-1 reg
scoreboard players remove %udiv_neg_rhs_hi reg 1

execute if score %udiv_neg_rhs_lo reg matches -1 run scoreboard players add %udiv_neg_rhs_hi reg 1
scoreboard players add %udiv_neg_rhs_lo reg 1

# the entire for loop ...

scoreboard players set %udiv_iter reg 64
function intrinsic:i64_udivrem/loop