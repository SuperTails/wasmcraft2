#	for (int i = 63; i >= 0; --i) {
#       r <<= 1;
#       r |= (lhs & 1ull) != 0;
#       if (r >= rhs) {
#           r -= rhs;
#           q |= q_bit;
#       }
#
#       lhs >>= 1;
#       q_bit <<= 1;
#	}
#
#   return q;

# r <<= 1
scoreboard players operation %param0%0 reg = %iud_q%lo reg
scoreboard players operation %param0%1 reg = %iud_q%hi reg
scoreboard players set %param1%0 reg 1
function intrinsic:shl_64
scoreboard players operation %iud_q%lo reg = %param0%0 reg
scoreboard players operation %iud_q%hi reg = %param0%1 reg

# r |= (lhs & 1ull) != 0
scoreboard players operation %iud_tmp%lo reg = %iud_lhs%lo reg
scoreboard players operation %iud_tmp%lo reg %= %%2 reg
scoreboard players operation %iud_q%lo reg += %iud_tmp%lo reg 

# if (r >= rhs) { q |= q_bit }

# if (r >= rhs) { r -= rhs }