#	uint64_t q = 0;
#   uint64_t r = 0;
#   uint64_t q_bit = 1;
#
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

scoreboard players set %iud_q%lo reg 0
scoreboard players set %iud_q%hi reg 0

scoreboard players set %iud_r%lo reg 0
scoreboard players set %iud_r%hi reg 0

scoreboard players set %iud_q_bit%lo reg 0
scoreboard players set %iud_q_bit%hi reg 0

scoreboard players set %iud_i reg 63

function intrinsic:i64_udiv/loop