# 
# %param0%0 - lhs
# %param1%0 - rhs

# pseudocode:
# rotl(p0, p1) {
#	carry = p0 & (1 << 31)
# 	if p1 > 0 {
#		p0 *= 2
#		p0 += carry;
#		rotl(p0, p1 - 1);
#	}
# }

execute store success score %%temprotl_carry reg if score %param0%0 reg matches ..-1

execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%0 reg += %param0%0 reg
execute if score %param1%0 reg matches 1.. run scoreboard players operation %param0%0 reg += %%temprotl_carry reg
scoreboard players remove %param1%0 reg 1
execute if score %param1%0 reg matches 1.. run function intrinsic:rotl
