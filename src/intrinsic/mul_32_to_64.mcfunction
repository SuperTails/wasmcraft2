# Arguments:
# %param0%0 - Left operand
# %param1%0 - Right operand
# %return%0 - Low word of product
# %return%1 - High word of product

scoreboard players operation %%tempmul_p0_save reg = %param0%0 reg
scoreboard players operation %%tempmul_p1_save reg = %param1%0 reg

# %0%0 = p0
scoreboard players operation %%tempmul_%0%0 reg = %param0%0 reg
# %1%0 = p1
scoreboard players operation %%tempmul_%1%0 reg = %param1%0 reg

# %5%0 = (p0 & 0xFFFF)
# p0_lo = %5%0
scoreboard players operation %%tempmul_%5%0 reg = %%tempmul_%0%0 reg
scoreboard players operation %%tempmul_%5%0 reg %= %%65536 reg

# %6%0 = (p1 & 0xFFFF)
# p1_lo = %6%0
scoreboard players operation %%tempmul_%6%0 reg = %%tempmul_%1%0 reg
scoreboard players operation %%tempmul_%6%0 reg %= %%65536 reg

# %7%0 = (%5%0 * %6%0)
# lo_product = %7%0
scoreboard players operation %%tempmul_%7%0 reg = %%tempmul_%6%0 reg
scoreboard players operation %%tempmul_%7%0 reg *= %%tempmul_%5%0 reg

# %8%0 = (%7%0 & 0xFFFF)
# lo_product_lo = lo_product & 0xFFFF;
scoreboard players operation %%tempmul_%8%0 reg = %%tempmul_%7%0 reg
scoreboard players operation %%tempmul_%8%0 reg %= %%65536 reg

# %9%0 = (lo_product >> 16) & 0xFFFF;
# lo_product_hi = %9%0
scoreboard players set %%tempmul_%temp3 reg 16
scoreboard players operation %param0%0 reg = %%tempmul_%7%0 reg
scoreboard players operation %param1%0 reg = %%tempmul_%temp3 reg
function intrinsic:lshr
scoreboard players operation %%tempmul_%9%0 reg = %param0%0 reg

# %10%0 = (p0 >> 16)
# p0_hi = %10%0
scoreboard players set %%tempmul_%temp4 reg 16
scoreboard players operation %param0%0 reg = %%tempmul_%0%0 reg
scoreboard players operation %param1%0 reg = %%tempmul_%temp4 reg
function intrinsic:lshr
scoreboard players operation %%tempmul_%10%0 reg = %param0%0 reg

# %11%0 = p1_lo * p0_hi
# mid_product_1 = %11%0
# %12%0 = lo_product_hi + mid_product_1
scoreboard players operation %%tempmul_%11%0 reg = %%tempmul_%6%0 reg
scoreboard players operation %%tempmul_%11%0 reg *= %%tempmul_%10%0 reg
scoreboard players operation %%tempmul_%12%0 reg = %%tempmul_%9%0 reg
scoreboard players operation %%tempmul_%12%0 reg += %%tempmul_%11%0 reg

# %13%0 = (lo_product_hi + mid_product_1) & 0xFFFF
scoreboard players operation %%tempmul_%13%0 reg = %%tempmul_%12%0 reg
scoreboard players operation %%tempmul_%13%0 reg %= %%65536 reg

scoreboard players set %%tempmul_%temp6 reg 16
scoreboard players operation %param0%0 reg = %%tempmul_%12%0 reg
scoreboard players operation %param1%0 reg = %%tempmul_%temp6 reg
function intrinsic:lshr
scoreboard players operation %%tempmul_%14%0 reg = %param0%0 reg
scoreboard players set %%tempmul_%temp7 reg 16
scoreboard players operation %param0%0 reg = %%tempmul_%1%0 reg
scoreboard players operation %param1%0 reg = %%tempmul_%temp7 reg
function intrinsic:lshr
scoreboard players operation %%tempmul_%15%0 reg = %param0%0 reg
scoreboard players operation %%tempmul_%16%0 reg = %%tempmul_%15%0 reg
scoreboard players operation %%tempmul_%16%0 reg *= %%tempmul_%5%0 reg
scoreboard players operation %%tempmul_%17%0 reg = %%tempmul_%13%0 reg
scoreboard players operation %%tempmul_%17%0 reg += %%tempmul_%16%0 reg
scoreboard players set %%tempmul_%temp8 reg 16
scoreboard players operation %param0%0 reg = %%tempmul_%17%0 reg
scoreboard players operation %param1%0 reg = %%tempmul_%temp8 reg
function intrinsic:lshr
scoreboard players operation %%tempmul_%18%0 reg = %param0%0 reg

scoreboard players operation %%tempmul_%19%0 reg = %%tempmul_%15%0 reg
scoreboard players operation %%tempmul_%19%0 reg *= %%tempmul_%10%0 reg
scoreboard players operation %%tempmul_%20%0 reg = %%tempmul_%14%0 reg
scoreboard players operation %%tempmul_%20%0 reg += %%tempmul_%19%0 reg
scoreboard players operation %%tempmul_%21%0 reg = %%tempmul_%17%0 reg
scoreboard players operation %%tempmul_%21%0 reg *= %%65536 reg
scoreboard players operation %%tempmul_%22%0 reg = %%tempmul_%20%0 reg
scoreboard players operation %%tempmul_%22%0 reg += %%tempmul_%18%0 reg


scoreboard players operation %param0%0 reg = %%tempmul_%21%0 reg
scoreboard players operation %param1%0 reg = %%tempmul_%8%0 reg
function intrinsic:or
scoreboard players operation %%tempmul_%23%0 reg = %return%0 reg

scoreboard players operation %return%0 reg = %%tempmul_%23%0 reg
scoreboard players operation %return%1 reg = %%tempmul_%22%0 reg

scoreboard players operation %param0%0 reg = %%tempmul_p0_save reg
scoreboard players operation %param1%0 reg = %%tempmul_p1_save reg