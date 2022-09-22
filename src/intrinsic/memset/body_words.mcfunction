#
# i32 x = %mst_x
# i32 y = %mst_y
# i32 z = %mst_z
#
# i32 value = %mst_value_word
# i32 bytes = %mst_length

# C pseudocode:
#
# i32 x, y, z;
# i32 value;
# i32 bytes;
#
# setptr(x, y, z);
#
# void memset_body_words() {
#	assert(bytes >= 4)
#   do {
#		*memoryptr = value;
#		
#		++z;
#		if (z == PAGE_SPAN_Z) {
#			++y;
#		}
#		if (y == PAGE_SPAN_Y) {
#			++x;
#		}
#
#
#		if (y == PAGE_SPAN_Y) {
#			setptr_x(x);
#			y = 0;
#		}
#
#		if (z == PAGE_SPAN_Z) {
#			setptr_y(y);
#			z = 0;
#		}
#
#		setptr_z(z);
#
#		bytes -= 4;
#   } while (bytes >= 4);
# }

execute at @s store result block ~ ~ ~ RecordItem.tag.Memory int 1 run scoreboard players get %mst_value_word reg

scoreboard players add %mst_z reg 1
execute if score %mst_z reg = %%PAGE_SPAN_Z reg run scoreboard players add %mst_y reg 1
execute if score %mst_y reg = %%PAGE_SPAN_Y reg run scoreboard players add %mst_x reg 1

execute if score %mst_y reg = %%PAGE_SPAN_Y reg store result entity @s Pos[0] double 1 run scoreboard players get %mst_x reg
execute if score %mst_y reg = %%PAGE_SPAN_Y reg run scoreboard players set %mst_y reg 0

execute if score %mst_z reg = %%PAGE_SPAN_Z reg store result entity @s Pos[1] double 1 run scoreboard players get %mst_y reg
execute if score %mst_z reg = %%PAGE_SPAN_Z reg run scoreboard players set %mst_z reg 0

execute store result entity @s Pos[2] double 1 run scoreboard players get %mst_z reg

scoreboard players remove %mst_length reg 4
scoreboard players add %param0%0 reg 4

execute if score %mst_length reg matches 4.. run function intrinsic:memset/body_words