
function intrinsic:setptr
execute at @e[tag=memoryptr] store result score %return%0%lo reg run data get block ~ ~ ~ RecordItem.tag.Memory 1
execute at @e[tag=memoryptr] store result score %return%0%hi reg run data get block ~ ~ ~1 RecordItem.tag.Memory 1