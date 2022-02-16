execute store result score %work%1%23%lo%temp reg run data get storage wasm:localstack stack.data[6]
execute store result score %work%1%23%hi%temp reg run data get storage wasm:localstack stack.data[7]
scoreboard players operation %temp%0%lo reg = %work%1%23%lo%temp reg
scoreboard players operation %temp%0%hi reg = %work%1%23%hi%temp reg
scoreboard players operation %work%1%24%lo%temp reg = %temp%0%lo reg
scoreboard players operation %work%1%24%hi%temp reg = %temp%0%hi reg
function intrinsic:i64divrem/wasm_1_1
scoreboard players set %condtaken reg 1