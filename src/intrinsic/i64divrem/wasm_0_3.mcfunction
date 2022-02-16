execute store result score %work%0%33%lo%temp reg run data get storage wasm:localstack stack.data[8]
execute store result score %work%0%33%hi%temp reg run data get storage wasm:localstack stack.data[9]
scoreboard players operation %temp%0%lo reg = %work%0%33%lo%temp reg
scoreboard players operation %temp%0%hi reg = %work%0%33%hi%temp reg
scoreboard players operation %work%0%34%lo%temp reg = %temp%0%lo reg
scoreboard players operation %work%0%34%hi%temp reg = %temp%0%hi reg
function intrinsic:i64divrem/wasm_0_1
scoreboard players set %condtaken reg 1