scoreboard players operation %return%0%lo reg = %work%0%34%lo%temp reg
scoreboard players operation %return%0%hi reg = %work%0%34%hi%temp reg
data modify storage wasm:localstack stack set from storage wasm:localstack stack.tail
scoreboard players set %condtaken reg 1