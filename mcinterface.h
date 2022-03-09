#ifdef __cplusplus
extern "C" {
#endif

extern void print(int value);

enum Block {
    AIR,
    COBBLESTONE,
    GRANITE,
    ANDESITE,
    DIORITE,
    LAPIS_BLOCK,
    IRON_BLOCK,
    GOLD_BLOCK,
    DIAMOND_BLOCK,
    REDSTONE_BLOCK,
    EMERALD_BLOCK,
    DIRT_BLOCK,
    OAK_LOG_BLOCK,
    OAK_LEAVES_BLOCK,
};

// ptr must be aligned to 32 bytes
extern void store_8(int *ptr, int value);

extern void turtle_x(int value);
extern void turtle_y(int value);
extern void turtle_z(int value);

// Sets the block at the turtle's position
extern void turtle_set(enum Block block);

// Returns the block at the turtle's position
extern enum Block turtle_get(void);

// Returns 1 if the block at the turtle's position matches the argument
static int turtle_check(enum Block block) {
    return block == turtle_get();
}

extern int turtle_get_char(void);

// Pauses execution and continues it on the next tick
extern void sleep();

#ifdef __cplusplus
}
#endif