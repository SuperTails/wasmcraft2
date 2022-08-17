#ifndef MCINTERFACE_H_DEFINED
#define MCINTERFACE_H_DEFINED

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

// Fills a volume relative to the turtle's postion.
// The x, y, and z span arguments are effectively the size of the region minus one,
// so `turtle_fill(block, 0, 0, 0)` is equivalent to `turtle_set(block)`
// This function is UNSTABLE and MAY NOT COMPILE.
extern void turtle_fill(enum Block block, int x_span, int y_span, int z_span);

// Sets the block at the turtle's position.
extern void turtle_set(enum Block block);

// Returns the block at the turtle's position
extern enum Block turtle_get(void);

// Returns 1 if the block at the turtle's position matches the argument
inline int turtle_check(enum Block block) {
    return block == turtle_get();
}

extern int turtle_get_char(void);

// Pauses execution and continues it on the next tick
extern void mc_sleep();

extern void mc_putc(int ch);

#ifdef __cplusplus
}
#endif

#endif