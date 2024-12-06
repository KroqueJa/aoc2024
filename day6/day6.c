#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

// Incrementing mod 4 turns right
enum Direction {
    NORTH = 0,
    EAST = 1,
    SOUTH = 2,
    WEST = 3
};

typedef struct GridNode {
    uint16_t row;
    uint16_t col;
    bool blocker;
    bool visited;
    struct GridNode* neighbors[4];
} GridNode;

typedef struct Guard {
    GridNode* node;
    enum Direction direction;
    size_t nodes_visited;
} Guard;

Guard init_guard(GridNode* node, enum Direction direction)
{
    Guard guard;
    guard.node = node;
    guard.direction = direction;
    guard.nodes_visited = 1;

    return guard;
}

GridNode* init_grid(uint16_t rows, uint16_t cols, char* charGrid) {
    GridNode* grid = calloc(rows * cols, sizeof(GridNode));

    // Initialize each GridNode
    for (uint16_t r = 0; r < rows; ++r) {
        for (uint16_t c = 0; c < cols; ++c) {
            GridNode* node = &grid[r * cols + c];
            node->row = r;
            node->col = c;

            // Adjust index for line breaks
            size_t charGridIndex = r * (cols + 1) + c;  // +1 accounts for '\n'
            if (charGrid[charGridIndex] == '#') {
                node->blocker = true;
            }
            else {
                node->blocker = false;
            }
            node->visited = false;

            // Set neighbors
            node->neighbors[NORTH] = (r > 0) ? &grid[(r - 1) * cols + c] : NULL;
            node->neighbors[SOUTH] = (r < rows - 1) ? &grid[(r + 1) * cols + c] : NULL;
            node->neighbors[WEST] = (c > 0) ? &grid[r * cols + c - 1] : NULL;
            node->neighbors[EAST] = (c < cols - 1) ? &grid[r * cols + c + 1] : NULL;
        }
    }

    return grid;
}

void step(Guard* guard)
{
    // Wherever the guard is, the node is now visited
    // (This gives us some redundant bool writes)
    if (guard->node == NULL) {
        return; // Stop if the guard is on a NULL node
    }
    guard->node->visited = true;

    GridNode* next = guard->node->neighbors[guard->direction];


    if (next != NULL && next->blocker) {
        // Turn the guard
        guard->direction = (guard->direction + 1) % 4;
        return;
    }

    // Otherwise, take one step
    guard->node = next;
    if (guard->node != NULL && !guard->node->visited) ++(guard->nodes_visited);  // Don't count steps over already visited nodes

}

GridNode* find(GridNode* cursor, uint16_t row, uint16_t col)
{
    while (cursor->row != row) cursor = cursor->neighbors[SOUTH];
    while (cursor->col != col) cursor = cursor->neighbors[EAST];

    return cursor;
}

int main(int argc, char** argv)
{
    if (argc != 2) {
        printf("Usage: day6 [your_input]\n");
        exit(0);
    }

    FILE* f = fopen(argv[1], "rb");
    if (f == NULL) {
        printf("ERROR | Could not open file %s\n", argv[1]);
        exit(1);
    }

    // Read file into buffer
    fseek(f, 0L, SEEK_END);
    size_t f_sz = ftell(f);

    char buf[f_sz+1];
    // Rewind file
    fseek(f, 0L, SEEK_SET);

    fread(buf, 1, f_sz, f);

    fclose(f);

    // Set up char buffer
    size_t line_width = 0;
    while (buf[line_width++] != '\n');

    size_t num_lines = f_sz / line_width;

    char* charGrid = malloc(line_width * num_lines * sizeof(char));

    uint16_t guard_row, guard_col;
    for (size_t r = 0; r < num_lines; ++r) {
        for (size_t c = 0; c < line_width-1; ++c) {
            charGrid[line_width * r + c] = buf[line_width * r + c];
            // Save the guard's position for later
            // (For completeness we should probably search for '<', '>' and 'v' too but meh
            if (charGrid[line_width * r + c] == '^') {
                guard_row = r;
                guard_col = c;
            }
        }
    }

    // Initialize grid
    GridNode* grid = init_grid(num_lines, line_width - 1, charGrid);

    // Free the charGrid, we no longer need it
    free(charGrid);

    // Place guard on grid
    GridNode* start = find(grid, guard_row, guard_col);
    Guard guard = init_guard(start, NORTH);

    // ======== PART 1 ========

    // Just start walking and turning!
    while (guard.node != NULL) {
        step(&guard);
    }

    printf("%lu\n", guard.nodes_visited);

    // ======== PART 2 ========
    // If I'd been "truer" with the grid (actually not representing paths into blockers) there would probably have been a fancy node-removal algo to use for this
    // Now instead we do loop detection, by introducing a second guard who takes two steps every time the first guard takes one. If they ever face the same direction
    // on the same node, there's a loop.
    // Or, we just take an enormous amount of steps and if we're not done then there's a loop.

    Guard guard2 = init_guard(start, NORTH);

    size_t result = 0;
    size_t steps_taken = 0;
    for (size_t r = 0; r < num_lines; ++r) {
        for (size_t c = 0; c < line_width - 1; ++c) {

            // Place an obstacle
            GridNode* obstacle_node = find(grid, r, c);

            // Skip if the obstacle node is already a blocker or it's the start node
            if (obstacle_node->blocker || obstacle_node == start) continue;

            obstacle_node->blocker = true;

            // Reset the guard
            guard = init_guard(start, NORTH);
            steps_taken = 0;

            // Take a gazillion steps (or 5400, which seems to be enough)
            while (guard.node != NULL && steps_taken < 5400) {
                step(&guard);
                ++steps_taken;
            }

            // If we're still on the grid after many steps, there's a loop
            if (guard.node != NULL) {
                ++result;
            }

            // Remove the obstacle
            obstacle_node->blocker = false;
        }
    }


    printf("%lu\n", result);

    // Free the grid
    free(grid);
}