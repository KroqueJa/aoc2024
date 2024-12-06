#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <pthread.h>
#include <stdatomic.h>

// Incrementing mod 4 turns right
enum Direction {
    NONE = -1,
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
    enum Direction visited_dir;
    struct GridNode* neighbors[4];
} GridNode;

typedef struct Guard {
    GridNode* node;
    enum Direction direction;
    size_t nodes_visited;
} Guard;

typedef struct {
    GridNode* grid;
    GridNode* start;
    uint16_t rows;
    uint16_t cols;
    atomic_size_t* result;
    size_t start_row;
    size_t end_row;
} ThreadData;

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
            node->visited_dir = NONE;

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
    if (guard->node == NULL) {
        return; // Stop if the guard is on a NULL node
    }

    if (guard->node->visited_dir == guard->direction) {
        return; // Loop detected, stop the guard
    }
    
    guard->node->visited_dir = guard->direction;
    guard->node->visited = true;

    GridNode* next = guard->node->neighbors[guard->direction];

    while (next != NULL && next->blocker) {
        // Turn the guard
        guard->direction = (guard->direction + 1) % 4;
        next = guard->node->neighbors[guard->direction];
    }

    // Move to the next node
    guard->node = next;
    if (guard->node != NULL && !guard->node->visited) ++(guard->nodes_visited);
}

void clear_visited(GridNode* grid, uint16_t rows, uint16_t cols) {
    for (uint16_t r = 0; r < rows; ++r) {
        for (uint16_t c = 0; c < cols; ++c) {
            GridNode* node = &grid[r * cols + c];
            node->visited_dir = NONE;
        }
    }
}

GridNode* find(GridNode* cursor, uint16_t row, uint16_t col)
{
    while (cursor->row != row) cursor = cursor->neighbors[SOUTH];
    while (cursor->col != col) cursor = cursor->neighbors[EAST];

    return cursor;
}

size_t solve_part_1(GridNode* grid, Guard* guard)
{
    while (guard->node != NULL) step(guard);
    return guard->nodes_visited;
}


void* solve_part_2_worker(void* arg) {
    ThreadData* data = (ThreadData*)arg;

    for (size_t r = data->start_row; r < data->end_row; ++r) {
        for (size_t c = 0; c < data->cols; ++c) {
            Guard guard = init_guard(data->start, NORTH);

            // Place an obstacle
            GridNode* obstacle_node = find(data->grid, r, c);

            // Skip if the obstacle node is already a blocker, the start node, or outside the guard's original path
            if (obstacle_node->blocker || obstacle_node == data->start || !obstacle_node->visited) continue;

            obstacle_node->blocker = true;

            // Clear the grid's visited_dir markers
            clear_visited(data->grid, data->rows, data->cols);

            bool loop_detected = false;
            while (guard.node != NULL) {
                if (guard.node->visited_dir == guard.direction) {
                    loop_detected = true;
                    break;
                }
                step(&guard);
            }

            if (loop_detected) {
                atomic_fetch_add(data->result, 1);
            }

            // Remove the obstacle
            obstacle_node->blocker = false;
        }
    }

    return NULL;
}


size_t solve_part_2_parallel(GridNode* grid, GridNode* start, uint16_t rows, uint16_t cols, int num_threads) {
    pthread_t threads[num_threads];
    ThreadData thread_data[num_threads];
    atomic_size_t result = 0;

    size_t rows_per_thread = rows / num_threads;
    size_t remaining_rows = rows % num_threads;

    size_t current_row = 0;
    for (int i = 0; i < num_threads; ++i) {
        thread_data[i].grid = grid;
        thread_data[i].start = start;
        thread_data[i].rows = rows;
        thread_data[i].cols = cols;
        thread_data[i].result = &result;
        thread_data[i].start_row = current_row;
        thread_data[i].end_row = current_row + rows_per_thread + (i < remaining_rows ? 1 : 0);

        pthread_create(&threads[i], NULL, solve_part_2_worker, &thread_data[i]);
        current_row = thread_data[i].end_row;
    }

    for (int i = 0; i < num_threads; ++i) {
        pthread_join(threads[i], NULL);
    }

    return result;
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

    char buf[f_sz + 1];
    fseek(f, 0L, SEEK_SET);

    fread(buf, 1, f_sz, f);
    fclose(f);

    size_t line_width = 0;
    while (buf[line_width++] != '\n');

    size_t num_lines = f_sz / line_width;

    char* charGrid = malloc(line_width * num_lines * sizeof(char));
    uint16_t guard_row, guard_col;

    for (size_t r = 0; r < num_lines; ++r) {
        for (size_t c = 0; c < line_width - 1; ++c) {
            charGrid[line_width * r + c] = buf[line_width * r + c];
            if (charGrid[line_width * r + c] == '^') {
                guard_row = r;
                guard_col = c;
            }
        }
    }

    GridNode* grid = init_grid(num_lines, line_width - 1, charGrid);
    free(charGrid);

    GridNode* start = find(grid, guard_row, guard_col);
    Guard guard = init_guard(start, NORTH);

    size_t result1 = solve_part_1(grid, &guard);
    printf("%lu\n", result1);

    int num_threads = 4; // Adjust based on your system
    size_t result2 = solve_part_2_parallel(grid, start, num_lines, line_width - 1, num_threads);
    printf("%lu\n", result2);

    free(grid);
    return 0;
}
