#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define MAX_NUM_PATHS 8192

typedef struct TrailScore {
    uint64_t score;
    uint64_t rating;
} TrailScore;

typedef struct MapInfo {
    int** map;
    size_t height;
    size_t width;
} MapInfo;


// These hash shenanigans are incredibly unsafe - the code crashes randomly if MAX_NUM_PATHS is too low :D
void insert_path(bool* paths, int path) {
    int index = (path + 1) % MAX_NUM_PATHS; // Hash function
    int start_index = index;

    while (paths[index]) {
        index = (index + 1) % MAX_NUM_PATHS;
        if (index == start_index) {
            // Table is full
            printf("Error: Hash map is full\n");
            return;
        }
    }

    paths[index] = true; // Insert the value
}

bool find_path(bool* paths, int path) {
    int index = (path + 1) % MAX_NUM_PATHS;
    int start_index = index;

    while (paths[index]) {
        if (index == path) {
            return true;
        }
        index = (index + 1) % MAX_NUM_PATHS;
        if (index == start_index) {
            break;
        }
    }

    return false;
}

void score_trail(TrailScore* score, const MapInfo* mapInfo, bool* paths, int current_path, bool** visited, bool has_passed_visited, const size_t row, const size_t col, const int from_height) {

    // If we are out of bounds or already visited, we are done with this branch
    if (row < 0 || row >= mapInfo->height || col < 0 || col >= mapInfo->width) return;

    // Likewise, if this was an invalid step, we don't score this step
    const int here_height = mapInfo->map[row][col];
    if (here_height - from_height != 1) return;

    // If we've been here before, we've been here before
    has_passed_visited = visited[row][col];

    // Mark the tile as visited
    visited[row][col] = true;

    current_path = ((current_path << 5) | (current_path >> 27)) ^ (row * 31 + col);

    // If we are on a 9, increase the score
    if (here_height == 9) {
        if (!has_passed_visited) score->score += 1;
        if (!find_path(paths, current_path)) {
            // We've never taken this path here
            insert_path(paths, current_path);
            score->rating += 1;
        }
    }

    // Recurse in all cardinal directions and accumulate the score
    score_trail(score, mapInfo, paths, current_path, visited, has_passed_visited, row-1, col, here_height);
    score_trail(score, mapInfo, paths, current_path, visited, has_passed_visited, row+1, col, here_height);
    score_trail(score, mapInfo, paths, current_path, visited, has_passed_visited, row, col-1, here_height);
    score_trail(score, mapInfo, paths, current_path, visited, has_passed_visited, row, col+1, here_height);
}

void clear_paths(bool* paths)
{
    size_t i = 0;
    while (i < MAX_NUM_PATHS && paths[i]) {
        paths[i] = false;
        ++i;
    }
}

void clear_visited(bool** visited, const size_t height, const size_t width)
{
    for (size_t r = 0; r < height; ++r) {
        for (size_t c = 0; c < width; ++c) {
            visited[r][c] = false;
        }
    }
}

int main(int argc, char** argv)
{
    if (argc != 2) {
        printf("Usage: day10 [your_input]\n");
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

    // Find height and width of grid
    size_t width = 0;
    while (buf[++width] != '\n');
    
    size_t height = f_sz / width - 1;

    // Read numbers onto grid
    int** map = malloc(height * sizeof(int*));
    bool** visited = malloc(height * sizeof(bool*));

    for (size_t r = 0; r < height; ++r) {
        map[r] = (int*)calloc(width, sizeof(int));
        visited[r] = (bool*)calloc(width, sizeof(bool));
        for (size_t c = 0; c < width; ++c) {
            map[r][c] = buf[r * width + c + r] - '0';
        }
    }

    MapInfo mapInfo = {map, height, width};
    bool* paths = (bool*)calloc(MAX_NUM_PATHS, sizeof(bool));

    TrailScore score = {0, 0};
    for (size_t r = 0; r < height; ++r) {
        for (size_t c = 0; c < width; ++c) {
            if (map[r][c] == 0) {
                clear_paths(paths);
                clear_visited(visited, height, width);
                score_trail(&score, &mapInfo, paths, 0, visited, false, r, c, -1);  // Start at height -1 so the recursion doesn't fail
            }
        }
    }

    printf("%llu\n", score.score);
    printf("%llu\n", score.rating);


    for (size_t r = 0; r < height; ++r) {
        free(map[r]);
        free(visited[r]);
    }
    free(map);
    free(visited);
    free(paths);

    return 0;
}
