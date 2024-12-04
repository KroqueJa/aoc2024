/*
   I hate bounds checking infinitely. Therefore I have devised a plan to solve this problem without them.
   We shall create four matrices with `true` values for 'X', 'M', 'A' and 'S' respectively. Then shift them
   around and see where they overlap. This way there is no bounds checking.

   This could be SIMD:d but that would be even more bounds checking. Bleh.
   */

#include <iostream>
#include <fstream>
#include <vector>
#include <string>

// Class for a matrix that can return shifted copies of itself
template <typename T>
class ShiftyMatrix {
    private:
        std::vector<std::vector<T>> grid;
        size_t H;
        size_t W;

    public:
        ShiftyMatrix(const size_t h, const size_t w) : grid(h, std::vector<T>(w, T())), H(h), W(w) {}

        std::vector<T>& operator[](size_t r) {
            return grid[r];
        }

        const std::vector<T>& operator[](size_t r) const {
            return grid[r];
        }

        size_t getHeight() const {
            return H;
        }

        size_t getWidth() const {
            return W;
        }

        bool operator==(const ShiftyMatrix& rhs) const {
            if (W != rhs.getWidth() || H != rhs.getHeight()) {
                return false;
            }
            for (size_t r = 0; r < H; ++r) {
                for (size_t c = 0; c < W; ++c) {
                    if (grid[r][c] != rhs[r][c]) return false;
                }
            }
            return true;
        }

        void print() const {
            for (size_t r = 0; r < H; ++r) {
                for (size_t c = 0; c < W; ++c) {
                    std::cout << grid[r][c] << " ";
                }
                std::cout << "\n";
            }
            std::cout << std::endl;
        };

        ShiftyMatrix shiftRight(size_t places) const {
            ShiftyMatrix out(H, W);
            size_t shift = std::min(places, W);

            for (size_t r = 0; r < H; ++r) {
                for (size_t c = 0; c < W; ++c) {
                    if (c < shift) {
                        out[r][c] = T();
                    } else {
                        out[r][c] = grid[r][c - shift];
                    }
                }
            }

            return out;
        }

        ShiftyMatrix shiftLeft(size_t places) const {
            ShiftyMatrix out(H, W);
            size_t shift = std::min(places, W);

            for (size_t r = 0; r < H; ++r) {
                for (size_t c = 0; c < W; ++c) {
                    if (c + shift < W) {
                        out[r][c] = grid[r][c + shift];
                    } else {
                        out[r][c] = T();
                    }
                }
            }

            return out;
        }

        ShiftyMatrix shiftUp(size_t places) const {
            ShiftyMatrix out(H, W);
            size_t shift = std::min(places, H);

            for (size_t r = 0; r < H; ++r) {
                if (r + shift < H) {
                    out[r] = grid[r + shift];
                } else {
                    out[r] = std::vector<T>(W, T());
                }
            }

            return out;
        }

        ShiftyMatrix shiftDown(size_t places) const {
            ShiftyMatrix out(H, W);
            size_t shift = std::min(places, H);

            for (size_t r = 0; r < H; ++r) {
                if (r >= shift) {
                    out[r] = grid[r - shift];
                } else {
                    out[r] = std::vector<T>(W, T());
                }
            }

            return out;
        }

        // Returns a `ShiftyMatrix` with only elements that match
        ShiftyMatrix where(const ShiftyMatrix& rhs) {
            if (rhs.getWidth() != W || rhs.getHeight() != H) {
                throw std::runtime_error("Can't `where` matrices of differing dimensions!");
            }
            ShiftyMatrix out(H, W);
            for (size_t r = 0; r < W; ++r) {
                for (size_t c = 0; c < H; ++c) {
                    if (grid[r][c] == rhs[r][c]) {
                        out[r][c] = grid[r][c];
                    }
                }
            }
            return out;
        }

        // Iterators for range-based for loops
        auto begin() { return grid.begin(); }
        auto end() { return grid.end(); }

        auto begin() const { return grid.cbegin(); }
        auto end() const { return grid.cend(); }
};

std::vector<std::vector<char>> readFileToMatrix(const std::string& filename) {
    std::ifstream file(filename);
    if (!file) {
        throw std::runtime_error("Failed to open file: " + filename);
    }

    std::vector<std::vector<char>> matrix;
    std::string line;

    // Read each line
    while (std::getline(file, line)) {
        // Convert the line into a vector<char> and add it to the matrix
        matrix.emplace_back(line.begin(), line.end());
    }

    return matrix;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cout << "Usage: day4 [your_input]" << std::endl;
        exit(0);
    }

    std::string filename = argv[1];

    std::vector<std::vector<char>> fileContents;
    try {
        fileContents = readFileToMatrix(filename);
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << '\n';
    }

    // -------- Part 1 --------

    // Create four shifty matrices
    const size_t height = fileContents.size();
    const size_t width = fileContents[0].size();

    ShiftyMatrix<bool> xs(height, width);
    ShiftyMatrix<bool> ms(height, width);
    ShiftyMatrix<bool> as(height, width);
    ShiftyMatrix<bool> ss(height, width);

    // Iterate over the input, populating the shifty matrices
    for (size_t i = 0; i < height; ++i) {
        for (size_t j = 0; j < width; ++j) {
            switch (fileContents[i][j]) {
                case 'X':
                    xs[i][j] = true;
                    break;
                case 'M':
                    ms[i][j] = true;
                    break;
                case 'A':
                    as[i][j] = true;
                    break;
                case 'S':
                    ss[i][j] = true;
                    break;
                default:
                    break;
            }
        }
    }

    // Shift matrices like a mad person
    ShiftyMatrix<bool> leftToRight = xs.where(ms.shiftLeft(1)).where(as.shiftLeft(2)).where(ss.shiftLeft(3));
    ShiftyMatrix<bool> rightToLeft = xs.where(ms.shiftRight(1)).where(as.shiftRight(2)).where(ss.shiftRight(3));
    ShiftyMatrix<bool> topToBottom = xs.where(ms.shiftUp(1)).where(as.shiftUp(2)).where(ss.shiftUp(3));
    ShiftyMatrix<bool> bottomToTop = xs.where(ms.shiftDown(1)).where(as.shiftDown(2)).where(ss.shiftDown(3));

    // Diagonals
    ShiftyMatrix<bool> toNorthWest = xs.where(ms.shiftLeft(1).shiftUp(1)).where(as.shiftLeft(2).shiftUp(2)).where(ss.shiftLeft(3).shiftUp(3));
    ShiftyMatrix<bool> toNorthEast = xs.where(ms.shiftRight(1).shiftUp(1)).where(as.shiftRight(2).shiftUp(2)).where(ss.shiftRight(3).shiftUp(3));
    ShiftyMatrix<bool> toSouthWest = xs.where(ms.shiftLeft(1).shiftDown(1)).where(as.shiftLeft(2).shiftDown(2)).where(ss.shiftLeft(3).shiftDown(3));
    ShiftyMatrix<bool> toSouthEast = xs.where(ms.shiftRight(1).shiftDown(1)).where(as.shiftRight(2).shiftDown(2)).where(ss.shiftRight(3).shiftDown(3));

    // Count true values
    size_t sum = 0;

    std::vector<ShiftyMatrix<bool>*> matrices = {&leftToRight, &rightToLeft, &topToBottom, &bottomToTop, &toNorthWest, &toNorthEast, &toSouthWest, &toSouthEast};

    for (const auto matrix: matrices) {
        for (const auto& row: *matrix) {
            for (const bool value: row) {
                if (value) ++ sum;
            }
        }
    }

    std::cout << sum << std::endl;
    return 0;
}

