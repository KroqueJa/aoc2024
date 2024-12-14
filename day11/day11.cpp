#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <future>

constexpr uint64_t CALCULATED_GENS = 75;

struct MemoKey {
    uint64_t number;
    uint64_t generations;

    MemoKey(const uint64_t number, const uint64_t generations)
        : number(number), generations(generations) {}

    bool operator<(const MemoKey& rhs) {
        if (number == rhs.number) {
            return generations < rhs.generations;
        }
        return number < rhs.number;
    }

    bool operator==(const MemoKey& rhs) const {
        return number == rhs.number && generations == rhs.generations;
    }
};

namespace std {
template <>
struct hash<MemoKey> {
    std::size_t operator()(const MemoKey& key) const noexcept {
        std::size_t h1 = std::hash<uint64_t>{}(key.number);
        std::size_t h2 = std::hash<uint64_t>{}(key.generations);
        return h1 ^ (h2 << 1);
    }
};
}  // namespace std

inline uint16_t numDigits(uint64_t number) {
    uint16_t digits = 0;
    while (number) {
        number /= 10;
        ++digits;
    }
    return digits;
}

std::pair<uint64_t, uint64_t> updateStone(const uint64_t number, bool& isSingle) {
    if (number == 0) {
        isSingle = true; // Only one result
        return {1, 0};
    }

    uint64_t digits = numDigits(number);
    if (digits % 2 == 0) {
        uint64_t denom = 1;
        for (uint16_t i = 0; i < digits / 2; ++i) denom *= 10;
        uint64_t left = number / denom;
        uint64_t right = number % denom;
        isSingle = false; // Two results
        return {left, right};
    }

    isSingle = true; // Only one result
    return {2024 * number, 0};
}

uint64_t search(std::unordered_map<MemoKey, uint64_t>& precalc, uint64_t target, uint64_t number, int depth = 0) {
    if (target == 0) {
        return 1; // Base case
    }

    // Fast-forward for single-digit numbers
    if (number < 10) {
        MemoKey key(number, target);
        auto resultIt = precalc.find(key);
        if (resultIt != precalc.end()) {
            return resultIt->second;
        }
    }

    bool isSingle;
    auto stones = updateStone(number, isSingle);

    if (isSingle) {
        // Single stone: run search synchronously
        return search(precalc, target - 1, stones.first, depth);
    } else if (depth < 2) {
        // Two stones: run each branch asynchronously
        auto future1 = std::async(std::launch::async, [&] {
            return search(precalc, target - 1, stones.first, depth + 1);
        });
        auto future2 = std::async(std::launch::async, [&] {
            return search(precalc, target - 1, stones.second, depth + 1);
        });

        // Wait for both results and combine them
        uint64_t result1 = future1.get();
        uint64_t result2 = future2.get();

        return result1 + result2;
    } else {
        // Deeper levels: run synchronously
        uint64_t result1 = search(precalc, target - 1, stones.first, depth + 1);
        uint64_t result2 = search(precalc, target - 1, stones.second, depth + 1);

        return result1 + result2;
    }
}

void inspect(
    std::unordered_map<MemoKey, uint64_t>& precalc,
    const uint64_t number,
    const uint64_t generations) {
    MemoKey key(number, generations);

    if (generations == 0) {
        precalc[key] = 1;
        return;
    }

    // Check if already precalculated
    if (precalc.find(key) != precalc.end()) {
        return;
    }

    bool isSingle;
    auto stones = updateStone(number, isSingle);

    uint64_t result = 0;
    if (isSingle) {
        inspect(precalc, stones.first, generations - 1);
        result += precalc[MemoKey(stones.first, generations - 1)];
    } else {
        inspect(precalc, stones.first, generations - 1);
        inspect(precalc, stones.second, generations - 1);
        result += precalc[MemoKey(stones.first, generations - 1)];
        result += precalc[MemoKey(stones.second, generations - 1)];
    }

    precalc[key] = result;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cout << "Usage: day11 [your_input]" << std::endl;
        return 0;
    }

    std::string filename = argv[1];
    std::vector<uint64_t> numbers;

    try {
        std::ifstream inputFile(filename);
        if (!inputFile.is_open()) {
            throw std::runtime_error("Failed to open file: " + filename);
        }

        uint64_t number;
        while (inputFile >> number) {
            numbers.push_back(number);
        }
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    std::unordered_map<MemoKey, uint64_t> precalc;
    uint64_t target = 75;

    std::vector<std::future<uint64_t>> futures;
    // Precalculate results for single-digit numbers
    for (uint64_t i = 0; i < 400000; ++i) {
        inspect(precalc, i, CALCULATED_GENS);
    }

    for (uint64_t number : numbers) {
        futures.emplace_back(std::async(std::launch::async, [&precalc, target, number]() {
            return search(precalc, target, number);
        }));
    }

    // Gather results
    uint64_t sum = 0;
    for (auto& future : futures) {
        sum += future.get();
    }

    std::cout << sum << std::endl;
    return 0;
}
