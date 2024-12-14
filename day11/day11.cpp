#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>

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

std::vector<uint64_t> updateStone(const uint64_t number) {
    if (number == 0) return {1};

    uint64_t digits = numDigits(number);
    if (digits % 2 == 0) {
        uint64_t denom = 1;
        for (uint16_t i = 0; i < digits / 2; ++i) denom *= 10;
        uint64_t left = number / denom;
        uint64_t right = number % denom;
        return {left, right};
    }

    return {2024 * number};
}

uint64_t search(std::unordered_map<MemoKey, uint64_t>& precalc, uint64_t target, uint64_t number) {
    if (target == 0) {
        // We are done!
        return 1;
    }

    // Check if we can fast-forward
    if (number < 10) {
        MemoKey key(number, target);
        auto resultIt = precalc.find(key);
        if (resultIt != precalc.end()) {
            // Precalculated result found
            return resultIt->second;
        }


    }
    std::vector<uint64_t> stones = updateStone(number);
    uint64_t result = 0;
    for (auto stone : stones) {
        result += search(precalc, target - 1, stone);
    }
    return result;
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

    // Check if already precalcized
    if (precalc.find(key) != precalc.end()) {
        return;
    }

    std::vector<uint64_t> stones = updateStone(number);
    uint64_t result = 0;

    for (auto stone : stones) {
        inspect(precalc, stone, generations - 1);
        result += precalc[MemoKey(stone, generations - 1)];
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

    // Precalculate results for single-digit numbers
    for (uint64_t i = 0; i < 400000; ++i) {
        inspect(precalc, i, CALCULATED_GENS);
    }

    uint64_t sum = 0;
    for (uint64_t number : numbers) {
        sum += search(precalc, target, number);
    }
    std::cout << sum << std::endl;

    return 0;
}
