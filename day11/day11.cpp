#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <unordered_map>
#include <future>


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
        isSingle = true;  // Only one result
        return {1, 0};
    }

    uint64_t digits = numDigits(number);
    if (digits % 2 == 0) {
        uint64_t denom = 1;
        for (uint16_t i = 0; i < digits / 2; ++i) denom *= 10;
        uint64_t left = number / denom;
        uint64_t right = number % denom;
        isSingle = false;  // Two results
        return {left, right};
    }

    isSingle = true;  // Only one result
    return {2024 * number, 0};
}


uint64_t inspect(
    std::unordered_map<MemoKey, uint64_t>& precalc,
    const uint64_t number,
    const uint64_t generations) {
    MemoKey key(number, generations);

    // Base case: If no generations are left
    if (generations == 0) {
        return 1; // A single valid sequence
    }

    // Check if already precalculated
    auto it = precalc.find(key);
    if (it != precalc.end()) {
        return it->second; // Return cached result
    }

    uint64_t result = 0;

    if (number == 0 || number == 1) {
        // Direct recursion for trivial cases
        result = inspect(precalc, number, generations - 1);
    } else {
        // Calculate the number of digits
        uint16_t numDigits = log10(number) + 1;

        if (numDigits % 2 == 0) {
            // Split number into left and right parts
            uint64_t denom = 1;
            for (uint16_t i = 0; i < numDigits / 2; ++i) denom *= 10;

            uint64_t left = number / denom;
            uint64_t right = number % denom;

            // Accumulate results directly from recursive calls
            result += inspect(precalc, left, generations - 1);
            result += inspect(precalc, right, generations - 1);
        } else {
            // Handle odd-digit case
            uint64_t newNumber = 2024 * number;
            result = inspect(precalc, newNumber, generations - 1);
        }
    }

    // Cache the computed result
    precalc[key] = result;
    return result;
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
    uint64_t sum = 0;
    for (uint64_t number : numbers) {
        sum += inspect(precalc, number, 75);
    }

    std::cout << sum << std::endl;
    return 0;
}
