/*
I did not write this! I just gave GPT my haskell code and asked it to convert it to java for fun. Then I debugged it a little.
It is correct, and follows the same thought process... I suppose :D
*/

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.*;

public class RuleBasedSorter {
    
    // Rule: a pair of integers
    static class Rule {
        int key, value;

        Rule(int key, int value) {
            this.key = key;
            this.value = value;
        }
    }

    // Reads and parses input file into rules and updates
    public static Pair<List<Rule>, List<List<Integer>>> readAndParseFile(String filePath) throws IOException {
        List<String> lines = Files.readAllLines(Paths.get(filePath));
        List<Rule> rules = new ArrayList<>();
        List<List<Integer>> updates = new ArrayList<>();
        boolean readingRules = true;

        for (String line : lines) {
            if (line.isEmpty()) {
                readingRules = false;
                continue;
            }
            if (readingRules) {
                String[] parts = line.split("\\|");
                rules.add(new Rule(Integer.parseInt(parts[0]), Integer.parseInt(parts[1])));
            } else {
                updates.add(Arrays.stream(line.split(","))
                        .map(Integer::parseInt)
                        .collect(Collectors.toList()));
            }
        }
        return new Pair<>(rules, updates);
    }

    // Compiles a list of rules into a rulebook
    public static Map<Integer, Set<Integer>> compileRulebook(List<Rule> rules) {
        Map<Integer, Set<Integer>> rulebook = new HashMap<>();
        for (Rule rule : rules) {
            rulebook.computeIfAbsent(rule.key, k -> new HashSet<>()).add(rule.value);
        }
        return rulebook;
    }

    // Checks if an update is valid
    public static boolean checkUpdate(Map<Integer, Set<Integer>> rulebook, List<Integer> update) {
        Set<Integer> seen = new HashSet<>();
        for (int i : update) {
            Set<Integer> forbidden = rulebook.getOrDefault(i, Collections.emptySet());
            if (!Collections.disjoint(seen, forbidden)) {
                return false;
            }
            seen.add(i);
        }
        return true;
    }

    // Extracts the middle element of a list (assumes odd length)
    public static int middle(List<Integer> list) {
        return list.get((list.size() - 1) / 2);
    }

    // Solves part 1
    public static int solvePart1(Map<Integer, Set<Integer>> rulebook, List<List<Integer>> updates) {
        return updates.stream()
                .filter(update -> checkUpdate(rulebook, update))
                .mapToInt(RuleBasedSorter::middle)
                .sum();
    }

    // Solves part 2
    public static int solvePart2(Map<Integer, Set<Integer>> rulebook, List<List<Integer>> updates) {
        return updates.stream()
                .filter(update -> !checkUpdate(rulebook, update))
                .map(update -> ruleBasedSort(rulebook, new ArrayList<>(), update))
                .mapToInt(RuleBasedSorter::middle)
                .sum();
    }

    // Sorts numbers into "checked" and "not yet checked" lists
    public static List<Integer> ruleBasedSort(Map<Integer, Set<Integer>> rulebook, List<Integer> checked, List<Integer> unchecked) {
        if (unchecked.isEmpty()) {
            return checked; // Base case
        }
        int i = unchecked.get(0); // Process the first element
        List<Integer> remainingUnchecked = unchecked.subList(1, unchecked.size());

        Set<Integer> forbidden = rulebook.getOrDefault(i, Collections.emptySet());

        if (!Collections.disjoint(checked, forbidden)) {
            // Forbidden numbers exist in checked; reinsert `i` in the correct position
            List<Integer> newChecked = insertBeforeAnyOf(forbidden, checked, i);
            return ruleBasedSort(rulebook, newChecked, remainingUnchecked);
        } else {
            // Add `i` to checked
            List<Integer> newChecked = new ArrayList<>(checked);
            newChecked.add(i);
            return ruleBasedSort(rulebook, newChecked, remainingUnchecked);
        }
    }

    public static List<Integer> insertBeforeAnyOf(Set<Integer> forbidden, List<Integer> list, int e) {
        List<Integer> result = new ArrayList<>();
        for (int i : list) {
            if (forbidden.contains(i)) {
                result.add(e); // Insert before the first forbidden element
                result.addAll(list.subList(result.size() - 1, list.size())); // Append the rest
                return result;
            }
            result.add(i);
        }
        result.add(e); // If no forbidden elements, add at the end
        return result;
    }

    public static void main(String[] args) throws IOException {
        if (args.length != 1) {
            System.out.println("Usage: java RuleBasedSorter <input>");
            return;
        }

        String filePath = args[0];
        Pair<List<Rule>, List<List<Integer>>> input = readAndParseFile(filePath);

        List<Rule> rules = input.first;
        List<List<Integer>> updates = input.second;

        Map<Integer, Set<Integer>> rulebook = compileRulebook(rules);

        int solution1 = solvePart1(rulebook, updates);
        System.out.println(solution1);

        int solution2 = solvePart2(rulebook, updates);
        System.out.println(solution2);
    }
}

// Helper class for returning pairs of values
class Pair<A, B> {
    public final A first;
    public final B second;

    public Pair(A first, B second) {
        this.first = first;
        this.second = second;
    }
}

