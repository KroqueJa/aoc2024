import numpy as np
import sys
from typing import Tuple, List

def parse(contents: bytes) -> Tuple[List[int], List[int]]:
    """
    Parses binary contents into two lists of integers. This is done without file splitting etc
    in an attempt to make the code as fast as possible.
    - Parses a number into the left list.
    - Skips spaces.
    - Parses a number into the right list.
    - Skips a newline.
    
    Args:
        contents: A bytes object containing the input data.
    
    Returns:
        A tuple of two lists: (left_list, right_list).
    """
    left_list = []
    right_list = []
    i = 0
    n = len(contents)
    
    while i < n:
        # Parse a number into the left list
        start = i
        while i < n and contents[i] >= ord('0') and contents[i] <= ord('9'):
            i += 1
        left_list.append(int(contents[start:i]))
        
        # Skip spaces
        while i < n and contents[i] == ord(' '):
            i += 1
        
        # Parse a number into the right list
        start = i
        while i < n and contents[i] >= ord('0') and contents[i] <= ord('9'):
            i += 1
        right_list.append(int(contents[start:i]))
        
        # Skip newline
        while i < n and (contents[i] == ord('\n') or contents[i] == ord('\r')):
            i += 1
    
    return left_list, right_list

def solve_part1(l1: List[int], l2: List[int]) -> int:
    '''
    Solves the first part of the problem by subtracting the two lists and taking the absolute value
    of the result.
    '''
    l1.sort()
    l2.sort()
    a1 = np.array(l1)
    a2 = np.array(l2)

    return np.sum(np.abs(a1 - a2))


def solve_part2(l1: List[int], l2: List[int]) -> int:
    # Create the counts using np.bincount
    counts = np.bincount(l2)
    similarity = 0
    
    # Iterate over l1 and calculate similarity
    for num in l1:
        if num < len(counts) and counts[num] > 0:  # Check if num is a valid "key" in counts
            similarity += num * counts[num]
    
    return similarity

if __name__ == '__main__':
    argc = len(sys.argv)
    if argc < 2:
        print('Usage: python ay1.py [input_file]')
        exit(0)
    filename = sys.argv[1]

    with open(filename, 'rb') as f:
        contents = f.read()

    l1, l2 = parse(contents)

    result1 = solve_part1(l1, l2)
    print(f'Part 1: {result1}')

    result2 = solve_part2(l1, l2)
    print(f'Part 2: {result2}')
