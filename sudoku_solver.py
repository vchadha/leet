class Solution:
    BOARD_SIZE: int = 9
    BOX_SIZE: int = 3
    POSSIBLE_CELL_VALUES: frozenset[int] = frozenset(range(1, BOARD_SIZE + 1))

    BLANK_CELL_VALUE: str = "."

    def solveSudoku(self, board: list[list[str]]) -> None:
        """
        Do not return anything, modify board in-place instead.
        """
        # Get initial sets of numbers in rows and columns and boxes
        col_lists, row_lists, box_lists, blank_loc_set = self.computeSolutionSet(board)

        # Validate the initial board state
        col_sets, row_sets, box_sets = self.validateInitialBoard(row_lists, col_lists, box_lists)

        # Initialize the blank solution set for each blank cell
        blank_solution_set: dict[tuple[int, int], frozenset[int]] = {}
        for row, col in blank_loc_set:
            blank_solution_set[(row, col)] = Solution.POSSIBLE_CELL_VALUES - row_sets[row] - col_sets[col] - box_sets[self.getBoxIndex(row, col)]

        # Try to solve the board
        if not self.populateBoard(board, col_sets, row_sets, box_sets, blank_loc_set, blank_solution_set):
            raise ValueError("Invalid board - cannot solve with current solution sets")

    def getBoxIndex(self, row: int, col: int) -> int:
        """
        Get the index of the sub square for a given row and column
        """
        return (row // Solution.BOX_SIZE) * Solution.BOX_SIZE + (col // Solution.BOX_SIZE)

    def computeSolutionSet(self, board: list[list[str]]) -> tuple[list[list[int]], list[list[int]], list[list[int]], list[tuple[int, int]]]:
        """
        Compute solution sets for a given board for all blank spaces
        """
        row_lists: list[list[int]] = [[] for _ in range(Solution.BOARD_SIZE)]
        col_lists: list[list[int]] = [[] for _ in range(Solution.BOARD_SIZE)]

        # | 0 | 1 | 2 |
        # | 3 | 4 | 5 |
        # | 6 | 7 | 8 |
        box_lists: list[list[int]] = [[] for _ in range(Solution.BOX_SIZE * Solution.BOX_SIZE)]

        blank_set: list[tuple[int, int]] = []

        # Populate the sets with the current values on the board
        for i in range(Solution.BOARD_SIZE):
            for j in range(Solution.BOARD_SIZE):
                if board[i][j] != Solution.BLANK_CELL_VALUE:
                    board_value = int(board[i][j])

                    row_lists[i].append(board_value)
                    col_lists[j].append(board_value)
                    box_lists[self.getBoxIndex(i, j)].append(board_value)
                else:
                    blank_set.append((i, j))

        return col_lists, row_lists, box_lists, blank_set

    def validateInitialBoard(self, row_lists: list[list[int]], col_lists: list[list[int]], box_lists: list[list[int]]) -> tuple[list[set[int]], list[set[int]], list[set[int]]]:
        """
        Validate the initial board state to ensure there are no duplicates in rows, columns, or boxes
        """
        row_sets: list[set[int]] = [set(row) for row in row_lists]
        col_sets: list[set[int]] = [set(col) for col in col_lists]
        box_sets: list[set[int]] = [set(box) for box in box_lists]

        for i in range(Solution.BOARD_SIZE):
            # Check for duplicates in rows
            if len(row_sets[i]) != len(row_lists[i]):
                duplicates = [x for x in row_lists[i] if row_lists[i].count(x) > 1]
                raise ValueError(f"Invalid board: duplicate {duplicates[0]} in row {i}")
            
            # Check for duplicates in columns
            if len(col_sets[i]) != len(col_lists[i]):
                duplicates = [x for x in col_lists[i] if col_lists[i].count(x) > 1]
                raise ValueError(f"Invalid board: duplicate {duplicates[0]} in column {i}")
            
            # Check for duplicates in boxes
            if len(box_sets[i]) != len(box_lists[i]):
                duplicates = [x for x in box_lists[i] if box_lists[i].count(x) > 1]
                raise ValueError(f"Invalid board: duplicate {duplicates[0]} in box {i}")
            
            # Check for values out of range in rows
            invalid = set(row_sets[i]) - Solution.POSSIBLE_CELL_VALUES
            if invalid:
                raise ValueError(f"Invalid board: out-of-range value {list(invalid)[0]} in row {i}")
            
            # Check for values out of range in columns
            invalid = set(col_sets[i]) - Solution.POSSIBLE_CELL_VALUES
            if invalid:
                raise ValueError(f"Invalid board: out-of-range value {list(invalid)[0]} in column {i}")
            
            # Check for values out of range in boxes
            invalid = set(box_sets[i]) - Solution.POSSIBLE_CELL_VALUES
            if invalid:
                raise ValueError(f"Invalid board: out-of-range value {list(invalid)[0]} in box {i}")
        
        return col_sets, row_sets, box_sets

    def populateBoard(self, board: list[list[str]], col_sets: list[set[int]], row_sets: list[set[int]], box_sets: list[set[int]], blank_loc_set: list[tuple[int, int]], blank_solution_set: dict[tuple[int, int], frozenset[int]]) -> bool:
        """
        Using the solution sets, do a depth first search to populate the board
        """
        # Base case - if there are no more blank spaces, we should be done
        if len(blank_loc_set) == 0:
            return True

        # Find blank cell with the least number of candidates to try first
        best_blank = min(blank_loc_set, key=lambda pos: len(blank_solution_set[pos]))
        best_blank_index = blank_loc_set.index(best_blank)

        current_blank = blank_loc_set[best_blank_index]
        blank_candidates = blank_solution_set[current_blank]
        curr_row, curr_col = current_blank

        for candidate in blank_candidates:
            # Create immutable copies of the sets and update them
            new_row_sets = [s.copy() for s in row_sets]
            new_col_sets = [s.copy() for s in col_sets]
            new_box_sets = [s.copy() for s in box_sets]
            new_row_sets[curr_row].add(candidate)
            new_col_sets[curr_col].add(candidate)
            new_box_sets[self.getBoxIndex(curr_row, curr_col)].add(candidate)

            # Update board
            board[curr_row][curr_col] = str(candidate)

            # Construct new list of the remaining blanks
            new_blank_loc_set = blank_loc_set[:best_blank_index] + blank_loc_set[best_blank_index + 1:]

            # Create an updated blank solution set (immutable: new dict with changes)
            new_blank_solution_set = blank_solution_set.copy()
            for row, col in new_blank_loc_set:
                if row == curr_row or col == curr_col or self.getBoxIndex(row, col) == self.getBoxIndex(curr_row, curr_col):
                    new_blank_solution_set[(row, col)] = blank_solution_set[(row, col)] - {candidate}

            if self.populateBoard(board, new_col_sets, new_row_sets, new_box_sets, new_blank_loc_set, new_blank_solution_set):
                return True

            # Backtrack: restore board
            board[curr_row][curr_col] = Solution.BLANK_CELL_VALUE

        return False


# Test cases
# Create a single instance of the solution to run test cases
solution = Solution()

board_1 = [["5","3",".",".","7",".",".",".","."],["6",".",".","1","9","5",".",".","."],[".","9","8",".",".",".",".","6","."],["8",".",".",".","6",".",".",".","3"],["4",".",".","8",".","3",".",".","1"],["7",".",".",".","2",".",".",".","6"],[".","6",".",".",".",".","2","8","."],[".",".",".","4","1","9",".",".","5"],[".",".",".",".","8",".",".","7","9"]]
expected_board_1= [['5', '3', '4', '6', '7', '8', '9', '1', '2'], ['6', '7', '2', '1', '9', '5', '3', '4', '8'], ['1', '9', '8', '3', '4', '2', '5', '6', '7'], ['8', '5', '9', '7', '6', '1', '4', '2', '3'], ['4', '2', '6', '8', '5', '3', '7', '9', '1'], ['7', '1', '3', '9', '2', '4', '8', '5', '6'], ['9', '6', '1', '5', '3', '7', '2', '8', '4'], ['2', '8', '7', '4', '1', '9', '6', '3', '5'], ['3', '4', '5', '2', '8', '6', '1', '7', '9']]
solution.solveSudoku(board_1)
assert board_1 == expected_board_1, f"Expected {expected_board_1} but got {board_1}"

board_2 = [[".",".",".",".",".",".",".",".","."],[".","9",".",".","1",".",".","3","."],[".",".","6",".","2",".","7",".","."],[".",".",".","3",".","4",".",".","."],["2","1",".",".",".",".",".","9","8"],[".",".",".",".",".",".",".",".","."],[".",".","2","5",".","6","4",".","."],[".","8",".",".",".",".",".","1","."],[".",".",".",".",".",".",".",".","."]]
expected_board_2 = [['7', '2', '1', '8', '5', '3', '9', '4', '6'], ['4', '9', '5', '6', '1', '7', '8', '3', '2'], ['8', '3', '6', '4', '2', '9', '7', '5', '1'], ['9', '6', '7', '3', '8', '4', '1', '2', '5'], ['2', '1', '4', '7', '6', '5', '3', '9', '8'], ['3', '5', '8', '2', '9', '1', '6', '7', '4'], ['1', '7', '2', '5', '3', '6', '4', '8', '9'], ['6', '8', '3', '9', '4', '2', '5', '1', '7'], ['5', '4', '9', '1', '7', '8', '2', '6', '3']]
solution.solveSudoku(board_2)
assert board_2 == expected_board_2, f"Expected {expected_board_2} but got {board_2}"
