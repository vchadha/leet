class Solution:
    IS_ALIVE: int = 1
    IS_DEAD: int = 0
    NEIGHBOR_OFFSETS = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]

    def gameOfLife(self, board: list[list[int]]) -> None:
        """
        Do not return anything, modify board in-place instead.
        """
        updated_cells: dict[tuple[int, int], int] = {}

        for i in range(len(board)):
            for j in range(len(board[0])):
                num_alive_neighbors: int = self.countAliveNeighbors(board, i, j)

                # If I am alive
                if self.isAlive(board, i, j):
                    # Underpopulation or overpopulation leads to death
                    if num_alive_neighbors < 2 or num_alive_neighbors > 3:
                        updated_cells[(i, j)] = Solution.IS_DEAD
                    # Else -> no-op

                # If I am dead
                else:
                    # I can ressurect!
                    if num_alive_neighbors == 3:
                        updated_cells[(i, j)] = Solution.IS_ALIVE
                    # Else -> no-op
        
        # Update board with new values
        for (i, j), new_value in updated_cells.items():
            board[i][j] = new_value

    def isAlive(self, board: list[list[int]], i: int, j: int) -> bool:
        """
        Check if a given cell is alive, treating out of bounds as dead
        """
        if i < 0 or i >= len(board) or j < 0 or j >= len(board[0]):
            return False
        
        return board[i][j] == Solution.IS_ALIVE

    def countAliveNeighbors(self, board: list[list[int]], i: int, j: int) -> int:
        """
        Count the number of alive neighbors for a given cell
        """
        return sum(1 for di, dj in Solution.NEIGHBOR_OFFSETS if self.isAlive(board, i + di, j + dj))
        
# Test cases
# Create a single instance of the solution to run test cases
solution = Solution()

board = [[0,1,0],[0,0,1],[1,1,1],[0,0,0]]
expected_board = [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
solution.gameOfLife(board)
assert board == expected_board, f"Expected {expected_board} but got {board}"
