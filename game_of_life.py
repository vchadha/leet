class Solution:
    IS_ALIVE: int = 1
    IS_DEAD: int = 0

    def isAlive(self, board: list[list[int]], i: int, j: int) -> bool:
        if i < 0 or i >= len(board) or j < 0 or j >= len(board[0]):
            return False
        
        return board[i][j] == Solution.IS_ALIVE

    def countAliveNeighbors(self, board: list[list[int]], i: int, j: int) -> int:
        num_alive_neighbors: int = 0

        # Check all 8 neighbors
        for di in [-1, 0, 1]:
            for dj in [-1, 0, 1]:
                if di == 0 and dj == 0:
                    continue
                if self.isAlive(board, i + di, j + dj):
                    num_alive_neighbors += 1

        return num_alive_neighbors

    def gameOfLife(self, board: list[list[int]]) -> None:
        """
        Do not return anything, modify board in-place instead.
        """
        old_board: list[list[int]] = [row.copy() for row in board]

        for i in range(len(board)):
            for j in range(len(board[0])):
                num_alive_neighbors: int = self.countAliveNeighbors(old_board, i, j)

                # If I am alive
                if self.isAlive(old_board, i, j):
                    # If neighbors < 2 -> die
                    if num_alive_neighbors < 2:
                        board[i][j] = Solution.IS_DEAD
                    # TODO: I could remove this condition since it is redundant
                    elif num_alive_neighbors == 2 or num_alive_neighbors == 3:
                        board[i][j] = Solution.IS_ALIVE
                    # Overpopulation leads to death
                    else:
                        board[i][j] = Solution.IS_DEAD
                # If I am dead
                else:
                    # I can ressurect!
                    if num_alive_neighbors == 3:
                        board[i][j] = Solution.IS_ALIVE
                    # Else -> no-op
        
board = [[0,1,0],[0,0,1],[1,1,1],[0,0,0]]
expected_board = [[0,0,0],[1,0,1],[0,1,1],[0,1,0]]
solution = Solution()
solution.gameOfLife(board)
assert board == expected_board, f"Expected {expected_board} but got {board}"