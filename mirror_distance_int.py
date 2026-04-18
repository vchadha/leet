class Solution:
    def mirrorDistance(self, n: int) -> int:
        """
        Calculate the mirror distance for a given integer n.
        
        The mirror distance is defined as abs(n - reverse(n))
        where reverse(n) is the integer obtained by reversing the digits of n.
        """
        return abs(n - self.reverse(n))

    def reverse(self, n: int) -> int:
        """
        Reverse the digits of an integer n.
        """
        reversed_str = str(n)[::-1]
        return int(reversed_str)