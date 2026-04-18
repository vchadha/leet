class Solution:
    def twoSum(self, nums: list[int], target: int) -> list[int]:
        """
        Find the indices of the first two numbers in the input list that add up to the target value.
        Raise an error if no such pair exists.
        """
        # Dict to store the complement of each number and its index
        complement_dict: dict[int, int] = {}
        for i in range(len(nums)):
            if nums[i] in complement_dict:
                return [complement_dict[nums[i]], i]
            else:
                complement_dict[target - nums[i]] = i
        
        raise ValueError("No two numbers add up to the target")

# Test cases
# Create a single instance of the solution to run test cases
solution = Solution()

nums_1 = [2,7,11,15]
target_1 = 9
expected_output_1 = [0, 1]
solution_1 = solution.twoSum(nums_1, target_1)
assert solution_1 == expected_output_1, f"Expected {expected_output_1} but got {solution_1}"

nums_2 = [3,2,4]
target_2 = 6
expected_output_2 = [1, 2]
solution_2 = solution.twoSum(nums_2, target_2)
assert solution_2 == expected_output_2, f"Expected {expected_output_2} but got {solution_2}"

nums_3 = [3,3]
target_3 = 6
expected_output_3 = [0, 1]
solution_3 = solution.twoSum(nums_3, target_3)
assert solution_3 == expected_output_3, f"Expected {expected_output_3} but got {solution_3}"
