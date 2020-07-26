let nums = [0, 1, 2];
nums.iter().each(|num| { assertEq(num, nums[num]); });