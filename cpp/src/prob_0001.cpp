/*
  Problem 1:
  calculate the sum of all integers in [1,1000)
  which satisfy 3|x or 5|x

  Solved: 2021-08-14
*/

#include <iostream>

unsigned int divides_it(unsigned int d, unsigned int x)
{
  return (x / d) * d == x;
}

unsigned int solve_prob_0001()
{
  unsigned int i;
  unsigned int sum;

  for (i = 1; i < 1000; i++)
    {
      if (divides_it(3, i) | divides_it(5, i))
        {
          sum += i;
        }
    }

  return sum;
}

int main()
{
  unsigned int i;

  std::cout << "Solution to Problem 1: " << solve_prob_0001() << "\n";

  return 0;
}
