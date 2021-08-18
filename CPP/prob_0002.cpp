#include <iostream>


// sum_even_fibs (unsigned long max_fib)
unsigned int sum_even_fibs(unsigned int max_fib)
{
  unsigned int a_prev = 1, a_curr = 1, tmp = 0;
  unsigned int sum = 0;

  while (a_curr < max_fib)
    {
      tmp = a_curr;
      a_curr = a_curr + a_prev;
      a_prev = tmp;

      if (a_curr % 2 == 0)
        {
          sum += a_curr;
        }
    }

  return sum;
}


int solve_prob_0002()
{
  return sum_even_fibs(4000000);
}


int main ()
{
  std::cout << "Solution to Problem 2: " << solve_prob_0002() << "\n";
  return 0;
}
