using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode2020
{
    class Program
    {
        static async Task Main(string[] args)
        {

            var result = System.IO.File.ReadAllLines(@"App_Data\view-source_https___adventofcode.com_2020_day_1_input.txt");

            var array = result.Select(s => int.Parse(s));

            Console.WriteLine(D1Q1(array));

            Console.WriteLine(D1Q2(array));
        }

        private static int D1Q1(IEnumerable<int> numbers)
        {
            foreach (var current in numbers)
            {
                foreach (var other in numbers)
                {
                    if (current == other) continue;
                    if (current + other == 2020)
                    {
                        return current * other;
                    }
                }
            }
            return 0;
        }

        private static int D1Q2(IEnumerable<int> numbers)
        {
            foreach (var current in numbers)
            {
                foreach (var other in numbers)
                {
                    foreach (var third in numbers)
                    {
                        if (current == other || current == third | other == third) continue;
                        if (current + other + third == 2020)
                        {
                            return current * other * third;
                        }
                    }
                }
            }
            return 0;
        }
    }
}
