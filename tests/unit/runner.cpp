#include <random>
#include <iostream>

#include "runner.hpp"

size_t RANDOM_SEED;
vector<TestCase> TESTCASES;

AddTest::AddTest(string_view name, void (*fn)())
{
	TESTCASES.push_back(TestCase(name, fn));
}

int main()
{
	std::random_device rd;
	RANDOM_SEED = rd();
	std::cout << "RANDOM_SEED = " << RANDOM_SEED << std::endl;

	for(auto [name, fn]: TESTCASES)
	{
		std::cout << "Testing \"" << name << "\"... " << std::flush;
		fn();
		std::cout << "ok" << std::endl;
	}

	return 0;
}
