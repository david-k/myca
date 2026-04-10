#pragma once

#include "utils.hpp"

#define TEST(SUIT_NAME, TEST_NAME, ...)                                                            \
	static void SUIT_NAME##_##TEST_NAME##_fn();                                                    \
	static AddTest SUIT_NAME##_##TEST_NAME(#SUIT_NAME"."#TEST_NAME, SUIT_NAME##_##TEST_NAME##_fn); \
	static void SUIT_NAME##_##TEST_NAME##_fn()

class TestError : public std::runtime_error
{
public:
	TestError() :
		std::runtime_error("") {}

	using std::runtime_error::runtime_error;
};

struct TestCase
{
	string_view name;
	void (*fn)();
};

struct AddTest
{
	AddTest(string_view name, void (*)());
};

extern size_t RANDOM_SEED;
extern vector<TestCase> TESTCASES;
