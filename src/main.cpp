#include <cassert>
#include <iostream>
#include <fstream>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <sstream>

#include "syntax.hpp"
#include "semantics.hpp"
#include "codegen.hpp"


using std::string;
using std::string_view;
using std::optional;

using namespace std::string_literals;
using namespace std::string_view_literals;


//==============================================================================
// Main
//==============================================================================
constexpr size_t MAX_MEMORY      = 50 * 1024*1024;
constexpr size_t MAX_TEMP_MEMORY = 50 * 1024*1024;


#define NEXT(args) (*++args)

int main(int, char *argv[])
{
	optional<string> arg_input_filename;
	optional<string> arg_output_filename;
	optional<string> arg_event_log_filename;
	bool arg_verbose = false;

	// Parse arguments
	{
		while(NEXT(argv))
		{
			if(*argv == "-o"sv)
			{
				if(!NEXT(argv))
				{
					std::cerr << "Error: expected output filename" << std::endl;
					return 1;
				}
				arg_output_filename = *argv;
			}
			else if(*argv == "-v"sv)
			{
				arg_verbose = true;
			}
			else if(*argv == "--log-events"sv)
			{
				if(!NEXT(argv))
				{
					std::cerr << "Error: expected event log filename" << std::endl;
					return 1;
				}
				arg_event_log_filename = *argv;
			}
			else
			{
				if(arg_input_filename) {
					std::cerr << "Error: input file already provided" << std::endl;
					return 1;
				}
				arg_input_filename = *argv;
			}
		}

		if(!arg_input_filename)
		{
			std::cerr << "Error: no input file provided" << std::endl;
			return 1;
		}

		if(!arg_output_filename)
		{
			std::cerr << "Error: no output file provided" << std::endl;
			return 1;
		}
	}


	std::ifstream file(*arg_input_filename);
	if(!file) {
		std::cerr << "Error: cannot read file: " << *arg_input_filename << std::endl;
		return 1;
	}

	std::ostringstream ss;
	ss << file.rdbuf();
	std::string source = std::move(ss).str();

	std::unique_ptr<char[]> memory(new char[MAX_MEMORY]);
	std::unique_ptr<char[]> temp_memory(new char[MAX_TEMP_MEMORY]);
	Arena main_arena(memory.get(), MAX_MEMORY);
	Arena temp_arena(temp_memory.get(), MAX_TEMP_MEMORY);

	try
	{
		Module mod = parse_module(source, Memory{&main_arena, temp_arena});

		std::ofstream event_sink;
		if(arg_event_log_filename)
		{
			event_sink.open(*arg_event_log_filename);
			mod.logger.emplace(&mod, event_sink);
		}

		sema(mod, main_arena);
		if(arg_verbose)
			print(mod, std::cout);

		std::stringstream c_code;
		CBackend backend(c_code);
		generate_c(mod, backend);

		std::ofstream c_file(*arg_output_filename);
		c_file << c_code.str();

		if(arg_verbose)
		{
			std::cout << "==================================================" << std::endl;
			std::cout << "Memory usage (arena): " << ((char*)main_arena.current_ptr() - memory.get()) << std::endl;
		}
	}
	catch(LexingError const &e)
	{
		std::cerr << e.what() << std::endl;
		return 1;
	}
	catch(ParseError const &e)
	{
		std::cerr << e.what() << std::endl;
		return 1;
	}
}
