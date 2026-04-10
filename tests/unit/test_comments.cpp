#include "runner.hpp"
#include "semantics/module.hpp"

#include <iostream>
#include <ranges>

//--------------------------------------------------------------------
// Utils
//--------------------------------------------------------------------
static constexpr size_t ARENA_SIZE = 50 * 1024*1024;
static std::pair<Arena, std::unique_ptr<char[]>> mk_arena()
{
	std::unique_ptr<char[]> memory(new char[ARENA_SIZE]);
	Arena arena(memory.get(), ARENA_SIZE);

	return {arena, std::move(memory)};
}

static vector<Comment> parse_all_comments(Module const &mod)
{
	vector<Comment> comments;
	for(TokenIdx idx(0); idx.value < mod.parser.token_count(); ++idx.value)
	{
		PrecedingText text = get_preceding_text(idx, mod.parser);
		comments.append_range(parse_comments(text));
	}

	return comments;
}

struct ExpectedComment
{
	CommentTarget target;
	std::initializer_list<string_view> lines;
};

static string_view str(CommentTarget t)
{
	switch(t)
	{
		case CommentTarget::NONE: return "NONE";
		case CommentTarget::NEXT_TOKEN: return "NEXT_TOKEN";
		case CommentTarget::PREVIOUS_TOKEN: return "PREVIOUS_TOKEN";
		case CommentTarget::AMBIGUOUS: return "AMBIGUOUS";
	}

	UNREACHABLE;
}

static void print_comment_error(
	ExpectedComment expected_comment,
	Comment actual_comment,
	vector<string_view> const &actual_lines
)
{
	std::cout << "ERROR" << std::endl;
	std::cout << "> Expected (";
	std::cout << str(expected_comment.target) << ", ";
	std::cout << expected_comment.lines.size() << " lines";
	std::cout << "):" << std::endl;
	for(string_view line: expected_comment.lines)
		std::cout << line << std::endl;

	std::cout << "> Actual (";
	if(actual_comment.target)
		std::cout << str(*actual_comment.target) << ", ";
	else
		std::cout << "<no target set>, ";
	std::cout << actual_lines.size() << " lines";
	std::cout << "):" << std::endl;
	for(string_view line: actual_lines)
		std::cout << line << std::endl;
}

static void test_comments(
	string_view source,
	std::span<ExpectedComment> expected_comments
)
{
	auto [arena, mem] = mk_arena();
	auto [temp_arena, temp_mem] = mk_arena();
	Module mod = parse_module(source, Memory(&arena, temp_arena));
	vector<Comment> comments = parse_all_comments(mod);

	if(comments.size() != std::size(expected_comments))
	{
		std::cout << "ERROR: Unexpected number of comments" << std::endl;
		for(Comment const &comment: comments)
		{
			std::cout << "------------------------------------" << std::endl;
			vector<string_view> lines = extract_clean_lines(comment);
			for(string_view line: lines)
				std::cout << line << std::endl;
		}
		throw TestError();
	}

	for(size_t i = 0; i < comments.size(); ++i)
	{
		ExpectedComment expected_comment = expected_comments[i];
		Comment actual_comment = comments[i];

		vector<string_view> actual_lines = extract_clean_lines(actual_comment);
		if(actual_lines.size() != expected_comment.lines.size())
		{
			print_comment_error(expected_comment, actual_comment, actual_lines);
			throw TestError();
		}

		for(size_t k = 0; k < actual_lines.size(); ++k)
		{
			if(actual_lines[k] != *(expected_comment.lines.begin() + k))
			{
				print_comment_error(expected_comment, actual_comment, actual_lines);
				throw TestError();
			}
		}

		if(actual_comment.target != expected_comment.target)
		{
			print_comment_error(expected_comment, actual_comment, actual_lines);
			throw TestError();
		}
	}
}

//--------------------------------------------------------------------
// Test cases
//--------------------------------------------------------------------
TEST(Comments, Attachment_1)
{
	string_view source =
R"(// A top-level comment not attached to anything

// not attached
/* neither is this */ /*
nor this */ // or this

// Attached to main
proc main() -> i32
{
	// This comment attaches to the let-statement of `a`.
	let a = 0;

	// For a comment to attach to a target, the target must appear on the line
	// following the comment. Thus, this comment does *not* attach to `b`.

	let b = 0;

	// Comments on the same line as the target are also supported

	/* I'm attached to `c` */ let c = 0; // I'm as well

	// Comments that appear on successive lines usually have the same target.
	/* This applies even if different comment styles are used. If the rules
	 * result in successive comments having different targets, the result is
	 * AMBIGUOUS.
	 * Thus, the 7 lines of comment are attached to `d`.
	 */
	let d = 0;

	// There is one exception to the above rule: Comments that appear on
	// successive lines but are separated by a semantically signigicant token
	// may in fact have different targets.

	/* This one attaches to `e` */ let e = 0;
	// But this one attaches to `f`
	let f = 0;

	let g = 0; // This is attached to `g`
	// but this is ambiguous
	let h = 0;

	// (Visually it might seem obvious that the first comment should attach to `g`
	// and the second one to `h`. However, in the presence of tabs, visual
	// alignment cannot be determined without knowing the tab width. To
	// circumvent this problem, we make this case AMBIGUOUS.)

	let i = 0; // this line is attached to `i`
	           // this and the next
	           // line are AMBNIGUOUS
	let j = 0;

	let k = 0; // This one is fine though and attaches to `k`
	let l = 0; // I attach to `l`

	let m = 0; // Attached to `m`
	// ambiguous as
	           // hell

	let s = 0; /*
	this is ambiguous */

	let n = 0;
	// i'm attached to `o`
	let o = 0;

	let p = 0; /*
	the attachement of this one
	is ambiguous */
	let q = 0;

	let r = 0; /* hi */ /* ho */
})";

	ExpectedComment expected_comments[] =
	{
		{
			CommentTarget::NONE,
			{"A top-level comment not attached to anything"}
		},
		{
			CommentTarget::NONE,
			{"not attached"}
		},
		{
			CommentTarget::NONE,
			{"neither is this"}
		},
		{
			CommentTarget::NONE,
			{
				"",
				"nor this",
			}
		},
		{
			CommentTarget::NONE,
			{"or this"}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"Attached to main"}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"This comment attaches to the let-statement of `a`."}
		},
		{
			CommentTarget::NONE,
			{"For a comment to attach to a target, the target must appear on the line"}
		},
		{
			CommentTarget::NONE,
			{"following the comment. Thus, this comment does *not* attach to `b`."}
		},
		{
			CommentTarget::NONE,
			{"Comments on the same line as the target are also supported"}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"I'm attached to `c`"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"I'm as well"}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"Comments that appear on successive lines usually have the same target."}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{
				"This applies even if different comment styles are used. If the rules",
				"result in successive comments having different targets, the result is",
				"AMBIGUOUS.",
				"Thus, the 7 lines of comment are attached to `d`.",
				"",
			}
		},
		{
			CommentTarget::NONE,
			{"There is one exception to the above rule: Comments that appear on"}
		},
		{
			CommentTarget::NONE,
			{"successive lines but are separated by a semantically signigicant token"}
		},
		{
			CommentTarget::NONE,
			{"may in fact have different targets."}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"This one attaches to `e`"}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"But this one attaches to `f`"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"This is attached to `g`"}
		},
		{
			CommentTarget::AMBIGUOUS,
			{"but this is ambiguous"}
		},
		{
			CommentTarget::NONE,
			{"(Visually it might seem obvious that the first comment should attach to `g`"}
		},
		{
			CommentTarget::NONE,
			{"and the second one to `h`. However, in the presence of tabs, visual"}
		},
		{
			CommentTarget::NONE,
			{"alignment cannot be determined without knowing the tab width. To"}
		},
		{
			CommentTarget::NONE,
			{"circumvent this problem, we make this case AMBIGUOUS.)"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"this line is attached to `i`"}
		},
		{
			CommentTarget::AMBIGUOUS,
			{"this and the next"}
		},
		{
			CommentTarget::AMBIGUOUS,
			{"line are AMBNIGUOUS"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"This one is fine though and attaches to `k`"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"I attach to `l`"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"Attached to `m`"}
		},
		{
			CommentTarget::AMBIGUOUS,
			{"ambiguous as"}
		},
		{
			CommentTarget::AMBIGUOUS,
			{"hell"}
		},
		{
			CommentTarget::AMBIGUOUS,
			{
				"",
				"this is ambiguous"
			}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"i'm attached to `o`"}
		},
		{
			CommentTarget::AMBIGUOUS,
			{
				"",
				"the attachement of this one",
				"is ambiguous",
			}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"hi"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"ho"}
		},
	};

	test_comments(source, expected_comments);
}

TEST(Comments, Attachment_2)
{
	string_view source =
R"(proc main()
/* nexte token */ { // prev token
	let g = 0; // This is attached to `g`

	// i belong to h
	let h = 0;
})";

	ExpectedComment expected_comments[] =
	{
		{
			CommentTarget::NEXT_TOKEN,
			{"nexte token"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"prev token"}
		},
		{
			CommentTarget::PREVIOUS_TOKEN,
			{"This is attached to `g`"}
		},
		{
			CommentTarget::NEXT_TOKEN,
			{"i belong to h"}
		},
	};

	test_comments(source, expected_comments);
}

TEST(Comments, Attachment_3)
{
	string_view source =
R"(//! What is my target?
proc main() {})";

	ExpectedComment expected_comments[] =
	{
		{
			CommentTarget::NEXT_TOKEN,
			{"What is my target?"}
		},
	};

	test_comments(source, expected_comments);
}

// TODO Add actual tests
//TEST(Comments, Options)
//{
//	string_view source =
//R"(//!test: This is a test
////!return_code: 2
//proc main()
//{
//	//!opt: here we go
//	let g = 0;
//
//	/*! yo: so */
//	let h = 0; //!what: is this
//})";
//
//	auto [arena, mem] = mk_arena();
//	auto [temp_arena, temp_mem] = mk_arena();
//	Module mod = parse_module(source, Memory(&arena, temp_arena));
//	ModuleOptions opts = gather_options(mod);
//
//	for(auto const &[target, options]: opts.opts)
//	{
//		std::cout << "---------------------------" << std::endl;
//		std::cout << str(target) << ":" << std::endl;
//		print_options(options);
//	}
//}
