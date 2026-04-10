#pragma once

#include <algorithm>
#include <cassert>
#include <filesystem>
#include <fstream>
#include <generator>
#include <initializer_list>
#include <iterator>
#include <ranges>
#include <sstream>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

using std::vector;
using std::string;
using std::string_view;
using std::variant;
using std::unordered_map;
using std::pair;
using namespace std::string_literals;

#define NULLABLE

#ifdef __clang__
	#define NO_DANGLING
#elif __GNUC__
	#define NO_DANGLING [[gnu::no_dangling]]
#endif

#define UNREACHABLE assert(!"unreachable")
#define TODO(MSG) throw std::runtime_error("TODO: "s + (MSG))

//--------------------------------------------------------------------
// Arena
//--------------------------------------------------------------------
// Adapted from https://nullprogram.com/blog/2023/09/27/
class Arena
{
public:
	explicit Arena(void *memory, ptrdiff_t size)
	{
		m_begin = (char*)memory;
		m_end = m_begin ? m_begin + size : nullptr;
	}

	template<typename T, typename ...Args>
	T* alloc(Args &&...args)
	{
		static_assert(std::is_trivially_destructible_v<T>, "Arena: Allocated type not trivially destructible");

		void *mem = alloc_raw(sizeof(T), alignof(T), 1);
		return new (mem) T(std::forward<Args>(args)...);
	}

	template<typename T, typename ...Args>
	T* alloc_n(size_t n, Args &&...args)
	{
		static_assert(std::is_trivially_destructible_v<T>, "Arena: Allocated type not trivially destructible");

		void *mem = alloc_raw(sizeof(T), alignof(T), n);
		return new (mem) T[n](std::forward<Args>(args)...);
	}

	void *alloc_raw(ptrdiff_t size, ptrdiff_t align, ptrdiff_t count)
	{
		ptrdiff_t padding = -(uintptr_t)m_begin & (align - 1);
		ptrdiff_t available = m_end - m_begin - padding;
		if (available < 0 || count > available/size) {
			throw std::bad_alloc();
		}
		void *p = m_begin + padding;
		m_begin += padding + count*size;
		return p;
	}

	void* current_ptr() { return (void*)m_begin; }
	void set_current_ptr(void *ptr) { m_begin = (char*)ptr; }

private:
	char *m_begin;
	char *m_end;
};

// Pass this to functions that need to allocate memory.
// Pass by value so the temp allocator is automatically cleared upon return.
struct Memory
{
	Arena *main;
	Arena temp;
};


//--------------------------------------------------------------------
// Misc
//--------------------------------------------------------------------
// Why is this not part of the standard library?
inline string operator + (string str, string_view view)
{
	str.append(view.begin(), view.end());
	return str;
}

inline string operator + (string_view view, string str)
{
	str.insert(str.begin(), view.begin(), view.end());
	return str;
}

// Locale-independent versions of std::isalpha etc
inline bool is_alphabetic(char ch)
{
	return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}
inline bool is_digit(char ch)
{
	return ch >= '0' && ch <= '9';
}
inline bool is_whitespace(char ch)
{
	return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r';
}

inline void ltrim(string_view &str)
{
	while(str.length() and is_whitespace(str.front()))
		str.remove_prefix(1);
}

inline void rtrim(string_view &str)
{
	while(str.length() and is_whitespace(str.back()))
		str.remove_suffix(1);
}

inline void trim(string_view &str)
{
	ltrim(str);
	rtrim(str);
}

inline string_view trimmed(string_view str)
{
	trim(str);
	return str;
}

inline std::generator<string_view> split_lines(string_view text)
{
	while(not text.empty())
	{
		size_t line_end = text.find('\n');
		if(line_end != string_view::npos)
			line_end += 1; // include \n

		string_view line = text.substr(0, line_end);
		text.remove_prefix(line.length());
		co_yield line;
	}
}

inline bool remove_prefix_if(string_view &text, string_view prefix)
{
	if(text.starts_with(prefix))
	{
		text.remove_prefix(prefix.length());
		return true;
	}
	return false;
}

inline bool is_whitespace(string_view str)
{
	return std::ranges::all_of(str, [](char c){ return is_whitespace(c);});
}

inline string read_file(std::filesystem::path const &path)
{
	std::ifstream f(path);
	if(not f)
		throw std::runtime_error("Opening file failed: " + path.string());

	std::stringstream ss;
	f >> ss.rdbuf();
	return std::move(ss).str();
}

template<typename T>
T parse_int(string_view s)
{
	T num;
	std::from_chars_result result = std::from_chars(s.begin(), s.end(), num);
	if(result.ec != std::errc())
		throw std::runtime_error("Conversion to integer failed");
	if(result.ptr != s.end())
		throw std::runtime_error("Conversion to integer failed: invalid trailing characters");

	return num;
}

// According to https://stackoverflow.com/a/50978188/3491462 this is not how
// Boost does it anymore but it will do for now.
inline void combine_hashes(size_t &seed, size_t hash)
{
    seed ^= hash + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

template<typename T>
inline size_t compute_hash(T const &t)
{
	return std::hash<T>()(t);
}

// The following is taken from https://github.com/AVasK/vx to make std::visit
// more ergonomic.
//
// Example:
//
//     std::variant<int, float> var;
//     var | match
//     {
//         [](int i) { ... },
//         [](float f) { ... },
//     };
template<class... Ts>
struct match : Ts...  {
	using Ts::operator()...;
};

// explicit deduction guide (not needed as of C++20)
template<class... Ts>
match(Ts...) -> match<Ts...>;

template <typename T, typename... Fs>
constexpr decltype(auto) operator | (T const &v, match<Fs...> const &match)
{
	using std::visit;
    return visit(match, v);
}

template <typename T, typename... Fs>
constexpr decltype(auto) operator | (T &v, match<Fs...> const &match)
{
	using std::visit;
    return visit(match, v);
}

template<typename T, typename ...Ss>
bool is(variant<Ss...> const &v)
{
	return std::holds_alternative<T>(v);
}

template<typename RangeT, typename FuncT = std::identity>
struct RangeFmt
{
	using value_type = typename RangeT::value_type;

	RangeFmt(
		RangeT const &range,
		char const *sep = ", ",
		FuncT const &func = std::identity{}
	) :
		range(range),
		sep(sep),
		func(func) {}

	RangeT const &range;
	char const *sep;
	FuncT const &func;
};

template<typename RangeT, typename FuncT>
inline std::ostream& operator << (std::ostream &os, RangeFmt<RangeT, FuncT> const &r)
{
	auto begin = std::begin(r.range);
	auto end = std::end(r.range);

	constexpr bool func_is_void = std::is_same_v<std::invoke_result_t<FuncT, decltype(*begin)>, void>;
	if(begin != end)
	{
		if constexpr(func_is_void) r.func(*begin);
		else os << r.func(*begin);

		while(++begin != end)
		{
			os << r.sep;
			if constexpr(func_is_void) r.func(*begin);
			else os << r.func(*begin);
		}
	}

	return os;
}

//--------------------------------------------------------------------
// Ranges/containers
//--------------------------------------------------------------------
// Segment arrays seem useful: https://danielchasehooper.com/posts/segment_array/
// (dynamic array with stable pointers, can be used with arena allocators)

// Disable warning to allow flexible array members.
// Unfortunately, it seems there is no specific warning flag for flexible array members, so we
// disable all pedantic warnings.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored  "-Wpedantic"

// A dynamically allocated array with a fixed size
template<typename T>
struct FixedArray
{
	size_t count = 0;
	T items[];

	T& head()
	{
		assert(count > 0);
		return items[0];
	}

	T const& head() const
	{
		assert(count > 0);
		return items[0];
	}

	std::ranges::subrange<T*> tail()
	{
		assert(count > 0);
		return {items + 1, items + count};
	}

	std::ranges::subrange<T const*> tail() const
	{
		assert(count > 0);
		return {items + 1, items + count};
	}

	T* begin() { return items; }
	T* end() { return items + count; }

	T const* begin() const { return items; }
	T const* end() const { return items + count; }
};

#pragma GCC diagnostic pop


template<typename T>
FixedArray<T>* alloc_fixed_array(size_t count, Arena &arena)
{
	FixedArray<T> *arr = (FixedArray<T>*)arena.alloc_raw(
		sizeof(FixedArray<T>) + sizeof(T) * count,
		alignof(FixedArray<T>),
		1
	);
	arr->count = count;
	return arr;
}

template<typename T>
FixedArray<T>* clone(FixedArray<T> const *arr, Arena &arena)
{
	FixedArray<T> *result = alloc_fixed_array<T>(arr->count, arena);
	for(size_t i = 0; i < arr->count; ++i)
		result->items[i] = clone(arr->items[i], arena);

	return result;
}


template<typename T>
struct List
{
	T value;
	List<T> *NULLABLE next = nullptr;
};

template<typename T>
std::generator<T&> to_range(List<T> *list)
{
	for(List<T> *item = list; item; item = item->next)
		co_yield item->value;
}

template<typename T>
std::generator<T const&> to_range(List<T> const *list)
{
	for(List<T> const *item = list; item; item = item->next)
		co_yield item->value;
}


template<typename T>
class ListBuilder
{
public:
	explicit ListBuilder(Arena &arena) :
		arena(&arena) {}

	T* append(T t)
	{
		List<T> *item = arena->alloc<List<T>>(t);
		*tail = item;
		tail = &(*tail)->next;
		m_count += 1;

		return &item->value;
	}

	List<T>* list() { return head; }
	List<T> const* list() const { return head; }

	FixedArray<T>* to_array(Arena &arr_arena)
	{
		FixedArray<T> *arr = alloc_fixed_array<T>(m_count, arr_arena);
		for(auto const &[idx, val]: to_range(list()) | std::views::enumerate)
			arr->items[idx] = val;

		return arr;
	}

	bool empty() const { return head == nullptr; }
	size_t count() const { return m_count; }

private:
	Arena *arena;
	List<T> *head = nullptr;
	List<T> **tail = &head;
	size_t m_count = 0;
};


struct TransparentStringHash
{
	using is_transparent = void;

	size_t operator () (string const &s) const
	{
		return std::hash<string>()(s);
	}

	size_t operator () (string_view const &s) const
	{
		return std::hash<string_view>()(s);
	}
};

template<typename TValue>
using UnorderedStringMap = unordered_map<
	string,
	TValue,
	TransparentStringHash,
	// By default, unordered_map would use std::equal_to<string> which isn't transparent.
	// However, std::equal_to<> is!
	std::equal_to<>
>;


//--------------------------------------------------------------------
// Int128
//--------------------------------------------------------------------
using Int128 = __int128_t;

inline std::ostream& operator << (std::ostream &os, Int128 value)
{
	constexpr size_t MAX_RESULT_LENGTH = sizeof("-170141183460469231731687303715884105728") - 1;
	char buffer[MAX_RESULT_LENGTH];
	char *pos = std::end(buffer);

	__uint128_t abs_value = value < 0 ? -__uint128_t(value) : value;
	do
	{
		--pos;
		*pos = '0' + (abs_value % 10);
		abs_value /= 10;
	}
	while(pos != std::begin(buffer) and abs_value != 0);

	if(value < 0)
		*(--pos) = '-';

	os.write(pos, std::end(buffer) - pos);
	return os;
}

inline string str(Int128 xint)
{
	std::stringstream ss;
	ss << xint;
	return std::move(ss).str();
}

//--------------------------------------------------------------------
// Simple table renderer for the terminal
//--------------------------------------------------------------------
struct TTable
{
	struct Style
	{
		int padding = 1;
	};

	void add_row(std::initializer_list<string> row) {
		rows.emplace_back(row);
	}

	inline void print(std::ostream &os) const;

	Style style;
	vector<vector<string>> rows;
};

inline vector<size_t> compute_column_widths(vector<vector<string>> const &rows)
{
	vector<size_t> column_widths;
	for(vector<string> const &row: rows)
	{
		if(column_widths.size() < row.size())
			column_widths.resize(row.size());

		for(size_t i = 0; i < row.size(); ++i)
			column_widths[i] = std::max(column_widths[i], row[i].length());
	}

	return column_widths;
}

enum VFramePosition
{
	V_TOP,
	V_MIDDLE,
	V_BOTTOM,
};

enum HFramePosition
{
	H_LEFT,
	H_MIDDLE,
	H_RIGHT,
};

constexpr string_view table_box_chars[][3] = {
	{"┌", "┬", "┐"},
	{"├", "┼", "┤"},
	{"└", "┴", "┘"},
};

inline void print_inner_horizontal_line(
	std::ostream &os,
	vector<size_t> const &column_widths,
	VFramePosition pos,
	TTable::Style const &style
)
{
	for(size_t col = 0; col < column_widths.size(); ++col)
	{
		if(col != 0)
			os << table_box_chars[pos][H_MIDDLE];

		for(size_t i = 0; i < column_widths[col] + 2*style.padding; ++i)
			os << "─";
	}
}

void TTable::print(std::ostream &os) const
{
	vector<size_t> column_widths = compute_column_widths(rows);
	os << "┌"; print_inner_horizontal_line(os, column_widths, V_TOP, style); os << "┐\n";
	for(size_t row_idx = 0; row_idx < rows.size(); ++row_idx)
	{
		if(row_idx != 0)
		{
			os << "├";
			print_inner_horizontal_line(os, column_widths, V_MIDDLE, style);
			os << "┤\n";
		}

		vector<string> const &row = rows[row_idx];
		size_t col_idx = 0;
		for(; col_idx < row.size(); ++col_idx)
		{
			os << "│" << string(style.padding, ' ');
			os << row[col_idx];
			os << string((column_widths[col_idx] - row[col_idx].length()) + style.padding, ' ');
		}

		for(; col_idx < column_widths.size(); ++col_idx)
			os << "│" << string(2*style.padding + column_widths[col_idx], ' ');

		os << "│\n";
	}

	os << "└"; print_inner_horizontal_line(os, column_widths, V_BOTTOM, style); os << "┘\n";
}
