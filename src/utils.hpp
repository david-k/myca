#pragma once

#include <string>
#include <string_view>
#include <sstream>
#include <memory>
#include <type_traits>
#include <utility>
#include <variant>
#include <cassert>


#define UNREACHABLE assert(!"unreachable")
#define TODO(MSG) throw std::runtime_error("TODO: "s + (MSG))

using namespace std::string_literals;


// Smart pointers
//==============================================================================
// Couldn't get `using` to work
template<class T, class Deleter = std::default_delete<T>>
class NullableOwnPtr : public std::unique_ptr<T, Deleter>
{
public:
	using std::unique_ptr<T, Deleter>::unique_ptr;
	using std::unique_ptr<T, Deleter>::operator=;

	NullableOwnPtr(std::unique_ptr<T, Deleter> p) :
		std::unique_ptr<T, Deleter>(std::move(p)) {}
};

// Supposed to be a non-null owning ptr. Even though it is not entirely true
// (because of non-destructive moves) it's good enough to communicate the intention
template<class T, class Deleter = std::default_delete<T>>
class OwnPtr : public std::unique_ptr<T, Deleter>
{
public:
	OwnPtr(T *p) :
		std::unique_ptr<T, Deleter>(p)
	{
		assert(this->get() && "OwnPtr initialized with null pointer");
	}

	template<typename S>
	OwnPtr(std::unique_ptr<S, std::default_delete<S>> p) :
		std::unique_ptr<T, Deleter>(std::move(p))
	{
		assert(this->get() && "OwnPtr initialized with null pointer");
	}
};


// Strings and output formatting
//==============================================================================
inline std::string operator + (std::string str, std::string_view view)
{
	str.append(view.begin(), view.end());
	return str;
}


constexpr auto deref = [](auto *p) -> const decltype(*p)& { return *p; };

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

template<typename T>
std::string str(T const &r)
{
	std::ostringstream os;
	os << r;
	return std::move(os).str();
}


// Variant matching
//==============================================================================
// The following is taken from https://github.com/AVasK/vx to make std::visit more ergonomic.
// Example:
//
//     std::variant<int, float> var;
//     var | match {
//         [](int i) { ... },
//         [](float f) { ... },
//     };
//
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
bool is(std::variant<Ss...> const &v)
{
	return std::holds_alternative<T>(v);
}


// Iterators/ranges
//==============================================================================
template<typename Iterator>
class PairValueIterator {
public:
    using iterator_type = Iterator;
    using value_type = typename Iterator::value_type::second_type;
    using reference = value_type&;
    using pointer = value_type*;
    using difference_type = typename std::iterator_traits<Iterator>::difference_type;
    using iterator_category = typename std::iterator_traits<Iterator>::iterator_category;

    PairValueIterator(iterator_type it) : it(it) {}

    reference operator*() const {
        return it->second;
    }

    pointer operator->() const {
        return &(it->second);
    }

    PairValueIterator& operator++() {
        ++it;
        return *this;
    }

    PairValueIterator operator++(int) {
        PairValueIterator tmp = *this;
        ++it;
        return tmp;
    }

    bool operator==(const PairValueIterator& other) const {
        return it == other.it;
    }

    bool operator!=(const PairValueIterator& other) const {
        return it != other.it;
    }

private:
    iterator_type it;
};


template<typename It>
struct Range
{
	It first;
	It last;
};

template<typename It>
inline It begin(Range<It> r) { return r.first; }

template<typename It>
inline It end(Range<It> r) { return r.last; }


template<typename RangeT>
decltype(auto) reversed(RangeT const &r)
{
	return Range{r.rbegin(), r.rend()};
}


// Misc
//==============================================================================
inline void combine_hashes(size_t &seed, size_t hash)
{
    seed ^= hash + 0x9e3779b9 + (seed<<6) + (seed>>2);
}

template<typename T>
inline size_t compute_hash(T const &t)
{
	return std::hash<T>()(t);
}


template<typename EnumT>
class Flags
{
public:
	using EnumType = EnumT;
	using ValueType = std::underlying_type_t<EnumT>;

	Flags() = default;
	Flags(EnumType f) :
		m_value{static_cast<ValueType>(f)} {}

	bool isset(EnumType f) const
	{
		return m_value & static_cast<ValueType>(f);
	}

	bool set(EnumType f) const
	{
		return m_value |= static_cast<ValueType>(f);
	}

private:
	ValueType m_value{};
};


class XInt64
{
public:
	XInt64() :
		m_is_negative(false),
		m_value(0) {}

	explicit XInt64(int64_t value) :
		m_is_negative(value < 0),
		m_value(std::abs(value)) {}

	explicit XInt64(uint64_t value) :
		m_is_negative(false),
		m_value(value) {}

	explicit XInt64(bool is_negative, uint64_t value) :
		m_is_negative(is_negative),
		m_value(value) {}

	int64_t as_signed() const
	{
		return m_is_negative ? -int64_t(m_value) : int64_t(m_value);
	}

	uint64_t as_unsigned() const
	{
		assert(!m_is_negative);
		return m_value;
	}

	bool is_negative() const { return m_is_negative; }

	XInt64 operator - () const
	{
		return XInt64(not m_is_negative, m_value);
	}

	friend XInt64 operator + (XInt64 a, XInt64 b)
	{
		if(a.is_negative() == b.is_negative())
			return XInt64(a.is_negative(), a.m_value + b.m_value);
		else
		{
			if(b.is_negative())
				std::swap(a, b);

			assert(a.is_negative());
			assert(not b.is_negative());

			if(a.m_value < b.m_value)
				return XInt64(false, b.m_value - a.m_value);
			else
				return XInt64(true, a.m_value - b.m_value);
		}
	}

	friend XInt64 operator - (XInt64 a, XInt64 b)
	{
		return a + (-b);
	}

	uint64_t raw() const { return m_value; }

	friend bool operator == (XInt64 a, XInt64 b)
	{
		if(a.m_value == 0 && b.m_value == 0)
			return true;

		return a.m_value == b.m_value && a.m_is_negative == b.m_is_negative;
	}

	friend bool operator < (XInt64 a, XInt64 b)
	{
		if(a.is_negative())
		{
			if(b.is_negative())
				return a.as_signed() < b.as_signed();

			return true;
		}
		else
		{
			if(b.is_negative())
				return false;

			return a.as_unsigned() < b.as_unsigned();
		}
	}

	friend bool operator <= (XInt64 a, XInt64 b)
	{
		return a == b || a < b;
	}

private:
	bool m_is_negative;
	uint64_t m_value;
};
