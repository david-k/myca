#pragma once

#include <iosfwd>
#include <type_traits>

#include "syntax.hpp"
#include "utils.hpp"

using std::string;


//==============================================================================
// CBackend
//==============================================================================
struct LineEnd_Tag {};
constexpr inline LineEnd_Tag LineEnd;

class CBackend
{
public:
	explicit CBackend(std::ostream &os);

	template<typename T>
	CBackend& operator << (T const &v)
		requires std::is_integral_v<T> || std::is_same_v<T, std::string> || std::is_same_v<T, std::string_view> || std::is_array_v<T>
	{
		if(insert_indent)
		{
			os << string(indent_level*4, ' ');
			insert_indent = false;
		}

		os << v;
		return *this;
	}

	// By using a special overload for RangeFmt (instead of using the templated one above) we ensure
	// that we only insert the indention if the range is not empty.
	template<typename RangeT, typename FuncT>
	CBackend& operator << (RangeFmt<RangeT, FuncT> const &r)
	{
		os << r;
		return *this;
	}

	CBackend& operator << (LineEnd_Tag);
	CBackend& operator << (Type const &type);

	void increase_indent() { indent_level += 1; }
	void decrease_indent() { indent_level -= 1; }

	string new_tmp_var() { return "__myca__tmp" + std::to_string(tmp_var_counter++); }

	void set_current_proc(ProcInstance *proc) { m_cur_proc = proc; }
	ProcInstance* proc() const { return m_cur_proc; }

private:
	std::ostream &os;
	int indent_level = 0;
	bool insert_indent = true;
	int tmp_var_counter = 0;

	ProcInstance *m_cur_proc = nullptr;
};


//==============================================================================
// C code generation
//==============================================================================
void generate_c(Module &mod, CBackend &backend);
