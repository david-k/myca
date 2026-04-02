#include "semantics/module.hpp"
#include "semantics/constraint_solver.hpp"
#include "semantics/type_env.hpp"

//--------------------------------------------------------------------
// Scope
//--------------------------------------------------------------------
Scope* Scope::new_child(bool child_accepts_item_decls)
{
	children.push_back(std::make_unique<Scope>(mod, child_accepts_item_decls, this));
	return children.back().get();
}

ScopeItem& Scope::declare(string_view name, ScopeItem &&item, TokenRange sloc)
{
	auto res = items_by_name.emplace(name, std::move(item));
	if(not res.second)
		throw_sem_error("An item with this name has already been declared: "s + name, sloc.first, mod);

	return res.first->second;
}

Var* Scope::declare_var(string_view name, IsMutable mutability, TokenRange sloc)
{
	ScopeItem &item = declare(name, Var(sloc, name, mutability), sloc);
	return &std::get<Var>(item);
}

void Scope::declare_type_var(GenericParameter *def)
{
	declare(def->name, def, def->range);
}

void Scope::declare_struct(StructItem *struct_)
{
	if(not accept_item_decls)
		return parent->declare_struct(struct_);

	declare(struct_->name, struct_, struct_->range);
}

void Scope::declare_proc(ProcItem *proc)
{
	if(not accept_item_decls)
		return parent->declare_proc(proc);

	declare(proc->name, proc, proc->range);
}

void Scope::declare_alias(AliasItem *alias)
{
	if(not accept_item_decls)
		return parent->declare_alias(alias);

	declare(alias->name, alias, alias->range);
}

ScopeItem& Scope::lookup(string_view const &name, TokenIdx sloc, bool traverse_upwards)
{
	if(ScopeItem *item = try_lookup(name, sloc, traverse_upwards))
		return *item;

	throw_sem_error("Name has not been declared: "s + name, sloc, mod);
}

ScopeItem* Scope::try_lookup(string_view const &name, TokenIdx sloc, bool traverse_upwards)
{
	auto it = items_by_name.find(name);
	if(it != items_by_name.end())
		return &it->second;

	if(parent && traverse_upwards)
		return &parent->lookup(name, sloc);

	return nullptr;
}

//--------------------------------------------------------------------
// EventLogger
//--------------------------------------------------------------------
EventLogger::EventLogger(Module *mod, std::ostream &os) :
	m_mod(mod),
	m_os(&os)
{
	*m_os <<
R"###(<!doctype html>
<html>
	<head>
		<title>Myca Compiler Log</title>

		<style>

pre.data {
	margin-top: 5px;
	padding: 5px;
	background-color: #eee;
}

code.myca-inline {
	background-color: #eee;
	white-space: pre;
	padding: 0 3px;
}

span.side-info {
	font-size: small;
	color: gray;
}

.entry-struct-subst {
	display: none;
}

		</style>
	</head>
	<body>
)###";
}

EventLogger::~EventLogger()
{
	try
	{
		*m_os <<
R"###(
	</body>
</html>
)###" << std::flush;
	}
	catch(...) {}
}

void EventLogger::on_declare_items_start()
{
	*m_os << "<h2>Pass 1: Declare items</h2>\n";
	*m_os << "<ul>\n";
}

void EventLogger::on_declare_items_end()
{
	*m_os << "</ul>\n";
}

void EventLogger::on_resolve_names_start()
{
	*m_os << "<h2>Pass 2: Resolve names</h2>\n";
	*m_os << "<ul>\n";
}

void EventLogger::on_resolve_names_end()
{
	*m_os << "</ul>\n";
}

void EventLogger::on_typecheck_start()
{
	*m_os << "<h2>Pass 3: Typecheck</h2>\n";
	*m_os << "<ul>\n";
}

void EventLogger::on_typecheck_end()
{
	*m_os << "</ul>\n";
}

void EventLogger::on_stmt_start(Stmt const &stmt)
{
	auto output_html = [&](optional<string> const &custom = nullopt)
	{
		*m_os << "<li>Statement:";
			if(custom)
				*m_os << " <code class='myca-inline'>" << *custom << "</code>";

			*m_os << "<ul>\n";
			if(not custom)
			{
				*m_os << "<li>\n";
					*m_os << "<pre class='data'><code>";
					print(stmt, *m_mod, *m_os); *m_os << "</code></pre>";
				*m_os << "</li>\n";
			}
	};

	stmt | match
	{
		[&](LetStmt const&)
		{
			output_html();
		},
		[&](ExprStmt const&)
		{
			output_html();
		},
		[&](BlockStmt const&) {},
		[&](ReturnStmt const&)
		{
			output_html();
		},
		[&](IfStmt const&)
		{
			output_html("if");
		},
		[&](WhileStmt const&)
		{
			output_html("while");
		},
		[&](DeclStmt const&)
		{
			output_html("struct");
		},
		[&](MatchStmt const&)
		{
			output_html("match");
		},
	};

	m_stmt_stack.push_back(&stmt);
}

void EventLogger::on_stmt_end()
{
	Stmt const *stmt = m_stmt_stack.back();
	m_stmt_stack.pop_back();

	auto output_html = [&]()
	{
			*m_os << "</ul>\n";
		*m_os << "</li>\n";
	};

	*stmt | match
	{
		[&](LetStmt const&)
		{
			output_html();
		},
		[&](ExprStmt const&)
		{
			output_html();
		},
		[&](BlockStmt const&) {},
		[&](ReturnStmt const&)
		{
			output_html();
		},
		[&](IfStmt const&)
		{
			output_html();
		},
		[&](WhileStmt const&)
		{
			output_html();
		},
		[&](DeclStmt const&)
		{
			output_html();
		},
		[&](MatchStmt const&)
		{
			output_html();
		},
	};
}

void EventLogger::on_expr_start(Expr const &expr)
{
	*m_os << "<li>Expression:\n";
		*m_os << "<code class='myca-inline'>"; print(expr, *m_mod, *m_os); *m_os << "</code>";
		*m_os << "<ul>\n";
}

void EventLogger::on_expr_end()
{
		*m_os << "</ul>\n";
	*m_os << "</li>\n";
}

void EventLogger::on_struct_substitution_start(StructInstance *inst)
{
	*m_os << "<li class='entry-struct-subst'>Struct substitution: \n";
		*m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, *m_os); *m_os << "</code>";
		*m_os << " <span class='side-info'>(" << inst << ")</span>\n";
		*m_os << "<ul>\n";
}

void EventLogger::on_struct_substitution_replaced(StructInstance *inst)
{
	(void)inst;
			*m_os << "<li>Replaced: ";
			*m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, *m_os); *m_os << "</code>";
			*m_os << " <span class='side-info'>(" << inst << ")</span>\n";
			*m_os << "</li>\n";
}

void EventLogger::on_struct_substitution_noop()
{
			*m_os << "<li>NOOP</li>\n";
}

void EventLogger::on_struct_substitution_end()
{
		*m_os << "</ul>\n";
	*m_os << "</li>\n";
}


void EventLogger::on_struct_register(StructInstance *inst)
{
	string_view deduction_state = "partially deduced";
	if(inst->is_deduction_complete())
		deduction_state = "fully deduced";

	*m_os << "<li>Register struct: \n";
		*m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, *m_os); *m_os << "</code>";
		*m_os << " <span class='side-info'>[" << deduction_state << "]</span>\n";
		*m_os << " <span class='side-info'>[" << inst << "]</span>\n";
	*m_os << "</li>\n";
}

void EventLogger::on_proc_register(ProcInstance *inst)
{
	string_view deduction_state = "partially deduced";
	if(inst->is_deduction_complete())
		deduction_state = "fully deduced";

	*m_os << "<li>Register proc: \n";
		*m_os << "<code class='myca-inline'>" << inst->proc()->name << "</code>";
		*m_os << " <span class='side-info'>[" << deduction_state << "]</span>\n";
		*m_os << " <span class='side-info'>[" << inst << "]</span>\n";
	*m_os << "</li>\n";
}

void EventLogger::on_proc_start(ProcItem *proc)
{
	*m_os << "<li>Procedure: ";
		*m_os << "<code class='myca-inline'>";
		*m_os << proc->name;
		*m_os << "</code>";
		*m_os << "<ul>\n";
}

void EventLogger::on_proc_end()
{
		*m_os << "</ul>\n";
	*m_os << "</li>\n";
}


void EventLogger::on_layout_computation_start()
{
	*m_os << "<h2>Pass 4: Layout computation</h2>\n";
	*m_os << "<ul>\n";
}

void EventLogger::on_layout_computation_end()
{
	*m_os << "</ul>\n";
}

void EventLogger::on_struct_layout_computation_start(StructInstance *inst)
{
	*m_os << "<li>Compute layout: \n";
		*m_os << "<code class='myca-inline'>"; print(Type(StructType(UNKNOWN_TOKEN_RANGE, inst)), *m_mod, *m_os); *m_os << "</code>";
		*m_os << " <span class='side-info'>(" << inst << ")</span>\n";

		*m_os << "<ul>\n";
			*m_os << "<li>Members:\n";
			*m_os << "<ul>\n";
				for(Parameter const &m: inst->own_var_members())
				{
					*m_os << "<li><code class='myca-inline'>";
						*m_os << m_mod->name_of(m) << ": ";
						print(*m.type, *m_mod, *m_os);
					*m_os << "</code></li>\n";
				}
			*m_os << "</ul>\n";
			*m_os << "</li>\n";

			*m_os << "<li>Recursive layout computations:\n";
			*m_os << "<ul>\n";

				m_os->flush();
}

void EventLogger::on_struct_layout_computation_end()
{
				*m_os << "</ul>\n";
			*m_os << "</li>\n";
		*m_os << "</ul>\n";
	*m_os << "</li>\n";
}

void EventLogger::on_data(ConstraintSolver const &sys)
{
	if(not sys.empty())
	{
		*m_os << "<li>Constraints:\n";
			*m_os << "<pre class='data'><code>";
			sys.print(*m_os);
			*m_os << "</code></pre>\n";
		*m_os << "</li>\n";
	}
}

void EventLogger::on_data(TypeEnv const &subst)
{
	if(not subst.empty())
	{
		*m_os << "<li>Substitution:\n";
			*m_os << "<pre class='data'><code>";
			subst.print(*m_os, *m_mod);
			*m_os << "</code></pre>\n";
		*m_os << "</li>\n";
	}
}
