#pragma once

#include "utils.hpp"
#include "semantics/instance_registry.hpp"

struct Scope;

struct DeclContainer : variant<StructItem*, ProcItem*>
{
	using variant::variant;
	StructItem* as_struct() const { return std::get<StructItem*>(*this); }
};

struct Struct
{
	optional<DeclContainer> decl_parent;
	Scope *type_scope;
	FixedArray<Parameter const*> *NULLABLE ctor_params = nullptr;
	int num_initial_var_members = 0;
	int variant_depth = 0;
};

struct Proc
{
	Scope *scope;
	FixedArray<Var*> *param_vars = nullptr;
};

enum class ResolutionState
{
	PENDING,
	IN_PROGRESS,
	DONE,
};
struct Alias
{
	Scope *scope; // Only contains type params
	ResolutionState resolution_state = ResolutionState::PENDING;
};

struct Var
{
	TokenRange sloc;
	string_view name;
	IsMutable mutability;
	Type *NULLABLE type = nullptr;
};

using ScopeItem = variant<Var, GenericParameter*, ProcItem*, StructItem*, AliasItem*>;

struct Scope
{
	explicit Scope(Module *mod, bool accept_item_decls, Scope *NULLABLE parent = nullptr) :
		mod(mod),
		parent(parent),
		accept_item_decls(accept_item_decls)
	{
		assert(accept_item_decls or parent);
	}

	// Scope handling
	Scope* new_child(bool child_accepts_item_decls);

	// Declaring items
	Var* declare_var(string_view name, IsMutable mutability, TokenRange sloc);
	void declare_type_var(GenericParameter *def);
	void declare_struct(StructItem *struct_);
	void declare_proc(ProcItem *proc);
	void declare_alias(AliasItem *alias);
	ScopeItem& declare(string_view name, ScopeItem &&item, TokenRange sloc);

	// Lookup
	ScopeItem& lookup(string_view const &name, TokenIdx sloc, bool traverse_upwards = true);
	ScopeItem* try_lookup(string_view const &name, TokenIdx sloc, bool traverse_upwards = true);

	Module *mod;
	Scope *NULLABLE parent = nullptr;
	vector<std::unique_ptr<Scope>> children;
	UnorderedStringMap<ScopeItem> items_by_name;
	bool accept_item_decls;
};

class EventLogger
{
public:
	EventLogger(Module *mod, std::ostream &os);
	~EventLogger();

	// Compiler pass events
	void on_declare_items_start();
	void on_declare_items_end();
	void on_resolve_names_start();
	void on_resolve_names_end();
	void on_typecheck_start();
	void on_typecheck_end();
	void on_layout_computation_start();
	void on_layout_computation_end();

	void on_struct_substitution_start(StructInstance *inst);
	void on_struct_substitution_replaced(StructInstance *inst);
	void on_struct_substitution_noop();
	void on_struct_substitution_end();
	void on_struct_layout_computation_start(StructInstance *inst);
	void on_struct_layout_computation_end();
	void on_struct_register(StructInstance *inst);

	void on_proc_register(ProcInstance *inst);
	void on_proc_start(ProcItem *proc);
	void on_proc_end();

	void on_stmt_start(Stmt const &stmt);
	void on_stmt_end();

	void on_expr_start(Expr const &expr);
	void on_expr_end();

	void on_data(TypeEnv const &env);
	void on_data(ConstraintSolver const &sys);

private:
	Module *m_mod;
	std::ostream *m_os;
	vector<Stmt const*> m_stmt_stack;
};

#define LOGGER(logger, event, ...)        \
	do {                                  \
		if(logger)                        \
			(logger)->event(__VA_ARGS__); \
	} while(false)                        \

struct SemaModule
{
	SemaModule(std::unique_ptr<Scope> global_scope, Arena &arena) :
		globals(std::move(global_scope)),
		insts(*globals->mod, arena),
		arena(arena) {}

	std::unique_ptr<Scope> globals;
	InstanceRegistry insts;
	Arena &arena;
	optional<EventLogger> logger{};
};
