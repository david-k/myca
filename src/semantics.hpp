#pragma once

#include <algorithm>
#include <bits/elements_of.h>
#include <memory>
#include <ostream>
#include <ranges>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <variant>
#include <span>
#include <vector>

#include "syntax.hpp"
#include "utils.hpp"


using std::variant;
using std::vector;
using std::string;
using std::string_view;
using std::unordered_map;
using std::unordered_set;
using std::span;


//==============================================================================
struct Scope;

struct Struct
{
	StructItem *parent;
	Scope *type_scope;
	FixedArray<Parameter const*> *NULLABLE ctor_params = nullptr;
	int num_initial_var_members = 0;
	int depth = 0;
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


//==============================================================================
// Type environment
//==============================================================================
bool type_var_occurs_in(VarType var, Type const &type);
Type materialize_known_int(KnownIntType known_int);

class TypeEnv
{
public:
	Type const* try_lookup(VarType var) const
	{
		auto it = m_env.find(var);
		if(it == m_env.end())
			return nullptr;

		return &it->second;
	}

	Type const& lookup(VarType var) const
	{
		auto it = m_env.find(var);
		assert(it != m_env.end());

		return it->second;
	}

	Type& lookup(VarType var)
	{
		auto it = m_env.find(var);
		assert(it != m_env.end());

		return it->second;
	}

	void add(VarType var, Type const &type)
	{
		// Required by unification and would lead to infinite recursion in substitute()
		assert(not type_var_occurs_in(var, type));

		auto res = m_env.emplace(var, type);
		assert(res.second);
	}

	bool empty() const { return m_env.empty(); }

	unordered_map<VarType, Type> const& env() const { return m_env; }

	void materialize()
	{
		for(auto &[_, type]: m_env)
		{
			if(KnownIntType *known_int = std::get_if<KnownIntType>(&type))
				type = materialize_known_int(*known_int);
		}
	}

	void print(std::ostream &os, Module const &mod) const
	{
		vector<std::pair<VarType, Type const*>> sorted;
		for(auto const &[var, type]: m_env)
			sorted.push_back({var, &type});

		std::ranges::sort(sorted, [](auto const &a, auto const &b)
		{
			VarType va = a.first;
			VarType vb = b.first;

			if(va.index() < vb.index())
				return true;

			return va | match
			{
				[&](TypeParameterVar p)
				{
					return p.def->name < std::get<TypeParameterVar>(vb).def->name;
				},
				[&](TypeDeductionVar d)
				{
					return d.id < std::get<TypeDeductionVar>(vb).id;
				}
			};
		});

		for(auto const &[var, type]: sorted)
		{
			::print(var, os);
			os << " ==> ";
			::print(*type, mod, os);
			os << std::endl;
		}
	}

private:
	unordered_map<VarType, Type> m_env;
};


//==============================================================================
// Instance registry
//==============================================================================
class InstanceRegistry;


struct TypeArgList
{
	// Fully resolved and must not contain KnownIntTypes
	FixedArray<Type> *args;

	unordered_set<VarType> occurring_vars{};
	bool has_type_deduction_vars = false;
};

struct MemoryLayout
{
	size_t size{};
	size_t alignment{};

	size_t extend(MemoryLayout other)
	{
		// Offset to ensure proper alignment
		size_t offset = 0;
		if(other.alignment > 0 && size % other.alignment)
			offset = other.alignment - (size % other.alignment); 

		size += offset + other.size;
		alignment = std::max(alignment, other.alignment);

		return size - other.size;
	}
};


//--------------------------------------------------------------------
// StructInstance
//--------------------------------------------------------------------
// Defines the memory region inside a struct that is used to store the struct's case members.
struct CaseMemberRegion
{
	// Offset from the start of the struct to which the case members reside in memory.
	// The start is directly after the last initial var member (with no padding).
	size_t start;
	// The total number of bytes reserved for the case members, starting from `start`.
	// This may by larger than the largest case member due to incorporating alignment (in case a
	// case member cannot directly be located at `start` due to alignment requirements).
	size_t size;

	size_t end() const { return start + size; }
};

using InstanceMember = variant<Parameter, StructInstance*>;

using TypeInstance = variant<StructInstance*, UnionInstance*>;

template<>
struct std::hash<::TypeInstance>
{
	size_t operator () (::TypeInstance const &v) const
	{
		return v | match
		{
			[&](StructInstance const *s) { return ::compute_hash(s); },
			[&](UnionInstance const *s) { return ::compute_hash(s); },
		};
	}
};


// A key that uniquely identifies a StructInstance.
struct StructInstanceKey
{
	StructInstanceKey(StructItem const *struct_, FixedArray<Type> *NULLABLE type_args, StructInstance *NULLABLE parent) :
		struct_(struct_),
		type_args(type_args and type_args->count == 0 ? nullptr : type_args),
		parent(parent) {}

	StructItem const *struct_;
	FixedArray<Type> *NULLABLE type_args;
	StructInstance *NULLABLE parent;
};

enum class LayoutComputationState
{
	PENDING,
	IN_PROGRESS,
	DONE,
};


// Once constructed, the type arguments never change
class StructInstance
{
public:
	StructInstance(
		StructItem const *struct_,
		TypeArgList const &type_args,
		StructInstance *parent,
		size_t id,
		InstanceRegistry *registry
	) :
		m_registry(registry),
		m_struct(struct_),
		m_type_args(type_args),
		m_parent(parent),
		m_id(id) {}


	Module const& mod() const;

	StructItem const* struct_() const { return m_struct; }
	StructInstance *NULLABLE parent() { return m_parent; }
	StructInstance const *NULLABLE parent() const { return m_parent; }
	TypeArgList const& type_args() const { return m_type_args; }

	StructInstance *NULLABLE root()
	{
		if(m_parent)
			return m_parent->root();

		return this;
	}

	size_t id() const { return m_id; }
	int depth() const { return m_struct->sema->depth; }

	TypeEnv create_type_env() const;

	StructInstanceKey key() { return StructInstanceKey(m_struct, m_type_args.args, m_parent); }

	// Returns true if all type arguments are concrete, i.e., if none of them contains a type variable
	bool is_concrete() const;

	// Returns true if none of the type arguments contains a TypeDeductionVar
	bool is_deduction_complete() const;

	// Returns the ProcType of the constructor, or nullptr of the struct does not have a constructor
	Type *NULLABLE try_get_ctor_type();

	StructInstance *NULLABLE implicit_case();

	int case_idx();

	std::generator<Parameter const&> own_var_members();
	std::generator<StructInstance*> own_case_members();

	std::generator<Parameter&> initial_var_members();
	std::generator<Parameter&> trailing_var_members();
	std::generator<Parameter const&> all_var_members();

	void finalize_typechecking();

	size_t get_param_count();
	Type* get_ctor_param_type_at(size_t idx);
	string_view get_ctor_param_name_at(size_t idx);
	DefaultValueExpr get_ctor_param_default_value(size_t idx);

	// Computes the struct's memory layout considering only its own members, ignoring any members of
	// a potential parent
	MemoryLayout compute_own_layout();

	MemoryLayout layout();

	CaseMemberRegion cases_layout() const { assert(m_cases_layout); return *m_cases_layout; }
	Type const* discriminator_type() const;

	unordered_set<TypeInstance> const& own_type_deps() const { return m_type_deps; }

private:
	InstanceRegistry *m_registry;

	StructItem const *m_struct;
	TypeArgList m_type_args;
	StructInstance *m_parent;
	size_t m_id;
	bool m_finalized = false;

	// Dependent properties (those that depend on `m_type_args`).
	// Lazily computed by compute_dependent_properties().
	//
	// The reason we compute these fields lazily and not in InstanceRegistry::register_struct() is
	// because if two structs refer to each other, then we could end up in a loop, because computing
	// these fields may require further resolution.
	bool m_dependent_properties_computed = false;
	FixedArray<InstanceMember> *m_members = nullptr;
	Type *m_ctor_proc_type = nullptr;
	FixedArray<Parameter const*> *m_ctor_params = nullptr;
	StructInstance *m_implicit_case = nullptr;
	int m_case_idx;

	// Memory layout
	LayoutComputationState m_layout_state = LayoutComputationState::PENDING;
	optional<MemoryLayout> m_own_layout{};
	Type *NULLABLE m_discriminator_type = nullptr; // if `m_struct->num_case_members > 0`
	optional<CaseMemberRegion> m_cases_layout{}; // if `m_struct->num_case_members > 0`
	unordered_set<TypeInstance> m_type_deps{};

	void compute_dependent_properties();
};


bool operator == (StructInstanceKey const &a, StructInstanceKey const &b);

template<>
struct std::hash<::StructInstanceKey>
{
	size_t operator () (::StructInstanceKey const &key) const
	{
		size_t h = ::compute_hash(key.struct_);
		::combine_hashes(h, ::compute_hash(key.parent));

		if(key.type_args)
		{
			for(Type const &t: *key.type_args)
				::combine_hashes(h, ::compute_hash(t));
		}

		return h;
	}
};


//--------------------------------------------------------------------
// ProcInstance
//--------------------------------------------------------------------
struct ProcInstanceKey
{
	ProcInstanceKey(ProcItem const *proc, FixedArray<Type> *NULLABLE type_args) :
		proc(proc),
		type_args(type_args and type_args->count == 0 ? nullptr : type_args) {}

	ProcItem const *proc;
	FixedArray<Type> *NULLABLE type_args;
};


class ProcInstance
{
public:
	ProcInstance(ProcItem const *proc, TypeArgList const &type_args, InstanceRegistry *registry) :
		m_registry(registry),
		m_proc(proc),
		m_type_args(type_args) {}

	InstanceRegistry* registry() { return m_registry; }
	ProcItem const* proc() const { return m_proc; }
	TypeArgList const& type_args() const { return m_type_args; }

	TypeEnv create_type_env() const;

	bool is_concrete() const { return m_type_args.occurring_vars.empty(); }

	bool is_deduction_complete() const { return not m_type_args.has_type_deduction_vars; }

	ProcInstanceKey key() { return ProcInstanceKey(m_proc, m_type_args.args); }

	Type* get_type()
	{
		if(not m_type)
			compute_dependent_properties();

		return m_type;
	}

	ProcType const& get_proc_type()
	{
		return std::get<ProcType>(*get_type());
	}

	size_t get_param_count();
	Type* get_param_type_at(size_t idx);
	string_view get_param_name_at(size_t idx);
	DefaultValueExpr get_param_default_value(size_t idx);

private:
	InstanceRegistry *m_registry;
	ProcItem const *m_proc;
	TypeArgList m_type_args;

	// Dependent properties (those that depend on `m_type_args`).
	// Lazily computed by compute_dependent_properties().
	Type *m_type = nullptr;

	void compute_dependent_properties();
};


bool operator == (ProcInstanceKey const &a, ProcInstanceKey const &b);

template<>
struct std::hash<::ProcInstanceKey>
{
	size_t operator () (::ProcInstanceKey const &key) const
	{
		size_t h = ::compute_hash(key.proc);

		if(key.type_args)
		{
			for(Type const &t: *key.type_args)
				::combine_hashes(h, ::compute_hash(t));
		}

		return h;
	}
};


//--------------------------------------------------------------------
// ProcTypeInstance
//--------------------------------------------------------------------
struct ProcTypeInstanceKey
{
	FixedArray<Type> *params;
	Type *ret;
};

struct ProcTypeInstance
{
	FixedArray<Type> *params;
	Type *ret;

	unordered_set<VarType> occurring_vars;
	bool has_type_deduction_vars;

	bool is_deduction_complete() const { return not has_type_deduction_vars; }
};



bool operator == (ProcTypeInstanceKey const &a, ProcTypeInstanceKey const &b);

template<>
struct std::hash<ProcTypeInstanceKey>
{
	size_t operator () (ProcTypeInstanceKey const &key) const
	{
		size_t h = compute_hash(*key.ret);

		for(Type const &t: *key.params)
			combine_hashes(h, compute_hash(t));

		return h;
	}
};


//--------------------------------------------------------------------
// UnionInstance
//--------------------------------------------------------------------
struct UnionInstanceKey
{
	span<Type const*> alternatives;
};

bool equiv(Type const &a, Type const &b);

struct TypeEquiv
{
	bool operator () (Type const &a, Type const &b) const
	{
		return equiv(a, b);
	}
};

class UnionInstance
{
public:

	UnionInstance(
		vector<Type const*> &&alternatives,
		unordered_set<VarType> &&occurring_vars,
		bool has_type_deduction_vars
	) :
		m_alternatives(std::move(alternatives)),
		m_occurring_vars(std::move(occurring_vars)),
		m_has_type_deduction_vars(has_type_deduction_vars) {}

	bool is_concrete() const { return m_occurring_vars.empty(); }
	bool has_type_deduction_vars() const { return m_has_type_deduction_vars; }
	bool is_deduction_complete() const { return not m_has_type_deduction_vars; }
	unordered_set<VarType> const& occurring_vars() const { return m_occurring_vars; }

	vector<Type const*> const& alternatives() { return m_alternatives; }

	size_t get_alt_idx(Type const &type) { compute_properties(); return m_alt_to_idx.at(type); }
	optional<size_t> try_get_alt_idx(Type const &type)
	{
		compute_properties();

		auto it = m_alt_to_idx.find(type);
		if(it == m_alt_to_idx.end())
			return nullopt;

		return it->second;
	}

	MemoryLayout layout();
	unordered_set<TypeInstance> const& type_deps() const { return m_type_deps; }

private:
	vector<Type const*> m_alternatives;
	unordered_set<VarType> m_occurring_vars;
	bool m_has_type_deduction_vars;

	// Memory layout
	LayoutComputationState m_layout_state = LayoutComputationState::PENDING;
	optional<MemoryLayout> m_layout{};
	unordered_set<TypeInstance> m_type_deps{};

	// Lazily computed properties
	using AltIdxMap = unordered_map<Type, size_t, std::hash<Type>, TypeEquiv>;
	bool m_properties_computed = false;
	AltIdxMap m_alt_to_idx;

	void compute_properties();
};



bool operator == (UnionInstanceKey const &a, UnionInstanceKey const &b);

template<>
struct std::hash<UnionInstanceKey>
{
	size_t operator () (UnionInstanceKey const &key) const
	{
		size_t h = compute_hash(key.alternatives.size());
		for(Type const *t: key.alternatives)
			combine_hashes(h, compute_hash(*t));

		return h;
	}
};


//--------------------------------------------------------------------
// InstanceRegistry
//--------------------------------------------------------------------
class InstanceRegistryListener
{
public:
	virtual void on_new_struct_instance(StructInstance*) {}
	virtual void on_new_union_instance(UnionInstance*) {}
	virtual void on_new_proc_instance(ProcInstance*) {}
};

struct NewInstanceListener : InstanceRegistryListener
{
	virtual void on_new_struct_instance(StructInstance *inst) override
	{
		structs.push_back(inst);
	}

	virtual void on_new_union_instance(UnionInstance *inst) override
	{
		unions.push_back(inst);
	}

	virtual void on_new_proc_instance(ProcInstance *inst) override
	{
		procs.push_back(inst);
	}

	vector<StructInstance*> structs;
	vector<UnionInstance*> unions;
	vector<ProcInstance*> procs;
};

struct BestEffortSubstitution {};
struct FullDeductionSubsitution
{
	TokenRange region_being_substituted;
};
struct FullSubsitution {};
using SubstitutionMode = variant<BestEffortSubstitution, FullDeductionSubsitution, FullSubsitution>;

class InstanceRegistry
{
public:
	InstanceRegistry(Module &mod, Arena &arena) :
		m_arena(arena),
		m_mod(mod) {}

	Module& mod() { return m_mod; }
	Arena& arena() { return m_arena; }

	StructInstance* get_struct_instance(
		StructItem const *struct_,
		FixedArray<Type> *NULLABLE type_args,
		StructInstance *NULLABLE parent
	);

	StructInstance* get_struct_instance(
		StructItem const *struct_,
		TypeArgList const &type_args,
		StructInstance *NULLABLE parent
	);

	// Apply `subst` to `type_args` before retrieving the StructInstance. Note that `subst` is
	// *not* applied to the type args of `parent`.
	//
	// Using this method can save some memory if the StructInstance already exists (compared to
	// the case of applying `subst` yourself).
	StructInstance* get_struct_instance(
		StructItem const *struct_,
		TypeArgList const &type_args,
		TypeEnv const &subst,
		StructInstance *NULLABLE parent,
		SubstitutionMode mode
	);

	ProcInstance* get_proc_instance(ProcItem const *proc, FixedArray<Type> *NULLABLE type_args);
	ProcInstance* get_proc_instance(ProcItem const *proc, TypeArgList const &type_args);
	ProcInstance* get_proc_instance(
		ProcItem const *proc,
		TypeArgList const &type_args,
		TypeEnv const &subst,
		SubstitutionMode mode
	);

	ProcTypeInstance* get_proc_type_instance(FixedArray<Type> *params, Type *ret);
	ProcTypeInstance* get_proc_type_instance(
		FixedArray<Type> *params,
		Type *ret,
		TypeEnv const &subst,
		SubstitutionMode mode
	);

	// `alternatives` must be the result of canonicalize_union_alternatives()
	UnionInstance* get_union_instance(vector<Type const*> &&alternatives);

	std::generator<StructInstance&> struct_instances();
	std::generator<UnionInstance&> union_instances();
	std::generator<ProcInstance&> proc_instances();

	void add_listener(InstanceRegistryListener *l);
	void remove_listener(InstanceRegistryListener *l);

private:
	Arena &m_arena;
	Module &m_mod;
	vector<InstanceRegistryListener*> m_listeners;

	// TODO After type-checking a full-expression, we can remove all instances
	//      for which is_deduction_complete() returns false
	unordered_map<StructInstanceKey, StructInstance> m_struct_instances;
	unordered_map<ProcInstanceKey, ProcInstance> m_proc_instances;
	unordered_map<ProcTypeInstanceKey, ProcTypeInstance> m_proc_type_instances;
	unordered_map<UnionInstanceKey, UnionInstance> m_union_instances;

	size_t next_struct_id() const { return m_struct_instances.size(); }
	StructInstance* add_struct_instance(StructInstance &&new_inst);
	ProcInstance* add_proc_instance(ProcInstance &&new_inst);
	ProcTypeInstance* add_proc_type_instance(FixedArray<Type> *params, Type *ret);
	UnionInstance* add_union_instance(vector<Type const*> &&alternatives);
};

// Visits each registered StructInstance, ensuring that new instances that might be added by the
// passed `func` are visited as well.
template<typename Func>
void for_each_struct_instance(InstanceRegistry &registry, Func &&func)
{
	NewInstanceListener listener;
	registry.add_listener(&listener);

	vector<StructInstance*> structs;
	for(StructInstance &struct_: registry.struct_instances())
		structs.push_back(&struct_);

	while(structs.size())
	{
		for(StructInstance *struct_: structs)
			func(struct_);

		structs = std::move(listener.structs);
		listener.structs = {};
	}

	registry.remove_listener(&listener);
}

template<typename Func>
void for_each_instance(InstanceRegistry &registry, Func &&func)
{
	NewInstanceListener listener;
	registry.add_listener(&listener);

	vector<StructInstance*> structs;
	vector<UnionInstance*> unions;
	vector<ProcInstance*> procs;

	for(StructInstance &struct_: registry.struct_instances())
		structs.push_back(&struct_);

	for(UnionInstance &union_: registry.union_instances())
		unions.push_back(&union_);

	for(ProcInstance &proc: registry.proc_instances())
		procs.push_back(&proc);

	while(structs.size() or unions.size() or procs.size())
	{
		for(StructInstance *struct_: structs)
			func(struct_);

		for(UnionInstance *union_: unions)
			func(union_);

		for(ProcInstance *proc: procs)
			func(proc);

		structs = std::move(listener.structs);
		unions = std::move(listener.unions);
		procs = std::move(listener.procs);
		listener.structs = {};
		listener.unions = {};
		listener.procs = {};
	}

	registry.remove_listener(&listener);
}


//==============================================================================
// Scope
//==============================================================================
struct Module;

using ScopeItem = variant<Var, TypeParameter*, ProcItem*, StructItem*, AliasItem*>;

struct Scope
{
	Module *mod;
	Scope *NULLABLE parent = nullptr;
	vector<std::unique_ptr<Scope>> children;
	UnorderedStringMap<ScopeItem> items_by_name;
	bool accept_item_decls;


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
	void declare_type_var(TypeParameter *def);
	void declare_struct(StructItem *struct_);
	void declare_proc(ProcItem *proc);
	void declare_alias(AliasItem *alias);
	ScopeItem& declare(string_view name, ScopeItem &&item, TokenRange sloc);

	// Lookup
	ScopeItem& lookup(string_view const &name, TokenIdx sloc, bool traverse_upwards = true);
};


//==============================================================================
// Module
//==============================================================================
struct SemaModule
{
	SemaModule(std::unique_ptr<Scope> global_scope, Arena &arena) :
		globals(std::move(global_scope)),
		insts(*globals->mod, arena),
		arena(arena) {}

	std::unique_ptr<Scope> globals;
	InstanceRegistry insts;
	Arena &arena;
};


bool is_builtin_type(Type const &type, BuiltinTypeDef builtin);
BuiltinTypeDef smallest_int_type_for(Int128 low, Int128 high);
BuiltinTypeDef smallest_int_type_for(Int128 value);

Type const* is_optional_ptr(StructInstance const *struct_);
Type const* is_optional_ptr(Type const &type);

Stmt clone(Stmt const &stmt, Arena &arena);
void substitute_types_in_stmt(Stmt &stmt, TypeEnv const &subst, InstanceRegistry &registry, SubstitutionMode mode);

MemoryLayout compute_layout(Type const &type);
void compute_type_layouts(Module &mod);

// Perform semantic analysis
void sema(Module &mod, Arena &arena);
