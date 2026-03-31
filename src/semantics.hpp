#pragma once

#include <algorithm>
#include <bits/elements_of.h>
#include <memory>
#include <ostream>
#include <ranges>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
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
// Scope items
//==============================================================================
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


//==============================================================================
// Type environment
//==============================================================================
bool equiv(Type const &a, Type const &b);
bool equiv(GenericArg const &a, GenericArg const &b);
bool type_var_occurs_in(GenericVar var, Type const &type);
bool type_var_occurs_in(GenericVar var, GenericArg const &arg);
Type materialize_known_int(KnownIntType known_int);


class TypeEnv
{
public:
	GenericArg const* try_lookup(GenericVar var) const
	{
		auto it = m_env.find(var);
		if(it == m_env.end())
			return nullptr;

		return &it->second;
	}

	GenericArg const& lookup(GenericVar var) const
	{
		auto it = m_env.find(var);
		assert(it != m_env.end());

		return it->second;
	}

	GenericArg& lookup(GenericVar var)
	{
		auto it = m_env.find(var);
		assert(it != m_env.end());

		return it->second;
	}

	bool update(GenericVar var, GenericArg const &new_type)
	{
		assert(not type_var_occurs_in(var, new_type));

		auto it = m_env.find(var);
		if(it == m_env.end())
		{
			m_env.emplace(var, new_type);
			return true;
		}

		bool updated = not equiv(new_type, it->second);
		it->second = new_type;
		return updated;
	}

	void add(GenericVar var, GenericArg const &type)
	{
		// Required by unification and would lead to infinite recursion in substitute()
		assert(not type_var_occurs_in(var, type));

		auto res = m_env.emplace(var, type);
		assert(res.second);
	}

	bool empty() const { return m_env.empty(); }
	unordered_map<GenericVar, GenericArg> const& mapping() const { return m_env; }

	void materialize(class InstanceRegistry &registry);
	void print(std::ostream &os, Module const &mod) const;

private:
	unordered_map<GenericVar, GenericArg> m_env;
};


//==============================================================================
// Instance registry
//==============================================================================
class InstanceRegistry;

bool have_common_vars(unordered_set<GenericVar> const &occurring_vars, TypeEnv const &env);

struct TypeArgList
{
	bool needs_subsitution(TypeEnv const &env) const
	{
		return has_known_ints or have_common_vars(occurring_vars, env);
	}

	// Fully resolved and must not contain KnownIntTypes
	FixedArray<GenericArg> *args;

	unordered_set<GenericVar> occurring_vars{};
	bool has_type_deduction_vars = false;
	bool has_known_ints = false;
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


// Both structs and procs are declaration containers in that they have their own scope that can
// contain further declarations
struct DeclContainerInst : variant<StructInstance*, ProcInstance*>
{
	using variant::variant;

	StructInstance* try_as_struct() const
	{
		if(StructInstance *const *inst = std::get_if<StructInstance*>(this))
			return *inst;

		return nullptr;
	}
	StructInstance* as_struct() const { return std::get<StructInstance*>(*this); }

	ProcInstance* as_proc() const { return std::get<ProcInstance*>(*this); }

	void finalize_typechecking();
	TypeEnv create_type_env() const;
	bool is_concrete() const;
	bool is_deduction_complete() const;

	Scope* scope() const;

	TypeArgList const& type_args() const;
	optional<DeclContainerInst> decl_parent() const;
};

// A key that uniquely identifies a StructInstance.
struct StructInstanceKey
{
	StructInstanceKey(
		StructItem const *struct_,
		FixedArray<GenericArg> *NULLABLE type_args,
		optional<DeclContainerInst> decl_parent
	) :
		struct_(struct_),
		type_args(type_args and type_args->count == 0 ? nullptr : type_args),
		decl_parent(decl_parent) {}

	StructItem const *struct_;
	FixedArray<GenericArg> *NULLABLE type_args;
	optional<DeclContainerInst> decl_parent;
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
		optional<DeclContainerInst> decl_parent,
		size_t id,
		InstanceRegistry *registry
	) :
		m_registry(registry),
		m_struct(struct_),
		m_type_args(type_args),
		m_decl_parent(decl_parent),
		m_id(id) {}


	Module const& mod() const;

	StructItem const* struct_() const { return m_struct; }
	optional<DeclContainerInst> decl_parent() const { return m_decl_parent; }
	TypeArgList const& type_args() const { return m_type_args; }
	bool is_case_member() const { return m_struct->is_case_member; }

	StructInstance *NULLABLE variant_parent() const
	{
		return is_case_member() ? m_decl_parent->as_struct() : nullptr;
	}

	StructInstance *variant_root()
	{
		if(is_case_member() and m_decl_parent)
			return m_decl_parent->as_struct()->variant_root();

		return this;
	}

	void typecheck_generic_args(class ConstraintGatheringSubst &subst, struct SemaContext &ctx);

	size_t id() const { return m_id; }
	int variant_depth() const { return m_struct->sema->variant_depth; }

	TypeEnv create_type_env() const;

	StructInstanceKey key() { return StructInstanceKey(m_struct, m_type_args.args, m_decl_parent); }

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
	unordered_set<TypeInstance> const& own_type_deps() const
	{
		assert(m_layout_state == LayoutComputationState::DONE);
		return m_type_deps;
	}

private:
	void compute_dependent_properties();

	InstanceRegistry *m_registry;
	StructItem const *m_struct;
	TypeArgList m_type_args;
	optional<DeclContainerInst> m_decl_parent;
	size_t m_id;
	bool m_finalized = false;

	// Dependent properties (those that depend on `m_type_args`).
	// Lazily computed by compute_dependent_properties().
	//
	// The reason we compute these fields lazily and not in InstanceRegistry::register_struct() is
	// because if two structs refer to each other, then we could end up in a loop, as computing
	// these fields may require further resolution.
	bool m_dependent_properties_computed = false;
	FixedArray<InstanceMember> *m_members = nullptr;
	Type *m_ctor_proc_type = nullptr;
	FixedArray<Parameter const*> *m_ctor_params = nullptr;
	StructInstance *m_implicit_case = nullptr;
	int m_case_idx;
	bool m_generic_args_typechecked = false;

	// Memory layout
	LayoutComputationState m_layout_state = LayoutComputationState::PENDING;
	optional<MemoryLayout> m_own_layout{};
	Type *NULLABLE m_discriminator_type = nullptr; // if `m_struct->num_case_members > 0`
	optional<CaseMemberRegion> m_cases_layout{}; // if `m_struct->num_case_members > 0`
	unordered_set<TypeInstance> m_type_deps{};
};


bool operator == (StructInstanceKey const &a, StructInstanceKey const &b);

template<>
struct std::hash<::StructInstanceKey>
{
	size_t operator () (::StructInstanceKey const &key) const
	{
		size_t h = compute_hash(key.struct_);
		combine_hashes(h, compute_hash(key.decl_parent.has_value()));
		if(key.decl_parent)
		{
			*key.decl_parent | match
			{
				[&](StructInstance *inst) { combine_hashes(h, compute_hash(inst)); },
				[&](ProcInstance *inst) { combine_hashes(h, compute_hash(inst)); },
			};
		}

		if(key.type_args)
		{
			for(GenericArg const &t: *key.type_args)
				combine_hashes(h, compute_hash(t));
		}

		return h;
	}
};


//--------------------------------------------------------------------
// ProcInstance
//--------------------------------------------------------------------
struct ProcInstanceKey
{
	ProcInstanceKey(ProcItem const *proc, FixedArray<GenericArg> *NULLABLE type_args) :
		proc(proc),
		type_args(type_args and type_args->count == 0 ? nullptr : type_args) {}

	ProcItem const *proc;
	FixedArray<GenericArg> *NULLABLE type_args;
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
	optional<DeclContainerInst> decl_parent() const { return nullopt; }

	TypeEnv create_type_env() const;

	void typecheck_generic_args(class ConstraintGatheringSubst &subst, struct SemaContext &ctx);

	bool is_concrete() const { return m_type_args.occurring_vars.empty() and not m_type_args.has_known_ints; }
	bool is_deduction_complete() const { return not m_type_args.has_type_deduction_vars and not m_type_args.has_known_ints; }
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
	void compute_dependent_properties();

	InstanceRegistry *m_registry;
	ProcItem const *m_proc;
	TypeArgList m_type_args;

	bool m_generic_args_typechecked = false;

	// Dependent properties (those that depend on `m_type_args`).
	// Lazily computed by compute_dependent_properties().
	Type *m_type = nullptr;
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
			for(GenericArg const &t: *key.type_args)
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
	bool needs_subsitution(TypeEnv const &env) const
	{
		return has_known_ints or have_common_vars(occurring_vars, env);
	}

	bool is_deduction_complete() const { return not has_type_deduction_vars and not has_known_ints; }
	void typecheck(class ConstraintGatheringSubst &subst, struct SemaContext &ctx);

	FixedArray<Type> *params;
	Type *ret;
	unordered_set<GenericVar> occurring_vars;
	bool has_type_deduction_vars;
	bool has_known_ints;
	bool has_been_typechecked = false;
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
// Used as a key for unordered_map
struct UnionInstanceKey
{
	// Refers to UnionInstanceKey::m_alternatives. This means m_alternatives must not be modified in
	// a way that affects the hash function. Currently, the m_alternatives is only modified by
	// typecheck_type, which sets the type for neseted Exprs. This is safe, as type_of(expr) is not
	// used when hashing expressions.
	span<Type*> alternatives;
};

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
		vector<Type*> &&alternatives,
		unordered_set<GenericVar> &&occurring_vars,
		bool has_type_deduction_vars,
		bool has_known_ints
	) :
		m_alternatives(std::move(alternatives)),
		m_occurring_vars(std::move(occurring_vars)),
		m_has_type_deduction_vars(has_type_deduction_vars),
		m_has_known_ints(has_known_ints) {}

	bool is_concrete() const { return m_occurring_vars.empty() and not m_has_known_ints; }
	bool has_type_deduction_vars() const { return m_has_type_deduction_vars; }
	bool has_known_ints() const { return m_has_known_ints; }
	bool is_deduction_complete() const { return not m_has_type_deduction_vars and not m_has_known_ints; }
	unordered_set<GenericVar> const& occurring_vars() const { return m_occurring_vars; }

	bool needs_subsitution(TypeEnv const &env) const
	{
		return m_has_known_ints or have_common_vars(m_occurring_vars, env);
	}

	using ConstTypeRange = std::ranges::subrange<vector<Type*>::const_iterator>;
	ConstTypeRange alternatives()
	{
		return ConstTypeRange(m_alternatives.cbegin(), m_alternatives.cend());
	}

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
	unordered_set<TypeInstance> const& type_deps()
	{
		// Make sure layout has been computed
		// TODO Do I also need to do this for StructInstance?
		layout();
		return m_type_deps;
	}

	void compute_properties();
	void typecheck(class ConstraintGatheringSubst &subst, struct SemaContext &ctx);

private:
	vector<Type*> m_alternatives;
	unordered_set<GenericVar> m_occurring_vars;
	bool m_has_type_deduction_vars;
	bool m_has_known_ints;
	bool m_has_been_typechecked = false;

	// Memory layout
	LayoutComputationState m_layout_state = LayoutComputationState::PENDING;
	optional<MemoryLayout> m_layout{};
	unordered_set<TypeInstance> m_type_deps{};

	// Lazily computed properties
	using AltIdxMap = unordered_map<Type, size_t, std::hash<Type>, TypeEquiv>;
	bool m_properties_computed = false;
	AltIdxMap m_alt_to_idx;
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
	virtual void on_new_proc_type_instance(ProcTypeInstance*) {}
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

	virtual void on_new_proc_type_instance(ProcTypeInstance *inst) override
	{
		proc_types.push_back(inst);
	}

	vector<StructInstance*> structs;
	vector<UnionInstance*> unions;
	vector<ProcInstance*> procs;
	vector<ProcTypeInstance*> proc_types;
};

struct BestEffortSubstitution {};
struct FullDeductionSubstitution
{
	TokenRange region_being_substituted;
};
struct FullSubstitution {};

using SubstitutionMode = variant<
	BestEffortSubstitution,
	FullDeductionSubstitution,
	FullSubstitution
>;

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
		FixedArray<GenericArg> *NULLABLE type_args,
		optional<DeclContainerInst> decl_parent
	);

	StructInstance* get_struct_instance(
		StructItem const *struct_,
		TypeArgList const &type_args,
		optional<DeclContainerInst> decl_parent
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
		optional<DeclContainerInst> decl_parent,
		SubstitutionMode mode
	);


	ProcInstance* get_proc_instance(ProcItem const *proc, FixedArray<GenericArg> *NULLABLE type_args);
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
	UnionInstance* get_union_instance(vector<Type*> &&alternatives);

	StructInstance* get_struct_self_instance(StructItem const *struct_);
	ProcInstance* get_proc_self_instance(ProcItem const *proc);
	DeclContainerInst get_self_instance(DeclContainer decl);

	std::generator<StructInstance&> struct_instances();
	std::generator<UnionInstance&> union_instances();
	std::generator<ProcInstance&> proc_instances();
	std::generator<ProcTypeInstance&> proc_type_instances();

	void add_listener(InstanceRegistryListener *l);
	void remove_listener(InstanceRegistryListener *l);

private:
	size_t next_struct_id() const { return m_struct_instances.size(); }
	StructInstance* add_struct_instance(StructInstance &&new_inst);
	ProcInstance* add_proc_instance(ProcInstance &&new_inst);
	ProcTypeInstance* add_proc_type_instance(FixedArray<Type> *params, Type *ret);
	UnionInstance* add_union_instance(vector<Type*> &&alternatives);

	Arena &m_arena;
	Module &m_mod;
	vector<InstanceRegistryListener*> m_listeners;

	// TODO After type-checking a full-expression, we can remove all instances
	//      for which is_deduction_complete() returns false
	unordered_map<StructInstanceKey, StructInstance> m_struct_instances;
	unordered_map<ProcInstanceKey, ProcInstance> m_proc_instances;
	unordered_map<ProcTypeInstanceKey, ProcTypeInstance> m_proc_type_instances;
	unordered_map<UnionInstanceKey, UnionInstance> m_union_instances;

	unordered_map<StructItem const*, StructInstance*> m_struct_self_instances;
	unordered_map<ProcItem const*, ProcInstance*> m_proc_self_instances;
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
	vector<ProcTypeInstance*> proc_types;

	for(StructInstance &struct_: registry.struct_instances())
		structs.push_back(&struct_);

	for(UnionInstance &union_: registry.union_instances())
		unions.push_back(&union_);

	for(ProcInstance &proc: registry.proc_instances())
		procs.push_back(&proc);

	for(ProcTypeInstance &proc_type: registry.proc_type_instances())
		proc_types.push_back(&proc_type);

	while(structs.size() or unions.size() or procs.size())
	{
		for(StructInstance *struct_: structs)
			func(struct_);

		for(UnionInstance *union_: unions)
			func(union_);

		for(ProcInstance *proc: procs)
			func(proc);

		for(ProcTypeInstance *proc_type: proc_types)
			func(proc_type);

		structs = std::move(listener.structs);
		unions = std::move(listener.unions);
		procs = std::move(listener.procs);
		proc_types = std::move(listener.proc_types);
		listener.structs = {};
		listener.unions = {};
		listener.procs = {};
		listener.proc_types = {};
	}

	registry.remove_listener(&listener);
}


//==============================================================================
// Sema context
//==============================================================================
struct TypeDeductionVar {};
struct ValueDeductionVar { Type *type; };
using DeductionVarKind = variant<TypeDeductionVar, ValueDeductionVar>;

struct DeductionVarDef
{
	uint32_t id;
	DeductionVarKind kind;
};

// Holds all the state needed during semantic analysis
struct SemaContext
{
	SemaContext(Module &mod, Arena &arena) :
		mod(&mod),
		arena(arena) {}

	GenericDeductionVar new_deduction_var(DeductionVarKind kind)
	{
		DeductionVarDef *def = arena.alloc<DeductionVarDef>(next_deduction_var_id++, kind);
		return GenericDeductionVar(def);
	}

	Module *mod;
	Arena &arena;
	uint32_t next_deduction_var_id = 0;
	ProcItem *NULLABLE proc = nullptr; // The current procedure being analyzed
};


//==============================================================================
// Unification
//==============================================================================
struct SemaContext;
struct ConstraintSystem;

enum class TypeConversion
{
	// Allows:
	// - Merging of KnownIntTypes
	// - Conversion of KnownIntTypes to those concrete integer types that can hold it
	NONE,

	// Allows:
	// - Everything allowed by NONE
	// - Any type conversions where the memory layout remains the same and the subtyping relation is
	//   respected. For example, the conversion Option.Some ==> Option is allowed, but i32 ==> u32 is
	//   not.
	TRIVIAL,

	// Allows:
	// - Everything allowed by TRIVIAL
	// - Implicit struct constructors
	// - Union constructors
	// - Safe integer promotions
	IMPLICIT_CTOR,
};

using ThrowExprErrorFn = void (*)(Expr const &expr, string const &reason, Module const &mod);
using ThrowPatternErrorFn = void (*)(Pattern const &pattern, string const &reason, Module const &mod);

class LazyErrorMsg
{
public:
	LazyErrorMsg(Expr const *expr, ThrowExprErrorFn fn) :
		m_error_fns(std::pair(expr, fn)) {}

	LazyErrorMsg(Pattern const *pattern, ThrowPatternErrorFn fn) :
		m_error_fns(std::pair(pattern, fn)) {}

	[[noreturn]] void throw_error(string const &reason, Module const &mod) const
	{
		m_error_fns | match
		{
			[&](std::pair<Expr const*, ThrowExprErrorFn> expr_fn)
			{
				expr_fn.second(*expr_fn.first, reason, mod);
			},
			[&](std::pair<Pattern const*, ThrowPatternErrorFn> pattern_fn)
			{
				pattern_fn.second(*pattern_fn.first, reason, mod);
			},
		};

		assert(!"ThrowErrorFn returned");
	}

private:
	variant <
		std::pair<Expr const*, ThrowExprErrorFn>,
		std::pair<Pattern const*, ThrowPatternErrorFn>
	> m_error_fns;
};


struct ConstructorConversion
{
	StructInstance *ctor;
};

struct UnionConversion
{
	UnionInstance *union_;
	Type alt;
};

using ConversionInfo = variant<ConstructorConversion, UnionConversion>;

struct TypeConversionEvent
{
	TypeConversionEvent(TypeConversion kind) :
		kind(kind) {}

	TypeConversionEvent(ConstructorConversion info) :
		kind(TypeConversion::IMPLICIT_CTOR),
		info(info) {}

	TypeConversionEvent(UnionConversion info) :
		kind(TypeConversion::IMPLICIT_CTOR),
		info(info) {}

	TypeConversion const kind;
	optional<ConversionInfo> const info;
};


class Subst
{
public:
	virtual GenericArg const* try_get(GenericDeductionVar var) = 0;

	virtual void apply_conversion(TypeConversionEvent const &event, Expr *expr) = 0;

	virtual void set(
		GenericDeductionVar var,
		TypeConversion var_conv,
		Expr *NULLABLE var_expr,
		GenericArg const &arg,
		TypeConversion arg_conv,
		Expr *NULLABLE arg_expr,
		optional<LazyErrorMsg> error_msg
	) = 0;
};

class Unifier
{
public:
	explicit Unifier(SemaContext const &ctx) :
		m_ctx(ctx) {}

	Unifier(Unifier const &rhs) :
		m_ctx(rhs.m_ctx),
		m_left(rhs.m_left),
		m_right(rhs.m_right),
		m_conv_left(rhs.m_conv_left),
		m_conv_right(rhs.m_conv_right),
		m_err(rhs.m_err),
		m_subst(rhs.m_subst),
		m_is_swapped(rhs.m_is_swapped),
		m_expr_left(rhs.m_expr_left),
		m_expr_right(rhs.m_expr_right),
		m_result(rhs.m_result) {}

	Unifier& left(GenericArg const &arg, TypeConversion conv, Expr *expr = nullptr)
	{
		m_left = &arg;
		m_conv_left = conv;
		m_expr_left = expr;
		m_is_swapped = false;

		return *this;
	}

	Unifier& right(GenericArg const &arg, TypeConversion conv, Expr *expr = nullptr)
	{
		m_right = &arg;
		m_conv_right = conv;
		m_expr_right = expr;
		m_is_swapped = false;

		return *this;
	}

	Unifier& set(Subst *subst) { m_subst = subst; return *this; }
	Unifier& set(optional<LazyErrorMsg> const &err) { m_err = err; return *this; }
	Unifier& result(GenericArg *res) { m_result = res; return *this; }

	void go();

	SemaContext const& ctx() const { return m_ctx; }
	optional<LazyErrorMsg> err() const { return m_err; }

private:
	bool try_unify_integer_types();
	bool try_unify_type_deduction_vars();
	bool try_unify_structs();
	bool try_unify_unions();
	bool try_unify_pointers();
	bool try_unify_arrays();
	void unify_generic_values();

	Unifier sides_swapped() const
	{
		Unifier swapped(*this);
		std::swap(swapped.m_left, swapped.m_right);
		std::swap(swapped.m_conv_left, swapped.m_conv_right);
		std::swap(swapped.m_expr_left, swapped.m_expr_right);
		swapped.m_is_swapped = not m_is_swapped;

		return swapped;
	}

	GenericArg const* lookup(GenericDeductionVar var) const
	{
		assert(m_subst);
		GenericArg const *arg = m_subst->try_get(var);
		assert(arg);

		return arg;
	}

	GenericArg const* try_lookup(GenericDeductionVar var) const
	{
		if(not m_subst)
			return nullptr;

		return m_subst->try_get(var);
	}

	void emit_conversion(TypeConversionEvent const &ev, Expr *expr)
	{
		if(not m_subst or not expr)
			return;

		m_subst->apply_conversion(ev, expr);
	}

	SemaContext const &m_ctx;
	GenericArg const *m_left;
	GenericArg const *m_right;
	TypeConversion m_conv_left;
	TypeConversion m_conv_right;
	optional<LazyErrorMsg> m_err;
	Subst *m_subst = nullptr;
	bool m_is_swapped = false;

	Expr *m_expr_left = nullptr;
	Expr *m_expr_right = nullptr;
	GenericArg *m_result = nullptr;
};


//==============================================================================
// Constraints
//==============================================================================
struct IntegerCheck
{
	LazyErrorMsg error_msg;
	Type const *type;
};

struct CastCheck
{
	Expr *expr;
	Type const *target_type;
};

struct LValueCheck
{
	Expr *expr;
	IsMutable mutability;
};

// A check that is performed on a type after all TypeDeductionVars have been deduced
using TypeCheck = variant<
	IntegerCheck,
	CastCheck,
	LValueCheck
>;


struct MemberTypeModifier
{
	friend bool operator == (MemberTypeModifier, MemberTypeModifier) = default;
	string_view member;
};
struct PointeeTypeModifier
{
	friend bool operator == (PointeeTypeModifier, PointeeTypeModifier) = default;
	PointerType::Kind pointer_kind;
};
struct NoModifier
{
	friend bool operator == (NoModifier, NoModifier) = default;
};
using ConstraintModifier = variant<NoModifier, MemberTypeModifier, PointeeTypeModifier>;

template<>
struct std::hash<ConstraintModifier>
{
	size_t operator () (ConstraintModifier m) const
	{
		size_t h = compute_hash(m.index());
		m | match
		{
			[&](MemberTypeModifier t) { combine_hashes(h, compute_hash(t.member)); },
			[&](PointeeTypeModifier t) { combine_hashes(h, compute_hash((int)t.pointer_kind)); },
			[&](NoModifier) {},
		};

		return h;
	}
};


using NodeIdx = size_t;

struct ConstraintEdge
{
	TypeConversion var_conv = TypeConversion::NONE;
	Expr *NULLABLE var_expr = nullptr;

	GenericArg arg;
	TypeConversion arg_conv = TypeConversion::NONE;
	Expr *NULLABLE arg_expr = nullptr;
	ConstraintModifier arg_modifier = NoModifier();

	optional<LazyErrorMsg> error_msg{};
};

inline bool operator == (ConstraintEdge const &a, ConstraintEdge const &b)
{
	return
		a.var_conv == b.var_conv and
		a.var_expr == b.var_expr and
		equiv(a.arg, b.arg) and
		a.arg_conv == b.arg_conv and
		a.arg_expr == b.arg_expr and
		a.arg_modifier == b.arg_modifier;
}

template<>
struct std::hash<GenericArg>
{
	size_t operator () (GenericArg const &arg) const
	{
		size_t h = 0;
		combine_hashes(h, compute_hash(arg.index()));
		arg | match
		{
			[&](Type const &t) { combine_hashes(h, compute_hash(t)); },
			[&](Expr const &e) { combine_hashes(h, compute_hash(e)); },
		};

		return h;
	}
};

template<>
struct std::hash<ConstraintEdge>
{
	size_t operator () (ConstraintEdge const &edge) const
	{
		size_t h = 0;
		combine_hashes(h, compute_hash((int)edge.var_conv));
		combine_hashes(h, compute_hash(edge.var_expr));
		combine_hashes(h, compute_hash(edge.arg));
		combine_hashes(h, compute_hash((int)edge.arg_conv));
		combine_hashes(h, compute_hash(edge.arg_expr));
		combine_hashes(h, compute_hash(edge.arg_modifier));

		return h;
	}
};


struct ErrorMsg { string msg; };

struct ConstraintNode
{
	bool add_edge(ConstraintEdge const &edge)
	{
		if(edge.var_conv == TypeConversion::NONE)
			return push_edges.insert(edge).second;

		return pull_edges.insert(edge).second;
	}

	GenericDeductionVar var;
	unordered_set<ConstraintEdge> push_edges{};
	unordered_set<ConstraintEdge> pull_edges{};
	optional<ErrorMsg> error = nullopt;
};

struct ConstraintSystem
{
	explicit ConstraintSystem(Module const &mod) :
		mod(mod) {}

	void add_check(TypeCheck const &check);
	bool add_relational_constraint(
		GenericDeductionVar var,
		ConstraintEdge const &edge
	);

	ConstraintNode& get_node(GenericDeductionVar var)
	{
		auto it = nodes.find(var);
		if(it != nodes.end())
			return it->second;

		auto res = nodes.insert({var, ConstraintNode{
			.var = var,
			.push_edges = {},
			.pull_edges = {},
		}});

		return res.first->second;
	}

	void print(std::ostream &os) const;

	Module const &mod;
	unordered_map<GenericDeductionVar, ConstraintNode> nodes;
	vector<TypeCheck> checks;
};

inline TypeConversion lub(TypeConversion a, TypeConversion b)
{
	return (TypeConversion)std::max((int)a, (int)b);
}

inline TypeConversion glb(TypeConversion a, TypeConversion b)
{
	return (TypeConversion)std::min((int)a, (int)b);
}

TypeEnv create_subst_from_constraints(ConstraintSystem &constraints, SemaContext &ctx);


//==============================================================================
// Scope
//==============================================================================
struct Module;

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
optional<GenericDeductionVar> get_if_type_deduction_var(Type const &type);

Stmt clone(Stmt const &stmt, Arena &arena);
void substitute_types_in_stmt(Stmt &stmt, TypeEnv const &subst, InstanceRegistry &registry, SubstitutionMode mode);

void declare_item(TopLevelItem &item, SemaContext &ctx);

enum class ResolutionContext
{
	GENERAL,
	DEFAULT_VALUE,
};

void resolve_type(Type &type, Scope *scope, ResolutionContext res_ctx, SemaContext &ctx);
void resolve_item(TopLevelItem &item, SemaContext &ctx);

MemoryLayout compute_layout(Type const &type);
void compute_type_layouts(Module &mod);

// Perform semantic analysis
void sema(Module &mod, Arena &arena);

enum Traversal
{
	CONTINUE,
	SKIP,
	ABORT,
};


template<typename Type, bool Condition>
struct add_const_if;

template<typename Type>
struct add_const_if<Type, false>
{
	using type = Type;
};

template<typename Type>
struct add_const_if<Type, true>
{
	using type = Type const;
};

template<typename Target, bool Condition>
using add_const_if_t = add_const_if<Target, Condition>::type;

template<typename T, typename BaseType>
concept MaybeConst = std::is_same_v<std::remove_cvref_t<T>, BaseType>;


#define CONST(T) add_const_if_t<T, is_const>

//
// Visiting Type
//
template<typename Visitor>
void visit_child_types(BuiltinType const&, Visitor&&) {}

template<typename Visitor>
void visit_child_types(KnownIntType const&, Visitor&&) {}

template<typename Visitor>
void visit_child_types(VarType const&, Visitor&&) {}

template<
	MaybeConst<PointerType> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	visitor(*t.pointee);
}

template<
	MaybeConst<ArrayType> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	visitor(*t.element);
}

template<typename Visitor>
void visit_child_types(ProcType const &t, Visitor &&visitor)
{
	for(Type const &p: *t.inst->params)
		visitor(p);

	visitor(*t.inst->ret);
}

template<
	MaybeConst<ProcTypeUnresolved> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(CONST(Type) &p: *t.params)
		visitor(p);

	visitor(*t.ret);
}

template<typename Visitor>
void visit_child_types(FixedArray<GenericArg> const *type_args, Visitor &&visitor)
{
	for(GenericArg const &arg: *type_args)
	{
		// TODO Exprs can also contain types, e.g. in as-expressions and when explicitly
		//      specifying the types for generic functions

		if(Type const *type_arg = std::get_if<Type>(&arg))
			visitor(*type_arg);
	}
}

template<typename Visitor>
void visit_child_types(StructType const &t, Visitor &&visitor)
{
	visit_child_types(t.inst->type_args().args, visitor);
}

template<typename Visitor>
void visit_child_types(UnionType const &t, Visitor &&visitor)
{
	for(Type const *p: t.inst->alternatives())
		visitor(*p);
}

template<
	MaybeConst<UnionTypeUnresolved> T,
	typename Visitor
>
void visit_child_types(T &&t, Visitor &&visitor)
{
	constexpr bool is_const = std::is_const_v<std::remove_reference_t<T>>;

	for(CONST(Type) &p: *t.alternatives)
		visitor(p);
}

template<
	MaybeConst<Path> T,
	typename Visitor
>
void visit_child_types(T &&path, Visitor &&visitor)
{
	visit_child_types(path.type_args, visitor);
	if(path.child)
		visitor(*path.child);
}

template<typename Visitor>
void visit_child_types(InlineStructType const&, Visitor&&) {}

template<
	MaybeConst<Type> T,
	typename Visitor
>
void visit_child_types(T &&type, Visitor &&visitor)
{
	type | match
	{
		[&](auto &t) { visit_child_types(t, visitor); },
	};
}


#undef CONST



template<typename Visitor>
void traverse(Pattern const &pattern, Visitor &&visitor)
{
	pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			visitor(p, pattern.provided_type);
		},
		[&](VarPattern const &p)
		{
			visitor(p, pattern.provided_type);
		},
		[&](DerefPattern const &p)
		{
			visitor(p, pattern.provided_type);
			traverse(*p.sub, visitor);
		},
		[&](AddressOfPattern const &p)
		{
			visitor(p, pattern.provided_type);
			traverse(*p.sub, visitor);
		},
		[&](ConstructorPattern const &p)
		{
			visitor(p, pattern.provided_type);
			for(PatternArgument const &arg: *p.args)
				traverse(arg.pattern, visitor);
		},
		[&](WildcardPattern const &p)
		{
			visitor(p, pattern.provided_type);
		}
	};
}

template<typename Visitor>
void traverse(Stmt const &stmt, Visitor &&visitor)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			visitor(s);
		},
		[&](ExprStmt const &s)
		{
			visitor(s);
		},
		[&](BlockStmt const &s)
		{
			visitor(s);
			for(Stmt const &child_stmt: *s.stmts)
				traverse(child_stmt, visitor);
		},
		[&](ReturnStmt const &s)
		{
			visitor(s);
		},
		[&](IfStmt const &s)
		{
			visitor(s);
			traverse(*s.then, visitor);
			if(s.else_)
				traverse(*s.else_, visitor);
		},
		[&](WhileStmt const &s)
		{
			visitor(s);
			traverse(*s.body, visitor);
		},
		[&](DeclStmt const &s)
		{
			visitor(s);
		},
		[&](MatchStmt const &s)
		{
			visitor(s);
			for(MatchArm const &arm: *s.arms)
				traverse(arm.stmt, visitor);
		},
	};
}
