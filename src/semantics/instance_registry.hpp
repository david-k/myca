#pragma once

#include "semantics/ast_operations.hpp"
#include "semantics/passes.hpp"
#include "semantics/substitution.hpp"

#include <span>
#include <unordered_set>

using std::span;
using std::unordered_set;

// For certain "heavy" types we compute unique instances that can be referred to by pointer. This
// keeps the `Type` struct lean, saves memory and can speeds some operations like type equality.
// Currently, we have StructInstance, UnionInstance, ProcTypeInstance and ProcInstance (the last one
// is not a type by the same principle applies).

class InstanceRegistry;
class ConstraintGatheringSubst;
struct SemaContext;

struct TermInfo
{
	void merge(TermInfo other)
	{
		has_generic_parameter_vars |= other.has_generic_parameter_vars;
		has_deduction_vars |= other.has_deduction_vars;
		has_known_ints |= other.has_known_ints;
	}

	bool has_generic_parameter_vars = false;
	bool has_deduction_vars = false;
	bool has_known_ints = false;
};

TermInfo gather_type_vars(
	Type const &type,
	unordered_set<GenericVar> &type_vars,
	bool deduction_vars_only = false
);

TermInfo gather_type_vars(
	Expr const &expr,
	unordered_set<GenericVar> &type_vars,
	bool deduction_vars_only
);

TermInfo gather_type_vars(
	GenericArg const &arg,
	unordered_set<GenericVar> &type_vars,
	bool deduction_vars_only
);

TermInfo get_term_info(Type const &type);
TermInfo get_term_info(Expr const &expr);
TermInfo get_term_info(GenericArg const &arg);

bool have_common_vars(unordered_set<GenericVar> const &occurring_vars, TypeEnv const &env);

struct TypeArgList
{
	bool needs_substitution(TypeEnv const &env) const
	{
		return has_known_ints or have_common_vars(occurring_vars, env);
	}

	// Fully resolved and must not contain KnownIntTypes
	FixedArray<GenericArg> *args;

	unordered_set<GenericVar> occurring_vars{};
	bool has_type_deduction_vars = false;
	bool has_known_ints = false;
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

	size_t hash();
};

class Callable
{
public:
	virtual ~Callable() = default;
	virtual variant<ProcItem const*, StructItem const*> callable_item() = 0;
	virtual size_t param_count() = 0;
	virtual string_view param_name_at(size_t idx) = 0;
	virtual DefaultValueExpr param_default_value_at(size_t idx) = 0;
	virtual void typecheck_generic_args(ConstraintGatheringSubst &subst, SemaContext &ctx) = 0;
	virtual Callable* get_substituted(
		TypeEnv const &env,
		InstanceRegistry &registry,
		SubstitutionOptions mode
	) = 0;
};

//--------------------------------------------------------------------
// StructInstance
//--------------------------------------------------------------------
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

enum class LayoutComputationState
{
	PENDING,
	IN_PROGRESS,
	DONE,
};

// Once constructed, the type arguments never change
class StructInstance : public Callable
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

	size_t hash();

	virtual void typecheck_generic_args(ConstraintGatheringSubst &subst, SemaContext &ctx);

	size_t id() const { return m_id; }
	int variant_depth() const;

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

	virtual variant<ProcItem const*, StructItem const*> callable_item() { return m_struct; }
	virtual size_t param_count();
	virtual string_view param_name_at(size_t idx);
	virtual DefaultValueExpr param_default_value_at(size_t idx);
	virtual Callable* get_substituted(
		TypeEnv const &env,
		InstanceRegistry &registry,
		SubstitutionOptions mode
	)
	{
		return substitute_in_struct(this, env, registry, mode);
	}

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
	optional<size_t> m_hash{};

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

class ProcInstance : public Callable
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

	virtual void typecheck_generic_args(ConstraintGatheringSubst &subst, SemaContext &ctx);

	bool is_concrete() const { return m_type_args.occurring_vars.empty() and not m_type_args.has_known_ints; }
	bool is_deduction_complete() const { return not m_type_args.has_type_deduction_vars and not m_type_args.has_known_ints; }
	ProcInstanceKey key() { return ProcInstanceKey(m_proc, m_type_args.args); }

	size_t hash();

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

	virtual variant<ProcItem const*, StructItem const*> callable_item() { return m_proc; }
	virtual size_t param_count();
	virtual string_view param_name_at(size_t idx);
	virtual DefaultValueExpr param_default_value_at(size_t idx);
	virtual Callable* get_substituted(
		TypeEnv const &env,
		InstanceRegistry &registry,
		SubstitutionOptions mode
	)
	{
		return substitute_in_proc(this, env, registry, mode);
	}

private:
	void compute_dependent_properties();

	InstanceRegistry *m_registry;
	ProcItem const *m_proc;
	TypeArgList m_type_args;
	optional<size_t> m_hash;

	bool m_generic_args_typechecked = false;

	// Dependent properties (those that depend on `m_type_args`).
	// Lazily computed by compute_dependent_properties().
	Type *m_type = nullptr;
};

bool operator == (ProcInstanceKey const &a, ProcInstanceKey const &b);

template<>
struct std::hash<ProcInstanceKey>
{
	size_t operator () (ProcInstanceKey const &key) const
	{
		size_t h = compute_hash(key.proc);

		if(key.type_args)
		{
			for(GenericArg const &t: *key.type_args)
				combine_hashes(h, compute_hash(t));
		}

		return h;
	}
};

//--------------------------------------------------------------------
// ProcTypeInstance
//--------------------------------------------------------------------
struct ProcTypeInstanceKey
{
	FixedArray<ProcTypeParameter> *params;
	Type *ret;
};

struct ProcTypeInstance
{
	bool needs_substitution(TypeEnv const &env) const
	{
		return has_known_ints or have_common_vars(occurring_vars, env);
	}

	bool is_deduction_complete() const { return not has_type_deduction_vars and not has_known_ints; }
	void typecheck(ConstraintGatheringSubst &subst, SemaContext &ctx);

	FixedArray<ProcTypeParameter> *params;
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

		for(ProcTypeParameter const &p: *key.params)
		{
			combine_hashes(h, compute_hash(p.type));
			combine_hashes(h, compute_hash(p.is_ref));
		}

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

	bool needs_substitution(TypeEnv const &env) const
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
	void typecheck(ConstraintGatheringSubst &subst, SemaContext &ctx);

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

vector<Type*> canonicalize_union_alternatives(FixedArray<Type> *alternatives, Arena &arena);
void canonicalize_union_alternatives(vector<Type*> &alternatives, Arena &arena);

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
		SubstitutionOptions mode
	);


	ProcInstance* get_proc_instance(ProcItem const *proc, FixedArray<GenericArg> *NULLABLE type_args);
	ProcInstance* get_proc_instance(ProcItem const *proc, TypeArgList const &type_args);
	ProcInstance* get_proc_instance(
		ProcItem const *proc,
		TypeArgList const &type_args,
		TypeEnv const &subst,
		SubstitutionOptions mode
	);

	ProcTypeInstance* get_proc_type_instance(FixedArray<ProcTypeParameter> *params, Type *ret);
	ProcTypeInstance* get_proc_type_instance(
		FixedArray<ProcTypeParameter> *params,
		Type *ret,
		TypeEnv const &subst,
		SubstitutionOptions mode
	);

	// `alternatives` must be the result of canonicalize_union_alternatives()
	UnionInstance* get_union_instance(vector<Type*> &&alternatives);

	StructInstance* get_struct_self_instance(StructItem const *struct_);
	ProcInstance* get_proc_self_instance(ProcItem const *proc);
	DeclContainerInst get_self_instance(struct DeclContainer decl);

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
	ProcTypeInstance* add_proc_type_instance(FixedArray<ProcTypeParameter> *params, Type *ret);
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
