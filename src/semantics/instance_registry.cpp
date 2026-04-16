#include "semantics/ast_traversal.hpp"
#include "semantics/error.hpp"
#include "semantics/instance_registry.hpp"
#include "semantics/type_properties.hpp"
#include "semantics/module.hpp"
#include "semantics/type_env.hpp"

#include <algorithm>
#include <variant>

static TermInfo gather_type_vars(GenericVar var, unordered_set<GenericVar> &type_vars, bool deduction_vars_only = false)
{
	if(std::holds_alternative<GenericDeductionVar>(var) or not deduction_vars_only)
		type_vars.insert(var);

	return TermInfo{
		.has_generic_parameter_vars = std::holds_alternative<GenericParameterVar>(var),
		.has_deduction_vars = std::holds_alternative<GenericDeductionVar>(var),
	};
}

static TermInfo gather_type_vars(DeclContainerInst cont, unordered_set<GenericVar> &type_vars, bool deduction_vars_only = false)
{
	TermInfo info;
	if(cont.decl_parent())
		info.merge(gather_type_vars(*cont.decl_parent(), type_vars, deduction_vars_only));

	TypeArgList const &type_args = cont | match
	{
		[&](StructInstance *decl_parent_inst) -> TypeArgList const&
		{
			return decl_parent_inst->type_args();
		},
		[&](ProcInstance *decl_parent_inst) -> TypeArgList const&
		{
			return decl_parent_inst->type_args();
		},
	};

	for(GenericVar v: type_args.occurring_vars)
		gather_type_vars(v, type_vars, deduction_vars_only);

	info.has_deduction_vars |= type_args.has_type_deduction_vars;
	info.has_known_ints |= type_args.has_known_ints;

	return info;
}

TermInfo gather_type_vars(Expr const &expr, unordered_set<GenericVar> &type_vars, bool deduction_vars_only)
{
	TermInfo info;
	auto visitor = [&](this auto &self, Expr const &expr) -> void
	{
		expr | match
		{
			[&](GenericVarExpr const &e)
			{
				info.merge(gather_type_vars(e.var, type_vars, deduction_vars_only));
			},
			[&](auto const &e) -> void
			{
				expr_visit_children(e, self);
			},
		};
	};
	visitor(expr);

	return info;
}

TermInfo gather_type_vars(GenericArg const &arg, unordered_set<GenericVar> &type_vars, bool deduction_vars_only)
{
	return arg | match
	{
		[&](Type const &t) { return gather_type_vars(t, type_vars, deduction_vars_only); },
		[&](Expr const &e) { return gather_type_vars(e, type_vars, deduction_vars_only); },
	};
}

TermInfo gather_type_vars(Type const &type, unordered_set<GenericVar> &type_vars, bool deduction_vars_only)
{
	return type | match
	{
		[&](BuiltinType const&) { return TermInfo{}; },
		[&](KnownIntType const&) { return TermInfo{.has_known_ints = true}; },
		[&](PointerType const &t)
		{
			return gather_type_vars(*t.pointee, type_vars, deduction_vars_only);
		},
		[&](ArrayType const &t)
		{
			assert(t.count_arg);

			TermInfo info;
			info.merge(gather_type_vars(*t.count_arg, type_vars, deduction_vars_only));
			info.merge(gather_type_vars(*t.element, type_vars, deduction_vars_only));
			return info;
		},
		[&](ProcType const&) -> TermInfo
		{
			assert(!"gather_type_vars: ProcType");
		},
		[&](StructType const &t)
		{
			return gather_type_vars(t.inst, type_vars, deduction_vars_only);
		},
		[&](UnionType const &t)
		{
			if(not deduction_vars_only or t.inst->has_type_deduction_vars())
			{
				for(GenericVar v: t.inst->occurring_vars())
					gather_type_vars(v, type_vars, deduction_vars_only);
			}

			return TermInfo{
				.has_deduction_vars = t.inst->has_type_deduction_vars(),
				.has_known_ints = t.inst->has_known_ints(),
			};
		},
		[&](VarType const &t)
		{
			return gather_type_vars(t.var, type_vars, deduction_vars_only);
		},
		[&](ProcTypeUnresolved const&) -> TermInfo { assert(!"gather_type_vars: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> TermInfo { assert(!"gather_type_vars: UnionTypeUnresolved"); },
		[&](Path const&) -> TermInfo { assert(!"gather_type_vars: Path"); },
		[&](InlineStructType const&) -> TermInfo { assert(!"gather_type_vars: InlineStructType"); },
	};
}

TermInfo get_term_info(GenericArg const &arg)
{
	// TODO Change gather_type_vars() so that we don't need to pass type_vars when we don't need it
	unordered_set<GenericVar> type_vars;
	return gather_type_vars(arg, type_vars, false);
}

TermInfo get_term_info(Type const &type)
{
	// TODO Change gather_type_vars() so that we don't need to pass type_vars when we don't need it
	unordered_set<GenericVar> type_vars;
	return gather_type_vars(type, type_vars, false);
}

TermInfo get_term_info(Expr const &expr)
{
	// TODO Change gather_type_vars() so that we don't need to pass type_vars when we don't need it
	unordered_set<GenericVar> type_vars;
	return gather_type_vars(expr, type_vars, false);
}

bool have_common_vars(unordered_set<GenericVar> const &occurring_vars, TypeEnv const &env)
{
	for(GenericVar const &var: occurring_vars)
	{
		if(env.try_lookup(var))
			return true;
	}

	return false;
}

TypeEnv DeclContainerInst::create_type_env() const
{
	return *this | match
	{
		[](auto *inst) { return inst->create_type_env(); }
	};
}

bool DeclContainerInst::is_concrete() const
{
	return *this | match
	{
		[](auto *inst) { return inst->is_concrete(); }
	};
}

bool DeclContainerInst::is_deduction_complete() const
{
	return *this | match
	{
		[](auto *inst) { return inst->is_deduction_complete(); }
	};
}

void DeclContainerInst::finalize_typechecking()
{
	*this | match
	{
		[](StructInstance *inst) { inst->finalize_typechecking(); },
		[](ProcInstance*) {},
	};
}

TypeArgList const& DeclContainerInst::type_args() const
{
	return *this | match
	{
		[](StructInstance *inst) -> TypeArgList const& { return inst->type_args(); },
		[](ProcInstance *inst) -> TypeArgList const& { return inst->type_args(); },
	};
}

optional<DeclContainerInst> DeclContainerInst::decl_parent() const
{
	return *this | match
	{
		[](StructInstance *inst) { return inst->decl_parent(); },
		[](ProcInstance *inst) { return inst->decl_parent(); },
	};
}

Scope* DeclContainerInst::scope() const
{
	return *this | match
	{
		[](StructInstance *inst) { return inst->struct_()->sema->type_scope; },
		[](ProcInstance *inst) { return inst->proc()->sema->scope; },
	};
}

size_t DeclContainerInst::hash()
{
	return std::visit([](auto *inst) { return inst->hash(); }, *this);
}

//--------------------------------------------------------------------
// StructInstance
//--------------------------------------------------------------------
size_t StructInstance::hash()
{
	if(not m_hash)
	{
		m_hash = 0;
		if(m_decl_parent)
			combine_hashes(*m_hash, m_decl_parent->hash());

		combine_hashes(*m_hash, compute_hash(m_struct->name));
		combine_hashes(*m_hash, compute_hash(*m_type_args.args));
	}

	return *m_hash;
}

int StructInstance::variant_depth() const
{
	return m_struct->sema->variant_depth;
}

Module const& StructInstance::mod() const
{
	return m_registry->mod();
}

TypeEnv StructInstance::create_type_env() const
{
	TypeEnv env;
	if(m_decl_parent)
		env = m_decl_parent->create_type_env();

	add_type_args_to_env(env, m_struct->type_params, m_type_args.args, *m_registry);
	return env;
}

bool StructInstance::is_concrete() const
{
	bool parent_concrete = m_decl_parent ? m_decl_parent->is_concrete() : true;
	return parent_concrete && m_type_args.occurring_vars.empty() and not m_type_args.has_known_ints;
}

bool StructInstance::is_deduction_complete() const
{
	bool parent_is_deduction_complete = m_decl_parent ?
		m_decl_parent->is_deduction_complete() : true;

	return parent_is_deduction_complete and not m_type_args.has_type_deduction_vars and not m_type_args.has_known_ints;
}

Type* StructInstance::try_get_ctor_type()
{
	if(not m_dependent_properties_computed)
		compute_dependent_properties();

	return m_ctor_proc_type;
}

StructInstance* StructInstance::implicit_case()
{
	if(not m_dependent_properties_computed)
		compute_dependent_properties();

	return m_implicit_case;
}

int StructInstance::case_idx()
{
	assert(is_case_member());

	if(m_decl_parent)
	{
		if(StructInstance *parent_inst = m_decl_parent->try_as_struct())
		{
			if(not parent_inst->m_dependent_properties_computed)
				parent_inst->compute_dependent_properties();
		}
	}

	return m_case_idx;
}

std::generator<Parameter const&> StructInstance::own_var_members()
{
	if(not m_members)
		compute_dependent_properties();

	co_yield std::ranges::elements_of(initial_var_members());
	co_yield std::ranges::elements_of(trailing_var_members());
}

std::generator<Parameter&> StructInstance::initial_var_members()
{
	if(not m_members)
		compute_dependent_properties();

	for(int i = 0; i < m_struct->sema->num_initial_var_members; ++i)
		co_yield std::get<Parameter>(m_members->items[i]);
}

std::generator<StructInstance*> StructInstance::own_case_members()
{
	if(not m_members)
		compute_dependent_properties();

	int first_idx = m_struct->sema->num_initial_var_members;
	int end_idx = first_idx + m_struct->num_case_members;
	for(int i = first_idx; i < end_idx; ++i)
		co_yield std::get<StructInstance*>(m_members->items[i]);
}

std::generator<Parameter&> StructInstance::trailing_var_members()
{
	if(not m_members)
		compute_dependent_properties();

	size_t first_idx = m_struct->sema->num_initial_var_members + m_struct->num_case_members;
	for(size_t i = first_idx; i < m_members->count; ++i)
		co_yield std::get<Parameter>(m_members->items[i]);
}

std::generator<Parameter const&> StructInstance::all_var_members()
{
	if(is_case_member())
		co_yield std::ranges::elements_of(m_decl_parent->as_struct()->initial_var_members());

	co_yield std::ranges::elements_of(own_var_members());

	if(is_case_member())
		co_yield std::ranges::elements_of(m_decl_parent->as_struct()->trailing_var_members());
}


void StructInstance::compute_dependent_properties()
{
	TypeEnv env = create_type_env();

	// Initialize the struct's own members (m_members)
	size_t num_instance_members = m_struct->num_var_members + m_struct->num_case_members;
	m_members = alloc_fixed_array<InstanceMember>(num_instance_members, m_registry->arena());
	int member_idx = 0;
	int case_idx = 0;
	for(Member const &m: *m_struct->members)
	{
		m | match
		{
			[&](VarMember const &var_member)
			{
				Parameter inst_var_member{
					.range = var_member.var.range,
					.type = clone_ptr(var_member.var.type, m_registry->arena()),
					// Why ExprPending?
					// - First, when typechecking the *usage* of a struct, we only need to know
					//   whether a member *has* a default value, but it doesn't matter what that
					//   default value is. This is war ExprPending() accomplishes.
					// - Second, if we directly assigned the default value expression here, we would
					//   also need to substitute the type parameters in it. However,
					//   var_member.default_value has not been type-checked at this point, and in
					//   order to typecheck var_member.default_value we may have to typecheck the
					//   default values of other structs, which may well end up in a cycle
					.default_value = var_member.var.default_value ?
						DefaultValueExpr(ExprPending()) : DefaultValueExpr(NoDefaultValue()),

					.is_ref = var_member.var.is_ref,
				};
				substitute_in_type(*inst_var_member.type, env, *m_registry, {
					SubstitutionPhase::INSTANTIATION,
					SubstitutionMode::BEST_EFFORT
				});

				new (m_members->items+member_idx) InstanceMember(inst_var_member);
				member_idx += 1;
			},
			[&](CaseMember case_member)
			{
				assert(case_member.struct_->type_params->count == 0);
				StructInstance *case_member_inst = m_registry->get_struct_instance(case_member.struct_, nullptr, this);
				case_member_inst->m_case_idx = case_idx;
				new (m_members->items+member_idx) InstanceMember(case_member_inst);
				if(case_member.struct_->is_implicit)
					m_implicit_case = case_member_inst;

				case_idx += 1;
				member_idx += 1;
			},
			[&](StructMember) {}
		};
	}

	// If the struct has a constructor, compute its ProcType (m_ctor_proc_type) and its list of
	// Parameters (m_ctor_params)
	if(m_struct->has_constructor())
	{
		size_t param_count = m_struct->sema->ctor_params->count;
		FixedArray<ProcTypeParameter> *ctor_param_types = alloc_fixed_array<ProcTypeParameter>(param_count, m_registry->arena());
		m_ctor_params = alloc_fixed_array<Parameter const*>(param_count, m_registry->arena());

		for(auto const &[idx, var_member]: all_var_members() | std::views::enumerate)
		{
			ctor_param_types->items[idx] = ProcTypeParameter{
				.type = clone(*var_member.type, m_registry->arena()),
				.is_ref = false,
			};
			m_ctor_params->items[idx] = &var_member;
		}

		Type *ctor_ret_type = m_registry->arena().alloc<Type>(StructType(UNKNOWN_TOKEN_RANGE, this));
		ProcTypeInstance *proc_type_inst = m_registry->get_proc_type_instance(ctor_param_types, ctor_ret_type);
		m_ctor_proc_type = m_registry->arena().alloc<Type>(ProcType{
			.range = UNKNOWN_TOKEN_RANGE,
			.inst = proc_type_inst,
			.callable = this,
		});
	}

	m_dependent_properties_computed = true;
}

size_t StructInstance::param_count()
{
	if(not m_ctor_params)
		compute_dependent_properties();

	return m_ctor_params->count;
}

string_view StructInstance::param_name_at(size_t idx)
{
	if(not m_ctor_params)
		compute_dependent_properties();

	return m_registry->mod().name_of(*m_ctor_params->items[idx]);
}

DefaultValueExpr StructInstance::param_default_value_at(size_t idx)
{
	if(not m_ctor_params)
		compute_dependent_properties();

	return m_ctor_params->items[idx]->default_value;
}

MemoryLayout StructInstance::compute_own_layout()
{
	if(m_layout_state == LayoutComputationState::DONE)
		return *m_own_layout;

	if(m_layout_state == LayoutComputationState::IN_PROGRESS)
		throw_sem_error("Cyclic type definition", m_struct->range.first, &m_registry->mod());

	LOGGER(m_registry->mod().sema->logger, on_struct_layout_computation_start, this);

	m_layout_state = LayoutComputationState::IN_PROGRESS;
	m_own_layout = MemoryLayout{};

	// Reserve space for the discriminator
	if(m_struct->num_case_members > 0)
	{
		BuiltinTypeDef discriminator_type = smallest_int_type_for(m_struct->num_case_members);
		m_discriminator_type = mk_builtin_type(discriminator_type, m_registry->arena());
		m_own_layout->extend(get_layout(discriminator_type));
	}

	// Reserve space for initial variable members
	for(Parameter &var_member: initial_var_members())
	{
		compute_direct_type_dependencies(*var_member.type, m_type_deps);
		MemoryLayout var_layout = compute_layout(*var_member.type);
		m_own_layout->extend(var_layout);
	}

	// Reserve space for case members
	if(m_struct->num_case_members > 0)
	{
		m_cases_layout = CaseMemberRegion{};
		m_cases_layout->start = m_own_layout->size;
		MemoryLayout case_members_layout{};
		for(StructInstance *case_member: own_case_members())
		{
			MemoryLayout case_layout = case_member->compute_own_layout();
			case_members_layout.size = std::max(case_members_layout.size, case_layout.size);
			case_members_layout.alignment = std::max(case_members_layout.alignment, case_layout.alignment);
		}
		size_t size_before_cases = m_own_layout->size;
		m_own_layout->extend(case_members_layout);
		m_cases_layout->size = m_own_layout->size - size_before_cases;
	}

	// Reserve space for trailing variable members (same as for initial variable members above)
	for(Parameter &var_member: trailing_var_members())
	{
		compute_direct_type_dependencies(*var_member.type, m_type_deps);
		MemoryLayout var_layout = compute_layout(*var_member.type);
		m_own_layout->extend(var_layout);
	}

	LOGGER(m_registry->mod().sema->logger, on_struct_layout_computation_end);

	m_layout_state = LayoutComputationState::DONE;
	return *m_own_layout;
}

void StructInstance::finalize_typechecking()
{
	if(m_finalized)
		return;

	if(not m_dependent_properties_computed)
		compute_dependent_properties();

	if(m_decl_parent)
		m_decl_parent->finalize_typechecking();

	// TODO Store and re-use the `env` created in compute_dependent_properties()
	TypeEnv env = create_type_env();

	// Substitute type vars in default values
	for(size_t i = 0; i < m_struct->members->count; ++i)
	{
		if(VarMember const *var_member = std::get_if<VarMember>(&m_struct->members->items[i]))
		{
			Parameter &inst_var_member = std::get<Parameter>(m_members->items[i]);
			if(Expr *default_value = var_member->var.default_value.try_get_expr())
			{
				default_value = clone_ptr(default_value, m_registry->arena());
				substitute_in_expr(*default_value, env, *m_registry, {
					SubstitutionPhase::INSTANTIATION,
					SubstitutionMode::BEST_EFFORT
				});
				inst_var_member.default_value = default_value;
			}
		}
	}

	m_finalized = true;
}

MemoryLayout StructInstance::layout()
{
	if(is_case_member())
		return m_decl_parent->as_struct()->layout();

	return compute_own_layout();
}

Type const* StructInstance::discriminator_type() const
{
	assert(m_layout_state == LayoutComputationState::DONE);
	assert(m_discriminator_type);

	return m_discriminator_type;
}

void StructInstance::typecheck_generic_args(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(m_generic_args_typechecked)
		return;

	::generate_constraints_for_generic_args(m_type_args.args, m_struct->type_params, subst, ctx);
	m_generic_args_typechecked = true;
}


bool operator == (StructInstanceKey const &a, StructInstanceKey const &b)
{
	if(a.struct_ != b.struct_)
		return false;

	if(a.decl_parent != b.decl_parent)
		return false;

	size_t type_arg_count = a.type_args ? a.type_args->count : 0;
	for(size_t i = 0; i < type_arg_count; ++i)
	{
		if(not equiv(a.type_args->items[i], b.type_args->items[i]))
			return false;
	}

	return true;
}

//--------------------------------------------------------------------
// UnionInstance
//--------------------------------------------------------------------
static void flatten_union_alternatives(vector<Type*> &alternatives, Arena &arena)
{
	auto it = alternatives.begin();
	while(it != alternatives.end())
	{
		if(UnionType const *sub_union = std::get_if<UnionType>(*it))
		{
			it = alternatives.erase(it);
			for(Type const *sub_alt: sub_union->inst->alternatives())
				it = alternatives.insert(it, clone_ptr(sub_alt, arena));
		}
		else
			++it;
	}
}

void canonicalize_union_alternatives(vector<Type*> &alternatives, Arena &arena)
{
	// 1. Flatten
	// 2. Sort
	// 3. Remove duplicates

	flatten_union_alternatives(alternatives, arena);

	std::ranges::sort(alternatives, [](Type const *a, Type const *b)
	{
		return *a < *b;
	});

	auto new_end = std::ranges::unique(alternatives, [](Type const *a, Type const *b)
	{
		return equiv(*a, *b);
	}).begin();
	alternatives.erase(new_end, alternatives.end());
}

vector<Type*> canonicalize_union_alternatives(FixedArray<Type> *alternatives, Arena &arena)
{
	vector<Type*> result;
	result.reserve(alternatives->count);
	for(Type &alt: *alternatives)
		result.push_back(&alt);

	canonicalize_union_alternatives(result, arena);

	return result;
}

MemoryLayout UnionInstance::layout()
{
	assert(is_concrete());

	compute_properties();

	if(m_layout_state == LayoutComputationState::DONE)
		return *m_layout;

	assert(m_layout_state != LayoutComputationState::IN_PROGRESS);
	m_layout_state = LayoutComputationState::IN_PROGRESS;

	BuiltinTypeDef discr_type = smallest_int_type_for(m_alternatives.size());
	m_layout = MemoryLayout();
	m_layout->extend(get_layout(discr_type));

	MemoryLayout alt_layouts;
	for(Type const *alt: m_alternatives)
	{
		compute_direct_type_dependencies(*alt, m_type_deps);
		alt_layouts.extend(compute_layout(*alt));
	}

	m_layout->extend(alt_layouts);
	m_layout_state = LayoutComputationState::DONE;

	return *m_layout;
}

void UnionInstance::compute_properties()
{
	if(m_properties_computed)
		return;

	for(auto const &[idx, alt]: m_alternatives | std::views::enumerate)
		m_alt_to_idx.emplace(*alt, idx);

	m_properties_computed = true;
}

bool operator == (UnionInstanceKey const &a, UnionInstanceKey const &b)
{
	if(a.alternatives.size() != b.alternatives.size())
		return false;

	for(size_t i = 0; i < a.alternatives.size(); ++i)
	{
		if(not equiv(*a.alternatives[i], *b.alternatives[i]))
			return false;
	}

	return true;
}

void UnionInstance::typecheck(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(m_has_been_typechecked)
		return;

	for(Type *alt: m_alternatives)
		generate_constraints_for_type(*alt, subst, ctx);

	m_has_been_typechecked = true;
}

//--------------------------------------------------------------------
// ProcInstance
//--------------------------------------------------------------------
bool operator == (ProcInstanceKey const &a, ProcInstanceKey const &b)
{
	if(a.proc != b.proc)
		return false;

	size_t type_arg_count = a.type_args ? a.type_args->count : 0;
	for(size_t i = 0; i < type_arg_count; ++i)
	{
		if(not equiv(a.type_args->items[i], b.type_args->items[i]))
			return false;
	}

	return true;
}

static Type* create_proc_type(ProcInstance *inst, TypeEnv const &env, InstanceRegistry &instances)
{
	ProcItem const *proc = inst->proc();
	FixedArray<ProcTypeParameter> *ctor_params = alloc_fixed_array<ProcTypeParameter>(proc->params->count, instances.arena());
	for(auto const &[idx, param]: *proc->params | std::views::enumerate)
	{
		Type param_type = clone(*param.type, instances.arena());
		substitute_in_type(param_type, env, instances, {SubstitutionPhase::INSTANTIATION, SubstitutionMode::BEST_EFFORT});
		ctor_params->items[idx] = ProcTypeParameter{
			.type = param_type,
			.is_ref = param.is_ref,
		};
	}

	Type *new_ret = clone_ptr(proc->ret_type, instances.arena());
	substitute_in_type(*new_ret, env, instances, {SubstitutionPhase::INSTANTIATION, SubstitutionMode::BEST_EFFORT});

	ProcTypeInstance *proc_type_inst = instances.get_proc_type_instance(ctor_params, new_ret);
	return instances.arena().alloc<Type>(ProcType{
		.range = UNKNOWN_TOKEN_RANGE,
		.inst = proc_type_inst,
		.callable = inst,
	});
}

size_t ProcInstance::hash()
{
	if(not m_hash)
	{
		m_hash = compute_hash(m_proc->name);
		combine_hashes(*m_hash, compute_hash(*m_type_args.args));
	}

	return *m_hash;
}

TypeEnv ProcInstance::create_type_env() const
{
	return TypeEnv::from_type_args(m_proc->type_params, m_type_args.args, *m_registry);
}

void ProcInstance::compute_dependent_properties()
{
	TypeEnv env = create_type_env();
	m_type = create_proc_type(this, env, *m_registry);
}

size_t ProcInstance::param_count()
{
	return get_proc_type().inst->params->count;
}

string_view ProcInstance::param_name_at(size_t idx)
{
	return m_registry->mod().name_of(m_proc->params->items[idx]);
}

DefaultValueExpr ProcInstance::param_default_value_at(size_t idx)
{
	return m_proc->params->items[idx].default_value;
}

void ProcInstance::typecheck_generic_args(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(m_generic_args_typechecked)
		return;

	::generate_constraints_for_generic_args(m_type_args.args, m_proc->type_params, subst, ctx);
	m_generic_args_typechecked = true;
}

//--------------------------------------------------------------------
// ProcTypeInstance
//--------------------------------------------------------------------
void ProcTypeInstance::typecheck(ConstraintGatheringSubst &subst, SemaContext &ctx)
{
	if(has_been_typechecked)
		return;

	for(ProcTypeParameter &param: *params)
		generate_constraints_for_type(param.type, subst, ctx);

	generate_constraints_for_type(*ret, subst, ctx);

	has_been_typechecked = true;
}

bool operator == (ProcTypeInstanceKey const &a, ProcTypeInstanceKey const &b)
{
	if(not equiv(*a.ret, *b.ret))
		return false;

	if(a.params->count != b.params->count)
		return false;

	for(size_t i = 0; i < a.params->count; ++i)
	{
		ProcTypeParameter const &param_a = a.params->items[i];
		ProcTypeParameter const &param_b = b.params->items[i];

		if(param_a.is_ref != param_b.is_ref) return false;
		if(not equiv(param_a.type, param_b.type)) return false;
	}

	return true;
}

//--------------------------------------------------------------------
// InstanceRegistry
//--------------------------------------------------------------------
static TypeArgList create_type_arg_list(FixedArray<GenericArg> *NULLABLE type_args, Arena &arena)
{
	if(not type_args)
		type_args = alloc_fixed_array<GenericArg>(0, arena);

	TypeArgList arg_list{.args = type_args};
	for(GenericArg const &arg: *type_args)
	{
		TermInfo info = gather_type_vars(arg, arg_list.occurring_vars, false);
		arg_list.has_type_deduction_vars |= info.has_deduction_vars;
		arg_list.has_known_ints |= info.has_known_ints;
	}

	return arg_list;
}

static void check_struct_parents(StructItem const *struct_, optional<DeclContainerInst> decl_parent)
{
	if(struct_->sema->decl_parent)
	{
		*struct_->sema->decl_parent | match
		{
			[&](StructItem *parent_item)
			{
				assert(decl_parent and decl_parent->try_as_struct()->struct_() == parent_item);
			},
			[&](ProcItem *parent_item)
			{
				assert(decl_parent and decl_parent->as_proc()->proc() == parent_item);
			},
		};
	}
	else {
		assert(not decl_parent.has_value());
	}
}

// `type_args` must already be resolved
StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	FixedArray<GenericArg> *NULLABLE type_args,
	optional<DeclContainerInst> decl_parent
)
{
	check_struct_parents(struct_, decl_parent);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, type_args, decl_parent));
	if(it != m_struct_instances.end())
		return &it->second;

	TypeArgList type_arg_list = create_type_arg_list(type_args, m_arena);
	return add_struct_instance(StructInstance(struct_, type_arg_list, decl_parent, next_struct_id(), this));
}

StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	TypeArgList const &type_args,
	optional<DeclContainerInst> decl_parent
)
{
	check_struct_parents(struct_, decl_parent);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, type_args.args, decl_parent));
	if(it != m_struct_instances.end())
		return &it->second;

	return add_struct_instance(StructInstance(struct_, type_args, decl_parent, next_struct_id(), this));
}

StructInstance* InstanceRegistry::get_struct_instance(
	StructItem const *struct_,
	TypeArgList const &type_args,
	TypeEnv const &subst,
	optional<DeclContainerInst> decl_parent,
	SubstitutionOptions mode
)
{
	check_struct_parents(struct_, decl_parent);

	void *original_mem_pos = m_arena.current_ptr();
	TypeArgList new_args = clone(type_args, m_arena);
	substitute_in_type_args(new_args, subst, *this, mode);

	auto it = m_struct_instances.find(StructInstanceKey(struct_, new_args.args, decl_parent));
	if(it != m_struct_instances.end())
	{
		// Free `new_args` since we only needed it for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_struct_instance(StructInstance(struct_, new_args, decl_parent, next_struct_id(), this));
}

StructInstance* InstanceRegistry::add_struct_instance(StructInstance &&new_inst)
{
	StructInstanceKey key = new_inst.key();
	StructInstance *inst = &m_struct_instances.emplace(key, std::move(new_inst)).first->second;
	LOGGER(m_mod.sema->logger, on_struct_register, inst);

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_struct_instance(inst);

	return inst;
}

StructInstance* InstanceRegistry::get_struct_self_instance(StructItem const *struct_)
{
	auto it = m_struct_self_instances.find(struct_);
	if(it != m_struct_self_instances.end())
		return it->second;

	FixedArray<GenericArg> *type_args = nullptr;
	if(struct_->type_params->count)
	{
		type_args = alloc_fixed_array<GenericArg>(struct_->type_params->count, m_arena);
		for(auto const &[idx, type_param]: *struct_->type_params | std::views::enumerate)
			new (type_args->items+idx) GenericArg(mk_generic_arg(&type_param, type_param.range));
	}

	optional<DeclContainerInst> decl_parent_inst = nullopt;
	if(struct_->sema->decl_parent)
		decl_parent_inst = get_self_instance(*struct_->sema->decl_parent);

	StructInstance *self_inst = get_struct_instance(struct_, type_args, decl_parent_inst);
	m_struct_self_instances.insert({struct_, self_inst});

	return self_inst;
}

ProcInstance* InstanceRegistry::get_proc_self_instance(ProcItem const *proc)
{
	auto it = m_proc_self_instances.find(proc);
	if(it != m_proc_self_instances.end())
		return it->second;

	FixedArray<GenericArg> *type_args = nullptr;
	if(proc->type_params->count)
	{
		type_args = alloc_fixed_array<GenericArg>(proc->type_params->count, m_arena);
		for(auto const &[idx, type_param]: *proc->type_params | std::views::enumerate)
			new (type_args->items+idx) GenericArg(mk_generic_arg(&type_param, type_param.range));
	}

	ProcInstance *self_inst = get_proc_instance(proc, type_args);
	m_proc_self_instances.insert({proc, self_inst});

	return self_inst;
}

DeclContainerInst InstanceRegistry::get_self_instance(DeclContainer decl)
{
	return decl | match
	{
		[&](StructItem *parent_item) -> DeclContainerInst
		{
			return get_struct_self_instance(parent_item);
		},
		[&](ProcItem *parent_item) -> DeclContainerInst
		{
			return get_proc_self_instance(parent_item);
		}
	};
}

std::generator<StructInstance&> InstanceRegistry::struct_instances()
{
	for(auto &[_, struct_]: m_struct_instances)
		co_yield struct_;
}

std::generator<UnionInstance&> InstanceRegistry::union_instances()
{
	for(auto &[_, union_]: m_union_instances)
		co_yield union_;
}

std::generator<ProcInstance&> InstanceRegistry::proc_instances()
{
	for(auto &[_, proc]: m_proc_instances)
		co_yield proc;
}

std::generator<ProcTypeInstance&> InstanceRegistry::proc_type_instances()
{
	for(auto &[_, proc_type]: m_proc_type_instances)
		co_yield proc_type;
}


// `type_args` must already be resolved and must not contain KnownIntTypes
ProcInstance* InstanceRegistry::get_proc_instance(ProcItem const *proc, FixedArray<GenericArg> *NULLABLE type_args)
{
	auto it = m_proc_instances.find(ProcInstanceKey(proc, type_args));
	if(it != m_proc_instances.end())
		return &it->second;

	TypeArgList type_arg_list = create_type_arg_list(type_args, m_arena);
	return add_proc_instance(ProcInstance(proc, type_arg_list, this));
}

ProcInstance* InstanceRegistry::get_proc_instance(ProcItem const *proc, TypeArgList const &type_args)
{
	auto it = m_proc_instances.find(ProcInstanceKey(proc, type_args.args));
	if(it != m_proc_instances.end())
		return &it->second;

	return add_proc_instance(ProcInstance(proc, type_args, this));
}

ProcInstance* InstanceRegistry::get_proc_instance(
	ProcItem const *proc,
	TypeArgList const &type_args,
	TypeEnv const &subst,
	SubstitutionOptions mode
)
{
	void *original_mem_pos = m_arena.current_ptr();
	TypeArgList new_args = clone(type_args, m_arena);
	substitute_in_type_args(new_args, subst, *this, mode);

	auto it = m_proc_instances.find(ProcInstanceKey(proc, new_args.args));
	if(it != m_proc_instances.end())
	{
		// Free `new_args` since we only needed it for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_proc_instance(ProcInstance(proc, new_args, this));
}

ProcInstance* InstanceRegistry::add_proc_instance(ProcInstance &&new_inst)
{
	ProcInstanceKey key = new_inst.key();
	ProcInstance *inst = &m_proc_instances.emplace(key, std::move(new_inst)).first->second;
	LOGGER(m_mod.sema->logger, on_proc_register, inst);

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_proc_instance(inst);

	return inst;
}


// `type_args` must already be resolved
ProcTypeInstance* InstanceRegistry::get_proc_type_instance(FixedArray<ProcTypeParameter> *params, Type *ret)
{
	ProcTypeInstanceKey key{params, ret};
	auto it = m_proc_type_instances.find(key);
	if(it != m_proc_type_instances.end())
		return &it->second;

	return add_proc_type_instance(params, ret);
}

ProcTypeInstance* InstanceRegistry::get_proc_type_instance(
	FixedArray<ProcTypeParameter> *params,
	Type *ret,
	TypeEnv const &subst,
	SubstitutionOptions mode
)
{
	void *original_mem_pos = m_arena.current_ptr();

	// Substitute params
	FixedArray<ProcTypeParameter> *new_params = clone(params, m_arena);
	for(ProcTypeParameter &new_param: *new_params)
		substitute_in_type(new_param.type, subst, *this, mode);

	// Substitute return type
	Type *new_ret = clone_ptr(ret, m_arena);
	substitute_in_type(*new_ret, subst, *this, mode);

	auto it = m_proc_type_instances.find(ProcTypeInstanceKey(new_params, new_ret));
	if(it != m_proc_type_instances.end())
	{
		// Free `new_params` and `new_ret` since we only needed them for lookup
		m_arena.set_current_ptr(original_mem_pos);
		return &it->second;
	}

	return add_proc_type_instance(new_params, new_ret);
}

ProcTypeInstance* InstanceRegistry::add_proc_type_instance(FixedArray<ProcTypeParameter> *params, Type *ret)
{
	unordered_set<GenericVar> occurring_vars;
	TermInfo info = gather_type_vars(*ret, occurring_vars);
	for(ProcTypeParameter const &p: *params)
		info.merge(gather_type_vars(p.type, occurring_vars));

	ProcTypeInstance *inst = &m_proc_type_instances.emplace(
		ProcTypeInstanceKey(params, ret), 
		ProcTypeInstance{
			.params = params,
			.ret = ret,
			.occurring_vars = std::move(occurring_vars),
			.has_type_deduction_vars = info.has_deduction_vars,
			.has_known_ints = info.has_known_ints,
		}
	).first->second;

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_proc_type_instance(inst);

	return inst;
}

UnionInstance* InstanceRegistry::get_union_instance(vector<Type*> &&alternatives)
{
	UnionInstanceKey key{alternatives};
	auto it = m_union_instances.find(key);
	if(it != m_union_instances.end())
		return &it->second;

	return add_union_instance(std::move(alternatives));
}

UnionInstance* InstanceRegistry::add_union_instance(vector<Type*> &&alternatives)
{
	unordered_set<GenericVar> occurring_vars;
	TermInfo info;
	for(Type const *alt: alternatives)
		info.merge(gather_type_vars(*alt, occurring_vars));

	UnionInstanceKey key(alternatives);
	UnionInstance *inst = &m_union_instances.emplace(
		key,
		UnionInstance(std::move(alternatives), std::move(occurring_vars), info.has_deduction_vars, info.has_known_ints)
	).first->second;

	for(InstanceRegistryListener *l: m_listeners)
		l->on_new_union_instance(inst);

	return inst;
}

void InstanceRegistry::add_listener(InstanceRegistryListener *l)
{
	m_listeners.push_back(l);
}

void InstanceRegistry::remove_listener(InstanceRegistryListener *l)
{
	m_listeners.erase(
		std::remove(m_listeners.begin(), m_listeners.end(), l),
		m_listeners.end()
	);
}
