#include "semantics/ast_operations.hpp"
#include "semantics/ast_traversal.hpp"
#include "semantics/context.hpp"
#include "semantics/error.hpp"
#include "semantics/module.hpp"
#include "semantics/type_properties.hpp"
#include <ostream>

namespace ranges = std::ranges;

//--------------------------------------------------------------------
// Equality
//--------------------------------------------------------------------
bool operator == (GenericVar const &a, GenericVar const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](GenericParameterVar const &p) { return p.def == std::get<GenericParameterVar>(b).def; },
		[&](GenericDeductionVar const &d) { return d.def == std::get<GenericDeductionVar>(b).def; },
	};
}

bool equiv(Type const &a, Type const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](BuiltinType const &ta)
		{
			BuiltinType const &tb = std::get<BuiltinType>(b);
			return ta.builtin == tb.builtin;
		},
		[&](KnownIntType const &ta)
		{
			KnownIntType const &tb = std::get<KnownIntType>(b);
			return ta.low == tb.low && ta.high == tb.high;
		},
		[&](VarType const &ta)
		{
			VarType const &tb = std::get<VarType>(b);
			return ta.var == tb.var;
		},
		[&](PointerType const &ta)
		{
			PointerType const &tb = std::get<PointerType>(b);
			return ta.kind == tb.kind and ta.mutability == tb.mutability and equiv(*ta.pointee, *tb.pointee);
		},
		[&](ArrayType const &ta)
		{
			ArrayType const &tb = std::get<ArrayType>(b);
			assert(ta.count_arg and tb.count_arg);
			return equiv(*ta.count_arg, *tb.count_arg) and equiv(*ta.element, *tb.element);
		},
		[&](StructType const &ta)
		{
			StructType const &tb = std::get<StructType>(b);
			return ta.inst == tb.inst;
		},
		[&](ProcType const &ta)
		{
			ProcType const &tb = std::get<ProcType>(b);
			return ta.inst == tb.inst;
		},
		[&](UnionType const &ta)
		{
			UnionType const &tb = std::get<UnionType>(b);
			return ta.inst == tb.inst;
		},
		[&](ProcTypeUnresolved const&) -> bool { assert(!"equiv: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"equiv: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"equiv: Path"); },
		[&](InlineStructType const&) -> bool { assert(!"equiv: InlineStructType"); },
	};
}

bool equiv(Expr const &a, Expr const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](IntLiteralExpr const &ea)
		{
			IntLiteralExpr const &eb = std::get<IntLiteralExpr>(b);
			return ea.value == eb.value;
		},
		[&](BoolLiteralExpr const &ea)
		{
			BoolLiteralExpr const &eb = std::get<BoolLiteralExpr>(b);
			return ea.value == eb.value;
		},
		[&](StringLiteralExpr const &ea)
		{
			StringLiteralExpr const &eb = std::get<StringLiteralExpr>(b);
			return ea.kind == eb.kind and ea.value == eb.value;
		},
		[&](UnaryExpr const &ea)
		{
			UnaryExpr const &eb = std::get<UnaryExpr>(b);
			return ea.op == eb.op and equiv(*ea.sub, *eb.sub);
		},
		[&](BinaryExpr const &ea)
		{
			BinaryExpr const &eb = std::get<BinaryExpr>(b);
			return ea.op == eb.op and equiv(*ea.left, *eb.left) and equiv(*ea.right, *eb.right);
		},
		[&](AddressOfExpr const &ea)
		{
			AddressOfExpr const &eb = std::get<AddressOfExpr>(b);
			return ea.mutability == eb.mutability and equiv(*ea.object, *eb.object);
		},
		[&](DerefExpr const &ea)
		{
			DerefExpr const &eb = std::get<DerefExpr>(b);
			return equiv(*ea.addr, *eb.addr);
		},
		[&](IndexExpr const &ea)
		{
			IndexExpr const &eb = std::get<IndexExpr>(b);
			return equiv(*ea.addr, *eb.addr) and equiv(*ea.index, *eb.index);
		},
		[&](MemberAccessExpr const &ea)
		{
			MemberAccessExpr const &eb = std::get<MemberAccessExpr>(b);
			return ea.member == eb.member and equiv(*ea.object, *eb.object);
		},
		[&](AssignmentExpr const &ea)
		{
			AssignmentExpr const &eb = std::get<AssignmentExpr>(b);
			return equiv(*ea.lhs, *eb.lhs) and equiv(*ea.rhs, *eb.rhs);
		},
		[&](AsExpr const &ea)
		{
			AsExpr const &eb = std::get<AsExpr>(b);
			return equiv(*ea.src_expr, *eb.src_expr) and equiv(*ea.target_type, *eb.target_type);
		},
		[&](ConstructorExpr const &ea)
		{
			ConstructorExpr const &eb = std::get<ConstructorExpr>(b);
			return equiv(*ea.ctor, *eb.ctor);
		},
		[&](ProcExpr const &ea)
		{
			ProcExpr const &eb = std::get<ProcExpr>(b);
			return ea.inst == eb.inst;
		},
		[&](CallExpr const &ea)
		{
			CallExpr const &eb = std::get<CallExpr>(b);
			if(not equiv(*ea.callable, *eb.callable))
				return false;

			if(ea.args->count != eb.args->count)
				return false;

			for(size_t i = 0; i < ea.args->count; ++i)
			{
				Argument const &arg_a = ea.args->items[i];
				Argument const &arg_b = eb.args->items[i];
				if(arg_a.param_idx != arg_b.param_idx) return false;
				if(arg_a.name != arg_b.name) return false;
				if(not equiv(arg_a.expr, arg_b.expr)) return false;
			}

			return true;
		},
		[&](SizeOfExpr const &ea)
		{
			SizeOfExpr const &eb = std::get<SizeOfExpr>(b);
			if(not ea.subject or not eb.subject)
				return ea.subject == eb.subject;

			return equiv(*ea.subject, *eb.subject);
		},
		[&](MakeExpr const&) -> bool
		{
			assert(!"[TODO] substitute_types_in_expr: MakeExpr");
		},
		[&](UnionInitExpr const &ea)
		{
			UnionInitExpr const &eb = std::get<UnionInitExpr>(b);
			return equiv(*ea.alt_expr, *eb.alt_expr) and equiv(*ea.alt_type, *eb.alt_type);
		},
		[&](VarExpr const &ea)
		{
			VarExpr const &eb = std::get<VarExpr>(b);
			return ea.var == eb.var;
		},
		[&](GenericVarExpr const &ea)
		{
			GenericVarExpr const &eb = std::get<GenericVarExpr>(b);
			return ea.var == eb.var;
		},
		[&](Path const&) ->bool { assert(!"substitute_types_in_expr: Path"); },
	};
}

bool equiv(GenericArg const &a, GenericArg const &b)
{
	if(a.index() != b.index())
		return false;

	return a | match
	{
		[&](Type const &t) { return equiv(t, std::get<Type>(b)); },
		[&](Expr const &e) { return equiv(e, std::get<Expr>(b)); },
	};
}

//--------------------------------------------------------------------
// Ordering
//--------------------------------------------------------------------
bool operator < (GenericVar const &a, GenericVar const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](GenericParameterVar va)
		{
			// TODO Use something other than pointers to make comparison deterministic
			return va.def < std::get<GenericParameterVar>(b).def;
		},
		[&](GenericDeductionVar va)
		{
			return va.def->id < std::get<GenericDeductionVar>(b).def->id;
		},
	};
}

bool operator < (Expr const &a, Expr const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](IntLiteralExpr const &ea)
		{
			return ea.value < std::get<IntLiteralExpr>(b).value;
		},
		[&](GenericVarExpr const &ea)
		{
			return ea.var < std::get<GenericVarExpr>(b).var;
		},
		[&](BinaryExpr const &ea)
		{
			BinaryExpr const &eb = std::get<BinaryExpr>(b);
			return std::tie(ea.op, *ea.left, *ea.right) < std::tie(eb.op, *eb.left, *eb.right);
		},
		[&](auto const&) -> bool
		{
			assert(!"[TODO] operator <: Expr");
		}
	};
}

bool operator < (GenericArg const &a, GenericArg const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](Type const &t) { return t < std::get<Type>(b); },
		[&](Expr const &e) { return e < std::get<Expr>(b); },
	};
}

bool operator < (Type const &a, Type const &b)
{
	if(a.index() != b.index())
		return a.index() < b.index();

	return a | match
	{
		[&](BuiltinType const &ta)
		{
			BuiltinType const &tb = std::get<BuiltinType>(b);
			return ta.builtin < tb.builtin;
		},
		[&](KnownIntType const &ta)
		{
			KnownIntType const &tb = std::get<KnownIntType>(b);
			return std::tie(ta.low, ta.high) < std::tie(tb.low, tb.high);
		},
		[&](VarType const &ta)
		{
			VarType const &tb = std::get<VarType>(b);
			return ta.var < tb.var;
		},
		[&](PointerType const &ta)
		{
			PointerType const &tb = std::get<PointerType>(b);
			return std::tie(ta.kind, ta.mutability, *ta.pointee) < std::tie(tb.kind, tb.mutability, *tb.pointee);
		},
		[&](ArrayType const &ta)
		{
			ArrayType const &tb = std::get<ArrayType>(b);
			assert(ta.count_arg and tb.count_arg);
			return std::tie(*ta.count_arg, *ta.element) < std::tie(*tb.count_arg, *tb.element);
		},
		[&](StructType const &ta)
		{
			StructType const &tb = std::get<StructType>(b);
			return ta.inst->hash() < tb.inst->hash();
		},
		[&](ProcType const &ta)
		{
			ProcType const &tb = std::get<ProcType>(b);

			// TODO Use something other than pointers to make comparison deterministic
			return ta.inst < tb.inst;
		},
		[&](UnionType const &ta)
		{
			UnionType const &tb = std::get<UnionType>(b);

			// TODO Use something other than pointers to make comparison deterministic
			return ta.inst < tb.inst;
		},
		[&](ProcTypeUnresolved const&) -> bool { assert(!"operator <: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"operator <: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"operator <: Path"); },
		[&](InlineStructType const&) -> bool { assert(!"operator <: InlineStructType"); },
	};
}

//--------------------------------------------------------------------
// Hashing
//--------------------------------------------------------------------
size_t std::hash<Type>::operator () (Type const &type) const
{
	size_t h = compute_hash(type.index());
	type | match
	{
		[&](BuiltinType const &t)
		{
			combine_hashes(h, compute_hash((int)t.builtin));
		},
		[&](KnownIntType const &t)
		{
			combine_hashes(h, compute_hash(t.low));
			combine_hashes(h, compute_hash(t.high));
		},
		[&](VarType const &t)
		{
			combine_hashes(h, compute_hash(t.var));
		},
		[&](PointerType const &t)
		{
			combine_hashes(h, compute_hash((int)t.kind));
			combine_hashes(h, compute_hash((int)t.mutability));
			combine_hashes(h, compute_hash(*t.pointee));
		},
		[&](ArrayType const &t)
		{
			combine_hashes(h, compute_hash(*t.element));
			combine_hashes(h, compute_hash(bool(t.count_arg)));
			if(t.count_arg)
			{
				combine_hashes(h, compute_hash(t.count_arg->index()));
				*t.count_arg | match
				{
					[&](IntLiteralExpr const &e)
					{
						combine_hashes(h, compute_hash(e.value));
					},
					[&](auto const&)
					{
						assert("[TODO] hash(Type): ArrayType: count: Expr");
					},
				};
			}
		},
		[&](StructType const &t)
		{
			combine_hashes(h, compute_hash(t.inst));
		},
		[&](ProcType const &t)
		{
			combine_hashes(h, compute_hash(t.inst));
		},
		[&](UnionType const &t)
		{
			combine_hashes(h, compute_hash(t.inst));
		},
		[&](ProcTypeUnresolved const&) { assert(!"hash<Type>: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) { assert(!"hash<Type>: UnionTypeUnresolved"); },
		[&](Path const&) { assert(!"hash<Type>: Path"); },
		[&](InlineStructType const&) { assert(!"hash<Type>: InlineStructType"); },
	};

	return h;
}

size_t std::hash<Expr>::operator () (Expr const &expr) const
{
	size_t h = compute_hash(expr.index());
	auto visitor = [&](this auto &self, Expr const &expr) -> void
	{
		expr | match
		{
			[&](IntLiteralExpr const &e)
			{
				combine_hashes(h, compute_hash(e.value));
			},
			[&](UnaryExpr const &e) -> void
			{
				combine_hashes(h, compute_hash((int)e.op));
				expr_visit_children(e, self);
			},
			[&](BinaryExpr const &e) -> void
			{
				combine_hashes(h, compute_hash((int)e.op));
				expr_visit_children(e, self);
			},
			[&](AsExpr const &e) -> void
			{
				combine_hashes(h, compute_hash(*e.target_type));
				expr_visit_children(e, self);
			},
			[&](VarExpr const &e)
			{
				combine_hashes(h, compute_hash(e.var));
			},
			[&](GenericVarExpr const &e)
			{
				combine_hashes(h, compute_hash(e.var));
			},
			[&](const auto&) // Not sure why I have to pass `this` explicitly here
			{
				assert(!"[TODO] hash<Expr>");
			},
		};
	};
	visitor(expr);

	return h;
}

size_t std::hash<GenericArg>::operator () (GenericArg const &arg) const
{
	size_t h = compute_hash(arg.index());
	arg | match
	{
		[&](Type const &t) { combine_hashes(h, compute_hash(t)); },
		[&](Expr const &e) { combine_hashes(h, compute_hash(e)); },
	};

	return h;
}

size_t std::hash<FixedArray<GenericArg>>::operator () (FixedArray<GenericArg> const &args) const
{
	size_t h = compute_hash(args.count);
	for(GenericArg const &arg: args)
		combine_hashes(h, compute_hash(arg));

	return h;
}

size_t std::hash<GenericVar>::operator () (GenericVar var) const
{
	size_t h = compute_hash(var.index());
	var | match
	{
		[&](GenericParameterVar v)
		{
			combine_hashes(h, compute_hash(v.def));
		},
		[&](GenericDeductionVar v)
		{
			combine_hashes(h, compute_hash(v));
		},
	};

	return h;
}

//--------------------------------------------------------------------
// Cloning
//--------------------------------------------------------------------
static DefaultValueExpr clone(DefaultValueExpr const &default_val_expr, Arena &arena)
{
	return default_val_expr | match
	{
		[&](NoDefaultValue) -> DefaultValueExpr { return NoDefaultValue(); },
		[&](ExprPending) -> DefaultValueExpr { return ExprPending(); },
		[&](Expr *expr) -> DefaultValueExpr
		{
			return clone_ptr(expr, arena);
		},
	};
}

FixedArray<Type>* clone(FixedArray<Type> const *types, Arena &arena)
{
	FixedArray<Type> *result = alloc_fixed_array<Type>(types->count, arena);
	for(size_t i = 0; i < types->count; ++i)
		result->items[i] = clone(types->items[i], arena);

	return result;
}

GenericArg clone(GenericArg const &arg, Arena &arena)
{
	return arg | match
	{
		[&](Type const &t) -> GenericArg { return clone(t, arena); },
		[&](Expr const &e) -> GenericArg { return clone(e, arena); },
	};
}

Type clone(Type const &type, Arena &arena)
{
	return type | match
	{
		[&](BuiltinType const &t) -> Type
		{
			return t;
		},
		[&](KnownIntType const &t) -> Type
		{
			return t;
		},
		[&](PointerType const &t) -> Type
		{
			return PointerType{
				.range = t.range,
				.pointee = clone_ptr(t.pointee, arena),
				.kind = t.kind,
				.mutability = t.mutability,
			};
		},
		[&](ArrayType const &t) -> Type
		{
			return ArrayType{
				.range = t.range,
				.element = clone_ptr(t.element, arena),
				.count_arg = clone_ptr(t.count_arg, arena),
			};
		},
		[&](StructType const &t) -> Type
		{
			return t;
		},
		[&](ProcTypeUnresolved const &t) -> Type
		{
			return ProcTypeUnresolved{
				.range = t.range,
				.ret = clone_ptr(t.ret, arena),
				.params = clone(t.params, arena),
			};
		},
		[&](ProcType const &t) -> Type
		{
			return t;
		},
		[&](UnionTypeUnresolved const &t) -> Type
		{
			assert(!"clone(Type): UnionTypeUnresolved");
			return UnionTypeUnresolved{
				.range = t.range,
				.alternatives = clone(t.alternatives, arena),
			};
		},
		[&](UnionType const &t) -> Type
		{
			return t;
		},
		[&](VarType const &t) -> Type
		{
			return t;
		},
		[&](Path const&) -> Type { UNREACHABLE; },
		[&](InlineStructType const&) -> Type { UNREACHABLE; },
	};
}

TypeArgList clone(TypeArgList const &args, Arena &arena)
{
	return TypeArgList{
		.args = clone(args.args, arena),
		.occurring_vars = args.occurring_vars,
		.has_type_deduction_vars = args.has_type_deduction_vars,
	};
}

FixedArray<Argument>* clone(FixedArray<Argument> const *args, Arena &arena)
{
	FixedArray<Argument> *result = alloc_fixed_array<Argument>(args->count, arena);
	for(size_t i = 0; i < args->count; ++i)
		result->items[i] = clone(args->items[i], arena);

	return result;
}

Expr clone(Expr const &expr, Arena &arena)
{
	return expr | match
	{
		[&](IntLiteralExpr const &e) -> Expr
		{
			return IntLiteralExpr{
				.range = e.range,
				.value = e.value,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](BoolLiteralExpr const &e) -> Expr
		{
			return BoolLiteralExpr{
				.range = e.range,
				.value = e.value,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](StringLiteralExpr const &e) -> Expr
		{
			return StringLiteralExpr{
				.range = e.range,
				.kind = e.kind,
				.value = e.value,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](UnaryExpr const &e) -> Expr
		{
			return UnaryExpr{
				.range = e.range,
				.sub = clone_ptr(e.sub, arena),
				.op = e.op,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](BinaryExpr const &e) -> Expr
		{
			return BinaryExpr{
				.range = e.range,
				.left = clone_ptr(e.left, arena),
				.right = clone_ptr(e.right, arena),
				.op = e.op,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](AddressOfExpr const &e) -> Expr
		{
			return AddressOfExpr{
				.range = e.range,
				.object = clone_ptr(e.object, arena),
				.mutability = e.mutability,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](DerefExpr const &e) -> Expr
		{
			return DerefExpr{
				.range = e.range,
				.addr = clone_ptr(e.addr, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](IndexExpr const &e) -> Expr
		{
			return IndexExpr{
				.range = e.range,
				.addr = clone_ptr(e.addr, arena),
				.index = clone_ptr(e.index, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](MemberAccessExpr const &e) -> Expr
		{
			return MemberAccessExpr{
				.range = e.range,
				.object = clone_ptr(e.object, arena),
				.member = e.member,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](AssignmentExpr const &e) -> Expr
		{
			return AssignmentExpr{
				.range = e.range,
				.lhs = clone_ptr(e.lhs, arena),
				.rhs = clone_ptr(e.rhs, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](AsExpr const &e) -> Expr
		{
			return AsExpr{
				.range = e.range,
				.src_expr = clone_ptr(e.src_expr, arena),
				.target_type = clone_ptr(e.target_type, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](ConstructorExpr const &e) -> Expr
		{
			return ConstructorExpr{
				.range = e.range,
				.ctor = clone_ptr(e.ctor, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](ProcExpr const &e) -> Expr
		{
			return ProcExpr{
				.range = e.range,
				.inst = e.inst,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](CallExpr const &e) -> Expr
		{
			return CallExpr{
				.range = e.range,
				.callable = clone_ptr(e.callable, arena),
				.args = clone(e.args, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](SizeOfExpr const &e) -> Expr
		{
			return SizeOfExpr{
				.range = e.range,
				.subject = clone_ptr(e.subject, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](MakeExpr const &e) -> Expr
		{
			return MakeExpr{
				.range = e.range,
				.init = clone_ptr(e.init, arena),
				.addr = clone_ptr(e.addr, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](Path const&) -> Expr
		{
			assert(!"[TODO] clone(Expr): Path");
		},
		[&](VarExpr const &e) -> Expr
		{
			return VarExpr{
				.range = e.range,
				.var = e.var,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](GenericVarExpr const &e) -> Expr
		{
			return GenericVarExpr{
				.range = e.range,
				.var = e.var,
				.type = clone_ptr(e.type, arena),
			};
		},
		[&](UnionInitExpr const &e) -> Expr
		{
			return UnionInitExpr{
				.range = e.range,
				.alt_expr = clone_ptr(e.alt_expr, arena),
				.alt_type = clone_ptr(e.alt_type, arena),
				.type = clone_ptr(e.type, arena),
			};
		},
	};
}

Argument clone(Argument const &arg, Arena &arena)
{
	return Argument{
		.range = arg.range,
		.expr = clone(arg.expr, arena),
		.name = arg.name,
		.param_idx = arg.param_idx,
	};
}

Parameter clone(Parameter const &param, Arena &arena)
{
	return Parameter{
		.range = param.range,
		.type = clone_ptr(param.type, arena),
		.default_value = clone(param.default_value, arena),
	};
}

FixedArray<PatternArgument>* clone(FixedArray<PatternArgument> const *args, Arena &arena);

Pattern clone(Pattern const &pattern, Arena &arena)
{
	return pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			return Pattern(
				VarPatternUnresolved(p.range, p.name, p.mutability),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](VarPattern const &p)
		{
			return Pattern(
				VarPattern(p.range, p.var, clone_ptr(p.type, arena)),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](DerefPattern const &p)
		{
			return Pattern(
				DerefPattern(p.range, clone_ptr(p.sub, arena), clone_ptr(p.type, arena)),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](AddressOfPattern const &p)
		{
			return Pattern(
				AddressOfPattern(
					p.range,
					clone_ptr(p.sub, arena),
					p.mutability,
					clone_ptr(p.type, arena)
				),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](ConstructorPattern const &p)
		{
			return Pattern(
				ConstructorPattern(
					p.range,
					clone_ptr(p.ctor, arena),
					clone(p.args, arena),
					p.has_parens,
					clone_ptr(p.type, arena)
				),
				clone_ptr(pattern.provided_type, arena)
			);
		},
		[&](WildcardPattern const &p)
		{
			return Pattern(
				WildcardPattern(p.range, clone_ptr(p.type, arena)),
				clone_ptr(pattern.provided_type, arena)
			);
		}
	};
}

PatternArgument clone(PatternArgument const &pat_arg, Arena &arena)
{
	return PatternArgument{
		.pattern = clone(pat_arg.pattern, arena),
		.param_name = pat_arg.param_name,
		.param_idx = pat_arg.param_idx,
	};
}

FixedArray<PatternArgument>* clone(FixedArray<PatternArgument> const *args, Arena &arena)
{
	FixedArray<PatternArgument> *result = alloc_fixed_array<PatternArgument>(args->count, arena);
	for(size_t i = 0; i < args->count; ++i)
		result->items[i] = clone(args->items[i], arena);

	return result;
}

Stmt clone(Stmt const &stmt, Arena &arena)
{
	return stmt | match
	{
		[&](LetStmt const &s) -> Stmt
		{
			return LetStmt{
				.range = s.range,
				.lhs = clone_ptr(s.lhs, arena),
				.init_expr = clone_ptr(s.init_expr, arena),
			};
		},
		[&](ExprStmt const &s) -> Stmt
		{
			return ExprStmt(s.range, clone_ptr(s.expr, arena));
		},
		[&](BlockStmt const &s) -> Stmt
		{
			return BlockStmt(s.range, clone(s.stmts, arena));
		},
		[&](ReturnStmt const &s) -> Stmt
		{
			return ReturnStmt(s.range, clone_ptr(s.ret_expr, arena));
		},
		[&](IfStmt const &s) -> Stmt
		{
			return IfStmt(
				s.range,
				clone_ptr(s.condition, arena),
				clone_ptr(s.then, arena),
				clone_ptr(s.else_, arena)
			);
		},
		[&](WhileStmt const &s) -> Stmt
		{
			return WhileStmt(
				s.range,
				clone_ptr(s.condition, arena),
				clone_ptr(s.body, arena)
			);
		},
		[&](DeclStmt const &s) -> Stmt
		{
			return s;
		},
		[&](MatchStmt const &s) -> Stmt
		{
			return MatchStmt(
				s.range,
				clone_ptr(s.expr, arena),
				clone(s.arms, arena)
			);
		},
	};
}

FixedArray<Stmt>* clone(FixedArray<Stmt> const *stmts, Arena &arena)
{
	FixedArray<Stmt> *result = alloc_fixed_array<Stmt>(stmts->count, arena);
	for(size_t i = 0; i < stmts->count; ++i)
		result->items[i] = clone(stmts->items[i], arena);

	return result;
}

MatchArm clone(MatchArm const &arm, Arena &arena)
{
	return MatchArm{
		.capture = clone(arm.capture, arena),
		.stmt = clone(arm.stmt, arena),
		.discr = arm.discr,
	};
}

FixedArray<MatchArm>* clone(FixedArray<MatchArm> const *arms, Arena &arena)
{
	FixedArray<MatchArm> *result = alloc_fixed_array<MatchArm>(arms->count, arena);
	for(size_t i = 0; i < arms->count; ++i)
		result->items[i] = clone(arms->items[i], arena);

	return result;
}

//--------------------------------------------------------------------
// Printing
//--------------------------------------------------------------------
static constexpr int INDENT_WIDTH = 4;

string_view str(BuiltinTypeDef t)
{
	switch(t)
	{
		case BuiltinTypeDef::NEVER: return "Never";
		case BuiltinTypeDef::UNIT: return "Unit";
		case BuiltinTypeDef::BOOL: return "bool";
		case BuiltinTypeDef::I8: return "i8";
		case BuiltinTypeDef::U8: return "u8";
		case BuiltinTypeDef::I32: return "i32";
		case BuiltinTypeDef::U32: return "u32";
		case BuiltinTypeDef::ISIZE: return "isize";
		case BuiltinTypeDef::USIZE: return "usize";
	}

	UNREACHABLE;
}

void print_type_args(FixedArray<GenericArg> const *type_args, Module const &mod, std::ostream &os)
{
	if(type_args->count)
	{
		os << "'(";
		print(type_args->head(), mod, os);
		for(GenericArg const &arg: type_args->tail())
		{
			os << ", ";
			print(arg, mod, os);
		}
		os << ")";
	}
}

void print(Path const &path, Module const &mod, std::ostream &os)
{
	os << mod.name_of(path);
	print_type_args(path.type_args, mod, os);

	if(path.child)
	{
		os << ".";
		print(*path.child, mod, os);
	}
}

static void print_proc_type(FixedArray<Type> const *params, Type const *ret_type, Module const &mod, std::ostream &os)
{
	os << "proc(";
	if(params->count)
	{
		print(params->head(), mod, os);
		for(Type const &param: params->tail())
		{
			os << ", ";
			print(param, mod, os);
		}
	}
	os << ") -> ";
	print(*ret_type, mod, os);
}

void print_decl_container(DeclContainerInst cont, Module const &mod, std::ostream &os)
{
	if(cont.decl_parent())
	{
		print_decl_container(*cont.decl_parent(), mod, os);
		os << ".";
	}

	cont | match
	{
		[&](StructInstance *inst)
		{
			os << inst->struct_()->name;
		},
		[&](ProcInstance *inst)
		{
			os << inst->proc()->name;
		},
	};

	print_type_args(cont.type_args().args, mod, os);
}

void print(GenericVar const &var, std::ostream &os)
{
	var | match
	{
		[&](GenericParameterVar v)
		{
			os << v.def->name;
		},
		[&](GenericDeductionVar v)
		{
			os << "?_" << v.def->id;
		},
	};
}

void print(Type const &type, Module const &mod, std::ostream &os)
{
	type | match
	{
		[&](BuiltinType const &t)
		{
			os << str(t.builtin);
		},
		[&](KnownIntType const &t)
		{
			os << "$KnownInt(" << t.low << "," << t.high << ")";
		},
		[&](PointerType const &t)
		{
			if(t.kind == PointerType::SINGLE)
				os << "^";
			else
				os << "[bare]";

			if(t.mutability == IsMutable::YES)
				os << "mut ";

			print(*t.pointee, mod, os);
		},
		[&](ArrayType const &t)
		{
			os << "[";
			if(t.count_arg) print(*t.count_arg, mod, os);
			else os << "_";
			os << "]";

			print(*t.element, mod, os);
		},
		[&](ProcType const &t)
		{
			print_proc_type(t.inst->params, t.inst->ret, mod, os);
		},
		[&](ProcTypeUnresolved const &t)
		{
			print_proc_type(t.params, t.ret, mod, os);
		},
		[&](StructType const &t)
		{
			print_decl_container(t.inst, mod, os);
		},
		[&](UnionTypeUnresolved const &t)
		{
			print(t.alternatives->head(), mod, os);
			for(Type const &alt: t.alternatives->tail())
			{
				os << " | ";
				print(alt, mod, os);
			}
		},
		[&](UnionType const &t)
		{
			UnionInstance::ConstTypeRange const &alts = t.inst->alternatives();
			if(alts.size() == 1)
			{
				os << "(";
				print(*alts.front(), mod, os);
				os << " |)";
			}
			else
			{
				print(*alts.front(), mod, os);
				for(Type const *alt: t.inst->alternatives() | std::views::drop(1))
				{
					os << " | ";
					print(*alt, mod, os);
				}
			}
		},
		[&](Path const &path)
		{
			print(path, mod, os);
		},
		[&](VarType const &t)
		{
			print(t.var, os);
		},
		[&](InlineStructType const&)
		{
			assert(!"[TODO] print(Type): InlineStructType");
		},
	};
}

string str(Type const &type, Module const &mod)
{
	std::stringstream ss;
	print(type, mod, ss);
	return std::move(ss).str();
}

string str(Expr const &expr, Module const &mod)
{
	std::stringstream ss;
	print(expr, mod, ss);
	return std::move(ss).str();
}

string str(Stmt const &stmt, Module const &mod)
{
	std::stringstream ss;
	print(stmt, mod, ss);
	return std::move(ss).str();
}

void print(Argument const &arg, Module const &mod, std::ostream &os)
{
	if(arg.name.size())
		os << "." << arg.name << "=";

	print(arg.expr, mod, os);
}

void print(GenericArg const &g, Module const &mod, std::ostream &os)
{
	g | match
	{
		[&](Type const &t) { print(t, mod, os); },
		[&](Expr const &e) { print(e, mod, os); },
	};
}

string str(GenericArg const &a, Module const &mod)
{
	std::stringstream ss;
	print(a, mod, ss);
	return std::move(ss).str();
}

void print(Expr const &expr, Module const &mod, std::ostream &os)
{
	expr | match
	{
		[&](IntLiteralExpr const &e)
		{
			os << e.value;
		},
		[&](BoolLiteralExpr const &e)
		{
			os << (e.value ? "true" : "false");
		},
		[&](StringLiteralExpr const &e)
		{
			switch(e.kind)
			{
				case StringLiteralKind::C: os << "c\"" << e.value << '"'; break;
			}
		},
		[&](UnaryExpr const &e)
		{
			os << "(";
			switch(e.op)
			{
				case UnaryOp::NOT: os << "not "; break;
				case UnaryOp::NEG: os << "-"; break;
			}
			print(*e.sub, mod, os);
			os << ")";
		},
		[&](BinaryExpr const &e)
		{
			os << "(";
			print(*e.left, mod, os);
			switch(e.op)
			{
				case BinaryOp::ADD: os << " + "; break;
				case BinaryOp::SUB: os << " - "; break;
				case BinaryOp::MUL: os << " * "; break;
				case BinaryOp::DIV: os << " / "; break;

				case BinaryOp::EQ: os << " == "; break;
				case BinaryOp::LT: os << " < "; break;
				case BinaryOp::LE: os << " <= "; break;
				case BinaryOp::GT: os << " > "; break;
				case BinaryOp::GE: os << " >= "; break;
			}
			print(*e.right, mod, os);
			os << ")";
		},
		[&](AddressOfExpr const &e)
		{
			os << "&";
			if(e.mutability == IsMutable::YES)
				os << "mut ";

			os << "(";
			print(*e.object, mod, os);
			os << ")";
		},
		[&](DerefExpr const &e)
		{
			os << "(";
			print(*e.addr, mod, os);
			os << ")^";
		},
		[&](IndexExpr const &e)
		{
			os << "(";
			print(*e.addr, mod, os);
			os << ")^";
		},
		[&](MemberAccessExpr const &e)
		{
			print(*e.object, mod, os);
			os << "." << e.member;
		},
		[&](AssignmentExpr const &e)
		{
			os << "(";
			print(*e.lhs, mod, os);
			os << " = ";
			print(*e.rhs, mod, os);
			os << ")";
		},
		[&](AsExpr const &e)
		{
			os << "(";
			print(*e.src_expr, mod, os);
			os << " as ";
			print(*e.target_type, mod, os);
			os << ")";
		},
		[&](ConstructorExpr const &e)
		{
			print(*e.ctor, mod, os);
		},
		[&](ProcExpr const &e)
		{
			os << e.inst->proc()->name;
			print_type_args(e.inst->type_args().args, mod, os);
		},
		[&](CallExpr const &e)
		{
			print(*e.callable, mod, os);
			os << "(";
			if(e.args->count)
			{
				print(e.args->head(), mod, os);
				for(Argument const &arg: e.args->tail())
				{
					os << ", ";
					print(arg, mod, os);
				}
			}
			os << ")";
		},
		[&](SizeOfExpr const &e)
		{
			os << "size_of(";
			print(*e.subject, mod, os);
			os << ")";
		},
		[&](MakeExpr const &e)
		{
			os << "make ";
			print(*e.init, mod, os);
			os << " @ ";
			print(*e.addr, mod, os);
		},
		[&](UnionInitExpr const &e)
		{
			os << "$InitUnion'(";
			print(*e.type, mod, os);
			os << ")(";
			print(*e.alt_expr, mod, os);
			os << ")";
		},
		[&](VarExpr const &e)
		{
			os << e.var->name;
		},
		[&](GenericVarExpr const &e)
		{
			print(e.var, os);
		},
		[&](Path const &p)
		{
			print(p, mod, os);
		}
	};
}

void print(PatternArgument const &arg, Module const &mod, std::ostream &os)
{
	if(arg.param_name.size())
		os << "." << arg.param_name << "=";

	print(arg.pattern, mod, os);
}

void print(Pattern const &pattern, Module const &mod, std::ostream &os)
{
	pattern | match
	{
		[&](VarPatternUnresolved const &p)
		{
			if(p.mutability == IsMutable::YES)
				os << "mut ";
			os << p.name;
		},
		[&](VarPattern const &p)
		{
			if(p.var->mutability == IsMutable::YES)
				os << "mut ";
			os << p.var->name;

			if(p.var->type and not pattern.provided_type)
			{
				os << ": ";
				print(*p.var->type, mod, os);
			}
		},
		[&](DerefPattern const &p)
		{
			print(*p.sub, mod, os);
			os << " ^";
		},
		[&](AddressOfPattern const &p)
		{
			print(*p.sub, mod, os);
			os << " &";
			if(p.mutability == IsMutable::YES)
				os << "mut";
		},
		[&](ConstructorPattern const &p)
		{
			print(*p.ctor, mod, os);
			os << "(";
			if(p.args->count)
			{
				print(p.args->head(), mod, os);
				for(PatternArgument const &arg: p.args->tail())
				{
					os << ", ";
					print(arg, mod, os);
				}
			}
			os << ")";
		},
		[&](WildcardPattern const&)
		{
			os << "_";
		}
	};

	if(pattern.provided_type)
	{
		os << ": ";
		print(*pattern.provided_type, mod, os);
	}
}

void print(StructItem const &struct_, bool is_top_level, int indent, Module const &mod, std::ostream &os);

void print(Stmt const &stmt, int indent, Module const &mod, std::ostream &os)
{
	stmt | match
	{
		[&](LetStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			os << "let ";
			print(*s.lhs, mod, os);
			if(s.init_expr)
			{
				os << " = ";
				print(*s.init_expr, mod, os);
			}

			os << ";";
		},
		[&](ExprStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			print(*s.expr, mod, os);
			os << ";";
		},
		[&](BlockStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "{\n";
			for(Stmt const &child_stmt: *s.stmts)
			{
				print(child_stmt, indent+1, mod, os);
				os << "\n";
			}
			os << string(indent*INDENT_WIDTH, ' ') << "}";
		},
		[&](ReturnStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ');
			os << "return";
			if(s.ret_expr)
			{
				os << " ";
				print(*s.ret_expr, mod, os);
			}
			os << ";";
		},
		[&](IfStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "if ";
			print(*s.condition, mod, os);
			os << "\n";
			print(*s.then, indent, mod, os);
			if(s.else_)
			{
				os << "\n" << string(indent*INDENT_WIDTH, ' ') << "else\n";
				print(*s.else_, indent, mod, os);
			}
		},
		[&](WhileStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "while ";
			print(*s.condition, mod, os);
			os << "\n";
			print(*s.body, indent, mod, os);
		},
		[&](DeclStmt const &s)
		{
			print(*s.item, true, indent, mod, os);
		},
		[&](MatchStmt const &s)
		{
			os << string(indent*INDENT_WIDTH, ' ') << "match ";
			print(*s.expr, mod, os);
			os << "\n";
			os << string(indent*INDENT_WIDTH, ' ') << "{\n";
			for(MatchArm const &arm: *s.arms)
			{
				os << string((indent+1)*INDENT_WIDTH, ' ') << "case ";
				print(arm.capture, mod, os);
				os << "\n";
				print(arm.stmt, indent+1, mod, os);
				os << "\n";
			}
			os << string(indent*INDENT_WIDTH, ' ') << "}";
		},
	};
}

void print(Stmt const &stmt, Module const &mod, std::ostream &os)
{
	print(stmt, 0, mod, os);
}

void print(GenericParameter const &param, Module const &mod, std::ostream &os)
{
	param.kind | match
	{
		[&](GenericTypeParameter const&)
		{
			os << param.name;
		},
		[&](GenericValueParameter const &p)
		{
			os << "let " << param.name << ": ";
			print(*p.type, mod, os);
		},
	};
}

void print(FixedArray<GenericParameter> const *type_params, Module const &mod, std::ostream &os)
{
	if(type_params->count)
	{
		os << "'(";
		print(type_params->head(), mod, os);
		for(GenericParameter const &param: type_params->tail())
		{
			os << ", ";
			print(param, mod, os);
		}
		os << ")";
	}
}

void print(Parameter const &param, Module const &mod, std::ostream &os)
{
	os << mod.name_of(param);
	if(param.type)
	{
		os << ": ";
		print(*param.type, mod, os);
	}
	if(param.default_value)
	{
		os << " = ";
		if(Expr *default_value = param.default_value.try_get_expr())
			print(*default_value, mod, os);
		else
			os << "<EXPR_PENDING>";
	}
}

void print(Member const &member, int indent, Module const &mod, std::ostream &os)
{
	member | match
	{
		[&](VarMember const &var_member)
		{
			os << string(indent*INDENT_WIDTH, ' ') << mod.name_of(var_member.var) << ": ";
			print(*var_member.var.type, mod, os);
			if(var_member.var.default_value)
			{
				os << " = ";
				if(Expr *default_value = var_member.var.default_value.try_get_expr())
					print(*default_value, mod, os);
				else
					os << "<EXPR_PENDING>";
			}
		},
		[&](CaseMember case_member)
		{
			print(*case_member.struct_, false, indent, mod, os);
		},
		[&](StructMember struct_member)
		{
			print(*struct_member.struct_, true, indent, mod, os);
		},
	};

	os << ",";
}

void print(StructItem const &struct_, bool is_top_level, int indent, Module const &mod, std::ostream &os)
{
	if(struct_.is_extern)
		os << string(indent*INDENT_WIDTH, ' ') << "extern ";

	if(is_top_level)
		os << string(indent*INDENT_WIDTH, ' ') << "struct ";
	else
		os << string(indent*INDENT_WIDTH, ' ') << "case ";

	os << struct_.name;
	print(struct_.type_params, mod, os);

	if(struct_.members->count)
	{
		os << "\n";
		os << string(indent*INDENT_WIDTH, ' ') << "{\n";
		print(struct_.members->head(), indent+1, mod, os);
		for(Member const &member: struct_.members->tail())
		{
			os << "\n";
			print(member, indent+1, mod, os);
		}
		os << "\n" << string(indent*INDENT_WIDTH, ' ') << "}";
	}
	else
		os << " {}";
}

void print(TopLevelItem const &item, Module const &mod, std::ostream &os)
{
	item | match
	{
		[&](ProcItem const &proc)
		{
			if(proc.is_extern)
				os << "extern ";

			os << "proc " << proc.name;
			print(proc.type_params, mod, os);
			os << "(";
			if(proc.params->count)
			{
				print(proc.params->head(), mod, os);
				for(Parameter const &param: proc.params->tail())
				{
					os << ", ";
					print(param, mod, os);
				}
			}
			os << ")";

			if(proc.ret_type)
			{
				os << " -> ";
				print(*proc.ret_type, mod, os);
			}

			if(proc.body)
			{
				os << "\n";
				print(*proc.body, mod, os);
			}
			else
				os << ";";
		},
		[&](StructItem const &struct_)
		{
			print(struct_, true, 0, mod, os);
		},
		[&](AliasItem const &alias)
		{
			os << "typealias " << alias.name;
			print(alias.type_params, mod, os);
			os << " = ";
			print(*alias.aliased_type, mod, os);
			os << ";";
		},
	};
}

void print(Module const &mod, std::ostream &os)
{
	for(TopLevelItem const &item: to_range(mod.items.list()))
	{
		print(item, mod, os);
		os << "\n\n";
	}
}

//--------------------------------------------------------------------
// Constructors
//--------------------------------------------------------------------
Type* mk_builtin_type(BuiltinTypeDef builtin, Arena &arena)
{
	return arena.alloc<Type>(BuiltinType(UNKNOWN_TOKEN_RANGE, builtin));
}

Type* mk_known_int_type(Int128 low, Int128 high, Arena &arena)
{
	return arena.alloc<Type>(KnownIntType(low, high));
}

Type* mk_pointer_type(Type *pointee, IsMutable mutability, Arena &arena)
{
	return arena.alloc<Type>(PointerType{
		.range = UNKNOWN_TOKEN_RANGE,
		.pointee = pointee,
		.kind = PointerType::SINGLE,
		.mutability = mutability,
	});
}

GenericArg mk_generic_arg(GenericParameter const *param, TokenRange range)
{
	return param->kind | match
	{
		[&](GenericTypeParameter const&) -> GenericArg
		{
			return VarType(range, GenericParameterVar(param));
		},
		[&](GenericValueParameter const&) -> GenericArg
		{
			return GenericVarExpr(range, GenericParameterVar(param));
		},
	};
}

GenericArg mk_generic_arg(GenericDeductionVar var)
{
	return var.def->kind | match
	{
		[&](TypeDeductionVar const&) -> GenericArg
		{
			return VarType(UNKNOWN_TOKEN_RANGE, var);
		},
		[&](ValueDeductionVar const&) -> GenericArg
		{
			return GenericVarExpr(UNKNOWN_TOKEN_RANGE, var);
		},
	};
}

//--------------------------------------------------------------------
// Misc
//--------------------------------------------------------------------
size_t std::hash<TypeInstance>::operator () (TypeInstance const &v) const
{
	return v | match
	{
		[&](StructInstance const *s) { return compute_hash(s); },
		[&](UnionInstance const *s) { return compute_hash(s); },
	};
}

void compute_direct_type_dependencies(Type const &type, unordered_set<TypeInstance> &deps)
{
	return type | match
	{
		[&](BuiltinType const&) {},
		[&](PointerType const&) {},
		[&](ArrayType const &t)
		{
			compute_direct_type_dependencies(*t.element, deps);
		},
		[&](StructType const &t)
		{
			deps.insert(t.inst);
		},
		[&](UnionType const &t)
		{
			deps.insert(t.inst);
		},

		[&](VarType const&) { assert(!"compute_layout: VarType"); },
		[&](ProcType const&) { assert(!"compute_layout: ProcType"); },
		[&](KnownIntType const&) { assert(!"compute_layout: KnownIntType"); },
		[&](ProcTypeUnresolved const&) { assert(!"compute_layout: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) { assert(!"compute_layout: UnionTypeUnresolved"); },
		[&](Path const&) { assert(!"compute_layout: Path"); },
		[&](InlineStructType const&) { assert(!"compute_layout: InlineStructType"); },
	};
}


bool type_var_occurs_in(GenericVar var, Expr const &expr)
{
	bool result = false;
	auto visitor = [&](this auto &self, Expr const &expr) -> void
	{
		expr | match
		{
			[&](GenericVarExpr const &e)
			{
				if(e.var == var)
					result = true;
			},
			[&](auto const&) -> void {}
		};

		expr_visit_children(expr, self);
	};
	visitor(expr);

	return result;
}

bool type_var_occurs_in(GenericVar var, Type const &type)
{
	return type | match
	{
		[&](BuiltinType const&) { return false; },
		[&](KnownIntType const&) { return false; },
		[&](VarType const &t)
		{
			return t.var == var;
		},
		[&](PointerType const &t)
		{
			return type_var_occurs_in(var, *t.pointee);
		},
		[&](ArrayType const &t)
		{
			assert(t.count_arg);
			return type_var_occurs_in(var, *t.count_arg) or type_var_occurs_in(var, *t.element);
		},
		[&](ProcType const &t)
		{
			return t.inst->occurring_vars.contains(var);
		},
		[&](StructType const &t)
		{
			return t.inst->type_args().occurring_vars.contains(var);
		},
		[&](UnionType const &t)
		{
			return t.inst->occurring_vars().contains(var);
		},

		[&](ProcTypeUnresolved const&) -> bool { assert(!"type_var_occurs_in: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> bool { assert(!"type_var_occurs_in: UnionTypeUnresolved"); },
		[&](Path const&) -> bool { assert(!"type_var_occurs_in: Path"); },
		[&](InlineStructType const&) -> bool { assert(!"type_var_occurs_in: InlineStructType"); },
	};
}

bool type_var_occurs_in(GenericVar var, GenericArg const &arg)
{
	return arg | match
	{
		[&](Type const &type) { return type_var_occurs_in(var, type); },
		[&](Expr const &expr) { return type_var_occurs_in(var, expr); }
	};
}

void const_eval(Type &type, Module const &mod)
{
	auto visitor = [&](this auto &self, Type const &type)
	{
		type | match
		{
			[&](ArrayType &t)
			{
				if(t.count_arg)
					const_eval(*t.count_arg, mod);
			},
			[&](auto&) {},
		};
		type_visit_children(type, self);
	};
	visitor(type);
}

static Expr const_eval_int_opt(Int128 a, Int128 b, BinaryOp op, TokenRange range)
{
	switch(op)
	{
		case BinaryOp::ADD: return IntLiteralExpr(range, a + b);
		case BinaryOp::SUB: return IntLiteralExpr(range, a - b);
		case BinaryOp::MUL: return IntLiteralExpr(range, a * b);
		case BinaryOp::DIV: return IntLiteralExpr(range, a / b);
		case BinaryOp::LT: return BoolLiteralExpr(range, a < b);
		case BinaryOp::LE: return BoolLiteralExpr(range, a <= b);
		case BinaryOp::GT: return BoolLiteralExpr(range, a > b);
		case BinaryOp::GE: return BoolLiteralExpr(range, a >= b);
		case BinaryOp::EQ: return BoolLiteralExpr(range, a == b);
		default: assert(!"const_eval_int_opt: invalid op");
	}
}

static Expr const_eval_bool_opt(bool a, bool b, BinaryOp op, TokenRange range)
{
	switch(op)
	{
		case BinaryOp::EQ: return BoolLiteralExpr(range, a == b);
		default: assert(!"const_eval_bool_opt: invalid op");
	}
}

// Best-effort constant evaluation
void const_eval(Expr &expr, Module const &mod)
{
	if(Type *type = expr.try_get_type())
		const_eval(*type, mod);

	expr | match
	{
		[&](IntLiteralExpr const&) {},
		[&](BoolLiteralExpr const&) {},
		[&](UnaryExpr const &e)
		{
			const_eval(*e.sub, mod);
			switch(e.op)
			{
				case UnaryOp::NEG:
				{
					if(IntLiteralExpr const *val = std::get_if<IntLiteralExpr>(e.sub))
						expr = IntLiteralExpr(e.range, -val->value);
				} break;

				case UnaryOp::NOT:
				{
					assert(!"[TODO] const_eval: UnaryExpr: NOT");
				}
			}
		},
		[&](BinaryExpr const &e)
		{
			const_eval(*e.left, mod);
			const_eval(*e.right, mod);
			
			IntLiteralExpr const *left_int = std::get_if<IntLiteralExpr>(e.left);
			IntLiteralExpr const *right_int = std::get_if<IntLiteralExpr>(e.right);
			if(left_int and right_int)
			{
				expr = const_eval_int_opt(left_int->value, right_int->value, e.op, e.range);
				return;
			}

			BoolLiteralExpr const *left_bool = std::get_if<BoolLiteralExpr>(e.left);
			BoolLiteralExpr const *right_bool = std::get_if<BoolLiteralExpr>(e.right);
			if(left_bool and right_bool)
			{
				expr = const_eval_bool_opt(left_bool->value, right_bool->value, e.op, e.range);
				return;
			}
		},
		[&](AsExpr const &e)
		{
			const_eval(*e.src_expr, mod);
			const_eval(*e.target_type, mod);
			*e.target_type | match
			{
				[&](BuiltinType t)
				{
					switch(t.builtin)
					{
						case BuiltinTypeDef::BOOL:
						{
							if(BoolLiteralExpr const *val = std::get_if<BoolLiteralExpr>(e.src_expr))
								expr = *val;
						} break;

						case BuiltinTypeDef::I8:
						case BuiltinTypeDef::U8:
						case BuiltinTypeDef::I32:
						case BuiltinTypeDef::U32:
						case BuiltinTypeDef::ISIZE:
						case BuiltinTypeDef::USIZE:
						{
							if(IntLiteralExpr const *val = std::get_if<IntLiteralExpr>(e.src_expr))
							{
								if(not integer_assignable_to(t.builtin, val->value))
									throw_sem_error("Invalid integer cast", e.range.first, &mod);

								expr = *val;
							}
						} break;

						case BuiltinTypeDef::NEVER:
						case BuiltinTypeDef::UNIT:
						{
							assert(!"[TODO] const_eval: AsExpr: NEVER, UNIT");
						}break;
					}
				},
				[&](auto const&)
				{
					throw_sem_error("Invalid type in constant cast expression", e.target_type->token_range().first, &mod);
				},
			};
		},
		[&](GenericVarExpr const&) {},
		[&](auto const &e)
		{
			throw_sem_error("Expected constant expression", e.range.first, &mod);
		}
	};
}

//--------------------------------------------------------------------
// Special option comments
//--------------------------------------------------------------------
class OptionGatherer
{
public:
	explicit OptionGatherer(Module const &mod) :
		m_mod(&mod)
	{
		for(TopLevelItem const &item: to_range(m_mod->items.list()))
			std::visit(*this, item);

		process_tokens_until_before(TokenIdx(m_mod->parser.token_count()));
	}

	void operator () (StructItem const &struct_)
	{
		TokenIdx first_token_idx = struct_.range.first;
		process_tokens_until_before(first_token_idx);

		PrecedingText preceding_text = get_preceding_text(first_token_idx, m_mod->parser);
		for(Comment const &c: parse_comments(preceding_text))
			process_comment(c, &struct_);

		m_next_token_to_process = first_token_idx + 1;
		for(Member const &member: *struct_.members)
		{
			member | match
			{
				[&](VarMember const &m)    { (*this)({&struct_, &m}); },
				[&](CaseMember const &m)   { (*this)(*m.struct_); },
				[&](StructMember const &m) { (*this)(*m.struct_); },
			};
		}

		process_tokens_until_before(struct_.range.last + 1);
		m_previous_target = &struct_;
	}

	void operator () (pair<StructItem const*, VarMember const*> m)
	{
		VarMember const *var_member = m.second;
		TokenIdx first_token_idx = var_member->var.range.first;
		process_tokens_until_before(first_token_idx);

		PrecedingText preceding_text = get_preceding_text(first_token_idx, m_mod->parser);
		for(Comment const &c: parse_comments(preceding_text))
			process_comment(c, m);

		m_next_token_to_process = first_token_idx + 1;
		process_tokens_until_before(var_member->var.range.last + 1);
		m_previous_target = m;
	}

	void operator () (ProcItem const &proc)
	{
		TokenIdx first_token_idx = proc.range.first;
		process_tokens_until_before(first_token_idx);

		PrecedingText preceding_text = get_preceding_text(first_token_idx, m_mod->parser);
		for(Comment const &c: parse_comments(preceding_text))
			process_comment(c, &proc);

		m_next_token_to_process = first_token_idx + 1;
		if(proc.body)
			stmt_visit(*proc.body, *this);

		process_tokens_until_before(proc.range.last + 1);
		m_previous_target = &proc;
	}

	void operator () (AliasItem const &alias)
	{
		TokenIdx first_token_idx = alias.range.first;
		process_tokens_until_before(first_token_idx);

		PrecedingText preceding_text = get_preceding_text(first_token_idx, m_mod->parser);
		for(Comment const &c: parse_comments(preceding_text))
			process_comment(c, &alias);

		m_next_token_to_process = first_token_idx + 1;
		process_tokens_until_before(alias.range.last + 1);
		m_previous_target = &alias;
	}

	void operator () (Stmt const &stmt)
	{
		TokenIdx first_token_idx = stmt.token_range().first;
		process_tokens_until_before(first_token_idx);

		PrecedingText preceding_text = get_preceding_text(first_token_idx, m_mod->parser);
		for(Comment const &c: parse_comments(preceding_text))
			process_comment(c, &stmt);

		m_next_token_to_process = first_token_idx + 1;
		stmt_visit_children(stmt, *this);

		process_tokens_until_before(stmt.token_range().last + 1);
		m_previous_target = &stmt;
	}

	ModuleOptions&& release()
	{
		return std::move(m_option_sets);
	}

private:
	void process_tokens_until_before(TokenIdx end_idx)
	{
		while(m_next_token_to_process.value < end_idx.value)
		{
			PrecedingText text = get_preceding_text(m_next_token_to_process, m_mod->parser);
			for(Comment const &c: parse_comments(text))
				process_comment(c, nullopt);

			m_next_token_to_process.value += 1;
			m_previous_target = nullopt;
		}
	}

	void process_comment(Comment const &comment, optional<OptionTarget> next_target)
	{
		assert(comment.target);
		if(comment.kind != CommentKind::SPECIAL)
			return;

		switch(*comment.target)
		{
			case CommentTarget::NONE:
			{
				if(not accept_global_options())
					throw std::runtime_error("Global options must appear at the beginning of the file");

				add_options_to_target(comment, TopLevelTarget());
			} break;

			case CommentTarget::PREVIOUS_TOKEN:
			{
				if(not m_previous_target)
					throw std::runtime_error("Option has no target");

				add_options_to_target(comment, *m_previous_target);
			} break;

			case CommentTarget::NEXT_TOKEN:
			{
				if(not next_target)
					throw std::runtime_error("Option has no target");

				add_options_to_target(comment, *next_target);
			} break;

			case CommentTarget::AMBIGUOUS:
				throw std::runtime_error("Option target is ambiguous");
		}
	}

	void add_options_to_target(Comment const &comment, OptionTarget target)
	{
		OptionSet &options = m_option_sets.opts[target];
		parse_options_from_comment(comment, options);
		if(options.opts.empty())
			m_option_sets.opts.erase(target);
	}

	bool accept_global_options() const
	{
		return m_next_token_to_process.value == 0;
	}

	Module const *m_mod;
	ModuleOptions m_option_sets;
	optional<OptionTarget> m_previous_target;
	TokenIdx m_next_token_to_process{0};
};

ModuleOptions gather_options(Module const &mod)
{
	OptionGatherer gatherer(mod);
	return gatherer.release();
}

void print_options(OptionSet const &options, std::ostream &os)
{
	vector<string_view> keys;
	keys.append_range(ranges::views::keys(options.opts));
	ranges::sort(keys);

	for(string_view key: keys)
		os << "  " << key << ": " << options.opts.at(key) << std::endl;
}

string str(OptionTarget target, Module const &mod)
{
	return target | match
	{
		[&](TopLevelTarget) { return "TopLevel"s; },
		[&](StructItem const *item)
		{
			return "struct "s + item->name;
		},
		[&](pair<StructItem const*, VarMember const*> m)
		{
			return string(m.first->name) + "::" + mod.name_of(m.second->var);
		},
		[&](ProcItem const *item)
		{
			return "proc "s + item->name;
		},
		[&](AliasItem const *item)
		{
			return "typealias "s + item->name;
		},
		[&](Stmt const *stmt)
		{
			return *stmt | match
			{
				[&](LetStmt const&)
				{
					return "LetStmt"s;
				},
				[&](ExprStmt const&)
				{
					return "ExprStmt"s;
				},
				[&](BlockStmt const&)
				{
					return "BlockStmt"s;
				},
				[&](ReturnStmt const&)
				{
					return "ReturnStmt"s;
				},
				[&](IfStmt const&)
				{
					return "IfStmt"s;
				},
				[&](WhileStmt const&)
				{
					return "WhileStmt"s;
				},
				[&](MatchStmt const&)
				{
					return "MatchStmt"s;
				},
				[&](DeclStmt const&)
				{
					return "DeclStmt"s;
				}
			};
		},
	};
}
