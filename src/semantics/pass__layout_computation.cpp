#include "semantics/module.hpp"
#include "semantics/passes.hpp"
#include "semantics/instance_registry.hpp"

MemoryLayout get_layout(BuiltinTypeDef type)
{
	switch(type)
	{
		case BuiltinTypeDef::NEVER: return {.size = 0, .alignment = 0};
		case BuiltinTypeDef::UNIT:  return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::BOOL:  return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::I8:    return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::U8:    return {.size = 1, .alignment = 1};
		case BuiltinTypeDef::I32:   return {.size = 4, .alignment = 4};
		case BuiltinTypeDef::U32:   return {.size = 4, .alignment = 4};
		case BuiltinTypeDef::ISIZE: return {.size = 8, .alignment = 8};
		case BuiltinTypeDef::USIZE: return {.size = 8, .alignment = 8};
	}

	UNREACHABLE;
}

MemoryLayout compute_layout(Type const &type)
{
	return type | match
	{
		[&](BuiltinType const &t)
		{
			return get_layout(t.builtin);
		},
		[&](PointerType const&)
		{
			return MemoryLayout{.size = 8, .alignment = 8};
		},
		[&](ArrayType const &t)
		{
			MemoryLayout element_layout = compute_layout(*t.element);
			return MemoryLayout{
				.size = element_layout.size * t.count(),
				.alignment = element_layout.alignment,
			};
		},
		[&](StructType const &t)
		{
			assert(t.inst->is_concrete());

			StructInstance *cur = t.inst;
			while(cur->is_case_member())
				cur = cur->variant_parent();

			if(is_optional_ptr(cur))
				return MemoryLayout{.size = 8, .alignment = 8};

			return cur->compute_own_layout();
		},
		[&](UnionType const &t) -> MemoryLayout
		{
			return t.inst->layout();
		},

		[&](VarType const&) -> MemoryLayout { assert(!"compute_layout: VarType"); },
		[&](ProcType const&) -> MemoryLayout { assert(!"compute_layout: ProcType"); },
		[&](KnownIntType const&) -> MemoryLayout { assert(!"compute_layout: KnownIntType"); },
		[&](ProcTypeUnresolved const&) -> MemoryLayout { assert(!"compute_layout: ProcTypeUnresolved"); },
		[&](UnionTypeUnresolved const&) -> MemoryLayout { assert(!"compute_layout: UnionTypeUnresolved"); },
		[&](Path const&) -> MemoryLayout { assert(!"compute_layout: Path"); },
		[&](InlineStructType const&) -> MemoryLayout { assert(!"compute_layout: InlineStructType"); },
	};
}

Type const* is_optional_ptr(Type const &type)
{
	StructType const *struct_type = std::get_if<StructType>(&type);
	if(not struct_type)
		return nullptr;

	return is_optional_ptr(struct_type->inst);
}

Type const* is_optional_ptr(StructInstance const *struct_)
{
	while(struct_->is_case_member())
		struct_ = struct_->variant_parent();

	if(struct_->struct_()->name == "Option")
	{
		Type const &first_arg = std::get<Type>(struct_->type_args().args->items[0]);
		if(is<PointerType>(first_arg))
			return &first_arg;
	}

	return nullptr;
}

void compute_type_layouts(Module &mod)
{
	for(StructInstance &struct_: mod.sema->insts.struct_instances())
	{
		if(not struct_.is_concrete())
			continue;

		struct_.compute_own_layout();
	}
}
