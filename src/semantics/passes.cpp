#include "semantics/passes.hpp"
#include "semantics/context.hpp"
#include "semantics/module.hpp"

void sema(Module &mod, Arena &arena, optional<EventLogger> &&logger)
{
	SemaContext ctx(mod, arena);
	ctx.mod->sema = std::make_unique<SemaModule>(std::make_unique<Scope>(ctx.mod, true), ctx.arena);
	if(logger)
		ctx.mod->sema->logger = std::move(logger);

	LOGGER(ctx.mod->sema->logger, on_declare_items_start);
	{
		register_items(ctx);
	}
	LOGGER(ctx.mod->sema->logger, on_declare_items_end);

	LOGGER(ctx.mod->sema->logger, on_resolve_names_start);
	{
		resolve_names(ctx);
	}
	LOGGER(ctx.mod->sema->logger, on_resolve_names_end);

	LOGGER(ctx.mod->sema->logger, on_typecheck_start);
	{
		typecheck(ctx);
	}
	LOGGER(ctx.mod->sema->logger, on_typecheck_end);

	{
		check_default_values(ctx);
	}

	LOGGER(ctx.mod->sema->logger, on_layout_computation_start);
	{
		compute_type_layouts(mod);
	}
	LOGGER(ctx.mod->sema->logger, on_layout_computation_end);
}
