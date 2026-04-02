#include "semantics/context.hpp"
#include "semantics/module.hpp"

optional<EventLogger>& SemaContext::logger()
{
	return mod->sema->logger;
}
