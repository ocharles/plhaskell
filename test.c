#include <HsFFI.h>

#ifdef __GLASGOW_HASKELL__
#include "PlHaskell_stub.h"
#endif

#include <postgres.h>
#include <executor/spi.h>
#include <commands/trigger.h>
#include <fmgr.h>
#include <access/heapam.h>
#include <utils/syscache.h>
#include <catalog/pg_proc.h>
#include <catalog/pg_type.h>

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

PG_FUNCTION_INFO_V1(plhaskell_call_handler);

Datum
plhaskell_call_handler(PG_FUNCTION_ARGS)
{
    Datum retval;

    hs_init(0, 0);

    hs_exit();

    return retval;
}
