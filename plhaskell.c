#include <dlfcn.h>

#include <postgres.h>
#include <executor/spi.h>
#include <commands/trigger.h>
#include <fmgr.h>
#include <access/heapam.h>
#include <utils/syscache.h>
#include "utils/builtins.h"
#include <catalog/pg_proc.h>
#include <catalog/pg_type.h>
#include "postgres.h"
#include "fmgr.h"
#include <string.h>

#include "access/htup_details.h"
#include "access/transam.h"
#include "funcapi.h"
#include "utils/hsearch.h"

#define CAT(a,b) XCAT(a,b)
#define XCAT(a,b) a ## b
#define STR(a) XSTR(a)
#define XSTR(a) #a

#ifdef PG_MODULE_MAGIC
PG_MODULE_MAGIC;
#endif

#include "dist/build/PlHaskell_stub.h"

extern void _PG_init(void);

PG_FUNCTION_INFO_V1(plhaskell_call_handler);

void plhaskell_init (void);

void
_PG_init(void)
{
  plhaskell_init();
}

Datum
plhaskell_call_handler(PG_FUNCTION_ARGS)
{
    Oid fn_oid = fcinfo->flinfo->fn_oid;
    HeapTuple procTup = SearchSysCache1(PROCOID, ObjectIdGetDatum(fn_oid));

    bool isnull;
    Datum prosrcdatum = SysCacheGetAttr(PROCOID, procTup, Anum_pg_proc_prosrc, &isnull);
    if (isnull)
        elog(ERROR, "null prosrc");

	Form_pg_proc procStruct = (Form_pg_proc) GETSTRUCT(procTup);

    Oid *argTypes;
    char **argNames;
    char *argModes;
    int argCount = get_func_arg_info(procTup, &argTypes, &argNames, &argModes);

    Datum *datum = malloc(sizeof(Datum));

    int r = plhaskell_test(
        TextDatumGetCString(prosrcdatum),
        procStruct->prorettype,
        datum,
        argTypes,
        argCount,
        fcinfo->arg,
        sizeof(Datum)
    );

    if (r < 0)
      elog(ERROR, (const char *) (*datum));
    if (r > 0)
      SET_VARSIZE (*datum, r);

    return *datum;
}
